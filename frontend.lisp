#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:plaster)

(defvar *default-salt* "gHjjaL213adjz9AC")
(defvar *user-pastes-per-page* 20)
(defvar *public-pastes-limit* 20)

(define-trigger db:connected ()
  ;; PID: -1 or _id of annotated parent.
  ;; Type: Has to be a type-name in plaster-types.
  ;; View: 0 Public, 1 Unlisted, 2 Private, 3 Encrypted
  (db:create 'plaster '((pid :id) (title (:varchar 64)) (author (:varchar 32)) (type (:varchar 16))
                        (time (:integer 5)) (text :text) (view (:integer 2)) (hits :integer) (ip (:varchar 4)))
             :indices '(pid author ip))
  (db:create 'plaster-types '((title (:varchar 16)) (name (:varchar 64)) (mime (:varchar 16))))
  (db:create 'plaster-users '((user (:varchar 32)) (theme (:varchar 32)) (default-type (:varchar 16))) :indices '(user))
  (db:create 'plaster-themes '((title (:varchar 32)) (name (:varchar 32)))))

(defun id->hash (id) (write-to-string id :base 36))

(defun hash->id (hash) (parse-integer hash :radix 36))

(defun user-url (user)
  (format NIL "/user/~a" (user:username (if (stringp user) (user:get user) user))))

(defun paste-url (id &optional annot)
  (format NIL "/view/~a~@[#annotation-~a~]" (id->hash id) (when annot (id->hash annot))))

(defun type->title (type)
  (gethash "title" (first (db:select 'plaster-types (db:query (:= 'mime type)) :fields '(title) :amount 1))))

(defun type->mode (type)
  (gethash "name" (first (db:select 'plaster-types (db:query (:= 'mime type)) :fields '(name) :amount 1))))

(defun format-time (time)
  (local-time:format-timestring
   NIL (local-time:unix-to-timestamp time)
   :format '((:year 4) #\. (:month 2) #\. (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2))))

(defmacro string-or (default &rest values)
  (let ((var (gensym)) (def (gensym)))
    `(let* ((,def ,default)
            (,var (or ,@values "")))
       (if (= 0 (length ,var)) ,def ,var))))

(defun crlf->lf (string)
  (cl-ppcre:regex-replace-all (format NIL "~C~C" #\return #\linefeed) string (string #\linefeed)))

(defun encrypt (text password)
  (let ((salt (or (config-tree :plaster :encrypt-salt) *default-salt*)))
    (when (< (length text) 16)
      (setf text (concatenate 'string text (make-string (- 16 (length text)) :initial-element #\Space))))
    (concatenate
     'string
     (cryptos:simple-hash text salt :iterations 1 :digest 'ironclad:md5) "-"
     (cryptos:encrypt text (cryptos:pbkdf2-key password salt :digest :sha256 :iterations 1)))))

(defun decrypt (text password)
  (destructuring-bind (hash text) (cl-ppcre:split "-" text)
    (let* ((salt (or (config-tree :plaster :encrypt-salt) *default-salt*))
           (decrypted (cryptos:decrypt text (cryptos:pbkdf2-key password salt :digest :sha256 :iterations 1)))
           (hashed (cryptos:simple-hash decrypted salt :iterations 1 :digest 'ironclad:md5)))
      (when (string-equal hashed hash)
        decrypted))))

(defun paste-accessible-p (paste &optional (user (auth:current)))
  (and paste
       (or (and (or (not (= (dm:field paste "view") 2))
                    (and user (string-equal (user:username user) (dm:field paste "author"))))
                (or (not (= (dm:field paste "view") 3))
                    (and (post/get "password")
                         (< 0 (length (post/get "password")))
                         ;; We've come this far, decrypt it and set it so we don't have to do it twice.
                         (setf (field paste "text")
                               (decrypt (dm:field paste "text") (post/get "password")))))
                ;; View permissions cascade from parent, so check it.
                (or (= (dm:field paste "pid") -1)
                    (paste-accessible-p (dm:get-one 'plaster (db:query (:= '_id (dm:field paste "pid")))) user)))
           (and user (user:check user "plaster.admin")))))

(defparameter *captcha-salt* (make-random-string))
(defparameter *captchas* '("divisible" "determined" "questionable" "difficult" "simplistic" "always" "never" "however" "occasionally" "certainly"
                           "creative" "video" "games" "whatever" "realistic" "severe" "explosion" "wizard" "witch" "confederation"
                           "united" "guess" "estimate" "uncertainty" "forgetful" "loathing" "nevermind" "incorrect" "detective" "deduction"
                           "reasoning" "evidence" "incident" "curiosity" "thoughtful" "assemble" "story" "conclusion" "possibility" "culprit"
                           "solved" "probability" "equation" "careful" "consider" "detail" "problematic" "complication" "comparison" "doubt"))
(defun generate-captcha ()
  (let* ((el (random-elt *captchas*))
         (elmix (copy-seq el)))
    (loop for i from 0 below 2
          do (setf (elt elmix (+ (random (- (length el) 2)) 1)) #\-))
    (values
     elmix
     (cryptos:pbkdf2-hash el *captcha-salt*))))

;; TODO: Take care of error reporting problems of api functions.
(defun paste-editable-p (user paste)
  (and user (or (user:check user "plaster.admin")
                (string-equal (dm:field paste "author") (user:username user)))))

(define-page plaster-index #@"plaster/^$" ()
  (redirect "/new"))

(define-page plaster-list #@"plaster/^recent" (:lquery (template "list.html"))
  (r-clip:process ($ (node)) :pastes (dm:get 'plaster (db:query (:and (:= 'view 0) (:= 'pid -1))) :sort '((time :DESC)) :amount *public-pastes-limit*)))

(define-page plaster-user #@"plaster/^user/([^/]*)(/([0-9]+))?" (:uri-groups (username NIL page) :lquery (template "user.html"))
  (let ((user (auth:current))
        (viewuser (user:get username))
        (page (or (parse-integer (or page "") :junk-allowed T) 1)))
    (r-clip:process
     ($ (node))
     :user viewuser
     :pastes (if (and user (string= (user:username user) (user:username viewuser)))
                 (dm:get 'plaster (db:query (:and (:= 'pid -1) (:= 'author username))) :sort '((time :DESC)))
                 (dm:get 'plaster (db:query (:and (:= 'pid -1) (:= 'view 0) (:= 'author username)))
                         :sort '((time :DESC)) :amount *user-pastes-per-page* :skip (* *user-pastes-per-page* (1- page))))
     :annots (if (and user (string= (user:username user) (user:username viewuser)))
                 (dm:get 'plaster (db:query (:and (:!= 'pid -1) (:= 'author username))) :sort '((time :DESC)))
                 (dm:get 'plaster (db:query (:and (:!= 'pid -1) (:= 'view 0) (:= 'author username)))
                         :sort '((time :DESC)) :amount *user-pastes-per-page* :skip (* *user-pastes-per-page* (1- page)))))))

(define-page plaster-view #@"plaster/^view(/([0-9a-zA-Z]*))?" (:uri-groups (NIL id) :lquery (template "view.html"))
  (let* ((user (auth:current))
         (paste (dm:get-one 'plaster (db:query (:and (:= '_id (hash->id (or id (get-var "id")))) (:= 'pid -1))))))
    (cond
      ((not paste)
       ($ "#content" (html "<h2>No such paste found.</h2>")))
      ((not (paste-accessible-p paste user))
       (if (= (dm:field paste "view") 3)
           ($ "#content" (html-file (template "plaster/passwordprompt.html")))
           ($ "#content" (html "<h2>You are not allowed to view this paste.</h2>"))))
      (T
       (setf (dm:field paste "editable") (paste-editable-p user paste))
       (when (= (dm:field paste "view") 3)
         ($ ".editorbar button" (each #'(lambda (node) ($ node (attr :formaction (format NIL "~a&password=~a" ($ node (attr :formaction) (node)) (get-var "password"))))))))
       (when-let ((model (dm:get-one 'plaster-users (db:query (:= 'user (user:username user))))))
         ($ "#editorthemescript" (text (format NIL "window.mirrorTheme=\"~a\";" (dm:field model "theme")))))
       (db:update 'plaster (db:query (:= '_id (dm:id paste))) `((hits . ,(1+ (dm:field paste "hits")))))))
    (r-clip:process
     ($ (node))
     :user user
     :paste paste
     :annots (mapcar #'(lambda (model)
                         (when (= (dm:field model "view") 3)
                           (setf (dm:field model "text") (decrypt (dm:field model "text") (get-var "password")))
                           (setf (dm:field model "editable") (paste-editable-p user model))))
                     (dm:get 'plaster (db:query (:= 'pid (dm:field paste "_id"))) :sort '(("time" . :ASC)))))))

(define-page plaster-new #@"plaster/^new" (:lquery (template "new.html"))
  (let* ((user (auth:current))
         (annotate (when-let ((annotate-id (get-var "annotate")))
                     (dm:get-one 'plaster (db:query (:and (:= '_id (hash->id annotate-id)) (:= 'pid -1))))))
         (repaste (when-let ((repaste-id (get-var "repaste")))
                    (dm:get-one 'plaster (db:query (:= '_id (hash->id repaste-id))))))
         ;; We have to do this here due to paste-accessible-p's side-effecting decryption.
         (accessible (or (and (not annotate) (not repaste))
                         (paste-accessible-p (or annotate repaste) user)))
         (text (crlf->lf (or (post-var "text")
                             (when annotate (dm:field annotate "text"))
                             (when repaste (dm:field repaste "text"))
                             "")))
         (type (or (post-var "type")
                   (when annotate (dm:field annotate "type"))
                   (when repaste (dm:field repaste "type"))))
         (title (or (post-var "title") ""))
         (view (post-var "view"))
         (password (or (post/get "password") ""))
         (err (when (string= (post-var "action") "paste")
                (handler-case (make-api-call "plaster/paste" :text text :annotate (post-var "annotate") :title title :type type
                                                             :view view :captcha (post-var "captcha") :hash (post-var "hash")
                                                             :password password :client "true")
                  (radiance-error (err) (message err))))))

    (if accessible
        (if (or (config-tree :plaster :anon) (not (string-equal (user:username user) "temp")))
            (progn
              ($ ".code" (text text))
              ($ (inline (format NIL "#viewselect option[value=\"~a\"]" view)) (attr :selected "selected"))
              ($ "#title" (val title))
              ($ "#viewpassword" (val password))
              (when-let ((model (dm:get-one 'plaster-users (db:query (:= 'user (user:username user))))))
                ($ "#editorthemescript" (text (format NIL "window.mirrorTheme=\"~a\";" (dm:field model "theme"))))
                (unless type
                  (setf type (dm:field model "default-type"))))
              (when annotate
                ($ "#annotateinfo" (text (format NIL "Annotating paste ~a." (id->hash (dm:field annotate "_id")))))
                ($ "#viewselect" (parent) (replace-with "public/private depending on its parent"))
                ($ "#annotateid" (attr :value (id->hash (dm:field annotate "_id")))))
              ($ (inline (format NIL "#typeselect option[value=\"~a\"]" (or type "text/plain"))) (attr :selected "selected")))
            ($ "#content" (html "<h2>Anonymous pasting is not permitted. Please log in first.</h2>")))
        ($ "#content" (html "<h2>You are not allowed to repaste/annotate this paste.</h2>")))
    (multiple-value-bind (captcha hash) (generate-captcha)
      (r-clip:process
       ($ (node))
       :user user
       :captcha captcha
       :captcha-hash hash
       :error err
       :types (dm:get 'plaster-types (db:query :all) :sort '((title :ASC)))))))

;; Still needs to be fixed.
(define-page plaster-edit #@"plaster/^edit(/([0-9a-zA-Z]*))?" (:uri-groups (NIL id) :lquery (template "edit.html"))
  (let* ((user (auth:current))
         (paste (dm:get-one 'plaster (db:query (:= '_id (hash->id (or id (get-var "id"))))))))
    (cond
      ((not paste)
       ($ "#content" (html "<h2>No such paste found.</h2>")))
      ((not (paste-editable-p user paste))
       ($ "#content" (html "<h2>You are not allowed to view this paste.</h2>")))
      (T
       (if (not (paste-accessible-p paste user))
           ($ "#content" (html-file (template "plaster/passwordprompt.html")))
           (let* ((text (or (post-var "text") (dm:field paste "text")))
                  (title (or (post-var "title") (dm:field paste "title")))
                  (type (or (post-var "type") (dm:field paste "type")))
                  (view (or (post-var "view") (dm:field paste "view")))
                  (password (post/get "password"))
                  (err (handler-case
                           (cond ((string= (post-var "action") "edit")
                                  (api-call "plaster/paste" :id (get-var "id") :text text :title title :type type
                                                            :view view :password password :client "true"))
                                 ((string= (post-var "action") "delete")
                                  (api-call "plaster/paste" :DELETE (get-var "id") :password password :client "true")))
                         (radiance-error (err) (text err)))))
             (when-let ((model (dm:get-one 'plaster-users (db:query (:= 'user (user:username user))))))
               ($ "#editorthemescript" (text (format NIL "window.mirrorTheme=\"~a\";" (dm:field model "theme")))))
             ($ "#title" (attr :value title))
             ($ (inline (format NIL "#typeselect option[value=\"~a\"]" type)) (attr :selected "selected"))
             ($ (inline (format NIL "#viewselect option[value=\"~a\"]" view)) (attr :selected "selected"))
             ($ "#viewpassword" (attr :value password))
             ($ "#editid" (attr :value (id->hash (dm:field paste "_id"))))
             ($ ".code" (text text))))))
    (r-clip:process
     ($ (node))
     :user user
     :paste paste
     :error err
     :types (dm:get 'plaster-types (db:query :all) :sort '((title :ASC))))))
