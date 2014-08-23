#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:plaster)

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
         (paste (dm:get-one 'plaster (db:query (:and (:= '_id (hash->id (or id (get-var "id")))) (:= 'pid -1)))))
         (err NIL))
    (cond
      ((not paste)
       (setf err "No such paste found."))
      ((not (paste-accessible-p paste user))
       (if (= (dm:field paste "view") 3)
           ($ "#content" (html-file (template "plaster/passwordprompt.html")))
           ($ "#content" (html "<h2>You are not allowed to view this paste.</h2>"))))
      (T
       (setf (dm:field paste "editable") (paste-editable-p paste user))
       (when (= (dm:field paste "view") 3)
         ($ ".editorbar button" (each #'(lambda (node) ($ node (attr :formaction (format NIL "~a&password=~a" ($ node (attr :formaction) (node)) (get-var "password"))))))))
       (when user
         (when-let ((model (dm:get-one 'plaster-users (db:query (:= 'user (user:username user))))))
           ($ "#editorthemescript" (text (format NIL "window.mirrorTheme=\"~a\";" (dm:field model "theme"))))))
       (db:update 'plaster (db:query (:= '_id (dm:id paste))) `((hits . ,(1+ (dm:field paste "hits")))))))
    (r-clip:process
     ($ (node))
     :user user
     :paste paste
     :error err
     :annots (when paste
               (mapc #'(lambda (model)
                           (when (= (dm:field model "view") 3)
                             (setf (dm:field model "text") (decrypt (dm:field model "text") (get-var "password")))
                             (setf (dm:field model "editable") (paste-editable-p model user))))
                       (dm:get 'plaster (db:query (:= 'pid (dm:id paste))) :sort '((time :ASC))))))))

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
                (handler-case (paste-add text (post-var "annotate") title type view
                                         password (post-var "captcha") (post-var "hash") "true")
                  (radiance-error (err) (message err))))))
    (if accessible
        (if (or (config-tree :plaster :anon) user)
            (progn
              ($ ".code" (text text))
              ($ "#title" (val title))
              ($ "#viewpassword" (val password))
              (when user
                (when-let ((model (dm:get-one 'plaster-users (db:query (:= 'user (user:username user))))))
                  ($ "#editorthemescript" (text (format NIL "window.mirrorTheme=\"~a\";" (dm:field model "theme"))))
                  (unless type
                    (setf type (dm:field model "default-type"))))))
            (setf err "Anonymous pasting is not permitted. Please log in first."))
        (setf err "You are not allowed to repaste/annotate this paste."))
    (multiple-value-bind (captcha hash) (generate-captcha)
      (r-clip:process
       ($ (node))
       :user user
       :captcha captcha
       :captcha-hash hash
       :error err
       :types (dm:get 'plaster-types (db:query :all) :sort '((title :ASC)))))
    ($ (inline (format NIL "#viewselect option[value=\"~a\"]" view)) (attr :selected "selected"))
    (when annotate
      ($ "#annotateinfo" (text (format NIL "Annotating paste ~a." (id->hash (dm:field annotate "_id")))))
      ($ "#viewselect" (parent) (replace-with "public/private depending on its parent"))
      ($ "#annotateid" (attr :value (id->hash (dm:field annotate "_id")))))
    ($ (inline (format NIL "#typeselect option[value=\"~a\"]" (or type "text/plain"))) (attr :selected "selected"))))

(define-page plaster-edit #@"plaster/^edit(/([0-9a-zA-Z]*))?" (:uri-groups (NIL id) :lquery (template "edit.html"))
  (let* ((user (or (auth:current) (user:get :anonymous)))
         (paste (dm:get-one 'plaster (db:query (:= '_id (hash->id (or id (post/get "id")))))))
         (err))
    (cond
      ((not paste)
       (setf err "No such paste found."))
      ((not (paste-editable-p paste user))
       (setf err "You are not allowed to edit this paste."
             paste NIL))
      (T
       (if (not (paste-accessible-p paste user))
           ($ "#content" (html-file (template "plaster/passwordprompt.html")))
           (let* ((text (or (post-var "text") (dm:field paste "text")))
                  (title (or (post-var "title") (dm:field paste "title")))
                  (type (or (post-var "type") (dm:field paste "type")))
                  (view (or (post-var "view") (dm:field paste "view")))
                  (password (post/get "password")))
             (setf err (handler-case
                           (cond ((string= (post-var "action") "edit")
                                  (paste-edit (or id (post/get "id")) text title type view password "true"))
                                 ((string= (post-var "action") "delete")
                                  (paste-delete (or id (post/get "id")) password "true")))
                         (radiance-error (err)
                           (message err))))
             (when-let ((model (dm:get-one 'plaster-users (db:query (:= 'user (user:username user))))))
               ($ "#editorthemescript" (text (format NIL "window.mirrorTheme=\"~a\";" (dm:field model "theme")))))
             ($ "#title" (attr :value title))
             ($ "#viewpassword" (attr :value (or password "")))))))
    (r-clip:process
     ($ (node))
     :user user
     :paste paste
     :error err
     :types (dm:get 'plaster-types (db:query :all) :sort '((title :ASC))))
    (when paste
      ($ (inline (format NIL "#typeselect option[value=\"~a\"]" (or (post-var "type") (dm:field paste "type")))) (attr :selected "selected"))
      ($ (inline (format NIL "#viewselect option[value=\"~a\"]" (or (post-var "view") (dm:field paste "view")))) (attr :selected "selected")))))
