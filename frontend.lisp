(in-package #:plaster)

(defparameter *pastes-per-page* 25)
(defparameter *password-salt* "Something ˢᵉᶜʳᵉᵗ")

(define-trigger db:connected ()
  (db:create 'plaster-pastes '((title (:varchar 32))
                               (time (:integer 5))
                               (author (:varchar 32))
                               (visibility (:integer 1))
                               (password (:varchar 128))
                               (text :text)))
  (db:create 'plaster-annotations '((paste :id)
                                    (annotation :id))))

(define-trigger user:ready ()
  (user:add-default-permissions
   (perm plaster paste new)
   (perm plaster paste view)
   (perm plaster paste list)
   (perm plaster paste user)
   (perm plaster paste edit own)
   (perm plaster paste delete own))

  (user:grant
   "anonymous"
   (perm plaster paste new)
   (perm plaster paste view)
   (perm plaster paste list)
   (perm plaster paste user)))

(defun ensure-paste (paste-ish)
  (etypecase paste-ish
    (dm:data-model paste-ish)
    (string (ensure-paste (parse-integer paste-ish)))
    (integer (or (dm:get-one 'plaster-pastes (db:query (:= '_id paste-ish)))
                 (error 'request-not-found :message (format NIL "No paste with ID ~a was found." paste-ish))))))

(defun paste-annotations (paste)
  (let* ((paste (ensure-paste paste))
         (rels (dm:get 'plaster-annotations (db:query (:= 'paste (dm:id paste))))))
    (loop for rel in rels
          collect (ensure-paste (dm:field rel "annotation")))))

(defun paste-parent (paste)
  (let* ((paste (ensure-paste paste))
         (rel (dm:get-one 'plaster-annotations (db:query (:= 'annotation (dm:id paste))))))
    (when rel
      (ensure-paste (dm:field rel "paste")))))

(defun ensure-visibility (vis-ish)
  (etypecase (or* vis-ish)
    (null 1)
    (integer (check-type vis-ish (integer 1 3)) vis-ish)
    (string (cond ((string-equal vis-ish "public") 1)
                  ((string-equal vis-ish "unlisted") 2)
                  ((string-equal vis-ish "private") 3)
                  (T (ensure-visibility (parse-integer vis-ish)))))))

(defun ensure-password (visibility password)
  (let ((password (or* password))
        (visibility (ensure-visibility visibility)))
    (when (and (= visibility 3) (not password))
      (api-error "A password is required for private visibility."))
    (when (and (/= visibility 3) password)
      (api-error "Cannot set a password on public or unlisted visibility.")))
  (when password (cryptos:pbkdf2-hash password *password-salt*)))

(defun register-annotation (annotation paste)
  (when (paste-parent paste)
    (api-error "Cannot annotate an annotation."))
  (db:insert 'plaster-annotations
             `(("paste" . ,(dm:id (ensure-paste paste)))
               ("annotation" . ,(dm:id (ensure-paste annotation))))))

(defun create-paste (text &key title parent visibility password author)
  (when (and parent visibility)
    (api-error "Cannot set the visibility of an annotation."))
  (db:with-transaction ()
    (let* ((paste (dm:hull 'plaster-pastes))
           (parent (when parent (ensure-paste parent)))
           (visibility (if parent 2 (ensure-visibility visibility)))
           (password (ensure-password visibility password)))
      (setf (dm:field paste "text") text
            (dm:field paste "title") (or title "")
            (dm:field paste "time") (get-universal-time)
            (dm:field paste "author") author
            (dm:field paste "visibility") visibility
            (dm:field paste "password") password)
      (dm:insert paste)
      (when parent (register-annotation paste parent))
      paste)))

(defun delete-paste (paste)
  (db:with-transaction ()
    (let ((paste (ensure-paste paste)))
      (mapc #'dm:delete (paste-annotations paste))
      (db:remove 'plaster-annotations
                 (db:query (:or (:= 'paste (dm:id paste))
                                (:= 'annotation (dm:id paste)))))
      (dm:delete paste)
      paste)))

(defun edit-paste (paste &key text title visibility password)
  (db:with-transaction ()
    (let* ((paste (ensure-paste paste)))
      (when text
        (setf (dm:field paste "text") text))
      (when title
        (setf (dm:field paste "title") title))
      (when (and (paste-parent paste) visibility)
        (api-error "Cannot set the visibility of an annotation."))
      (when visibility
        (setf (dm:field paste "visibility") (ensure-visibility visibility)))
      (when password
        (setf (dm:field paste "password") (ensure-password (or visibility (dm:field paste "visibility"))
                                                           password)))
      (dm:save paste))))

(defun paste-url (paste &optional (parent (paste-parent paste)))
  (let ((paste (ensure-paste paste)))
    (make-url :domains '("plaster")
              :path (format NIL "view/~a"
                            (if parent
                                (dm:id parent)
                                (dm:id paste)))
              :fragment (princ-to-string (dm:id paste)))))

(defun api-paste-output (paste)
  (cond ((string= "true" (post/get "browser"))
         (redirect (paste-url paste)))
        (T
         (api-output (loop for field in (dm:fields paste)
                           collect (cons field (dm:field paste field)))))))

(defun call-with-password-protection (function paste &optional (password (post/get "password")))
  (cond ((or (not (eql 3 (dm:field paste "visibility")))
             (string= (dm:field paste "password") (cryptos:pbkdf2-hash password *password-salt*)))
         (funcall function))
        ((or* password)
         (error 'request-denied :message "The supplied password is incorrect."))
        (T
         (r-clip:process (@template "password.ctml")))))

(defmacro with-password-protection ((paste &optional (password '(post/get "password"))) &body body)
  `(call-with-password-protection
    (lambda () ,@body) ,paste ,password))

(define-page edit "plaster/edit(/(.*))?" (:uri-groups (NIL id) :lquery "edit.ctml")
  (let* ((paste (if id
                    (ensure-paste id)
                    (dm:hull 'plaster-pastes)))
         (parent (if id
                     (paste-parent paste)
                     (when (get-var "annotate") (ensure-paste (get-var "annotate"))))))
    (if id
        (check-permission '(edit delete) paste)
        (check-permission 'new))
    (with-password-protection ((or parent paste))
      (r-clip:process T :paste paste
                        :password (post/get "password")
                        :parent (when parent (dm:id parent))
                        :repaste (get-var "repaste")
                        :error (get-var "error")
                        :message (get-var "message")))))

(define-page view "plaster/view/(.*)" (:uri-groups (id) :lquery "view.ctml")
  (let* ((paste (ensure-paste id))
         (parent (paste-parent paste)))
    (check-permission 'view paste)
    (if parent
        (redirect (paste-url paste parent))
        (with-password-protection (paste)
          (r-clip:process T :paste paste
                            :password (post/get "password")
                            :annotations (sort (paste-annotations paste)
                                               #'< :key (lambda (a) (dm:field a "time"))))))))

(define-page list "plaster/list(/(.*))?" (:uri-groups (NIL page) :lquery "list.ctml")
  (check-permission 'list)
  (let* ((page (or (when page (parse-integer page :junk-allowed T)) 0))
         (pastes (dm:get 'plaster-pastes (db:query (:= 'visibility 1))
                         :sort '((time :DESC))
                         :skip (* page *pastes-per-page*)
                         :amount *pastes-per-page*)))
    (r-clip:process T :pastes pastes
                      :page page
                      :has-more (<= *pastes-per-page* (length pastes)))))

(define-page user "plaster/user/(.*)(/(.*))?" (:uri-groups (username NIL page) :lquery "user.ctml")
  (check-permission 'user)
  (let* ((page (or (when page (parse-integer page :junk-allowed T)) 0))
         (user (user:get username)))
    (unless user
      (error 'request-not-found :message (format NIL "No such user ~s." username)))
    (let ((pastes (dm:get 'plaster-pastes
                          (if (and (auth:current) (or (eql (auth:current) user)
                                                      (user:check (auth:current) '(perm plaster))))
                              (db:query (:= 'author username))
                              (db:query (:and (:= 'author username)
                                              (:= 'visibility 1))))
                          :sort '((time :DESC))
                          :skip (* page *pastes-per-page*)
                          :amount *pastes-per-page*)))
      (r-clip:process T :pastes pastes
                        :user user
                        :username (user:username user)
                        :page page
                        :has-more (<= *pastes-per-page* (length pastes))))))

(profile:define-panel pastes (:user user :lquery "user-panel.ctml")
  (let ((pastes (dm:get 'plaster-pastes
                        (if (and (auth:current) (or (eql (auth:current) user)
                                                    (user:check (auth:current) '(perm plaster))))
                            (db:query (:= 'author (user:username user)))
                            (db:query (:and (:= 'author (user:username user))
                                            (:= 'visibility 1))))
                        :sort '((time :DESC))
                        :amount *pastes-per-page*)))
    (r-clip:process T :pastes pastes)))

(defun check-password (paste password)
  (let ((paste (ensure-paste paste))
        (parent (paste-parent paste)))
    (when parent (setf paste parent))
    (when (and (= 3 (dm:field paste "visibility"))
               (string/= (cryptos:pbkdf2-hash password *password-salt*)
                         (dm:field (ensure-paste paste) "password")))
      (api-error "Invalid password for paste ~a" (dm:id paste)))))

(defun permitted (action &optional paste (user (or (auth:current) (user:get "anonymous"))))
  (if (listp action)
      (loop for a in action thereis (permitted a paste user))
      (or (and paste
               (equal (dm:field paste "author") (user:username user))
               (user:check user `(plaster paste ,action own)))
          (user:check user `(plaster paste ,action)))))

(defun check-permission (action &optional paste (user (or (auth:current) (user:get "anonymous"))))
  (unless (permitted action paste user)
    (error 'request-denied :message (format NIL "You do not have the permission to ~a pastes."
                                            action))))

(define-api plaster/new (text &optional title parent visibility password current-password) ()
  (check-permission 'new)
  (when parent (check-password parent current-password))
  (let ((paste (create-paste text :title title
                                  :parent parent
                                  :visibility visibility
                                  :password password
                                  :author (user:username (or (auth:current) (user:get "anonymous"))))))
    (api-paste-output paste)))

(define-api plaster/edit (id &optional text title visibility password current-password) ()
  (let ((paste (ensure-paste id)))
    (check-permission 'edit paste)
    (check-password paste current-password)
    (edit-paste id :text text :title title :visibility visibility :password password)
    (api-paste-output paste)))

(define-api plaster/delete (id &optional current-password) ()
  (let ((paste (ensure-paste id)))
    (check-permission 'delete paste)
    (check-password paste current-password)
    (delete-paste paste)
    (if (string= "true" (post/get "browser"))
        (redirect (uri-to-url "plaster/edit"
                              :representation :external
                              :query '(("message" . "Paste deleted"))))
        (api-output `(("_id" . ,(dm:id paste)))))))
