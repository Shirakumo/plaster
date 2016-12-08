(in-package #:plaster)

(defparameter *pastes-per-page* 25)
(defparameter *password-salt* "Something ˢᵉᶜʳᵉᵗ")

(define-trigger db:connected ()
  (db:create 'plaster-pastes '((title (:varchar 32))
                               (time (:integer 5))
                               (visibility (:integer 1))
                               (password (:varchar 128))
                               (text :text)))
  (db:create 'plaster-annotations '((paste :id)
                                    (annotation :id))))

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

(defun create-paste (text &key title parent visibility password)
  (db:with-transaction ()
    (let* ((paste (dm:hull 'plaster-pastes))
           (parent (when parent (ensure-paste parent)))
           (visibility (ensure-visibility visibility))
           (password (ensure-password visibility password)))
      (setf (dm:field paste "text") text
            (dm:field paste "title") (or title "")
            (dm:field paste "time") (get-universal-time)
            (dm:field paste "visibility") visibility
            (dm:field paste "password") password)
      (dm:insert paste)
      (when parent (register-annotation paste parent))
      paste)))

(defun delete-paste (paste)
  (db:with-transaction ()
    (let ((paste (ensure-paste paste)))
      (mapc #'dm:delete (paste-annotations paste))
      (db:remove 'plaster-annotations (db:query (:= 'paste (dm:id paste))))
      (db:remove 'plaster-annotations (db:query (:= 'annotation (dm:id paste))))
      paste)))

(defun edit-paste (paste &key text title visibility password)
  (let* ((paste (ensure-paste paste)))
    (when text
      (setf (dm:field paste "text") text))
    (when title
      (setf (dm:field paste "title") title))
    (when visibility
      (setf (dm:field paste "visibility") (ensure-visibility visibility)))
    (when password
      (setf (dm:field paste "password") (ensure-password (or visibility (dm:field paste "visibility"))
                                                         password)))
    (dm:save paste)))

(defun api-paste-output (paste)
  (cond ((string= "true" (post/get "browser"))
         (let ((parent (paste-parent paste)))
           (redirect (make-url :domains '("plaster")
                               :path (format NIL "view/~a"
                                             (if parent
                                                 (dm:id parent)
                                                 (dm:id paste)))
                               :fragment (princ-to-string (dm:id paste))))))
        (T
         (api-output (loop for field in (dm:fields paste)
                           collect (cons field (dm:field paste field)))))))

(define-page edit "plaster/edit(/(.*))?" (:uri-groups (NIL id) :lquery "edit.ctml")
  (let ((paste (if id
                   (ensure-paste id)
                   (dm:hull 'plaster-pastes))))
    (r-clip:process T :paste paste
                      :parent (get-var "annotate")
                      :repaste (get-var "repaste")
                      :error (get-var "error")
                      :message (get-var "message"))))

(define-page view "plaster/view/(.*)" (:uri-groups (id) :lquery "view.ctml")
  (let* ((paste (ensure-paste id))
         (annotations (sort (paste-annotations paste)
                            #'< :key (lambda (a) (dm:field a "time")))))
    (r-clip:process T :paste paste :annotations annotations)))

(define-page list "plaster/list(/(.*))?" (:uri-groups (NIL page) :lquery "list.ctml")
  (let* ((page (or (when page (parse-integer page :junk-allowed T)) 0))
         (pastes (dm:get 'plaster-pastes (db:query :all)
                         :sort '((time :DESC))
                         :skip (* page *pastes-per-page*)
                         :amount *pastes-per-page*)))
    (r-clip:process T :pastes pastes
                      :page page
                      :has-more (<= *pastes-per-page* (length pastes)))))

(define-api plaster/new (text &optional title parent visibility password) ()
  (let ((paste (create-paste text :title title :parent parent :visibility visibility :password password)))
    (api-paste-output paste)))

(define-api plaster/edit (id &optional text title visibility password) ()
  (let ((paste (edit-paste id :text text :title title :visibility visibility :password password)))
    (api-paste-output paste)))

(define-api plaster/delete (id) ()
  (let ((paste (ensure-paste id)))
    (dm:delete paste)
    (if (string= "true" (post/get "browser"))
        (redirect (uri-to-url "plaster/edit"
                              :representation :external
                              :query '(("message" . "Paste deleted"))))
        (api-output `(("_id" . ,(dm:id paste)))))))
