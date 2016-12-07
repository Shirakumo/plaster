(in-package #:plaster)

(define-trigger db:connected ()
  (db:create 'plaster-pastes '((title (:varchar 32))
                               (time (:integer 5))
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

(defun create-paste (text &key title parent)
  (db:with-transaction ()
    (let ((paste (dm:hull 'plaster-pastes))
          (parent (when parent (ensure-paste parent))))
      (setf (dm:field paste "text") text
            (dm:field paste "title") (or title "")
            (dm:field paste "time") (get-universal-time))
      (dm:insert paste)
      (when parent
        (when (paste-parent parent)
          (error "Cannot annotate an annotation."))
        (db:insert 'plaster-annotations
                   `(("paste" . ,(dm:id parent))
                     ("annotation" . ,(dm:id paste)))))
      paste)))

(defun delete-paste (paste)
  (db:with-transaction ()
    (let ((paste (ensure-paste paste)))
      (mapc #'dm:delete (paste-annotations paste))
      (db:remove 'plaster-annotations (db:query (:= 'paste (dm:id paste))))
      (db:remove 'plaster-annotations (db:query (:= 'annotation (dm:id paste))))
      paste)))

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

(define-api plaster/new (text &optional title parent) ()
  (let ((paste (create-paste text :title title :parent parent)))
    (api-paste-output paste)))

(define-api plaster/edit (id &optional text title) ()
  (let ((paste (ensure-paste id)))
    (when text (setf (dm:field paste "text") text))
    (when title (setf (dm:field paste "title") title))
    (dm:save paste)
    (api-paste-output paste)))

(define-api plaster/delete (id) ()
  (let ((paste (ensure-paste id)))
    (dm:delete paste)
    (if (string= "true" (post/get "browser"))
        (redirect (uri-to-url "plaster/edit"
                              :representation :external
                              :query '(("message" . "Paste deleted"))))
        (api-output `(("_id" . ,(dm:id paste)))))))
