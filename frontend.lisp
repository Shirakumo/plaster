(in-package #:plaster)

(define-trigger db:connected ()
  (db:create 'plaster-pastes '((title (:varchar 32))
                               (time (:integer 5))
                               (text :text))))

(defun ensure-paste (paste-ish)
  (etypecase paste-ish
    (dm:data-model paste-ish)
    (string (ensure-paste (parse-integer paste-ish)))
    (integer (or (dm:get-one 'plaster-pastes (db:query (:= '_id paste-ish)))
                 (error 'request-not-found :message (format NIL "No paste with ID ~a was found." paste-ish))))))

(defun create-paste (text &key title)
  (let ((paste (dm:hull 'plaster-pastes)))
    (setf (dm:field paste "text") text
          (dm:field paste "title") (or title "")
          (dm:field paste "time") (get-universal-time))
    (dm:insert paste)))

(define-page edit "plaster/edit(/(.*))?" (:uri-groups (NIL id) :lquery "edit.ctml")
  (let ((paste (if id
                   (ensure-paste id)
                   (dm:hull 'plaster-pastes))))
    (r-clip:process T :paste paste)))

(define-page view "plaster/view/(.*)" (:uri-groups (id) :lquery "view.ctml")
  (r-clip:process T :paste (ensure-paste id)))

(define-api plaster/new (text &optional title) ()
  (let ((paste (create-paste text :title title)))
    (if (string= "true" (post/get "browser"))
        (redirect (make-uri :domains '("plaster")
                            :path (format NIL "view/~a" id)))
        (api-output `(("id" . ,(dm:id paste)))))))
