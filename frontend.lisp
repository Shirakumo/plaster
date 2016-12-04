(in-package #:plaster)

(define-page edit "plaster/edit" (:lquery "edit.ctml")
  (r-clip:process T))

(define-page view "plaster/view/(.*)" (:uri-groups (id) :lquery "view.ctml")
  (let* ((id (parse-integer id))
         (paste (first (db:select 'plaster-pastes (db:query (:= '_id id)) :amount 1))))
    (unless paste
      (error 'request-not-found :message (format NIL "No paste with ID ~a was found." id)))
    (r-clip:process
     T
     :title (gethash "title" paste)
     :time (gethash "time" paste)
     :text (gethash "text" paste))))

(define-trigger db:connected ()
  (db:create 'plaster-pastes '((title (:varchar 32))
                               (time (:integer 5))
                               (text :text))))

(define-api plaster/new (text &optional title) ()
  (let ((id (db:insert 'plaster-pastes `((title . ,title)
                                         (time . ,(get-universal-time))
                                         (text . ,text)))))
    (if (string= "true" (post/get "browser"))
        (redirect (make-uri :domains '("plaster")
                            :path (format NIL "view/~a" id)))
        (api-output `(("id" . ,id))))))
