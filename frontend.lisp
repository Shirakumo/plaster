(in-package #:plaster)

(define-page edit "plaster/edit" ())

(define-page view "plaster/view/(.*)" (:uri-groups (id)))
