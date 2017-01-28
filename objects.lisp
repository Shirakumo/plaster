#|
 This file is a part of Purplish
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.plaster)

(defparameter *paste-types*
  (list* "text"
         (sort (mapcar #'pathname-name
                       (uiop:directory-files (@static "codemirror/mode/")))
               #'string<)))

(defparameter *paste-themes*
  (list* "default"
         (sort (mapcar #'pathname-name
                       (uiop:directory-files (@static "codemirror/theme/")))
               #'string<)))

(define-trigger db:connected ()
  (db:create 'pastes '((title (:varchar 32))
                       (time (:integer 5))
                       (type (:varchar 32))
                       (author (:varchar 32))
                       (visibility (:integer 1))
                       (password (:varchar 128))
                       (text :text)))
  (db:create 'annotations '((paste :id)
                            (annotation :id))))

(define-trigger user:ready ()
  (defaulted-config 25 :pastes-per-page)
  (defaulted-config 50 :api :default-amount)
  (defaulted-config 100 :api :maximum-amount)
  (defaulted-config (make-random-string) :password-salt)

  (defaulted-config (list
                     (perm plaster paste new)
                     (perm plaster paste view)
                     (perm plaster paste list)
                     (perm plaster paste user)
                     (perm plaster paste edit own)
                     (perm plaster paste delete own))
                    :permissions :default)

  (defaulted-config (list
                     (perm plaster paste new)
                     (perm plaster paste view)
                     (perm plaster paste list)
                     (perm plaster paste user))
                    :permissions :anonymous)

  (apply #'user:add-default-permissions (config :permissions :default))
  (apply #'user:grant "anonymous" (config :permissions :anonymous)))

(defun ensure-paste (paste-ish)
  (etypecase paste-ish
    (dm:data-model paste-ish)
    (string (ensure-paste
             (or (ignore-errors (db:ensure-id paste-ish))
                 (error 'request-not-found :message (format NIL "No paste with ID ~s was found." paste-ish)))))
    (integer (or (dm:get-one 'pastes (db:query (:= '_id paste-ish)))
                 (error 'request-not-found :message (format NIL "No paste with ID ~a was found." paste-ish))))))

(defun paste-annotations (paste)
  (let* ((paste (ensure-paste paste))
         (rels (dm:get 'annotations (db:query (:= 'paste (dm:id paste))))))
    (loop for rel in rels
          collect (ensure-paste (dm:field rel "annotation")))))

(defun paste-parent (paste)
  (let* ((paste (ensure-paste paste))
         (rel (dm:get-one 'annotations (db:query (:= 'annotation (dm:id paste))))))
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
  (when password (cryptos:pbkdf2-hash password (config :password-salt))))

(defun ensure-paste-type (type)
  (let ((type (string-downcase (or type "text"))))
    (unless (find type *paste-types* :test #'string=)
      (error 'api-argument-invalid :argument "type"
                                   :message (format T "Type must be one of ~{~s~^, ~}." *paste-types*)))
    type))

(defun register-annotation (annotation paste)
  (when (paste-parent paste)
    (api-error "Cannot annotate an annotation."))
  (db:insert 'annotations
             `(("paste" . ,(dm:id (ensure-paste paste)))
               ("annotation" . ,(dm:id (ensure-paste annotation))))))

(defun create-paste (text &key title parent visibility password author type)
  (when (and parent visibility)
    (api-error "Cannot set the visibility of an annotation."))
  (db:with-transaction ()
    (let* ((paste (dm:hull 'pastes))
           (parent (when parent (ensure-paste parent)))
           (visibility (if parent 2 (ensure-visibility visibility)))
           (password (ensure-password visibility password)))
      (setf (dm:field paste "text") text
            (dm:field paste "title") (or title "")
            (dm:field paste "time") (get-universal-time)
            (dm:field paste "type") (ensure-paste-type type)
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
      (db:remove 'annotations
                 (db:query (:or (:= 'paste (dm:id paste))
                                (:= 'annotation (dm:id paste)))))
      (dm:delete paste)
      paste)))

(defun edit-paste (paste &key text title visibility password type)
  (db:with-transaction ()
    (let* ((paste (ensure-paste paste)))
      (when text
        (setf (dm:field paste "text") text))
      (when title
        (setf (dm:field paste "title") title))
      (when type
        (setf (dm:field paste "type") (ensure-paste-type type)))
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

(defun check-password (paste password)
  (let ((paste (ensure-paste paste))
        (parent (paste-parent paste)))
    (when parent (setf paste parent))
    (when (and (= 3 (dm:field paste "visibility"))
               (string/= (cryptos:pbkdf2-hash password (config :password-salt))
                         (dm:field (ensure-paste paste) "password")))
      (api-error "Invalid password for paste ~a" (dm:id paste)))))

(defun permitted-p (action &optional paste (user (or (auth:current) (user:get "anonymous"))))
  (if (listp action)
      (loop for a in action thereis (permitted-p a paste user))
      (or (and paste
               (equal (dm:field paste "author") (user:username user))
               (user:check user `(plaster paste ,action own)))
          (user:check user `(plaster paste ,action)))))

(defun check-permission (action &optional paste (user (or (auth:current) (user:get "anonymous"))))
  (unless (permitted-p action paste user)
    (error 'request-denied :message (format NIL "You do not have the permission to ~a pastes."
                                            action))))

(defun reformat-paste (paste &key include-annotations)
  (let ((table (make-hash-table :test 'eql)))
    (flet ((copy (field)
             (setf (gethash field table) (dm:field paste field))))
      (mapcar #'copy '("title" "time" "author" "visibility" "text" "type")))
    (when include-annotations
      (setf (gethash "annotations" table)
            (mapcar #'reformat-paste (paste-annotations paste))))
    table))
