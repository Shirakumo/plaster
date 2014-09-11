#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:plaster)

(define-implement-hook admin
  (admin:define-panel general plaster (:lquery (template "admin-general.ctml") :icon "fa-file-text" :tooltip "General plaster settings")
    (cond
      ((string= (post-var "action") "Submit")
       (setf (config-tree :plaster :anon) (not (null (post-var "anon")))
             (config-tree :plaster :captcha) (not (null (post-var "captcha")))
             (config-tree :plaster :maxpastes) (parse-integer (post-var "maxpastes"))
             (config-tree :plaster :cooldown) (parse-integer (post-var "cooldown"))))
      ((string= (post-var "action") "Add")
       (cond ((string= (post-var "form") "types")
              (db:insert 'plaster-types
                         `(("title" . ,(post-var "title"))
                           ("name" . ,(post-var "name"))
                           ("mime" . ,(post-var "mime")))))
             ((string= (post-var "form") "themes")
              (db:insert 'plaster-themes
                         `(("title" . ,(post-var "title"))
                           ("name" . ,(post-var "name")))))))
      ((string= (post-var "action") "Delete")
       (let ((table (cond ((string= (post-var "form") "types") "plaster-types")
                          ((string= (post-var "form") "themes") "plaster-themes"))))
         (dolist (id (or (post-var "selected[]") (list (post-var "id"))))
           (db:remove table (db:query (:= "_id" id)))))))
    
    ($ "input[name=\"anon\"]" (attr :checked (if (config-tree :plaster :anon) "checked")))
    ($ "input[name=\"captcha\"]" (attr :checked (if (config-tree :plaster :captcha) "checked")))
    ($ "input[name=\"maxpastes\"]" (val (or (config-tree :plaster :maxpastes) "-1")))
    ($ "input[name=\"cooldown\"]" (val (or (config-tree :plaster :cooldown) "0")))

    (r-clip:process
     T
     :themes (dm:get 'plaster-themes (db:query :all))
     :types (dm:get 'plaster-types (db:query :all))))

  (admin:define-panel preferences plaster (:lquery (template "admin-preferences.ctml") :icon "" :tooltip "")
    (let* ((username (user:username (auth:current)))
           (prefs (dm:get-one 'plaster-users (db:query (:= 'user username)))))

      (r-clip:process
       T
       :notice (get-var "notice")
       :themes (dm:get 'plaster-themes (db:query :all))
       :types (dm:get 'plaster-types (db:query :all)))
      
      ($ (inline (format NIL "#theme option[value=\"~a\"]" (if prefs (dm:field prefs "theme") "default"))) (attr :selected "selected"))
      ($ (inline (format NIL "#type option[value=\"~a\"]" (if prefs (dm:field prefs "default-type") "text"))) (attr :selected "selected")))))
