#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:plaster)

(define-api plaster/raw (id &optional (password "")) ()
  (let ((paste (dm:get-one 'plaster (db:query (:= '_id (hash->id id))))))
    (if paste
        (if (paste-accessible-p paste)
            (progn (setf (content-type *response*) "text/plain")
                   (dm:field paste "text"))
            (error 'api-auth-error :message "You are not allowed to view this paste."))
        (error 'api-error :message "No such paste found."))))

(define-api plaster/paste (id &optional (password "")) ()
  (let ((paste (dm:get-one 'plaster (db:query (:= '_id (hash->id id))))))
    (if paste
        (if (paste-accessible-p paste)
            (api-output
             (alexandria:plist-hash-table
              (list :id (id->hash (dm:field paste "_id"))
                    :pid (id->hash (dm:field paste "pid"))
                    :title (dm:field paste "title")
                    :author (dm:field paste "author")
                    :type (dm:field paste "type")
                    :time (dm:field paste "time")
                    :view (dm:field paste "view")
                    :hits (dm:field paste "hits")
                    :text (dm:field paste "text"))))
            (error 'api-auth-error :message "You are not allowed to view this paste."))
        (error 'api-error :message "No such paste found."))))

(define-api plaster/paste/add (text &optional (annotate "-1") (title "") (type "text") (view "0") (password "") (captcha "") (hash "") (client "false")) (:method T)
  "Create a new paste"
  (api-output
   (paste-add text annotate title type view password captcha hash client)))

(define-api plaster/paste/edit (id &optional text title type view password (client "false")) ()
  "Edit an existing paste"
  (api-output
   (paste-edit id text title type view password client)))

(define-api paste/delete (id &optional password (client "false")) (:method T)
  "Delete an existing paste"
  (api-output
   (paste-delete id password client)))

(defun paste-add (text annotate title type view password captcha hash client)
  (let ((user (auth:current))
        (annotate (hash->id annotate))
        (title (string-or "Untitled" title))
        (type (string-or "text" type))
        (view (parse-integer (string-or "0" view)))
        (client (string-equal client "true"))
        (maxpastes (config-tree :plaster :maxpastes))
        (cooldown (config-tree :plaster :cooldown))
        (last-time (let ((set (db:select 'plaster (db:query (:= 'ip (remote *request*)))
                                         :amount 1 :sort '((time :DESC)) :fields '(time))))
                     (when set (gethash "time" (first set))))))
    (when (= annotate -1) (setf annotate NIL))

    (assert-api
      ((< 0 (length text))
       "Text must be at least one character long.")
      ((and (< -1 view) (< view 4))
       "View must be between 0 and 3.")
      ((<= (length title) 64)
       "Title must be less than 65 characters long.")
      ((or user (config-tree :plaster :anon))
       "Anonymous pasting is not permitted.")
      ((or (not (= 2 view)) user)
       "Anonymous users cannot create private pastes.")
      ((db:select 'plaster-types (db:query (:= 'mime type)))
       "Invalid type specified.")
      ((or (not user) (not maxpastes) (< maxpastes 0) (< (db:count 'plaster (db:query (:= 'author (user:username user)))) maxpastes))
       (format NIL "Max paste limit of ~a exceeded." maxpastes))
      ((or (not cooldown) (not last-time) (< cooldown (- (get-unix-time) last-time)))
       (format NIL "Please wait ~d seconds between pastes." cooldown)))

    (when (and (not user) (config-tree :plaster :captcha))
      (assert-api
        ((< 0 (length hash))
         "Captcha hash is missing.")
        ((string= hash (cryptos:pbkdf2-hash captcha *captcha-salt*))
         "Incorrect captcha.")))
    
    (when annotate
      (setf annotate (dm:get-one 'plaster (db:query (:and (:= '_id annotate) (:= 'pid -1)))))
      (setf view (dm:field annotate "view"))
      (assert-api
        ((not (null annotate))
         "No such paste to annotate found.")
        ((paste-accessible-p annotate user)
         "You are not allowed to repaste/annotate this paste.")))

    (when (= view 3)
      (assert-api
        ((and password (< 5 (length password)))
         "Encrypted view mode requires a password of at least 6 characters.")
        ((<= (length password) 32)
         "Passwords must be less than 32 characters long."))
      (setf text (encrypt text password)))
    
    (when (and password (= (length password) 0)) (setf password NIL))
    (with-model model ("plaster" NIL pid (mtitle title) author time (mtext text) (mview view) (mtype type) hits ip)
      (setf pid (or (when annotate (dm:id annotate)) -1)
            mtitle title
            author (if user (user:username user) "anonymous")
            time (get-unix-time)
            mtext text
            mview view
            mtype type
            hits 0
            ip (remote *request*))
      (dm:insert model)
      (if client
          (redirect (format NIL "/view/~a~@[?password=~a~]"
                            (id->hash (dm:id (or annotate model))) password))
          (alexandria:plist-hash-table (list :id (dm:id model)))))))

(defun paste-edit (id text title type view password client)
  (let ((user (auth:current))
        (paste (dm:get-one "plaster" (db:query (:= '_id (hash->id id)))))
        (client (string-equal client "true")))
    (setf view (if view (parse-integer view) (dm:field paste "view")))
    
    (assert-api
      ((not (null paste))
       "No such paste found.")
      ((paste-editable-p paste user)
       "You are not allowed to edit this paste.")
      ((or (not text) (< 0 (length text)))
       "Text must be at least one character long.")
      ((or (not view) (and (< -1 view) (< view 4)))
       "View must be between 0 and 3.")
      ((db:select "plaster-types" (db:query (:= 'mime type)))
       "Invalid type specified."))

    (when (= view 3)
      (assert-api
        ((and password (< 5 (length password)))
         "Encrypted view mode requires a password of at least 6 characters.")
        ((<= (length password) 32)
         "Passwords must be less than 32 characters long.")
        (text
         "Text to encrypt is required."))
      (setf text (encrypt text password)))

    (setf (dm:field paste "title") (string-or (dm:field paste "title") title)
          (dm:field paste "view") view
          (dm:field paste "type") (or type (dm:field paste "type"))
          (dm:field paste "text") (or text (dm:field paste "text")))
    (dm:save paste)
    (if client
        (redirect (format NIL "/view/~a~@[?password=~a~]"
                          (if (= (dm:field paste "pid") -1) id (id->hash (dm:field paste "pid"))) password))
        "Paste edited.")))

(defun paste-delete (id password client)
  (let ((user (auth:current))
        (paste (dm:get-one "plaster" (db:query (:= '_id (hash->id id)))))
        (client (string-equal client "true")))
    (assert-api 
      ((not (null paste))
       "No such paste found.")
      ((paste-editable-p paste user)
       "You are not allowed to edit this paste."))

    (dm:delete paste)
    (db:remove "plaster" (db:query (:= 'pid (dm:id paste))))
    (if client
        (redirect (format NIL "/view/~a~@[?password=~a~]"
                                 (if (= (dm:field paste "pid") -1) id (id->hash (dm:field paste "pid"))) password))
        "Paste deleted.")))

;; (define-api user/settings () (:method :GET :access-branch "*")
;;   "View user settings."
;;   (let ((prefs (dm:get-one "plaster-user" (db:query (:= "user" (user:field (user:current) "username"))))))
;;     (core:api-return 200 "User settings."
;;                      :data (plist->hash-table
;;                             :username (user:field (user:current) "username")
;;                             :theme (if prefs (dm:field prefs "theme") "default")
;;                             :default-type (if prefs (dm:field prefs "default-type") "text")))))

;; (define-api user/settings/save (&optional theme type nuke (client "false")) (:method T :access-branch "*")
;;   "Change user settings."
;;   (user-save theme type nuke client))

;; (defun user-save (theme type nuke client)
;;   (let ((username (user:field (user:current) "username"))
;;         (client (string-equal client "true")))
;;     (assert-api (:apicall "user/settings/save" :module "plaster" :code 400 :text)
;;       ((or (not theme) (db:select "plaster-themes" (db:query (:= "name" theme))))
;;        "Not a valid theme.")
;;       ((or (not type) (db:select "plaster-types" (db:query (:= "mime" type))))
;;        "Not a valid type."))
    
;;     (let ((prefs (dm:get-one "plaster-user" (db:query (:= "user" username)))))
;;       (when (null prefs)
;;         (setf prefs (dm:hull "plaster-user")
;;               (dm:field prefs "user") username))
;;       (when theme (setf (dm:field prefs "theme") (server:post "theme")))
;;       (when type (setf (dm:field prefs "default-type") (server:post "type")))
;;       (if (dm:hull-p prefs)
;;           (dm:insert prefs)
;;           (dm:save prefs)))
    
;;     (when (and nuke (string= nuke "nuke"))
;;       (let ((count (db:count "plaster" (db:query (:= "author" username)))))
;;         (db:remove "plaster" (db:query (:= "author" username)))
;;         (when client
;;           (server:redirect (make-uri (format NIL "user./settings/plaster/preferences?notice=~a pastes deleted." count))))))

;;     (if client
;;         (server:redirect (make-uri "user./settings/plaster/preferences?notice=Preferences updated."))
;;         (core:api-return 200 "Preferences saved."))))

;; (define-api user/import (&optional service) (:access-branch "*")
;;   "Check if importing with the specified service is available. No service lists all available."
;;   (if service
;;       (core:api-return 200 service
;;                        :data (plist->hash-table
;;                               service (not (null (config-tree :plaster :import (find-symbol (string-upcase service) :KEYWORD))))))
;;       (core:api-return 200 "Available imports"
;;                        :data (mapcar #'car (config-tree :plaster :import)))))

;; (define-api user/import/pastebin (username password &optional (client "false")) (:access-branch "*")
;;   "Import pastes from pastebin."
;;   (flet ((request (where &rest params)
;;            (apply #'drakma:http-request where :external-format-in :utf-8 :external-format-out :utf-8 params)))
;;     (let ((apikey (config-tree :plaster :import :pastebin :apikey))
;;           (user (user:field (user:current) "username")))
;;       (assert-api (:apicall "user/import/pastebin" :module "plaster" :code 400 :text)
;;         ((not (null apikey))
;;          "Pastebin import not configured."))
;;       (let ((userkey (request "http://pastebin.com/api/api_login.php"
;;                               :method :post :parameters `(("api_dev_key" . ,apikey)
;;                                                           ("api_user_name" . ,username)
;;                                                           ("api_user_password" . ,password)))))
;;         (assert-api (:apicall "user/import/pastebin" :module "plaster" :code 400 :text)
;;           ((not (string-equal userkey "Bad API request, invalid api_dev_key"))
;;            "Pastebin import not configured.")
;;           ((not (string-equal userkey "Bad API request, invalid login"))
;;            "Invalid login.")
;;           ((not (string-equal userkey "Bad API request, account not active"))
;;            "Specified account is inactive.")
;;           ((not (string-equal (subseq userkey 0 (length "Bad API")) "Bad API"))
;;            "Internal API error."))

;;         (let ((pastes (request "http://pastebin.com/api/api_post.php"
;;                                :method :post :parameters `(("api_dev_key" . ,apikey)
;;                                                            ("api_user_key" . ,userkey)
;;                                                            ("api_results_limit" . "100")
;;                                                            ("api_option" . "list"))))
;;               (lquery:*lquery-master-document*)
;;               (success ())
;;               (adapted ())
;;               (failed ()))
;;           (assert-api (:apicall "user/import/pastebin" :module "plaster" :code 400 :text)
;;             ((not (string-equal (subseq userkey 0 (length "Bad API")) "Bad API"))
;;              "Internal API error."))

;;           ($ (initialize (format NIL "<data>~a</data>" pastes)))
;;           (dolist (node ($ "data" (children)))
;;             (v:debug :plaster.import.pastebin "Importing ~a" ($ node "paste_key" (text) (node)))
;;             (let ((content (request "http://pastebin.com/raw.php"
;;                                     :method :get :parameters `(("i" . ,($ node "paste_key" (text) (node)))))))
;;               (if (and content (not (string-equal content "Error, this is a private paste. If this is your private paste, please login to Pastebin first.")))
;;                   (with-model (model pid title author type time text view hits) ("plaster" NIL)
;;                     (let ((realtype (pastebin->type ($ node "paste_format_short" (text) (node)))))
;;                       (if realtype
;;                           (push (dm:field model "_id") success)
;;                           (progn (setf realtype "text/plain")
;;                                  (push (dm:field model "_id") adapted)))
;;                       (setf title (string-or "Untitled" ($ node "paste_title" (text) (node)))
;;                             pid -1
;;                             author user
;;                             type realtype
;;                             time ($ node "paste_date" (text) (node))
;;                             text content
;;                             view (parse-integer ($ node "paste_private" (text) (node)))
;;                             hits (parse-integer ($ node "paste_hits" (text) (node))))
;;                       (dm:insert model)))
;;                   (push ($ node "paste_key" (text) (node)) failed))))
;;           (if client
;;               (server:redirect (make-uri (format NIL "user./settings/plaster/preferences?notice=~a pastes imported, ~a type adapted, ~a failed."
;;                                                  (length success) (length adapted) (length failed))))
;;               (core:api-return 200 "A"
;;                                :data (plist->hash-table
;;                                       :success success
;;                                       :adapted adapted
;;                                       :failed failed))))))))
