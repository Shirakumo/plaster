#|
 This file is a part of Purplish
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem #:plaster
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :components ((:file "module")
               (:file "objects")
               (:file "frontend")
               (:file "api"))
  :depends-on ((:interface :database)
               (:interface :user)
               (:interface :auth)
               (:interface :profile)
               (:interface :rate)
               :r-data-model
               :r-clip
               :crypto-shortcuts))
