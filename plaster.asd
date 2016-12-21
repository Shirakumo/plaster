(in-package #:cl-user)
(asdf:defsystem #:plaster
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :components ((:file "module")
               (:file "frontend"))
  :depends-on ((:interface :database)
               (:interface :user)
               (:interface :auth)
               (:interface :profile)
               (:interface :rate)
               :r-data-model
               :r-clip
               :crypto-shortcuts))
