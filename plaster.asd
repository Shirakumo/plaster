(in-package #:cl-user)
(asdf:defsystem #:plaster
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :components ((:file "module")
               (:file "frontend"))
  :depends-on ((:interface :database)
               (:interface :data-model)
               :r-clip
               :crypto-shortcuts))
