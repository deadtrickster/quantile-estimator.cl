(asdf:defsystem :quantile-estimator.test
  :version "0.0.1"
  :description "Tests for quantile-estimator.cl"
  :maintainer "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :author "Ilya Khaprov <ilya.khaprov@publitechs.com> and CONTRIBUTORS"
  :licence "MIT"
  :depends-on ("quantile-estimator"
               "prove"
               "log4cl"
               "mw-equiv")
  :serial t
  :components ((:module "t"
                :serial t
                :components
                ((:file "package")
                 (:test-file "dummy")
                 (:test-file "quantile")
                 (:test-file "estimator"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (asdf:test-op :after (op c)
                         (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                         (asdf:clear-system c)))
