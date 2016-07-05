(asdf:defsystem :quantile-estimator
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :depends-on ("alexandria")
  :author "Ilya Khaprov <ilya.kharpov@publitechs.com>"
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "quantile")
                 (:file "estimator"))))
  :description "Implementation of Graham Cormode and S. Muthukrishnan's 
Effective Computation of Biased Quantiles over Data Streams in ICDE’05")
