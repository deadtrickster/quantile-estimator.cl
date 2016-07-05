(in-package #:cl-user)

(defpackage #:quantile-estimator
  (:use #:cl #:alexandria)
  (:export #:make-quantile
           #:quantile-quantile
           #:quantile-inaccuracy
           #:quantile.delta
           #:make-estimator
           #:estimator-observations
           #:estimator-sum
           #:estimator-invariants
           #:estimator.observe
           #:estimator.query))
