(in-package :quantile-estimator.test)

(plan 1)

(subtest "Quantile"
  (let ((q (make-quantile 0.5d0 0.05d0)))
    (is (quantile-quantile q) 0.5d0)
    (is (quantile-inaccuracy q) 0.05d0)
    (ok (< (quantile.delta q 0.9d0 2)
           0.23))))

(finalize)
