(in-package #:quantile-estimator)

(defclass quantile ()
  ((quantile :initarg :quantile :reader quantile-quantile)
   (inaccuracy :initarg :inaccuracy :reader quantile-inaccuracy)
   (coefficient_i :initarg :coefficient_i)
   (coefficient_ii :initarg :coefficient_ii)))

(defun make-quantile (quantile inaccuracy)
  (make-instance 'quantile :quantile quantile
                           :inaccuracy inaccuracy
                           :coefficient_i (/ (* 2 inaccuracy) (- 1 quantile))
                           :coefficient_ii (/ (* 2 inaccuracy) quantile)))

(defun quantile.delta (q rank n)
  (with-slots (quantile coefficient_i coefficient_ii) q
    (if (<= rank (floor (* quantile n)))
        (* coefficient_i (- n rank))
        (* coefficient_ii rank))))
