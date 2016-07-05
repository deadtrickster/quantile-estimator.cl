(in-package #:quantile-estimator)

(defconstant +buffer-size+ 512)

(defclass estimator ()
  ((invariants :initarg :invariants :reader estimator-invariants)
   (buffer :initarg :buffer :initform (make-array +buffer-size+))
   (buffer-pos :initarg :buffer-pos :initform 0)
   (head :initarg :head :initform nil)
   (observations :initform 0 :accessor estimator-observations)
   (sum :initform 0 :accessor estimator-sum)))

(defclass sample ()
  ((value :initarg :value :accessor sample-value)
   (rank :initarg :rank :accessor sample-rank)
   (delta :initarg :delta :accessor sample-delta)
   (successor :initarg :successor :accessor sample-successor)))

(defun make-sample (value rank delta successor)
  (make-instance 'sample :value value
                         :rank rank
                         :delta delta
                         :successor successor))

(defun make-estimator (&rest invariants)
  (unless invariants
    (setf invariants (list (make-quantile 0.5d0 0.05d0)
                           (make-quantile 0.9d0 0.01d0)
                           (make-quantile 0.99d0 0.001d0))))
  (make-instance 'estimator :invariants invariants))

(defun estimator.observe (estimator value)
  (with-slots (buffer buffer-pos observations sum) estimator
    (setf (aref buffer buffer-pos) value)
    (when (= +buffer-size+ (incf buffer-pos))
      (estimator.flush estimator))
    (incf observations)
    (incf sum value)))

(defun estimator.query (estimator rank)
  (estimator.flush estimator)

  (with-slots (head observations) estimator
    (let ((current head))
      (when current
        (let* ((mid-rank (floor (* rank observations)))
               (max-rank (+ mid-rank (floor (/ (estimator.invariant estimator mid-rank) 2))))
               (rank 0.0d0))
          (loop
            (if (sample-successor current)
                (progn (incf rank (sample-rank current))
                       (when (> (+ rank (sample-rank (sample-successor current)) (sample-delta (sample-successor current)))
                                max-rank)
                         (return))

                       (setf current (sample-successor current)))
                (return)))

          (sample-value current))))))

(defun estimator.flush (estimator)
  (with-slots (buffer buffer-pos) estimator
    (unless (= 0 buffer-pos)
      (sort buffer #'>)
      (estimator.replace-batch estimator)
      (setf buffer-pos 0)
      (estimator.compress estimator))))

(defun estimator.replace-batch (estimator)
  (with-slots (buffer buffer-pos observations head) estimator
    (let ((start 0))
      (setf head (or head (prog1 (make-sample (aref buffer start) 1 0 nil)
                            (incf start))))
      (let ((rank 0.0d0)
            (current head))

        (loop for i from start below buffer-pos
              as s = (aref buffer i) do
                (when (< s (sample-value head))
                  (setf head (make-sample s 1 0 head)))

                (loop
                  (unless (and (sample-successor current)
                               (< (sample-value (sample-successor current)) s))
                    (return))
                  (incf rank (sample-rank current))
                  (setf current (sample-successor current)))

                (unless (sample-successor current)
                  (setf (sample-successor current) (make-sample s 1 0 nil)))

                (setf (sample-successor current)
                      (make-sample s 1 (- (estimator.invariant estimator rank) 1) (sample-successor current))))))))

(defun estimator.invariant (estimator rank)
  (with-slots (invariants observations) estimator
    (let ((min (1+ observations)))

      (loop for invariant in invariants
            as delta = (quantile.delta invariant rank observations)
            when (< delta min)
              do (setf min delta))

      (floor min))))

(defun estimator.compress (estimator)
  (with-slots (head) estimator
    (let ((rank 0.0d0)
          (current head))
      (loop
        (unless (and current
                     (sample-successor current))
          (return))

        (when (<= (+ (sample-rank current)
                      (sample-rank (sample-successor current))
                      (sample-delta (sample-successor current)))
                  (estimator.invariant estimator rank))
          (let ((removed (sample-successor current)))

            (setf (sample-value current) (sample-value removed)
                  (sample-rank current) (+ (sample-rank current)
                                            (sample-rank removed))
                  (sample-delta current) (sample-delta removed)
                  (sample-successor current) (sample-successor removed))))

        (incf rank (sample-rank current))
        (setf current (sample-successor current))))))
