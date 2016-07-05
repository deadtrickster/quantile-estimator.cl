(in-package :quantile-estimator.test)

(plan 1)

(subtest "Quantile Estimator"

  (subtest "Constructor"
    (subtest "provides default quantiles"
      (let* ((estimator (make-estimator))
             (quantiles (estimator-invariants estimator)))

        (is (length quantiles) 3)

        (is (quantile-quantile (first quantiles)) 0.5d0)
        (is (quantile-quantile (second quantiles)) 0.9d0)
        (is (quantile-quantile (third quantiles)) 0.99d0)))

    (subtest "accepts custom quantiles"
      (let* ((quantiles (list (make-quantile 0.7d0 0.01)
                              (make-quantile 0.8d0 0.01)))
             (estimator (apply #'make-estimator quantiles)))
        (is (estimator-invariants estimator) quantiles :test #'equalp))))

  (subtest "Operations & properties"
    (subtest "observe"
      (let ((estimator (make-estimator)))
        (estimator.observe estimator 0.5)
        (is (estimator-observations estimator) 1)))

    (subtest "observations"
      (let ((estimator (make-estimator)))
        (is (estimator-observations estimator) 0)

        (loop repeat 42 do
          (estimator.observe estimator (random 10)))

        (is (estimator-observations estimator) 42)))

    (subtest "sum"
      (let ((estimator (make-estimator)))
        (is (estimator-observations estimator) 0)

        (loop for i from 0 below 42 do
          (estimator.observe estimator (1+ i)))

        (is (estimator-sum estimator) 903)))

    (subtest "query"
      (subtest "should return quantile value for a given rank"
        (let ((estimator (make-estimator)))
          (estimator.observe estimator 0.8)
          (estimator.observe estimator 0.4)
          (estimator.observe estimator 0.9)
          (estimator.observe estimator 0.6)

          (is (estimator.query estimator 0.5) 0.6)
          (is (estimator.query estimator 0.9) 0.8)
          (is (estimator.query estimator 0.99) 0.8))

        (let ((estimator (make-estimator)))
          (estimator.observe estimator 3)
          (estimator.observe estimator 5.2)
          (estimator.observe estimator 13)
          (estimator.observe estimator 4)

          (is (estimator.query estimator 0.5) 4)
          (is (estimator.query estimator 0.9) 5.2)
          (is (estimator.query estimator 0.99) 5.2))

        (let ((estimator (make-estimator)))
          (estimator.observe estimator 3)

          (is (estimator.query estimator 0.5) 3)
          (is (estimator.query estimator 0.9) 3)
          (is (estimator.query estimator 0.99) 3)))
      (subtest "should return nul if no observations"
        (let ((estimator (make-estimator)))
          (is (estimator.query estimator 0.9) nil))))))

(finalize)
