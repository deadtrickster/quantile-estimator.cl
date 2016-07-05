## Quantile estimator for streams [![Build Status](https://travis-ci.org/deadtrickster/quantile-estimator.cl.svg?branch=master)](https://travis-ci.org/deadtrickster/quantile-estimator.cl) [![Coverage Status](https://coveralls.io/repos/github/deadtrickster/quantile-estimator.cl/badge.svg?branch=master)](https://coveralls.io/github/deadtrickster/quantile-estimator.cl?branch=master)

Common Lisp Implementation of Graham Cormode and S. Muthukrishnan's [Effective
Computation of Biased Quantiles over Data Streams][1] in ICDE’05.

Based on [Ruby Implementation][2].

```lisp
(let ((estimator (make-estimator)))
  (estimator.observe estimator 0.8)
  (estimator.observe estimator 0.4)
  (estimator.observe estimator 0.9)
  (estimator.observe estimator 0.6)

  (is (estimator.query estimator 0.5) 0.6)
  (is (estimator.query estimator 0.9) 0.8)
  (is (estimator.query estimator 0.99) 0.8))
```

## License
MIT

[1]: http://www.cs.rutgers.edu/~muthu/bquant.pdf
[2]: https://github.com/matttproud/ruby_quantile_estimation
