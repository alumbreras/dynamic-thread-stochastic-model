Implementation notes
=============

The most costly part of the Expectation-Maximization is the maximization step. Since we do not have an analytic solution, we use numerical optimization methods.

The chosen method is the Nelder-Mead. It allows non-linear opimization, $n$ dimensions (though it has been reported to perform badly for high dimensions) and some implementations also deal with boundaries.

Strangely, the `neldermead::fminbnd` produces some errors with our model and our data. `stats::nlminb` and `dfoptim::nmkb` produce the same results (as they should!) and the speed is similar, `nmkb` being a bit faster.

`vectoptim::nmkb.vec` seems to use paralellization, but the package is no longer mantained as has been removed from the CRAN repositories.

`mcGlobaloptim::multiStartoptim` combines local searches with, for instance, Nelder Mead, and Monte Carlo sampling to add (I guess) some randomisation in order to find the global optimum. However, I guess the price to pay is a slowing down of speed. Anyway, I didn't get it to work.


`stats::optim`(..., method='L-BFGS-B',...)
 
Given all this, we use `dfoptim::nmkb` for the optimizations.