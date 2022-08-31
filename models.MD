# Model Descriptions

greg and whib: count as a function of year and reversals

wost: count as a function of year, initial depth, intial depth squared, pre-recession, and post-recession

(note: covariates are not presently standardized)

both poisson and negative binomial versions of models are fit for all 3 species
both use log link functions
poisson using glm, negative binomial using glm.nb in MASS package

for both types of models, we set the maximum number of iterations to 1000 to facilitate convergence


forecasts are made via basic predict functionality in R on the link scale with the model residual scale to produce point estimates and 95% CIs


# Evaluation

evaluation of forecasts is done using the log score and rps based on 100000 samples from the forecasted distributions
because the log score hits a boundary when there is 0 density, producing scores of Inf i've also calculated a "limiting" value by including one instance of the true value out of the 100000 (swapped out so total sample size stays constant)
calculations are done using the scoringRules package

# References

Alexander Jordan, Fabian Krueger, Sebastian Lerch (2019). Evaluating Probabilistic Forecasts with scoringRules. Journal of
  Statistical Software, 90(12), 1-37. DOI 10.18637/jss.v090.i12

Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0
