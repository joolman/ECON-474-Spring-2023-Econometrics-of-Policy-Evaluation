# 1. Copy and paste the code from last lecture up the end of estimating marginal
#     effects for fit2
# 2. create an OLS equivalent for fit1 and fit2
#     - in fit2 replace the interactions (`*`) with additions (`+`)
# 3. compare the logistic regression marginal effects to the OLS marginal effects
#     hint: call `$estimate` from the logit marginal effects to reduce output
# 4. From the two OLS regressions, create an object yhat1 and yhat2 that are the
#     `$fitted.values`
#     - using `mean()` what proportion of the times is yhat < 0 or is yhat > 1?
#     - are you concerned about bias in either of these LPMs?
