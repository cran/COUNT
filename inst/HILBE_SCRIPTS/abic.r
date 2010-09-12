# R script for Stata abic command
# From Hilbe, Negative Binomial regression, 2nd ed, Cambridge Univ. Press
#  and Hilbe, Logistic Regression Models, Chapman & Hall/CRC
model  <- <insert model name>
obs    <- model$df.null + 1
aic    <- model$aic
xvars  <- model$rank
rdof   <- model$df.residual
aic_n  <- aic/obs
ll     <- xvars - aic/2
bic_r  <- model$deviance - (rdof * log(obs)
bic_l  <- -2*ll + xvars * log(obs)
bic_qh <- -2*(ll - xvars * log(xvars))/obs
aic; aic_n; bic_l; bic_qh