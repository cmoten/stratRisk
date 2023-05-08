#'Functions to compute inherent risk loss
#'
#'@param prob probability of a loss.
#'@param upper upper bound of a 90% confidence interval of the impact in dollars.
#'@param lower lower bound of a 90% confidence interval of the impact in dollars.
#'@return a vector of estimated losses in dollars.
#'@examples
#'prob_loss <- c(.4, .15, .35, .1, .05, .12, .035)
#'impact_lower <- c(5e5, 2e6, 15e4, 2e5, 5e5, 5e5, 1e6)
#'impact_upper <- c(1e7, 5e6, 3e5, 5e5, 2e6, 1e7, 25e6)
#'calc_lognorm_loss(prob_loss, impact_upper, impact_lower)


calc_lognorm_loss <- function(prob, upper, lower){
  scale_factor <- 2 * qnorm(.95)
  mu <- (log(upper) + log(lower))/2
  sigma <- (log(upper) - log(lower))/scale_factor
  exp_loss <- prob * exp(mu + 0.5 * sigma^2)
  exp_loss <- ifelse(is.nan(exp_loss), 0, exp_loss)
  exp_loss
}
