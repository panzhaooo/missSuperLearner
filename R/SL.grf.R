#' @title Wrapper for grf
#' @description Wrapper function for generalized random forests algorithm
#' via \code{regression_forest()} or \code{probability_forest()} from the 
#' \pkg{grf} package.
#'
#' Trains a regression forest that can be used to estimate
#' the conditional mean function \eqn{mu(x) = E[Y | X = x]}, or a probability 
#' forest that can be used to estimate the conditional class probabilities
#' \eqn{P[Y = 1 | X = x]}.
#'
#' @param X The covariates.
#' @param Y The outcome.
#' @param newX Test dataframe.
#' @param family Gaussian or binomial.
#' @param obsWeights Observation-level weights.
#' @param ... Any remaining arguments.
#'
#' @return A trained regression/probability forest object. If 
#' \code{tune.parameters} is enabled, then tuning information will be included 
#' through the `tuning.output` attribute.
#' 
#' @seealso \code{\link[grf]{regression_forest}}
#' \code{\link[grf]{probability_forest}}
#'
#' @export
SL.grf <- function(Y, X, newX, family, obsWeights, ...) {
  .SL.require("grf")
        
  if (family$family == "gaussian") {
    fit <- grf::regression_forest(X, Y, sample.weights = obsWeights)
    pred <- stats::predict(fit, newX)$predictions
  }
        
  if (family$family == "binomial") {
    Y <- as.factor(Y)
    fit <- grf::probability_forest(X, Y, sample.weights = obsWeights)
    pred <- stats::predict(fit, newX)$predictions[, "1"]
  }
        
  fit <- list(object = fit)
  class(fit) <- c("SL.grf")
  out <- list(pred = pred, fit = fit)
  return(out)
}


#' @title Prediction for SL.grf
#' @description Prediction for SL.grf
#'
#' @param object SL.grf object
#' @param newdata Dataframe to generate predictions
#' @param family Gaussian or binomial
#' @param ... Unused additional arguments
#'
#' @seealso \code{\link{SL.grf}} \code{\link[grf]{regression_forest}}
#' \code{\link[grf]{probability_forest}}
#' \code{\link[grf]{predict.regression_forest}}
#' \code{\link[grf]{predict.probability_forest}}
#'
#' @export
predict.SL.grf <- function(object, newdata, family, ...) {
  .SL.require("grf")
  pred <- stats::predict(object$object, newdata)$predictions
  if (family$family == "binomial") {
    pred <- pred[, "1"]
  } 
  pred
}

