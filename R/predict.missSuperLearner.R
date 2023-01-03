#' Predict method for missSuperLearner object
#'
#' Obtains predictions on a new data set from a missSuperLearner fit. Need
#' the original data for imputation, or if one of the library algorithms uses 
#' the original data in its predict method.
#'
#' If \code{newdata} is omitted, the predicted values from \code{object} are
#' returned.  
#' 
#' Each algorithm in the Super Learner library needs to have a
#' corresponding prediction function with the ``predict.'' prefixed onto the
#' algorithm name (e.g. \code{predict.SL.glm} for \code{SL.glm}).
#' 
#' @param object Fitted object from \code{missSuperLearner}.
#' @param newdata New X values for prediction.
#' @param X Original data set used to fit \code{object}, if needed.
#' @param Y Original outcome used to fit \code{object}, if needed.
#' @param onlySL Logical. If \code{TRUE}, only compute predictions for 
#' algorithms with non-zero coefficients in the super learner object. Default 
#' is \code{FALSE} (computes predictions for all algorithms in library).
#' @param \dots Additional arguments passed to the \code{predict.SL.*}
#' functions
#'
#' @return A list of two components:
#' \item{pred}{ Predicted values from the \code{missSuperLearner} fit}
#' \item{library.predict}{ Predicted values for each algorithm in library}
#'
#' @author Pan Zhao \email{pan.zhao@@inria.fr}
#'
#' @seealso \code{\link{missSuperLearner}}
#'
#' @keywords models
#' @method predict missSuperLearner
#' @export
predict.missSuperLearner <- function(object, newdata, X = NULL, Y = NULL,
                                     onlySL = FALSE, ...) {
  if (missing(newdata)) {
    out <- list(pred = object$SL.predict, library.predict = object$library.predict)
    return(out)
  }
  
  if (!object$control$saveFitLibrary) {
    stop("This missSuperLearner fit was created using control$saveFitLibrary = FALSE, so new predictions cannot be made.")
  }
  if (is.null(X)) {
    if (is.null(object$X)) {
      stop("X is NULL and this missSuperLearner fit was created using outputX = FALSE, so imputation cannot be done. ")
    } else {
      warning("Replacing the null X by the X used for tranin this missSuperLearner. ")
      X <- object$X
    }
  }

  k <- length(object$libraryNames)
  predY <- matrix(NA, nrow = nrow(newdata), ncol = k)
  colnames(predY) <- object$libraryNames
  
  # imputation
  imp <- .imputeX(X, newdata, object$SL.library, 
                  length(object$SL.library$imputeAlgorithm), object$mice.params)
  impX <- imp$tempLearn
  impNewdata <- imp$tempValid
  rm(imp)
  
  # prediction
  if (onlySL) {
    whichLibrary <- which(object$coef > 0)
    predY <- matrix(0, nrow = nrow(newdata), ncol = k)
    for (mm in whichLibrary) {
      s <- object$SL.library$library[mm, 2]
      i <- object$SL.library$library[mm, 3]
      newdataMM <- subset(impNewdata[[i]],
                          select = object$whichScreen[s, , i])
      family <- object$family
      XMM <- subset(impX[[i]], 
                    select = object$whichScreen[s, , i])
      predY[, mm] <- do.call('predict', list(object = object$fitLibrary[[mm]],
                                             newdata = newdataMM,
                                             family = family,
                                             X = XMM,
                                             Y = Y,
                                             ...))
    }
    getPred <- object$method$computePred(predY = predY, coef = object$coef, control = object$control)
    out <- list(pred = getPred, library.predict = predY)
  } else {
    for (mm in seq(k)) {
      s <- object$SL.library$library[mm, 2]
      i <- object$SL.library$library[mm, 3]
      newdataMM <- subset(impNewdata[[i]], 
                          select = object$whichScreen[s, , i])
      family <- object$family
      XMM <- subset(impX[[i]], 
                    select = object$whichScreen[s, , i])
      predY[, mm] <- do.call('predict', list(object = object$fitLibrary[[mm]],
                                             newdata = newdataMM,
                                             family = family,
                                             X = XMM,
                                             Y = Y,
                                             ...))
    }
    getPred <- object$method$computePred(predY = predY, coef = object$coef, 
                                         control = object$control)
    out <- list(pred = getPred, library.predict = predY)
  }
  return(out)
}

