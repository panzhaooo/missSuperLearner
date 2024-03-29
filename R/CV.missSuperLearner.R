#' Function to get V-fold cross-validated risk estimate for missSuperLearner
#' 
#' Function to get V-fold cross-validated risk estimate for missSuperLearner. This
#' function simply splits the data into V folds and then calls missSuperLearner.
#' Most of the arguments are passed directly to missSuperLearner.
#' 
#' The \code{missSuperLearner} function builds a estimator, but does not contain 
#' an estimate on the performance of the estimator. Various methods exist for
#' estimator performance evaluation. If you are familiar with the super learner
#' algorithm, it should be no surprise we recommend using cross-validation to
#' evaluate the honest performance of the super learner estimator. The function
#' \code{CV.missSuperLearner} computes the usual V-fold cross-validated risk
#' estimate for the missSuperLearner (and all algorithms in \code{SL.library} 
#' for comparison).
#' 
#' @aliases CV.missSuperLearner print.CV.missSuperLearner coef.CV.missSuperLearner
#' 
#' @param Y The outcome in the training data set. Must be a numeric vector.
#' @param X The predictor variables in the training data set, usually a
#' data.frame.
#' @param imputeAlgo The imputation algorithms. A character vector of imputation 
#' algorithms.
#' @param mice.params A list of arguments specified for the \code{mice} function.
#' @param max.try The maximal number of tries to generate valid cross-validation
#' folds with missing data.
#' @param V The number of folds for \code{CV.missSuperLearner}. This argument will
#' be depreciated and moved into the \code{cvControl}. If Both \code{V} and
#' \code{cvControl} set the number of cross-validation folds, an error message
#' will appear. The recommendation is to use \code{cvControl}. This is not the
#' number of folds for \code{missSuperLearner}. The number of folds for
#' \code{missSuperLearner} is controlled with \code{innerCvControl}.
#' @param family Currently allows \code{gaussian} or \code{binomial} to
#' describe the error distribution. Link function information will be ignored
#' and should be contained in the method argument below.
#' @param SL.library Either a character vector of prediction algorithms or a
#' list containing character vectors. See details below for examples on the
#' structure. A list of functions included in the SuperLearner package can be
#' found with \code{listWrappers()}.
#' @param method A list (or a function to create a list) containing details on
#' estimating the coefficients for the super learner and the model to combine
#' the individual algorithms in the library. See \code{?method.template} for
#' details.  Currently, the built in options are either "method.NNLS" (the
#' default), "method.NNLS2", "method.NNloglik", "method.CC_LS",
#' "method.CC_nloglik", or "method.AUC".  NNLS and NNLS2 are non-negative least
#' squares based on the Lawson-Hanson algorithm and the dual method of Goldfarb
#' and Idnani, respectively.  NNLS and NNLS2 will work for both gaussian and
#' binomial outcomes.  NNloglik is a non-negative binomial likelihood
#' maximization using the BFGS quasi-Newton optimization method. NN* methods
#' are normalized so weights sum to one. CC_LS uses Goldfarb and Idnani's
#' quadratic programming algorithm to calculate the best convex combination of
#' weights to minimize the squared error loss. CC_nloglik calculates the convex
#' combination of weights that minimize the negative binomial log likelihood on
#' the logistic scale using the sequential quadratic programming algorithm.
#' AUC, which only works for binary outcomes, uses the Nelder-Mead method via
#' the optim function to minimize rank loss (equivalent to maximizing AUC).
#' @param id Optional cluster identification variable. For the cross-validation
#' splits, \code{id} forces observations in the same cluster to be in the same
#' validation fold. \code{id} is passed to the prediction and screening
#' algorithms in SL.library, but be sure to check the individual wrappers as
#' many of them ignore the information.
#' @param verbose Logical; TRUE for printing progress during the computation
#' (helpful for debugging).
#' @param control A list of parameters to control the estimation process.
#' Parameters include \code{saveFitLibrary} and \code{trimLogit}. See
#' \code{\link{SuperLearner.control}} for details.
#' @param cvControl A list of parameters to control the outer cross-validation
#' process. The outer cross-validation is the sample spliting for evaluating
#' the SuperLearner. Parameters include \code{V}, \code{stratifyCV},
#' \code{shuffle} and \code{validRows}. See
#' \code{\link{SuperLearner.CV.control}} for details.
#' @param innerCvControl A list of lists of parameters to control the inner
#' cross-validation process. It should have \code{V} elements in the list, each
#' a valid \code{cvControl} list. If only a single value, then replicated
#' across all folds. The inner cross-validation are the values passed to each
#' of the \code{V} \code{SuperLearner} calls. Parameters include \code{V},
#' \code{stratifyCV}, \code{shuffle} and \code{validRows}. See
#' \code{\link{SuperLearner.CV.control}} for details.
#' @param obsWeights Optional observation weights variable. As with \code{id}
#' above, \code{obsWeights} is passed to the prediction and screening
#' algorithms, but many of the built in wrappers ignore (or can't use) the
#' information. If you are using observation weights, make sure the library you
#' specify uses the information.
#' @param saveAll Logical; Should the entire \code{missSuperLearner} object be
#' saved for each fold?
#' @param parallel Options for parallel computation of the V-fold step. Use
#' "seq" (the default) for sequential computation. \code{parallel =
#' 'multicore'} to use \code{mclapply} for the V-fold step (but note that
#' \code{missSuperLearner()} will still be sequential). The default for mclapply is
#' to check the \code{mc.cores} option, and if not set to default to 2 cores.
#' Be sure to set \code{options()$mc.cores} to the desired number of cores if
#' you don't want the default. Or \code{parallel} can be the name of a snow
#' cluster and will use \code{parLapply} for the V-fold step. For both
#' multicore and snow, the inner \code{missSuperLearner} calls will be sequential.
#' @param env Environment containing the learner functions. Defaults to the
#' calling environment.
#' 
#' @return An object of class \code{CV.missSuperLearner} (a list) with components:
#' 
#' \item{call}{ The matched call. } 
#' \item{AllSL}{ If \code{saveAll = TRUE}, a
#' list with output from each call to \code{missSuperLearner}, otherwise NULL. }
#' \item{SL.predict}{ The predicted values from the super learner when each
#' particular row was part of the validation fold. } 
#' \item{discreteSL.predict}{
#' The traditional cross-validated selector. Picks the algorithm with the
#' smallest cross-validated risk (in super learner terms, gives that algorithm
#' coefficient 1 and all others 0). } 
#' \item{whichDiscreteSL}{ A list of length
#' \code{V}. The elements in the list are the algorithm that had the smallest
#' cross-validated risk estimate for that fold. } 
#' \item{library.predict}{ A
#' matrix with the predicted values from each algorithm in \code{SL.library}.
#' The columns are the algorithms in \code{SL.library} and the rows represent
#' the predicted values when that particular row was in the validation fold
#' (i.e. not used to fit that estimator). } 
#' \item{coef}{ A matrix with the
#' coefficients for the super learner on each fold. The columns are the
#' algorithms in \code{SL.library} the rows are the folds. } 
#' \item{folds}{ A
#' list containing the row numbers for each validation fold. } 
#' \item{V}{ Number
#' of folds for \code{CV.missSuperLearner}. } 
#' \item{libraryNames}{ A character
#' vector with the names of the algorithms in the library. The format is
#' 'predictionAlgorithm_screeningAlgorithm' with '_All' used to denote the
#' prediction algorithm run on all variables in X. } 
#' \item{SL.library}{ Returns
#' \code{SL.library} in the same format as the argument with the same name
#' above. } 
#' \item{method}{ A list with the method functions. } 
#' \item{Y}{ The
#' outcome }
#' 
#' @author Pan Zhao \email{pan.zhao@@inria.fr}
#' 
#' @seealso \code{\link{missSuperLearner}}
#' @keywords models
#' 
#' @examples
#' 
#' \dontrun{
#' set.seed(1)
#' ## training set
#' n <- 500
#' p <- 10
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' colnames(X) <- paste("X", 1:p, sep="")
#' X <- data.frame(X)
#' Y <- X[, 1] + sqrt(abs(X[, 2] * X[, 3])) + X[, 2] - X[, 3] + rnorm(n)
#' 
#' ## missing values in X
#' for (i in 1:p) {
#'   X[runif(n) < 0.1, i] <- NA
#' }
#' 
#' ## build Library and run Super Learner
#' SL.library <- c("SL.glm", "SL.ranger", "SL.grf", "SL.mean")
#' 
#' test <- CV.missSuperLearner(Y = Y, X = X, V = 10, SL.library = SL.library,
#'                             imputeAlgo = c("mean", "median", "mice"),
#'                             verbose = TRUE, method = "method.NNLS")
#' test
#' summary(test)
#' ## Look at the coefficients across folds
#' coef(test)
#' 
#' # Example with specifying cross-validation options for both 
#' # CV.missSuperLearner (cvControl) and the internal missSuperLearners (innerCvControl)
#' test <- CV.missSuperLearner(Y = Y, X = X, SL.library = SL.library,
#'                             cvControl = list(V = 10, shuffle = FALSE),
#'                             innerCvControl = list(list(V = 5)),
#'                             verbose = TRUE, method = "method.NNLS")
#' 
#' ## examples with snow
#' library(parallel)
#' cl <- makeCluster(2, type = "PSOCK") # can use different types here
#' clusterSetRNGStream(cl, iseed = 2343)
#' testSNOW <- CV.missSuperLearner(Y = Y, X = X, SL.library = SL.library, method = "method.NNLS",
#'                                 parallel = cl)
#' summary(testSNOW)
#' stopCluster(cl)
#' }
#' 
#' @export
CV.missSuperLearner <- function (Y, X,
                                 V = NULL, family = stats::gaussian(), SL.library, 
                                 imputeAlgo = c("mean", "median", "mice"),
                                 mice.params = list(m = 5), max.try = 10,
                                 method = "method.NNLS", id = NULL, verbose = FALSE, 
                                 control = list(saveFitLibrary = FALSE), 
                                 cvControl = list(), innerCvControl = list(), obsWeights = NULL,
                                 saveAll = TRUE, parallel = "seq", env = parent.frame()) {
  call <- match.call()
  requireNamespace("SuperLearner")
  N <- dim(X)[1L]
  p <- dim(X)[2L]
  if (any(names(cvControl) == "V") & !is.null(V)) {
    stop(paste0("You specified a value for V and a value in the cvControl, please only use one, preferably the cvControl"))
  }
  
  cvControl <- do.call(SuperLearner::SuperLearner.CV.control, cvControl)
  if (!is.null(V)) {
    cvControl$V <- V
  }
  folds <- SuperLearner::CVFolds(N = N, id = id, Y = Y, cvControl = cvControl)
  V <- cvControl$V
  
  if (length(innerCvControl) > 0) {
    if (length(innerCvControl) == 1) {
      warning("Only a single innerCvControl is given, will be replicated across all cross-validation split calls to missSuperLearner")
      newInnerCvControl <- vector("list", cvControl$V)
      for (ii in seq(cvControl$V)) {
        newInnerCvControl[[ii]] <- unlist(innerCvControl, recursive = FALSE)
      }
      innerCvControl <- newInnerCvControl
    }
    if (length(innerCvControl) != cvControl$V) 
      stop("innerCvControl must be a list with V cvControl lists")
  } else {
    innerCvControl <- vector("list", cvControl$V)
    for (ii in seq(cvControl$V)) {
      innerCvControl[[ii]] <- list()
    }
  }
  foldsList <- Map(list, folds = folds, cvControl = innerCvControl)
  
  if (is.null(obsWeights)) {
    obsWeights <- rep(1, N)
  }
  if (!identical(length(obsWeights), N)) {
    stop("obsWeights vector must have the same dimension as Y")
  }
  
  if (is.character(method)) {
    if (exists(method, envir = asNamespace("SuperLearner"))) {
      method_fun <- get(method, envir = asNamespace("SuperLearner"))
      method <- method_fun()
    } else {
      if (exists(method, mode = "list")) {
        method <- get(method, mode = "list")
      } else if (exists(method, mode = "function")) {
        method <- get(method, mode = "function")()
      }
    }
  } else if (is.function(method)) {
    method <- method()
  }
  if (!is.list(method)) {
    stop("method is not in the appropriate format. Check out help('method.template')")
  }
  
  library <- .create.missLibrary(SL.library, imputeAlgo)
  .check.SL.library(library = c(unique(library$library$predAlgorithm), library$screenAlgorithm), imputeAlgo)
  libraryNames <- paste(library$library$predAlgorithm, 
                        library$screenAlgorithm[library$library$rowScreen],
                        library$imputeAlgorithm[library$library$numImpute],
                        sep = "_")
  
  k <- nrow(library$library)
  AllSL <- vector("list", V)
  names(AllSL) <- paste("training", 1:V, sep = " ")
  SL.predict <- rep(NA, N)
  discreteSL.predict <- rep.int(NA, N)
  whichDiscreteSL <- rep.int(NA, V)
  library.predict <- matrix(NA, nrow = N, ncol = k)
  colnames(library.predict) <- libraryNames
  
  if (p < 2 & !identical(library$screenAlgorithm, "All")) {
    warning("Screening algorithms specified in combination with single-column X.")
  }
  
  .crossValFun <- function(valid, Y, dataX, family, id, obsWeights,
                           SL.library, method, verbose, control, saveAll) {
    cvLearn <- dataX[-valid[[1]], , drop = FALSE]
    cvOutcome <- Y[-valid[[1]]]
    cvValid <- dataX[valid[[1]], , drop = FALSE]
    cvId <- id[-valid[[1]]]
    cvObsWeights <- obsWeights[-valid[[1]]]
    fit.SL <- missSuperLearner(Y = cvOutcome, X = cvLearn, newX = cvValid,
                               imputeAlgo = imputeAlgo, mice.params = mice.params, max.try = max.try,
                               family = family, SL.library = SL.library, method = method, 
                               id = cvId, verbose = verbose, control = control, 
                               cvControl = valid[[2]], obsWeights = cvObsWeights, outputX = FALSE, env = env)
    out <- list(cvAllSL = if (saveAll) fit.SL, 
                cvSL.predict = fit.SL$SL.predict, 
                cvdiscreteSL.predict = fit.SL$library.predict[, which.min(fit.SL$cvRisk)], 
                cvwhichDiscreteSL = names(which.min(fit.SL$cvRisk)), 
                cvlibrary.predict = fit.SL$library.predict, 
                cvcoef = fit.SL$coef)
    return(out)
  }
  
  if (inherits(parallel, "cluster")) {
    .SL.require("parallel")
    cvList <- parallel::parLapply(parallel, X = foldsList, 
                                  fun = .crossValFun, Y = Y, dataX = X, family = family, 
                                  SL.library = SL.library, method = method, id = id, 
                                  obsWeights = obsWeights, verbose = verbose, control = control, 
                                  saveAll = saveAll)
  } else if (parallel == "multicore") {
    .SL.require("parallel")
    cvList <- parallel::mclapply(foldsList, FUN = .crossValFun, 
                                 Y = Y, dataX = X, family = family, SL.library = SL.library, 
                                 method = method, id = id, obsWeights = obsWeights,
                                 verbose = verbose, control = control, saveAll = saveAll, 
                                 mc.set.seed = FALSE)
  } else if (parallel == "seq") {
    cvList <- lapply(foldsList, FUN = .crossValFun, Y = Y, 
                     dataX = X, family = family, SL.library = SL.library, 
                     method = method, id = id, obsWeights = obsWeights,
                     verbose = verbose, control = control, saveAll = saveAll)
  } else {
    stop("parallel option was not recognized, use parallel = \"seq\" for sequential computation.")
  }
  
  AllSL <- lapply(cvList, "[[", "cvAllSL")
  SL.predict[unlist(folds, use.names = FALSE)] <- unlist(lapply(cvList, "[[", "cvSL.predict"), use.names = FALSE)
  discreteSL.predict[unlist(folds, use.names = FALSE)] <- unlist(lapply(cvList, "[[", "cvdiscreteSL.predict"), use.names = FALSE)
  whichDiscreteSL <- lapply(cvList, "[[", "cvwhichDiscreteSL")
  library.predict[unlist(folds, use.names = FALSE), ] <- do.call("rbind", lapply(cvList, "[[", "cvlibrary.predict"))
  coef <- do.call("rbind", lapply(cvList, "[[", "cvcoef"))
  colnames(coef) <- libraryNames
  
  out <- list(call = call, AllSL = AllSL, SL.predict = SL.predict, 
              discreteSL.predict = discreteSL.predict, whichDiscreteSL = whichDiscreteSL, 
              library.predict = library.predict, coef = coef, folds = folds, 
              V = V, libraryNames = libraryNames, SL.library = library, 
              method = method, Y = Y)
  class(out) <- c("CV.missSuperLearner", "CV.SuperLearner")
  return(out)
}

#' Summary Function for Cross-Validated missSuperLearner
#' @param object An object of class "CV.missSuperLearner", the result of a call to CV.missSuperLearner. 
#' @param obsWeights Optional vector for observation weights.
#' @param ... Additional arguments.
#'
#' @export
summary.CV.missSuperLearner <- function(object, obsWeights = NULL, ...){
  if ("env" %in% names(object)) {
    env <- object$env
  }
  else {
    env <- parent.frame()
  }
  method <- if (is.null(as.list(object$call)[["method"]])) {
              method <- "method.NNLS"
            } else if (is.symbol(as.list(object$call)[["method"]])) {
              method <- get(paste(as.list(object$call)[["method"]]), 
                            envir = env)
            } else {
              method <- as.list(object$call)[["method"]]
            }
  if (is.call(method)) {
    method <- deparse(substitute(method))
  }
  library.names <- colnames(stats::coef(object))
  V <- object$V
  n <- length(object$SL.predict)
  if (is.null(obsWeights)) {
    obsWeights <- rep(1, length(object$Y))
  }
  folds <- object$folds
  SL.predict <- object$SL.predict
  discreteSL.predict <- object$discreteSL.predict
  library.predict <- object$library.predict
  Y <- object$Y
  Risk.SL <- rep(NA, length = V)
  Risk.dSL <- rep(NA, length = V)
  Risk.library <- matrix(NA, nrow = length(library.names), 
                         ncol = V)
  rownames(Risk.library) <- library.names
  if (method %in% c("method.NNLS", "method.NNLS2", "method.CC_LS",
                    "SuperLearner::method.NNLS", "SuperLearner::method.NNLS2", "SuperLearner::method.CC_LS")) {
    for (ii in seq_len(V)) {
      Risk.SL[ii] <- mean(obsWeights[folds[[ii]]] * (Y[folds[[ii]]] - 
                                                     SL.predict[folds[[ii]]])^2)
      Risk.dSL[ii] <- mean(obsWeights[folds[[ii]]] * (Y[folds[[ii]]] - 
                                                      discreteSL.predict[folds[[ii]]])^2)
      Risk.library[, ii] <- apply(library.predict[folds[[ii]], 
                                                , drop = FALSE], 2, function(x) mean(obsWeights[folds[[ii]]] * 
                                                                                     (Y[folds[[ii]]] - x)^2))
    }
    se <- (1/sqrt(n)) * c(stats::sd(obsWeights * (Y - SL.predict)^2), 
                          stats::sd(obsWeights * (Y - discreteSL.predict)^2), apply(library.predict, 
                                                                             2, function(x) stats::sd(obsWeights * (Y - x)^2)))
  }
  else if (method %in% c("method.NNloglik", "method.CC_nloglik",
                         "SuperLearner::method.NNloglik", "SuperLearner::method.CC_nloglik")) {
    for (ii in seq_len(V)) {
      Risk.SL[ii] <- -mean(obsWeights[folds[[ii]]] * ifelse(Y[folds[[ii]]], 
                                                            log(SL.predict[folds[[ii]]]), log(1 - SL.predict[folds[[ii]]])))
      Risk.dSL[ii] <- -mean(obsWeights[folds[[ii]]] * ifelse(Y[folds[[ii]]], 
                                                             log(discreteSL.predict[folds[[ii]]]), log(1 - 
                                                                                                       discreteSL.predict[folds[[ii]]])))
      Risk.library[, ii] <- apply(library.predict[folds[[ii]], 
                                                , drop = FALSE], 2, function(x) {
                                                  -mean(obsWeights[folds[[ii]]] * ifelse(Y[folds[[ii]]], 
                                                                                         log(x), log(1 - x)))
                                                })
    }
    se <- rep.int(NA, (length(library.names) + 2))
  }
  else if (method %in% c("method.AUC", "SuperLearner::method.AUC")) {
    requireNamespace("cvAUC")
    for (ii in seq_len(V)) {
      Risk.SL[ii] <- cvAUC::cvAUC(predictions = SL.predict[folds[[ii]]], 
                                  labels = Y[folds[[ii]]], folds = NULL)$cvAUC
      Risk.dSL[ii] <- cvAUC::cvAUC(predictions = discreteSL.predict[folds[[ii]]], 
                                   labels = Y[folds[[ii]]], folds = NULL)$cvAUC
      Risk.library[, ii] <- apply(library.predict[folds[[ii]], 
                                                , drop = FALSE], 2, function(x) cvAUC::cvAUC(predictions = x, 
                                                                                             labels = Y[folds[[ii]]], folds = NULL)$cvAUC)
    }
    se <- rep.int(NA, (length(library.names) + 2))
  }
  else {
    stop("summary function not available for missSuperLearner with loss function/method used")
  }
  Table <- data.frame(Algorithm = c("Super Learner", "Discrete SL", library.names),
                      Ave = c(mean(Risk.SL), mean(Risk.dSL), 
                              apply(Risk.library, 1, mean)),
                      se = se, Min = c(min(Risk.SL), min(Risk.dSL),
                                       apply(Risk.library, 1, min)),
                      Max = c(max(Risk.SL), 
                                                                                                                                                                 max(Risk.dSL), apply(Risk.library, 1, max)))
  out <- list(call = object$call, method = method, V = V, Risk.SL = Risk.SL, 
              Risk.dSL = Risk.dSL, Risk.library = Risk.library, Table = Table)
  class(out) <- "summary.CV.missSuperLearner"
  return(out)
}


print.summary.CV.missSuperLearner <- function(x, digits = max(2, getOption("digits") - 2), ...) {
  cat("\nCall: ", deparse(x$call, width.cutoff = .9*getOption("width")), "\n", fill = getOption("width"))
  cat("Risk is based on: ")
  if(x$method %in% c("method.NNLS", "method.NNLS2", "method.CC_LS",
                     "SuperLearner::method.NNLS", "SuperLearner::method.NNLS2", "SuperLearner::method.CC_LS")) {
    cat("Mean Squared Error")
  } else if (x$method %in% c("method.NNloglik", "method.CC_nloglik",
                             "SuperLearner::method.NNloglik", "SuperLearner::method.CC_nloglik")) {
    cat("Negative Log Likelihood (-2*log(L))")
  } else if (x$method %in% c("method.AUC", "SuperLearner::method.AUC")) {
    cat("Area under ROC curve (AUC)")
  } else {
    stop("summary method not available")
  }
  cat("\n\nAll risk estimates are based on V = ", x$V, "\n\n")
  print(x$Table, digits = digits, row.names = FALSE)
}
