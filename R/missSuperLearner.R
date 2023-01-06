#' Super Learner Prediction Function That Handles Missing Data
#' 
#' A Prediction Function for the Super Learner that handles missing data. The
#' \code{missSuperLearner} function takes a training set pair (X, Y) where X may
#' contain missing data (NAs), and returns the predicted values based on a 
#' validation set.
#' 
#' \code{missSuperLearner} fits the super learner prediction algorithm that handles
#' missing data. The weights for each prediction algorithm in \code{SL.library}, 
#' in combination with each imputation algorithm in \code{imputeAlgo}, is estimated, 
#' along with the fit of each algorithm.
#' 
#' The imputation algorithms. To incorporate the imputation step into the 
#' cross-validation scheme, three widely used methods are considered: \code{mean},
#' \code{median}, and multivariate imputation by chained equations (\code{mice}).
#' 
#' The pre-screen algorithms. These algorithms first rank the variables in
#' \code{X} based on either a univariate regression p-value of the
#' \code{randomForest} variable importance.  A subset of the variables in
#' \code{X} is selected based on a pre-defined cut-off.  With this subset of
#' the X variables, the algorithms in \code{SL.library} are then fit.
#' 
#' The \code{missSuperLearner} package inherits a few prediction and screening 
#' algorithm wrappers from the \code{SuperLearner} package. The full list of 
#' wrappers can be viewed with \code{listWrappers()}. The design of the 
#' \code{missSuperLearner} package is such that the user can easily add their 
#' own wrappers. 
#' 
#' Note that we add a new wrapper function \code{SL.grf} for the generalized
#' random forests (GRF) algorithm from the \code{grf} package. GRF can handle 
#' missing data implicitly using the missing incorporated in attributes criterion
#' (MIA; Twala et al., 2008).
#' 
#' @param Y The outcome in the training data set. Must be a numeric vector.
#' @param X The predictor variables in the training data set, usually a
#' data.frame.
#' @param newX The predictor variables in the validation data set. The
#' structure should match X. If missing, uses X for newX.
#' @param family Currently allows \code{gaussian} or \code{binomial} to
#' describe the error distribution. Link function information will be ignored
#' and should be contained in the method argument below.
#' @param SL.library Either a character vector of prediction algorithms or a
#' list containing character vectors. See details below for examples on the
#' structure. A list of functions included in the SuperLearner package can be
#' found with \code{listWrappers()}.
#' @param imputeAlgo The imputation algorithms. A character vector of imputation 
#' algorithms.
#' @param mice.params A list of arguments specified for the \code{mice} function.
#' @param max.try The maximal number of tries to generate valid cross-validation
#' folds with missing data.
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
#' @param verbose logical; TRUE for printing progress during the computation
#' (helpful for debugging).
#' @param control A list of parameters to control the estimation process.
#' Parameters include \code{saveFitLibrary} and \code{trimLogit}. See
#' \code{\link{SuperLearner.control}} for details.
#' @param cvControl A list of parameters to control the cross-validation
#' process. Parameters include \code{V}, \code{stratifyCV}, \code{shuffle} and
#' \code{validRows}. See \code{\link{SuperLearner.CV.control}} for details.
#' @param obsWeights Optional observation weights variable. As with \code{id}
#' above, \code{obsWeights} is passed to the prediction and screening
#' algorithms, but many of the built in wrappers ignore (or can't use) the
#' information. If you are using observation weights, make sure the library you
#' specify uses the information.
#' @param outputX logical; TRUE for outputting \code{X} in the returned list 
#' (helpful for future prediction).
#' @param env Environment containing the learner functions. Defaults to the
#' calling environment.
#' 
#' @return \item{call}{ The matched call. } 
#' \item{libraryNames}{ A character
#' vector with the names of the algorithms in the library. The format is
#' 'predictionAlgorithm_screeningAlgorithm' with '_All' used to denote the
#' prediction algorithm run on all variables in X. } 
#' \item{SL.library}{ Returns \code{SL.library} in the same format as the 
#' argument with the same name above. } 
#' \item{imputeAlgo}{ Returns \code{imputeAlgo} in the same format as the 
#' argument with the same name above. }
#' \item{mice.params}{ Returns \code{mice.params} in the same format as the 
#' argument with the same name above. }
#' \item{X}{ Returns \code{X} in the same format as the 
#' argument with the same name above. }
#' \item{SL.predict}{ The predicted values from the super learner for
#' the rows in \code{newX}. } 
#' \item{coef}{ Coefficients for the super learner.
#' } 
#' \item{library.predict}{ A matrix with the predicted values from each
#' algorithm in \code{SL.library} for the rows in \code{newX}. } 
#' \item{Z}{ The
#' Z matrix (the cross-validated predicted values for each algorithm in
#' \code{SL.library}). } 
#' \item{cvRisk}{ A numeric vector with the V-fold
#' cross-validated risk estimate for each algorithm in \code{SL.library}. Note
#' that this does not contain the CV risk estimate for the SuperLearner, only
#' the individual algorithms in the library. } 
#' \item{family}{ Returns the
#' \code{family} value from above } 
#' \item{fitLibrary}{ A list with the fitted
#' objects for each algorithm in \code{SL.library} on the full training data
#' set. } 
#' \item{cvFitLibrary}{ A list with fitted objects for each algorithm in
#' \code{SL.library} on each of \code{V} different training data sets.  }
#' \item{varNames}{ A character vector with the names of the variables in
#' \code{X}. } 
#' \item{validRows}{ A list containing the row numbers for the
#' V-fold cross-validation step. } 
#' \item{method}{ A list with the method
#' functions. } 
#' \item{whichScreen}{ A logical matrix indicating which variables
#' passed each screening algorithm. } 
#' \item{control}{ The \code{control} list.
#' } 
#' \item{cvControl}{ The \code{cvControl} list. } 
#' \item{errorsInCVLibrary}{ A
#' logical vector indicating if any algorithms experienced an error within the
#' CV step. } 
#' \item{errorsInLibrary}{ A logical vector indicating if any
#' algorithms experienced an error on the full data. } 
#' \item{env}{ Environment
#' passed into function which will be searched to find the learner functions.
#' Defaults to the calling environment.  } 
#' \item{times}{ A list that contains
#' the execution time of the SuperLearner, plus separate times for model
#' fitting and prediction.  }
#' 
#' @author Pan Zhao \email{pan.zhao@@inria.fr}
#' @keywords models
#' 
#' @examples
#' 
#' \dontrun{
#' ## simulate data
#' set.seed(1)
#' ## training set
#' n <- 500
#' p <- 10
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' colnames(X) <- paste("X", 1:p, sep="")
#' X <- data.frame(X)
#' Y <- X[, 1] + sqrt(abs(X[, 2] * X[, 3])) + X[, 2] - X[, 3] + rnorm(n)
#' 
#' ## test set
#' m <- 1000
#' newX <- matrix(rnorm(m * p), nrow = m, ncol = p)
#' colnames(newX) <- paste("X", 1:p, sep="")
#' newX <- data.frame(newX)
#' newY <- newX[, 1] + sqrt(abs(newX[, 2] * newX[, 3])) + newX[, 2] -
#'   newX[, 3] + rnorm(m)
#'   
#' ## missing values in X and newX
#' for (i in 1:p) {
#'   X[runif(n) < 0.1, i] <- NA
#'   newX[runif(n) < 0.1, i] <- NA
#' }
#' 
#' # generate Library and run Super Learner
#' SL.library <- c("SL.glm", "SL.ranger", "SL.gam",
#'                 "SL.grf", "SL.mean")
#' test <- missSuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,
#'                          imputeAlgo = c("mean", "median", "mice"),
#'                          verbose = TRUE, method = "method.NNLS")
#' test
#' 
#' # library with screening
#' SL.library <- list(c("SL.glmnet", "All"), 
#'                    c("SL.glm", "screen.randomForest", "All", "screen.SIS"), 
#'                    "SL.randomForest", 
#'                    c("SL.polymars", "All"), 
#'                    "SL.mean")
#' test <- missSuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,
#'                          imputeAlgo = c("mean", "median", "mice"),
#'                          verbose = TRUE, method = "method.NNLS")
#' test
#' 
#' # binary outcome
#' set.seed(1)
#' N <- 200
#' X <- matrix(rnorm(N*10), N, 10)
#' X <- as.data.frame(X)
#' Y <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] +
#'   .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))
#'   
#' for (i in 1:10) {
#'   X[runif(n) < 0.1, i] <- NA
#' }
#' 
#' SL.library <- c("SL.glmnet", "SL.glm", "SL.knn", "SL.mean")
#' 
#' # least squares loss function
#' test.NNLS <- missSuperLearner(Y = Y, X = X, SL.library = SL.library,
#'                               verbose = TRUE, method = "method.NNLS", family = binomial())
#' test.NNLS
#' 
#' # negative log binomial likelihood loss function
#' test.NNloglik <- missSuperLearner(Y = Y, X = X, SL.library = SL.library,
#'                                   verbose = TRUE, method = "method.NNloglik", family = binomial())
#' test.NNloglik
#' 
#' # 1 - AUC loss function
#' test.AUC <- missSuperLearner(Y = Y, X = X, SL.library = SL.library,
#'                              verbose = TRUE, method = "method.AUC", family = binomial())
#' test.AUC
#' 
#' }
#' 
#' @describeIn missSuperLearner A Super Learner Prediction Function That Handles Missing Data
#' @export
missSuperLearner <- function (Y, X, newX = NULL, family = stats::gaussian(), SL.library,
                              imputeAlgo = c("mean", "median", "mice"),
                              mice.params = list(m = 5), max.try = 10,
                              method = "method.NNLS", id = NULL, verbose = FALSE, control = list(), 
                              cvControl = list(), obsWeights = NULL, outputX = FALSE, env = parent.frame()) {
  requireNamespace("SuperLearner")
  time_start = proc.time()
  
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
    stop("Method is not in the appropriate format. Check out help('method.template').")
  }
  if (!is.null(method$require)) {
    sapply(method$require, function(x) require(force(x), character.only = TRUE))
  }
  
  control <- do.call(SuperLearner::SuperLearner.control, control)
  cvControl <- do.call(SuperLearner::SuperLearner.CV.control, cvControl)
  
  library <- .create.missLibrary(SL.library, imputeAlgo)
  .check.SL.library(library = c(unique(library$library$predAlgorithm), library$screenAlgorithm), imputeAlgo)
  
  call <- match.call(expand.dots = TRUE)
  if (!inherits(X, "data.frame")) {
    message("X is not a data frame. Check the algorithms in SL.library to make sure they are compatible with non data.frame inputs.")
  }
  varNames <- colnames(X)
  N <- dim(X)[1L]
  p <- dim(X)[2L]
  k <- nrow(library$library)
  kScreen <- length(library$screenAlgorithm)
  kImpute <- length(library$imputeAlgorithm)
  Z <- matrix(NA, N, k)
  libraryNames <- paste(library$library$predAlgorithm, 
                        library$screenAlgorithm[library$library$rowScreen],
                        library$imputeAlgorithm[library$library$numImpute],
                        sep = "_")
  
  if (p < 2 & !identical(library$screenAlgorithm, "All")) {
    warning("Screening algorithms specified in combination with single-column X.")
  }
  
  fitLibEnv <- new.env()
  assign("fitLibrary", vector("list", length = k), envir = fitLibEnv)
  assign("libraryNames", libraryNames, envir = fitLibEnv)
  evalq(names(fitLibrary) <- libraryNames, envir = fitLibEnv)
  
  errorsInCVLibrary <- rep(0, k)
  errorsInLibrary <- rep(0, k)
  
  if (!is.null(newX) & !identical(colnames(X), colnames(newX))) {
    stop("The variable names and order in newX must be identical to the variable names and order in X.")
  }
  if (sum(is.na(Y)) > 0) {
    stop("Handling missing values in Y is currently not supported. Check Y for missing values.")
  }
  if (!is.numeric(Y)) {
    stop("The outcome Y must be a numeric vector.")
  }
  if (is.character(family)) 
    family <- get(family, mode = "function", envir = parent.frame())
  if (is.function(family)) 
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized.")
  }
  if (family$family != "binomial" & isTRUE("cvAUC" %in% method$require)) {
    stop("'method.AUC' is designed for the 'binomial' family only.")
  }
  
  validRows <- SuperLearner::CVFolds(N = N, id = id, Y = Y, cvControl = cvControl)
  check <- .check.validRows(validRows, X)
  itr <- 0
  while (!check & itr < max.try) {
    validRows <- SuperLearner::CVFolds(N = N, id = id, Y = Y, cvControl = cvControl)
    check <- .check.validRows(validRows, X)
    itr <- itr + 1
  }
  if (!check) {
    stop(paste("Did not manage to sample the folds in such a way that no training sample contains columns with only NAs in", max.try, "tries."))
  }
  
  if (is.null(id)) {
    id <- seq(N)
  }
  if (!identical(length(id), N)) {
    stop("id vector must have the same dimension as Y.")
  }
  if (is.null(obsWeights)) {
    obsWeights <- rep(1, N)
  }
  if (!identical(length(obsWeights), N)) {
    stop("obsWeights vector must have the same dimension as Y.")
  }
  
  .crossValFUN <- function(valid, Y, dataX, id, obsWeights, library,
                           kScreen, kImpute, k, p, libraryNames, saveCVFitLibrary) {
    # Imputation
    tempOutcome <- Y[-valid]
    tempId <- id[-valid]
    tempObsWeights <- obsWeights[-valid]
    impX <- .imputeX(dataX[-valid, , drop = FALSE], dataX[valid, , drop = FALSE], 
                     library, kImpute, mice.params)
    tempLearn <- impX$tempLearn
    tempValid <- impX$tempValid
    rm(impX)
    
    # Screening
    tempWhichScreen <- array(NA, dim = c(kScreen, p, kImpute))
    for (s in seq(kScreen)) {
      for (i in seq(kImpute)) {
        if (any(library$library$rowScreen == s & library$library$numImpute == i)) {
          if (exists(library$screenAlgorithm[s], envir = asNamespace("SuperLearner"))) {
            screen_fn <- get(library$screenAlgorithm[s], envir = asNamespace("SuperLearner"))
          } else {
            screen_fn <- get(library$screenAlgorithm[s], envir = env)
          }
          testScreen <- try(do.call(screen_fn, list(Y = tempOutcome, X = tempLearn[[i]], 
                                                    family = family, id = tempId, obsWeights = tempObsWeights)))
          if(inherits(testScreen, "try-error")) {
            warning(paste("replacing failed screening algorithm,", library$screenAlgorithm[s], ", with All()", "\n "))
            tempWhichScreen[s, , i] <- TRUE
          } else {
            tempWhichScreen[s, , i] <- testScreen
          }
          if(verbose) {
            message(paste("Number of covariates in ", library$screenAlgorithm[s],
                          " with imputation algorithm ", library$imputeAlgorithm[i],
                          " is: ", sum(tempWhichScreen[s, , i]), sep = ""))
          }
        }
      }
    }
   
    out <- matrix(NA, nrow = length(valid), ncol = k)
    if (saveCVFitLibrary) {
      model_out <- vector(mode = "list", length = k)
    } else {
      model_out <- NULL
    }
    
    # Prediction
    for (s in seq(k)) {
      i <- library$library$numImpute[s]
      if (exists(library$library$predAlgorithm[s], envir = asNamespace("SuperLearner"))) {
        pred_fn <- get(library$library$predAlgorithm[s], envir = asNamespace("SuperLearner"))
      } else {
        pred_fn <- get(library$library$predAlgorithm[s], envir = env)
      }
      testAlg <- try(do.call(pred_fn, list(Y = tempOutcome, 
                                           X = subset(tempLearn[[i]], select = tempWhichScreen[library$library$rowScreen[s], , i], drop=FALSE), 
                                           newX = subset(tempValid[[i]], select = tempWhichScreen[library$library$rowScreen[s], , i], drop=FALSE), 
                                           family = family, id = tempId, obsWeights = tempObsWeights)))
      if(inherits(testAlg, "try-error")) {
        warning(paste("Error in algorithm ", library$library$predAlgorithm[s], 
                      " with imputation algorithm ", library$imputeAlgorithm[i],
                      "\n  This Algorithm will be removed from the missSuperLearner (i.e. given weight 0) \n" ))
      } else {
        out[, s] <- testAlg$pred
        if(saveCVFitLibrary){
          model_out[[s]] <- testAlg$fit
        }
      }
      if (verbose) message(paste("CV", libraryNames[s]))
    }
    if (saveCVFitLibrary) {
      names(model_out) <- libraryNames
    }
    invisible(list(out = out, model_out = model_out))
  }
  
  time_train_start = proc.time()
  crossValFUN_out <- lapply(validRows, FUN = .crossValFUN, 
                            Y = Y, dataX = X, id = id, obsWeights = obsWeights,
                            library = library, kScreen = kScreen, kImpute = kImpute,
                            k = k, p = p, libraryNames = libraryNames, 
                            saveCVFitLibrary = control$saveCVFitLibrary)
  Z[unlist(validRows, use.names = FALSE), ] <- do.call("rbind", lapply(crossValFUN_out, "[[", "out"))
  if (control$saveCVFitLibrary) {
    cvFitLibrary <- lapply(crossValFUN_out, "[[", "model_out")
  } else {
    cvFitLibrary <- NULL
  }
  
  # Check for errors. If any algorithms had errors, replace entire column with 0
  # even if error is only in one fold.
  errorsInCVLibrary <- apply(Z, 2, function(x) anyNA(x))
  if (sum(errorsInCVLibrary) > 0) {
    Z[, as.logical(errorsInCVLibrary)] <- 0
  }
  if (all(Z == 0)) {
    stop("All algorithms dropped from library.")
  }
  
  getCoef <- method$computeCoef(Z = Z, Y = Y, libraryNames = libraryNames, 
                                obsWeights = obsWeights, control = control, 
                                verbose = verbose, 
                                errorsInLibrary = errorsInCVLibrary)
  coef <- getCoef$coef
  names(coef) <- libraryNames
  
  time_train = proc.time() - time_train_start
  
  if (!("optimizer" %in% names(getCoef))) {
    getCoef["optimizer"] <- NA
  }
  
  # now fit all algorithms in library on entire learning data set and predict on newX
  # imputation
  if (is.null(newX)) {
    impX <- .imputeX(X, NULL, library, kImpute, mice.params)$tempLearn
    impNewX <- impX
  } else {
    imp <- .imputeX(X, newX, library, kImpute, mice.params)
    impX <- imp$tempLearn
    impNewX <- imp$tempValid
    rm(imp)
  }

  m <- dim(impNewX[[1]])[1L]
  predY <- matrix(NA, nrow = m, ncol = k)
  
  time_predict_start = proc.time()
  # screening
  whichScreen <- array(NA, dim = c(kScreen, p, kImpute))
  for (s in seq(kScreen)) {
    for (i in seq(kImpute)) {
      if (any(library$library$rowScreen == s & library$library$numImpute ==i)) {
        if (exists(library$screenAlgorithm[s], envir = asNamespace("SuperLearner"))) {
          screen_fn <- get(library$screenAlgorithm[s], envir = asNamespace("SuperLearner"))
        } else {
          screen_fn <- get(library$screenAlgorithm[s], envir = env)
        }
        testScreen <- try(do.call(screen_fn, list(Y = Y, X = impX[[i]], 
                                                  family = family, id = id, 
                                                  obsWeights = obsWeights)))
        if(inherits(testScreen, "try-error")) {
          warning(paste("replacing failed screening algorithm,", library$screenAlgorithm[s], ", with All()", "\n "))
          whichScreen[s, , i] <- TRUE
        } else {
          whichScreen[s, , i] <- testScreen
        }
      }
    }
  }
  
  # prediction
  .predFun <- function(index, lib, Y, dataX, newX, whichScreen, 
                       family, id, obsWeights, verbose, control, libraryNames) {
    if (exists(lib$predAlgorithm[index], envir = asNamespace("SuperLearner"))) {
      pred_fn <- get(lib$predAlgorithm[index], envir = asNamespace("SuperLearner"))
    } else {
      pred_fn <- get(lib$predAlgorithm[index], envir = env)      
    }
    s <- lib$rowScreen[index]
    i <- lib$numImpute[index]
    testAlg <- try(do.call(pred_fn, list(Y = Y, 
                                         X = subset(dataX[[i]], select = whichScreen[s, , i], drop = FALSE), 
                                         newX = subset(newX[[i]], select = whichScreen[s, , i], drop = FALSE),
                                         family = family, id = id, obsWeights = obsWeights)))
    if (inherits(testAlg, "try-error")) {
      warning(paste("Error in algorithm ", lib$predAlgorithm[index], 
                    " with imputation algorithm ", library$imputeAlgorithm[i],
                    " on full data. \n  The Algorithm will be removed from the Super Learner (i.e. given weight 0). \n"))
      out <- rep.int(NA, times = nrow(newX))
    } else {
      out <- testAlg$pred
      if (control$saveFitLibrary) {
        eval(bquote(fitLibrary[[.(index)]] <- .(testAlg$fit)), envir = fitLibEnv)
      }
    }
    if (verbose) {
      message(paste("full", libraryNames[index]))
    }
    invisible(out)
  }
  
  predY <- do.call("cbind", lapply(seq(k), FUN = .predFun, 
                                   lib = library$library, Y = Y, dataX = impX, newX = impNewX, 
                                   whichScreen = whichScreen, family = family, id = id, 
                                   obsWeights = obsWeights, verbose = verbose, control = control, 
                                   libraryNames = libraryNames))
  
  errorsInLibrary <- apply(predY, 2, function(algorithm) anyNA(algorithm))
  if (sum(errorsInLibrary) > 0) {
    if (sum(coef[as.logical(errorsInLibrary)]) > 0) {
      warning(paste0("Re-running estimation of coefficients removing failed algorithm(s)\n", 
                     "Original coefficients are: \n", paste(coef, collapse = ", "), ".\n"))
      Z[, as.logical(errorsInLibrary)] <- 0
      if (all(Z == 0)) {
        stop("All algorithms dropped from library.")
      }
      getCoef <- method$computeCoef(Z = Z, Y = Y, libraryNames = libraryNames, 
                                    obsWeights = obsWeights, control = control, verbose = verbose, 
                                    errorsInLibrary = errorsInLibrary)
      coef <- getCoef$coef
      names(coef) <- libraryNames
    } else {
      warning("Coefficients already 0 for all failed algorithm(s).")
    }
  }
  
  getPred <- method$computePred(predY = predY, coef = coef, control = control)
  
  time_predict = proc.time() - time_predict_start
  
  colnames(predY) <- libraryNames

  if (sum(errorsInCVLibrary) > 0) {
    getCoef$cvRisk[as.logical(errorsInCVLibrary)] <- NA
  }
  
  time_end <- proc.time()
  times <- list(everything = time_end - time_start, 
               train = time_train, 
               predict = time_predict)
  
  out <- list(call = call,
              libraryNames = libraryNames, 
              SL.library = library,
              imputeAlgo = imputeAlgo,
              mice.params = mice.params,
              X = if (outputX) X,
              SL.predict = getPred, 
              coef = coef, 
              library.predict = predY, 
              Z = Z, 
              cvRisk = getCoef$cvRisk, 
              family = family,
              fitLibrary = get("fitLibrary", envir = fitLibEnv),
              cvFitLibrary = cvFitLibrary, 
              varNames = varNames, 
              validRows = validRows, 
              method = method, 
              whichScreen = whichScreen, 
              control = control, 
              cvControl = cvControl, 
              errorsInCVLibrary = errorsInCVLibrary, 
              errorsInLibrary = errorsInLibrary, 
              metaOptimizer = getCoef$optimizer, 
              env = env, 
              times = times)
  class(out) <- c("missSuperLearner", "SuperLearner")
  return(out)
}


