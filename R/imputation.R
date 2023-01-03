#' Imputation by mean
#' 
#' For each incomplete variable, replace the missing values using the mean of
#' observed values in training data. Note that for factor/integer variables, the
#' \strong{mode} is used instead of mean.
#' 
#' @param learn The training data.
#' @param val The validation data, or \code{NULL}.
#' @return A list of two components:
#' \item{imputed_learn}{ The imputed training data. }
#' \item{imputed_val}{ The imputed validation data, or \code{NULL} when 
#' \code{val = NULL}. }
#' @export
meanImpute <- function(learn, val) {
  n_learn <- nrow(learn)
  impute_by_mean <- function(x) {
    if (sum(is.na(x)) > 0) {
      x_learn <- x[1:n_learn]
      if (is.factor(x_learn) | is.integer(x_learn)) {
        x.vals <- unique(x_learn[!is.na(x_learn)])
        x.mean <- x.vals[which.max(tabulate(match(x_learn, x.vals)))]
      } else {
        x.mean <- mean(x_learn, na.rm = TRUE)
      }
      x[is.na(x)] <- x.mean
    }
    return(x)
  }
  
  imputedX <- as.data.frame(lapply(as.data.frame(rbind(learn, val)), 
                                   impute_by_mean))
  imputed_learn <- imputedX[1:n_learn, ]
  if (is.null(val)) {
    imputed_val <- NULL
  } else {
    imputed_val <- imputedX[-(1:n_learn), ]
  }
  return(list(imputed_learn = imputed_learn,
              imputed_val = imputed_val))
}


#' Imputation by median
#' 
#' For each incomplete variable, replace the missing values using the median of
#' observed values in training data. Note that for factor/integer variables, the
#' \strong{mode} is used instead of median.
#' 
#' @param learn The training data.
#' @param val The validation data, or \code{NULL}.
#' @return A list of two components:
#' \item{imputed_learn}{ The imputed training data. }
#' \item{imputed_val}{ The imputed validation data, or \code{NULL} when 
#' \code{val = NULL}. }
#' @export
medianImpute <- function(learn, val) {
  n_learn <- nrow(learn)
  impute_by_median <- function(x) {
    if (sum(is.na(x)) > 0) {
      x_learn <- x[1:n_learn]
      if (is.factor(x_learn) | is.integer(x_learn)) {
        x.vals <- unique(x_learn[!is.na(x_learn)])
        x.median <- x.vals[which.max(tabulate(match(x_learn, x.vals)))]
      } else {
        x.median <- stats::median(x_learn, na.rm = TRUE)
      }
      x[is.na(x)] <- x.median
    }
    return(x)
  }
  
  imputedX <- as.data.frame(lapply(as.data.frame(rbind(learn, val)), 
                                   impute_by_median))
  imputed_learn <- imputedX[1:n_learn, ]
  if (is.null(val)) {
    imputed_val <- NULL
  } else {
    imputed_val <- imputedX[-(1:n_learn), ]
  }
  return(list(imputed_learn = imputed_learn,
              imputed_val = imputed_val))
}


#' Imputation by averaging multiple complete data from mice
#' 
#' First, generate \code{m} multivariate imputations by chained equations (MICE) 
#' using the \code{mice} function from the \pkg{mice} package; then average
#' over the \code{m} imputations. Note that for factor/integer variables, the
#' \strong{mode} is used as average instead of mean.
#' 
#' @param learn The training data.
#' @param val The validation data, or \code{NULL}.
#' @param mice.params A list of arguments specified for the \code{mice} function.
#' @return A list of two components:
#' \item{imputed_learn}{ The imputed training data. }
#' \item{imputed_val}{ The imputed validation data, or \code{NULL} when 
#' \code{val = NULL}. }
#' @export
miceImpute <- function(learn, val, mice.params) {
  # first check for factor/integer column if in learn only takes one single value and NA
  # then impute by the single value
  n_learn <- nrow(learn)
  .checkSingleValue <- function(x) {
    x_learn <- x[1:n_learn]
    if (any(is.na(x_learn)) & (is.integer(x_learn) | is.factor(x_learn))) {
      x.vals <- unique(x_learn[!is.na(x_learn)])
      if (length(x.vals) == 1) {
        warning(paste0("Only a single value and NA in learn. Impute by that value."))
        x[is.na(x)] <- x.vals
      }
    }
    return(x)
  }
  X <- as.data.frame(lapply(as.data.frame(rbind(learn, val)), 
                            .checkSingleValue))
  if (is.null(val)) {
    ignore <- NULL
  } else {
    ignore <- c(rep(FALSE, n_learn), rep(TRUE, nrow(val)))
  }
  testMice <- try(do.call(mice::mice, c(mice.params, list(data = X,
                                                          ignore = ignore,
                                                          printFlag = FALSE))))
  if (inherits(testMice, "try-error")) {
    warning("Replacing failed 'mice' algorithm by simple mean imputation. \n")
    out <- meanImpute(learn, val)
  } else {
    .average.mice.imp <- function(x) {
      if (is.factor(x) | is.integer(x)) {
        vals <- unique(x)
        out <- vals[which.max(tabulate(match(x, vals)))]
      } else {
        out <- mean(x)
      }
      return(out)
    }
    for (i in 1:length(testMice$imp)) {
      if (nrow(testMice$imp[[i]]) > 0) {
        imp.vals <- sapply(1:nrow(testMice$imp[[i]]), 
                           function(j) .average.mice.imp(unlist(testMice$imp[[i]][j, ])))
        X[rownames(testMice$imp[[i]]), names(testMice$imp)[i]] <- imp.vals
      }
    }
    if (any(is.na(X))) {
      warning("There are still missing values after mice imputation; replacing the remainging NAs by simple mean imputation. \n")
      X <- meanImpute(X, NULL)$imputed_learn
    }
    if (is.null(val)) {
      imputed_val <- NULL
    } else {
      imputed_val <- X[-(1:n_learn), ]
    }
    out <- list(imputed_learn = X[1:n_learn, ],
                imputed_val = imputed_val)
  }
  return(out)
}


