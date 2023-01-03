# some internal functions for the missSuperLearner package

.create.missLibrary <- function(SL.library, imputeAlgo) {
  if (is.character(SL.library)) {
    imputeAlgorithm <- imputeAlgo
    if ("SL.mean" %in% SL.library | "SL.grf" %in% SL.library) {
      imputeAlgorithm <- c(imputeAlgorithm, "raw")
    }
    screenAlgorithm <- "All"
    library <- data.frame(predAlgorithm = character(), rowScreen = integer(), 
                          numImpute = integer(), stringsAsFactors = FALSE)
    for (pA in SL.library) {
      if (pA == "SL.mean" | pA == "SL.grf") {
        library <- rbind(library, data.frame(predAlgorithm = pA, rowScreen = 1, 
                                             numImpute = match("raw", imputeAlgorithm), 
                                             stringsAsFactors = FALSE))
      } else {
        library <- rbind(library, data.frame(predAlgorithm = pA, rowScreen = 1, 
                                             numImpute = match(imputeAlgorithm[imputeAlgorithm != "raw"], imputeAlgorithm), 
                                             stringsAsFactors = FALSE))
      }
    }
  } else if (is.list(SL.library)) {
    predAlgo <- sapply(SL.library, FUN = "[", 1)
    imputeAlgorithm <- imputeAlgo
    if ("SL.mean" %in% predAlgo | "SL.grf" %in% predAlgo) {
      imputeAlgorithm <- c(imputeAlgorithm, "raw")
    }
    NumberScreen <- (sapply(SL.library, FUN = length) - 1)
    if (sum(NumberScreen == 0) > 0) {
      for(ii in which(NumberScreen == 0)) {
        SL.library[[ii]] <- c(SL.library[[ii]], "All")
        NumberScreen[ii] <- 1
      }
    }
    screenAlgorithm <- unique(unlist(lapply(SL.library, FUN="[", -1)))
    kImpute <- length(imputeAlgorithm[imputeAlgorithm != "raw"])
    library <- data.frame(predAlgorithm = character(), rowScreen = integer(), 
                          numImpute = integer(), stringsAsFactors = FALSE)
    for (i in 1:length(predAlgo)) {
      if (predAlgo[i] == "SL.mean" | predAlgo[i] == "SL.grf") {
        library <- rbind(library, data.frame(predAlgorithm = predAlgo[i],
                                             rowScreen = match(SL.library[[i]][-1], screenAlgorithm), 
                                             numImpute = match("raw", imputeAlgorithm), 
                                             stringsAsFactors = FALSE))
      } else {
        library <- rbind(library, data.frame(predAlgorithm = predAlgo[i],
                                             rowScreen = match(rep(SL.library[[i]][-1], each = kImpute), screenAlgorithm),
                                             numImpute = rep(match(imputeAlgorithm[imputeAlgorithm != "raw"], imputeAlgorithm), NumberScreen[i]), 
                                             stringsAsFactors = FALSE))
      }
    }
  } else {
    stop('format for SL.library is not recognized')
  }
  
  out <- list(library = library, 
              screenAlgorithm = screenAlgorithm,
              imputeAlgorithm = imputeAlgorithm)
  return(out)
}


.SL.require <- function(package, message = paste('loading required package (', package, ') failed', sep = '')) {
  if(!requireNamespace(package, quietly = FALSE)) {
    stop(message, call. = FALSE)
  }
  invisible(TRUE)
}


.check.SL.library <- function(library, imputeAlgo, addPackages = NULL) {
  if("mice" %in% imputeAlgo) .SL.require('mice', message = 'You have selected mice as an imputation algorithm but either do not have the mice package installed or it can not be loaded')
  if("SL.bayesglm" %in% library) .SL.require('arm', message = 'You have selected bayesglm as a library algorithm but either do not have the arm package installed or it can not be loaded')
  if("SL.cforest" %in% library) .SL.require('party', message = 'You have selected cforest as a library algorithm but either do not have the party package installed or it can not be loaded')
  if("SL.DSA" %in% library) .SL.require('DSA', message = 'You have selected DSA as a library algorithm but either do not have the DSA package installed or it can not be loaded')
  if("SL.gam" %in% library) .SL.require('gam', message = 'You have selected gam as a library algorithm but either do not have the gam package installed or it can not be loaded')
  if("SL.gbm" %in% library) .SL.require('gbm', message = 'You have selected gbm as a library algorithm but either do not have the gbm package installed or it can not be loaded')
  if("SL.glmnet" %in% library) .SL.require('glmnet', message = 'You have selected glmnet as a library algorithm but either do not have the glmnet package installed or it can not be loaded')
  if("SL.knn" %in% library) .SL.require('class', message = 'You have selected knn as a library algorithm but either do not have the class package installed or it can not be loaded')
  if("SL.logreg" %in% library) .SL.require('LogicReg', message = 'You have selected logreg as a library algorithm but either do not have the LogicReg package installed or it can not be loaded')
  if("SL.nnet" %in% library) .SL.require('nnet', message = 'You have selected nnet as a library algorithm but either do not have the nnet package installed or it can not be loaded')
  if("SL.polymars" %in% library) .SL.require('polspline', message = 'You have selected polymars or polyclass as a library algorithm but either do not have the polspline package installed or it can not be loaded')
  if("SL.randomForest" %in% library) .SL.require('randomForest', message = 'You have selected randomForest as a library algorithm but either do not have the randomForest package installed or it can not be loaded')
  if("SL.ridge" %in% library) .SL.require('MASS', message = 'You have selected lm.ridge as a library algorithm but either do not have the MASS package installed or it can not be loaded')
  if("SL.spls" %in% library) .SL.require('spls', message = 'You have selected spls as a library algorithm but either do not have the spls package installed or it can not be loaded')
  if("SL.svm" %in% library) .SL.require('e1071', message = 'You have selected svm as a library algorithm but either do not have the e1071 package installed or it can not be loaded')
  if("SL.ipredbagg" %in% library) .SL.require('ipred', message = 'You have selected bagging as a library algorithm but either do not have the ipred package installed or it can not be loaded')
  if("SL.gbm" %in% library) .SL.require('gbm', message = 'You have selected gbm as a library algorithm but either do not have the gbm package installed or it can not be loaded')
  if("SL.mars" %in% library) .SL.require('mda', message = 'You have selected mars as a library algorithm but either do not have the mda package installed or it can not be loaded')
  if("SL.earth" %in% library) .SL.require('earth', message = 'You have selected earth as a library algorithm but either do not have the earth package installed or it can not be loaded')
  if("SL.caret" %in% library) .SL.require('caret', message = 'You have selected caret as a library algorithm but either do not have the caret package installed or it can not be loaded')
  invisible(TRUE)
}


.check.validRows <- function(validRows, X) {
  test <- TRUE
  if (!inherits(X, "data.frame")) {
    X <- as.data.frame(X)
  }
  for (i in 1:length(validRows)) {
    idx <- unlist(validRows[-i])
    test <- test & all(sapply(X[idx, ], function(y) any(!is.na(y))))
  }
  return(test)
} 


.imputeX <- function(rawXlearn, rawXval, library, kImpute, mice.params) {
  tempLearn <- vector(mode = "list", length = kImpute)
  tempValid <- vector(mode = "list", length = kImpute)
  for (i in 1:kImpute) {
    iA <- library$imputeAlgorithm[i]
    if (iA == "raw") {
      tempLearn[[i]] <- rawXlearn
      tempValid[[i]] <- rawXval
    } else if (iA == "mean") {
      tempImpute <- try(do.call(meanImpute, list(learn = rawXlearn,
                                                 val = rawXval)))
      if (inherits(tempImpute, "try-error")) {
        stop(paste("The imputation algorithm ", iA, " failed. \n"))
      } else {
        tempLearn[[i]] <- tempImpute$imputed_learn
        tempValid[[i]] <- tempImpute$imputed_val
      }
    } else if (iA == "median") {
      tempImpute <- try(do.call(medianImpute, list(learn = rawXlearn,
                                                   val = rawXval)))
      if (inherits(tempImpute, "try-error")) {
        stop(paste("The imputation algorithm ", iA, " failed. \n"))
      } else {
        tempLearn[[i]] <- tempImpute$imputed_learn
        tempValid[[i]] <- tempImpute$imputed_val
      }
    } else if (iA == "mice") {
      tempImpute <- try(do.call(miceImpute, list(learn = rawXlearn,
                                                 val = rawXval,
                                                 mice.params = mice.params)))
      if (inherits(tempImpute, "try-error")) {
        stop(paste("The imputation algorithm ", iA, " failed. \n"))
      } else {
        tempLearn[[i]] <- tempImpute$imputed_learn
        tempValid[[i]] <- tempImpute$imputed_val
      }
    }
  }
  return(list(tempLearn = tempLearn,
              tempValid = tempValid))
}


