CrossValidate <- function(data, nfolds, EvaluateModel, model.names, control, verbose = FALSE) {
    # perform cross validation
    # ARGS
    # data           : a data frame
    # nfolds         : numeric scalar, number of folds (ex: 10)
    # EvaluateModel  : a list of functions such that calling
    #                  EvaluateModel[[i]](data, is.training, is.testing, control) yields
    #                  an object that is returned as part of the list returned by CrossValidate
    #                  EvaluateModel[[i]]() --> possibly an estimate of the generalized error
    # model.name     : chr scalar, vector of names for Models underlying EvaluateModel
    # control        : an object passed to each call to Model[[i]]
    # verbose        : logical, if TRUE, print as we calculate
    # RETURNS a nested list such that
    # list[[i]] is the result for Model[[i]]
    # list[[i]][[k]] is the object returned by calling Model[[i]] on fold k
    # and
    # list$model.names[[i]] is a name for list[[i]]

    #cat('starting CrossValidate', nrow(data), nfolds, length(Models), '\n'); browser()

    debugging <- TRUE
    debugging <- FALSE

    stopifnot(nfolds <= nrow(data))

    nmodels <- length(EvaluateModel)
    stopifnot(nmodels == length(model.names))

    # assign each sample randomly to a fold
    fold.1.to.n <- rep(1:nfolds, length.out=nrow(data))  # 1 2 ... nfold 1 2 ... nfold ...
    fold <- sample(fold.1.to.n, nrow(data))

    # accumulate results in this list of lists
    # give names to each result list element
    result <- list()
    for (i in 1:nmodels) {
        name <- model.names[[i]]
        result[[name]] <- list()
    }

    PrintModelFold <- function(what, this.model.index, this.fold) {
        s <- sprintf( 'CrossValidate: model %s %s model index %d fold %d\n'
                     ,model.names[[this.model.index]]
                     ,what
                     ,this.model.index
                     ,this.fold
                     )
        cat(s)
    }

    # examine each fold and each model
    for (this.fold in 1:nfolds) {
        is.testing <- fold == this.fold
        is.training <- !is.testing
        for (this.model.index in 1:nmodels) {
            if (verbose) {
                PrintModelFold('determinining error rate', this.model.index, this.fold)
            }

            if (debugging) {
                if (this.fold != 9) {
                    cat('CrossValidate debugging skipping fold', this.fold, '\n')
                    next
                }
            }

            Evaluate <- EvaluateModel[[this.model.index]]
            evaluate.result <- Evaluate( data = data
                                     ,is.training = is.training
                                     ,is.testing = is.testing
                                     ,control = control
                                     )
            if (verbose) {
                PrintModelFold('result', this.model.index, this.fold)
                print(evaluate.result)
            }
            result[[this.model.index]][[this.fold]] <- evaluate.result
        }
    }   
    result
}

CrossValidate.test <- function() {
    # unit test
    set.seed(123)
    verbose <- FALSE

    Model <- function(name, data, is.training, is.testing, control) {
        #cat('Model', name, '\n'); browser()
        data.train <- data[is.training,]
        data.test <- data[is.testing,]
        predictions <- switch( name
                              ,'A' = data.test$x
                              ,'B' = rep(NA, nrow(data.test))
                              )
        actuals <- rep(1, nrow(data.test))
        errors <- predictions - actuals
        result <- list( mean = mean(errors, na.rm = TRUE)
                       ,coverage = sum(!is.na(predictions)) / length(actuals)
                       )
        if (verbose) {
            str(data.train)
            str(data.test)
            str(predictions)
            str(actuals)
            str(errors)
        }
        stopifnot(result$coverage <= 1)
        result
    }

    ModelA <- function(data, is.training, is.testing, control) {
        Model('A', data, is.training, is.testing, control)
    }

    ModelB <- function(is.testing, data, is.training, control) {
        Model('B', data, is.training, is.testing, control)
    }

    data <- data.frame(x = c(1,2,3),
                       y = c(10,20,30))
    nfolds <- 2
    cv.result <- CrossValidate( data = data
                               ,nfolds = nfolds
                               ,EvaluateModel = list(ModelA, ModelB)
                               ,model.name = list('A', 'B')
                               ,verbose = verbose
                               )
    if (verbose) str(cv.result)

    # summarize across test
    model.A.means <- sapply( 1:nfolds
                            ,function(fold) cv.result$A[[fold]]$mean)
    stopifnot(model.A.means[[1]] == 1)
    stopifnot(model.A.means[[2]] == 1)
    
    model.B.coverage <- sapply(1:nfolds, function(fold) cv.result$B[[fold]]$coverage)
    stopifnot(model.B.coverage[[1]] == 0)
    stopifnot(model.B.coverage[[2]] == 0)
}

if (!exists('WithRestoredRandomSeed')) source('WithRestoredRandomSeed.R')
WithRestoredRandomSeed(CrossValidate.test)
