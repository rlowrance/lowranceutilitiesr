Formula <- function(response, predictors) {
    # return formula response ~ predictors
    force(response)
    force(predictors)
    result <- as.formula(paste0(response,
                                '~',
                                paste0(predictors, collapse = '+')))
    result
}
