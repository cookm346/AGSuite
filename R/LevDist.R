#' Levenshtein (edit) Distance
#'
#' Computes Levenshtein (Edit) Distance
#' @param training a vector of training strings
#' @param test a vector of test strings
#' @param measure the measure to be used (mean, median, min, max)
#' @return a vector containing the Levenshtein (edit) Distance of each test string
#' @export

levdist <- function (training, test, measure) {
    d <- adist(test, training)
    if (measure == "mean") {
        d <- as.matrix(apply(d, 1, mean))
    }
    if (measure == "median") {
        d <- as.matrix(apply(d, 1, median))
    }
    if (measure == "min") {
        d <- as.matrix(apply(d, 1, min))
    }
    if (measure == "max") {
        d <- as.matrix(apply(d, 1, max))
    }
    rownames(d) <- test
    colnames(d) <- paste("Levenshtein Distance ", "(", measure,
                         ")", sep = "")
    return(d)
}
