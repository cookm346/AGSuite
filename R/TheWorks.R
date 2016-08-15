#' The Works
#'
#' Computes all item-level statistics
#' @param training a vector of training strings
#' @param test a vector of test strings
#' @return a matrix containing string length, legal entry, four measures of novelty, four measures of ACS (Associative Chunk Strength), and four measures of Levenshtein (edit) distance
#' @export

theworks <- function(training, test){
    a <- strlength(test)
    b <- legalentry(training, test)
    c <- novelty(training, test, n_gram = 2, proportion = FALSE)
    d <- novelty(training, test, n_gram = 3, proportion = FALSE)
    e <- novelty(training, test, n_gram = 2, proportion = TRUE)
    f <- novelty(training, test, n_gram = 3, proportion = TRUE)
    g <- acs(training, test, n_gram = 2, anchor = FALSE)
    h <- acs(training, test, n_gram = 3, anchor = FALSE)
    i <- acs(training, test, n_gram = 2, anchor = TRUE)
    j <- acs(training, test, n_gram = 3, anchor = TRUE)
    k <- levdist(training, test, measure = "mean")
    l <- levdist(training, test, measure = "median")
    m <- levdist(training, test, measure = "min")
    n <- levdist(training, test, measure = "max")
    return(cbind(a, b, c, d, e, f, g, h, i, j, k, l, m, n))
}
