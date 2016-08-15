#' The Works
#'
#' Computes all item-level statistics
#' @param train a vector of training strings
#' @param test a vector of test strings
#' @return a matrix containing string length, legal entry, four measures of novelty, four measures of ACS (Associative Chunk Strength), and four measures of Levenshtein (edit) distance
#' @export

theworks <- function(train, test){
    a <- strlength(test)
    b <- legalentry(train, test)
    c <- novelty(train, test, n_gram = 2, proportion = FALSE)
    d <- novelty(train, test, n_gram = 3, proportion = FALSE)
    e <- novelty(train, test, n_gram = 2, proportion = TRUE)
    f <- novelty(train, test, n_gram = 3, proportion = TRUE)
    g <- acs(train, test, n_gram = 2, anchor = FALSE)
    h <- acs(train, test, n_gram = 3, anchor = FALSE)
    i <- acs(train, test, n_gram = 2, anchor = TRUE)
    j <- acs(train, test, n_gram = 3, anchor = TRUE)
    k <- levdist(train, test, measure = "mean")
    l <- levdist(train, test, measure = "median")
    m <- levdist(train, test, measure = "min")
    n <- levdist(train, test, measure = "max")
    return(cbind(a, b, c, d, e, f, g, h, i, j, k, l, m, n))
}
