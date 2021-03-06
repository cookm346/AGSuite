#' The Works
#'
#' Computes all item-level statistics
#' @param train a vector of training strings
#' @param test a vector of test strings
#' @return a matrix containing string length, legal entry, six measures of novelty, six measures of ACS (Associative Chunk Strength), four measures of Levenshtein (edit) distance, first-order redundancy, and analogical similarity
#' @export

theworks <- function(train, test){
    sl <- strlength(test)
    le <- legalentry(train, test)

    n2 <- novelty(train, test, n_gram = 2, proportion = FALSE)
    n3 <- novelty(train, test, n_gram = 3, proportion = FALSE)
    cn <- n2 + n3

    ncp2 <- novelty(train, test, n_gram = 2, proportion = TRUE)
    ncp3 <- novelty(train, test, n_gram = 3, proportion = TRUE)
    ncp <- ncp2 + ncp3

    gacs2 <- acs(train, test, n_gram = 2, anchor = FALSE)
    gacs3 <- acs(train, test, n_gram = 3, anchor = FALSE)
    gacs <- gacs2 + gacs3

    aacs2 <- acs(train, test, n_gram = 2, anchor = TRUE)
    aacs3 <- acs(train, test, n_gram = 3, anchor = TRUE)
    aacs <- aacs2 + aacs3

    ldmean <- levdist(train, test, measure = "mean")
    ldmedian <- levdist(train, test, measure = "median")
    ldmin <- levdist(train, test, measure = "min")
    ldmax <- levdist(train, test, measure = "max")

    r <- redundancy(test)

    p <- analogy(train, test)


    m <- cbind(sl, le, ldmin, ldmean, ldmedian, ldmax, gacs2, gacs3, gacs, aacs2, aacs3, aacs,
               n2, n3, cn, ncp2, ncp3, ncp, r, p)


    colnames(m) <- c("String Length", "Legal Entry", "Min Levenshtein", "Mean Levenshtein", "Median Levenshtein", "Max Levenshtein",
                     "Global Bigram ACS", "Global Trigram ACS", "Global ACS", "Anchor Bigram ACS",
                     "Anchor Trigram ACS", "Anchor ACS", "Bigram Novelty", "Trigram Novelty",
                     "Chunk Novelty", "Bigram NCP", "Trigram NCP", "NCP", "First-Order Redundancy", "Analogical Similarity")

    return(m)
}
