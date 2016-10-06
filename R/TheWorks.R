#' The Works
#'
#' Computes all item-level statistics
#' @param train a vector of training strings
#' @param test a vector of test strings
#' @return a matrix containing string length, legal entry, six measures of novelty, six measures of ACS (Associative Chunk Strength), and four measures of Levenshtein (edit) distance
#' @export

theworks <- function(train, test){
    sl <- strlength(test)
    le <- legalentry(train, test)
    n2 <- novelty(train, test, n_gram = 2, proportion = FALSE)
    n3 <- novelty(train, test, n_gram = 3, proportion = FALSE)
    ncp2 <- novelty(train, test, n_gram = 2, proportion = TRUE)
    ncp3 <- novelty(train, test, n_gram = 3, proportion = TRUE)
    
    cn <- n2 + n3
    ncp <- ncp2 + ncp3
    
    gacs2 <- acs(train, test, n_gram = 2, anchor = FALSE)
    gacs3 <- acs(train, test, n_gram = 3, anchor = FALSE)
    aacs2 <- acs(train, test, n_gram = 2, anchor = TRUE)
    aacs3 <- acs(train, test, n_gram = 3, anchor = TRUE)
    
    gacs <- gacs2 + gacs3
    aacs <- aacs2 + aacs3
    
    ldmean <- levdist(train, test, measure = "mean")
    ldmedian <- levdist(train, test, measure = "median")
    ldmin <- levdist(train, test, measure = "min")
    ldmax <- levdist(train, test, measure = "max")
    
    
    m <- cbind(sl, le, n2, n3, ncp2, ncp3, cn, ncp, gacs2, gacs3, 
                 aacs2, aacs3, gacs, aacs, ldmean, ldmedian, ldmin, ldmax)
    
    colnames(m)[c(7, 8, 13, 14)] <- c("Chunk Novelty, Novel Chunk Proportion, Global ACS, Anchor ACS")
    
    return(m)
}
