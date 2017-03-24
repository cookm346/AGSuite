#' Redundancy
#'
#' Computes first-order redundancy of each letter string
#' @param test a vector of test strings
#' @return a vector containing the first-order redundancy of each test string
#' @export

redundancy <- function(test){

    r <- matrix(0, length(test))

    for(i in 1:length(test)){
        r[i] <- re(test[i])
    }

    rownames(r) <- test
    colnames(r) <- "Redundancy"

    return(r)
}

re <- function(x){
    n <- nchar(x)
    x <- substring(x, seq(1, (nchar(x) - 1), 1), seq((2), nchar(x), 1))
    m <- length(unique(x))
    p <- matrix(0, m)
    for(i in 1:m){
        p[i] <- sum(x %in% unique(x)[i]) / (n - 1)
    }
    return(1 - (  (-1*sum(p * log2(p)))  /  log2(n - 1)  ))
}

