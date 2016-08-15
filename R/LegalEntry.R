#' Legal Enrty
#'
#' Computes whether the test string contains a legal entry (0 = illegal, 1 = legal)
#' @param training a vector of training strings
#' @param test a vector of test strings
#' @return a vector of legal entries values for each test string
#' @export

legalentry <- function(training, test){
    g <- matrix(" ", length(training))

    for(i in 1:length(training)){
        g[i] <- substring(training[i], 1, 1)
    }

    g <- unique(g)
    r <- matrix(0, length(test))

    for(i in 1:length(test)){
        if(any(substring(test[i], 1, 1) == g)){
            r[i] <- 1
        }
    }
    rownames(r) <- test
    colnames(r) <- "Legal Entry"
    return(r)
}
