#' Novelty
#'
#' Computes test string novelty
#' @param train a vector of training strings
#' @param test a vector of test strings
#' @param n_gram the chunk sizes to analyze
#' @param proportion whether the output represents raw values or is proportional to length of the string
#' @return a vector of the novelty of each test string
#' @export

novelty <- function(train, test, n_gram = 2, proportion = FALSE) {
    #extract all training grams and store to matrix g
    g <- matrix(" ", length(train), (max(nchar(train))) - 1)

    for(i in 1:(length(train))){
        for(j in 1:(nchar(train[i]) - (n_gram - 1))){
            g[i, j] <- substring(train[i], j, (j + (n_gram - 1)))
        }
    }

    r <- matrix(0, length(test))

    for(i in 1:length(test)) {
        count <- (nchar(test[i]) - (n_gram - 1))
        for(j in 1:(nchar(test[i]) - (n_gram - 1))) {
            if(any(substring(test[i], j, (j + (n_gram - 1))) == g)) {
                count <- count - 1
            }
            r[i] <- count
        }
    }

    p_name <- ""

    if(proportion == TRUE){
        r <- r / (nchar(test) - (n_gram - 1))
        p_name <- "Proportion"
    }

    if(n_gram == 2){
        colnames(r) <- paste("Bigram Novelty", p_name, sep = " ")
    }
    if(n_gram == 3){
        colnames(r) <- paste("Trigram Novelty", p_name, sep = " ")
    }
    if(n_gram == 4){
        colnames(r) <- paste("4-gram Novelty", p_name, sep = " ")
    }
    if(n_gram == 5){
        colnames(r) <- paste("5-gram Novelty", p_name, sep = " ")
    }

    rownames(r) <- test
    return(r)
}
