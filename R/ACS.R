#' ACS (Associative Chunk Strength)
#'
#' Computes Associative Chunk Strength
#' @param training a vector of training strings
#' @param test a vector of test strings
#' @param n_gram the chunk sizes to analyze
#' @param anchor whether the entire string is analyzed, or only the bgeinning and end of the string (i.e., anchors)
#' @return a vector containing the ACS of each test string
#' @export

#ACS FUNCTION
acs <- function(training, test, n_gram = 2, anchor = FALSE){

    #extract all training grams and store to matrix g
    g <- matrix(" ", length(training), (max(nchar(training))) - (n_gram - 1))

    for(i in 1:(length(training))){
        for(j in 1:(nchar(training[i]) - (n_gram - 1))){
            g[i, j] <- substring(training[i], j, (j + (n_gram - 1)))
        }
    }

    if(anchor == TRUE){

        a <- matrix(0, (length(training) * 2))

        for(i in 1:length(training)){

            a[i] <- g[i]
            a[i + length(training)] <- g[nchar(training[i]) - (n_gram - 1)]
        }

        g <- a
    }

    #count occurance of test grams observed in training grams
    acs <- matrix(0, length(test))

    for(i in 1:(length(test))){
        count <- 0
        for(j in 1:(nchar(test[i]) - (n_gram - 1))){
            count <- count + sum(substring(test[i], j, (j + (n_gram - 1))) == g)
        }
        acs[i] <- count
    }

    if(anchor == TRUE){
        acs <- acs / 2
        acs[which(nchar(test) == n_gram)] <- acs[which(nchar(test) == n_gram)] * 2
        a_name <- "Anchor"
    } else {
        acs <- acs / nchar(test)
        a_name <- "Global"
    }


    rownames(acs) <- test

    if(n_gram == 2){
        colnames(acs) <- paste(a_name, "Bigram ACS", sep = " ")
    }
    if(n_gram == 3){
        colnames(acs) <- paste(a_name, "Trigram ACS", sep = " ")
    }
    if(n_gram == 4){
        colnames(acs) <- paste(a_name, "4-gram ACS", sep = " ")
    }
    if(n_gram == 5){
        colnames(acs) <- paste(a_name, "5-gram ACS", sep = " ")
    }

    return(acs)
}


