#' Legal Enrty
#'
#' Computes whether the test string contains a legal entry (0 = illegal, 1 = legal)
#' @param train a vector of training strings
#' @param test a vector of test strings
#' @return a vector of legal entries values for each test string
#' @export

legalentry <- function(train, test){

    #generate vector with length of number of training strings
    # to store the first charcter of each test string
    g <- matrix(" ", length(train))

    #extract the first character from each training string
    # and store in vector g
    for(i in 1:length(train)){
        g[i] <- substring(train[i], 1, 1)
    }

    #extract unique elements from list of unique characters
    g <- unique(g)

    #generate results vector with length of number of test strings
    # to store binary value describing if the test string
    # has a legal entry
    r <- matrix(0, length(test))

    #for each test string check if any of the first charcters
    # from the training strings match the first element of the training string
    # if there is a match, place a 1 in the corresponding element of the results vector
    for(i in 1:length(test)){
        if(any(substring(test[i], 1, 1) == g)){
            r[i] <- 1
        }
    }

    rownames(r) <- test
    colnames(r) <- "Legal Entry"
    return(r)
}
