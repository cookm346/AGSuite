#' Analogical Similarity
#'
#' Computes the number of times the pattern of the test string (regardless of surface structure) appears in training strings
#' @param train a vector of training strings
#' @param test a vector of test strings
#' @return a vector containing the number of pattern matches that occur between the test item and all training items
#' @export


analogy <- function(train, test){

    ptrn <- matrix(0, length(test))

    for(i in 1:length(test)){
        ptrn[i] <- Repetition_match(test[i], train)
    }

    rownames(ptrn) <- test
    colnames(ptrn) <- "Pattern Matches"

    return(ptrn)
}


Repetition <- function (x) {
    x <- gsub("^\\s+|\\s+$", "", x)
    n <- matrix(0, nchar(x))
    UniqueSymbols <- unique(strsplit(x, "")[[1]])
    for (i in 1:nchar(x)) {
        for (j in 1:length(UniqueSymbols)) {
            if (substr(x, i, i) == UniqueSymbols[j]) n[i] <- j
        }
    }
    return(paste(n, collapse=""))
}

# This function counts the number of exact matches
# between a string "Probe" and all items in a
# vector "Memory". It uses Repetition above

Repetition_match <- function (Probe, Memory) {
    n <- 0
    for (i in 1:length(Memory)) {
        if (Repetition(Probe) == Repetition(Memory[i])) n <- n + 1
    }
    return(n)
}
