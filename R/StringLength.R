#' String Length
#'
#' Computes the length of each letter string
#' @param test a vector of test strings
#' @return a vector containing the length of each test string
#' @export


strlength <- function(test){
    sl <- as.matrix(nchar(test))
    rownames(sl) <- test
    colnames(sl) <- "String Length"
    return(sl)
}
