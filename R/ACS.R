#' ACS (Associative Chunk Strength)
#'
#' Computes Associative Chunk Strength
#' @param train a vector of training strings
#' @param test a vector of test strings
#' @param n_gram the chunk sizes to analyze
#' @param anchor whether the entire string is analyzed, or only the beginning and end of the string (i.e., anchors)
#' @return a vector containing the ACS of each test string
#' @export

#ACS FUNCTION
acs <- function(train, test, n_gram = 2, anchor = FALSE){
    

    #HELPER FUNCTIONS
    
    #get all n-grams from vector
    grams <- function(x, n_grams){
        #generate matrix to store the training grams
        grams <- matrix(" ", length(x), (max(nchar(x))) - (n_gram - 1))
        #extract all training strings grams and save them in matrix grams
        for(i in 1:(length(x))){
            for(j in 1:(nchar(x[i]) - (n_gram - 1))){
                grams[i, j] <- substring(x[i], j, (j + (n_gram - 1)))
            }
        }
        return(grams)
    }

    #get all achor grams from vector of grams
    anchorGrams <- function(grams){
        anchor_grams <- matrix(0, nrow(grams), 2)
        for(i in 1:nrow(grams)){
            
            current_row <- grams[i, ]
            anchor_grams[i, 1] <- current_row[1]
            current_row_grams <- current_row[!(current_row == " ")]
            anchor_grams[i, 2] <- current_row_grams[length(current_row_grams)]
        }
        return(anchor_grams)
    }

    #extract n-grams, convert to vector, remove empty elements
    train_grams <- grams(train, 2)
    if(anchor == TRUE){
        train_grams <- anchorGrams(train_grams)
    }
    train_grams <- c(train_grams)
    train_grams <- train_grams[!(train_grams == " ")]
    
    
    test_grams <- grams(test, 2)
    if(anchor == TRUE){
        test_grams <- anchorGrams(test_grams)
    }
    
    
    acs <- matrix(0, length(test))
    
    for(i in 1:nrow(test_grams)){
        count <- 0
        for(j in 1:ncol(test_grams)){
            count <- count + sum(train_grams %in% test_grams[i,j] == TRUE)
        }
        acs[i] <- count
    }
    
    
    #divide anchor acs by 2
    #divide global acs by number of grams of each test item
    if(anchor == TRUE){
        acs <- acs / 2
        a_name <- "Anchor"
    } else {
        acs <- acs / apply(test_grams, 1, function(test_grams){length(test_grams[!(test_grams == " ")])})
        a_name <- "Global"
    }
    
    
    rownames(acs) <- test
    
    if(n_gram == 2)  colnames(acs) <- paste(a_name, "Bigram ACS", sep = " ")
    if(n_gram == 3)  colnames(acs) <- paste(a_name, "Trigram ACS", sep = " ")
    if(n_gram == 4)  colnames(acs) <- paste(a_name, "4-gram ACS", sep = " ")
    if(n_gram == 5)  colnames(acs) <- paste(a_name, "5-gram ACS", sep = " ")
    
    
    return(acs)
    
}


