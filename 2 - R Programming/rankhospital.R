rankhospital <- function(state, outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    if(nrow(data[data$State == state, ]) == 0) {
        stop("invalid state");
    } else if(!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
        stop("invalid outcome")
    }
    
    index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
    
    data[,index] <- suppressWarnings(as.numeric(data[,index]))
    data <- na.omit(data)
    
    subset <- data[data$State == state, ]
    subset <- subset[order(subset[,index], subset[,2], na.last=TRUE),2] 
    subset <- na.omit(subset)
    num <- ifelse(num == "best", 1, ifelse(num == "worst", length(subset), as.numeric(num)))
    
    subset[num]
}