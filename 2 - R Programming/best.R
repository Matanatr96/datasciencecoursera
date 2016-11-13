##This function takes in 2 parameters state, the hospital's state, and outcome, the type of disease the user has
##This function calculates, based on the two parameters, the best hospital.
##This function returns the best hospital
best <- function(state, outcome) {
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    if(nrow(data[data$State == state, ]) == 0) {
        stop("invalid state");
    } else if(!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
        stop("invalid outcome")
    }
    
    index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
    data[ ,index] <- suppressWarnings(as.numeric(data[ ,index]))
    data <- na.omit(data)
    
    subset <- data[data$State == state, ]
    subset <- subset[order(subset[ ,index], na.last=TRUE), 2]
    subset <- na.omit(subset)
    
    subset[1]
}