rankall <- function(outcome, num = 'best') {
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    
    index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    data[,index] <- suppressWarnings(as.numeric(data[,index]))
    data <- na.omit(data);
    
    data.sorted <- data[order(data[ ,index], data[ ,2], na.last=TRUE),]
    data.sorted <- data.sorted[!is.na(data.sorted[ ,index]),]
    
    num <- ifelse(num == "best", 1, ifelse(num == "worst", length(data.sorted), as.numeric(num)))
    
    states <- data.sorted[ ,7];
    states <- sort(unique(states))
    
    #create a helper function to return the better hospital at a rank
    state_hospital_data <- function(state) {
        subset <- subset(data.sorted, State == state)
        subset <- subset[num, c(2,7,index)]
        subset$State <- state
        return(subset)
    }
    
    state_data <- lapply(states, state_hospital_data)
    frame <- as.data.frame(do.call(rbind, lapply(states, state_hospital_data)), row.names=states)
    colnames(frame) <- c("hospital", "state")
    return(frame)
}