pollutemean <- function(directory, pollutant, id = 1:332) {
    meanNum = 0
    for (index in id) {
        filen <- sprintf("%03d.csv", id)
        filen <- paste(directory, filen, sep = '/')
        print(filen)
        table <- read.csv(filen)
        meanNum <- meanNum + mean(table[[pollutant]], na.rm = TRUE)
        print(meanNum)
    }
    value <- (meanNum / length(id))
    print(value)
   
}

pollutantmean <- function(directory, pollutant, id = 1:10) {
    filenames <- sprintf("%03d.csv", id)
    filenames <- paste(directory, filenames, sep="/")
    table <- lapply(filenames, read.csv, header = TRUE)
    
    x <- do.call(rbind.data.frame, table)
    print(mean(x[, pollutant], na.rm = TRUE))
    
}

complete <- function(directory, id = 1:332) {
  
}

corr <- function(directory, threshold = 0) {
  
}