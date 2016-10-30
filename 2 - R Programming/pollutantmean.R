pollutantmean <- function(directory, pollutant, id = 1:10) {
    filenames <- paste(directory, sprintf("%03d.csv", id), sep="/")
    table <- lapply(filenames, read.csv, header = TRUE)
    x <- do.call(rbind.data.frame, table)
    print(mean(x[[pollutant]], na.rm = TRUE))
    
}

corr <- function(directory, threshold = 0) {
  
}