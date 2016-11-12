pollutantmean <- function(directory, pollutant, id = 1:332) {
    filenames <- paste(directory, sprintf("%03d.csv", id), sep="/")
    table <- lapply(filenames, read.csv, header = TRUE)
    x <- do.call(rbind.data.frame, table)
    print(mean(x[[pollutant]], na.rm = TRUE))
    
}