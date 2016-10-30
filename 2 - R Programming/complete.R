complete <- function(directory, id = 1:332) {
    completeCases <- function(filenames) {
        sum(complete.cases(read.csv(filenames)))
    }
    filenames <- paste(directory, sprintf("%03d.csv", id), sep="/")
    data.frame(id = id, nobs = unlist(lapply(filenames, completeCases)))
}
