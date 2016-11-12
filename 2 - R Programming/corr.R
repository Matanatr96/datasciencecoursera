corr <- function(directory, threshold = 0) {
    cases <- complete(directory)
    cases <- cases[cases['nobs'] > threshold, ]$id
    correlation <- numeric()
    for (index in cases) {
        filenames <- read.csv(paste(directory, sprintf('%03d.csv', index), sep = '/'))
        difference <- filenames[complete.cases(filenames), ]
        correlation <- c(correlation, cor(difference$sulfate, difference$nitrate))
    }
    return(correlation)
}

