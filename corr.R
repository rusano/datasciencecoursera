corr <- function(directory, threshold = 0) {
    ## "directory" is a character vector of length 1 indicating
    ## the location of the CSV files
    
    library(stringr)
    
    ## "threshold" is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    cr <- numeric(0)
    id <- 1:332
    
    for (i in id) {
        files <- paste(directory, "/", str_pad(i, 3, pad = "0"), ".csv", sep = "")
        x <- read.csv(files)
        num <- sum(!is.na(x[,2]) & !is.na(x[,3]))
        if (num > threshold) { 
            x <- x[!is.na(x[,2]) & !is.na(x[,3]),]
            cr <- c(cr, cor(x[,2], x[,3]))
            }
        
    } 
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    cr
}