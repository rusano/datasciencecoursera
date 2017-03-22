complete <- function(directory, id = 1:332) {
    ## "directory" is a character vector of length 1 indicating
    ## the location of the CSV files
    library(stringr)
    
    ## "id" is an integer vector indicating the monitor ID numbers
    ## to be used
    df <- data.frame(id=numeric(0), nobs=numeric(0))
    
    for (i in id) {
        files <- paste(directory, "/", str_pad(i, 3, pad = "0"), ".csv", sep = "")
        x <- read.csv(files)
        num <- sum(!is.na(x[,2]) & !is.na(x[,3]))
        df <- rbind(df, c(id=i, nobs=num))
    }    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where "id" is the monitor ID number and "nobs" is the
    ## number of complete cases
    names(df)[1] <- paste("id")
    names(df)[2] <- paste("nobs")
    df
}