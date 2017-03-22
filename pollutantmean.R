pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## "directory" is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## "pollutant" is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate"
    
    ## "id" is an integer vector indicating the monitor ID numbers
    ## to be used
    library(stringr)
    
    sum <- 0
    num <- 0
    for (i in id) {
        files <- paste(directory, "/", str_pad(i, 3, pad = "0"), ".csv", sep = "")
        x <- read.csv(files)
    
        if (pollutant == "sulfate"){
            sum <- sum + sum(x$sulfate, na.rm = TRUE)
            num <- num + sum(!is.na(x$sulfate))
        }
        else if (pollutant == "nitrate"){
            sum <- sum + sum(x$nitrate, na.rm = TRUE)
            num <- num + sum(!is.na(x$nitrate))
        }
    }
    
    ## Return the mean of the pollutant across all monitors list
    ## in the "id" vector (ignoring NA values)
    ## NOTE: Do not round the result
    
    sum / num
}