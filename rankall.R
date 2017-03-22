rankall <- function(outcome, num = "best") {
    ## Read outcome data
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    data[, 11] <- as.numeric(data[, 11])
    data[, 17] <- as.numeric(data[, 17])
    data[, 23] <- as.numeric(data[, 23])
    
    ## Check that state and outcome are valid
    
    if(!(num %in% c("best", "worst", 1:10000))) {
        stop("invalid num")
    }
    else if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    
    if(outcome == "heart attack") {
        hospital <- data[, c(2, 7, 11)]
    }
    else if(outcome == "heart failure") {
        hospital <- data[, c(2, 7, 17)]
    }
    else if(outcome == "pneumonia") {
        hospital <- data[, c(2, 7, 23)]
    }
    colnames(hospital) <- c("hospital", "state", "out")
    
    hospital <- hospital[order(hospital[, 2], hospital[, 3], hospital[, 1], na.last = NA),]
    
    if(num == "best") {
        num <- 1
    }
    else if(num == "worst") {
        num <- nrow(hospital)
    }
    
    hospital
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}
