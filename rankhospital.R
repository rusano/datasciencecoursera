rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    data[, 11] <- as.numeric(data[, 11])
    data[, 17] <- as.numeric(data[, 17])
    data[, 23] <- as.numeric(data[, 23])
    
    ## Check that state and outcome are valid
    
    if(!(state %in% data[,7])) {
        stop("invalid state")
    }
    else if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    data_state <- subset(data, data[, 7] == state)
    
    if(outcome == "heart attack") {
        hospital <- data_state[, c(2, 11)]
    }
    else if(outcome == "heart failure") {
        hospital <- data_state[, c(2, 17)]
    }
    else if(outcome == "pneumonia") {
        hospital <- data_state[, c(2, 23)]
    }
    
    hospital <- hospital[order(hospital[, 2], hospital[, 1], na.last = NA),]
    
    if(num == "best") {
        num <- 1
    }
    else if(num == "worst") {
        num <- nrow(hospital)
    }
    
    if(num > nrow(hospital)) {
        answer <- NA
    }
    else {
        answer <- hospital[as.numeric(num),][, 1]
    }
    
    answer
}