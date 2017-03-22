best <- function(state, outcome) {
    
    ## Read outcome data
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    data[, 11] <- as.numeric(data[, 11])
    data[, 17] <- as.numeric(data[, 17])
    data[, 23] <- as.numeric(data[, 23])
    
    ## Check that state and outcome are valid
    
    if(!(state %in% data[,7])) {
        stop(paste("Error in best('", state, "' '",
                   outcome, "') : invalid state", sep = ""))
    }
    else if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop(paste("Error in best('", state, "' '",
                   outcome, "') : invalid outcome", sep = ""))
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    data_state <- subset(data, data[, 7] == state)
    
    if(outcome == "heart attack") {
        hospital <- data_state[which.min(data_state[, 11]), 2]
    }
    else if(outcome == "heart failure") {
        hospital <- data_state[which.min(data_state[, 17]), 2]
    }
    else if(outcome == "pneumonia") {
        hospital <- data_state[which.min(data_state[, 23]), 2]
    }
    
    hospital <- sort(hospital, decreasing = FALSE)[1]
    hospital
    
}