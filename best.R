best <- function(state, outcome) {
    # Read outcome data
    data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    
    # Check state and outcome validity
    vOutcome <- c('heart attack', 'heart failure', 'pneumonia')
    stateValid <- nrow(subset(data, data[,7] == state)) > 0
    outcomeValid <- tolower(outcome) %in% vOutcome
    if (!stateValid) stop('invalid state')
    if (!outcomeValid) stop('invalid outcome')
    
    # evaluate which column to use based on specified outcome
    col <- ifelse(tolower(outcome) == 'heart attack', 13, 
                  ifelse(tolower(outcome) == 'heart failure', 19, 
                         ifelse(tolower(outcome) == 'pneumonia', 25, 0)))
    # subset by state
    ds <- data[which(data$State == state),]
    # convert values to numeric
    ds[,col] <- as.numeric(ds[,col])
    # return first ordered by col
    ds[order(ds[,col], na.last=NA),2][1]
}