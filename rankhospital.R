rankhospital <- function(state, outcome, num = "best") {
    # Read outcome data
    data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    
    # Check state and outcome validity
    vNum <- c('best', 'worst')
    vOutcome <- c('heart attack', 'heart failure', 'pneumonia')
    stateValid <- nrow(subset(data, data[,7] == state)) > 0
    outcomeValid <- tolower(outcome) %in% vOutcome
    numValid <- tolower(num) %in% vNum
    if (!stateValid) stop('invalid state')
    if (!outcomeValid) stop('invalid outcome')
    
    # validate input rank
    n <- ifelse(numValid & tolower(num) == 'best', 1, 
                ifelse(!numValid, as.numeric(num), 0))
    # evaluate which column to use based on specified outcome
    col <- ifelse(tolower(outcome) == 'heart attack', 11, 
                  ifelse(tolower(outcome) == 'heart failure', 17, 
                         ifelse(tolower(outcome) == 'pneumonia', 23, 0)))
    # subset by state
    ds <- data[which(data$State == state),]
    # convert values to numeric
    ds[,col] <- as.numeric(ds[,col])
    # return first ordered by col
    orDs <- ds[order(ds[,col], ds[,2], na.last=NA),2]
    if (numValid & tolower(num) == 'worst')
        n <- length(orDs)
    out <- ifelse(n > length(orDs), NA, orDs[n])
    # return it
    out
}