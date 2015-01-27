best <- function(state, outcome) {
    # Read outcome data
    data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    
    # Check state and outcome validity
    vOutcome <- c('heart attack', 'heart failure', 'pneumonia')
    stateValid <- nrow(subset(data, data[,7] == state)) > 0
    outcomeValid <- tolower(outcome) %in% vOutcome
    if (!stateValid) stop('invalid state')
    if (!outcomeValid) stop('invalid outcome')
    
    
    # Prepare hospital name
    hospNames <- ''
    # Heart Attack evaluation
    if (tolower(outcome) == 'heart attack') {
        # Convert to numeric before evaluating
        data[,13] <- as.numeric(data[,13])
        value <- min(data[which(data[,7] == state),13], na.rm = TRUE)
        hospNames <- data[which(data[,13] == value),2]
    }
    # Heart Failure evaluation
    else if (tolower(outcome) == 'heart failure') {
        # Convert to numeric before evaluating
        data[,19] <- as.numeric(data[,19])
        value <- min(data[which(data[,7] == state),19], na.rm = TRUE)
        hospNames <- data[which(data[,19] == value),2]
    }
    # Pneumonia evaluation
    else if (tolower(outcome) == 'pneumonia') {
        # Convert to numeric before evaluating
        data[,25] <- as.numeric(data[,25])
        value <- min(data[which(data[,7] == state),25], na.rm = TRUE)
        hospNames <- data[which(data[,25] == value),2]
    }
    # Return hospital name
    # get first aphabetically order hospital name
    sort(hospNames)[1]
}