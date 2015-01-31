rankall <- function(outcome, num = "best") {
    # Read outcome data
    data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    
    # Check state and outcome validity
    vNum <- c('best', 'worst')
    vOutcome <- c('heart attack', 'heart failure', 'pneumonia')
    outcomeValid <- tolower(outcome) %in% vOutcome
    numValid <- tolower(num) %in% vNum
    if (!outcomeValid) stop('invalid outcome')
    
    # validate input rank
    n <- ifelse(numValid & tolower(num) == 'best', 1, 
                ifelse(!numValid, as.numeric(num), 0))
    # evaluate which column to use based on specified outcome
    col <- ifelse(tolower(outcome) == 'heart attack', 11, 
                  ifelse(tolower(outcome) == 'heart failure', 17, 
                         ifelse(tolower(outcome) == 'pneumonia', 23, 0)))
    
    # prepre ranked data frame
    df <- data.frame(hospital=character(), state=character())
    # loop through states
    for (st in unique(data[order(data$State), 'State'])) {
        # rank each state
        rst <- rankstate(data, st, col)
        # validate worst
        if (tolower(num) == 'worst') n <- nrow(rst)
        # validate n shouln'd exceeds ranked each states
        if (n > nrow(rst))
            df <- rbind(df, data.frame(hospital='<NA>', state=st))
        # rank exists
        else df <- rbind(df, rst[n,])
    }
    # output ranked data frame
    df
}

rankstate <- function(raw, state, col) {
    # coerse to numeric
    raw[,col] = as.numeric(raw[,col])
    # select data by state
    ds <- raw[which(raw$State == state),]
    # order the respective column and hospital name
    orDs <- ds[order(ds[,col], ds[,2]),]
    # select only hospital name and state column
    ranked <- orDs[,c(2,7)]
    names(ranked) <- c('hospital', 'state')
    # return the ordered col by state
    ranked
}