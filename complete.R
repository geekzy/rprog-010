complete <- function(directory, id = 1:332) {
    # list file of the directory
    files <- list.files(directory, full.names=TRUE)
    # assuming all files are csv bind all rows together
    data <- do.call(rbind, lapply(files, read.csv))
    # prepare output
    out <- data.frame(id,nobs=0)
    # loop through id
    for (i in id) {
        # count rows with no NA
        good <- sum(complete.cases(data[which(data$ID == i),]))
        # put in data.frame
        out$nobs[id == i] <- good
    }
    # return it
    out
}