xcomplete <- function(directory, id = 1:332) {
    # list file of the directory
    files <- list.files(directory, full.names=TRUE)
    # assuming all files are csv bind all rows together
    data <- do.call(rbind, lapply(files, read.csv))
    # subset data of id range
    subdata <- data[which(data$ID %in% id),]
    # subset complete cases within id range
    subdata[which(complete.cases(subdata)),]
}

complete <- function(directory, id = 1:332) {
    # get complete subset of given id
    cdata <- xcomplete(directory, id);
    # prepare output
    out <- data.frame(id,nobs=0)
    # loop through id
    for (i in id) {
        # count rows with no NA
        good <- nrow(cdata[which(cdata$ID == i),])
        # put in data.frame
        out$nobs[id == i] <- good
    }
    # return it
    out
}