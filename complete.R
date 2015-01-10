complete <- function(directory, id = 1:332) {
    # list file of the directory
    files <- list.files(directory, full.names=TRUE)
    # assuming all files are csv bind all rows together
    data <- do.call(rbind, lapply(files, read.csv))
    # get subset of within id range
    data_subset <- data[is.element(data$ID, id),]
    # find rows with no NA
    good <- complete.cases(data_subset)
    # count complete cases of data_subset
    rows <- data_subset[good,]
    # split to group by ID
    s <- split(rows, rows$ID)
    # count rows based on group
    data.frame(id=id,nobs=sapply(s, nrow))
}