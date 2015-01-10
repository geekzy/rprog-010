pollutantmean <- function(directory, pollutant, id = 1:332) {
    # list file of the directory
    files <- list.files(directory, full.names=TRUE)
    # assuming all files are csv bind all rows together
    data <- do.call(rbind, lapply(files, read.csv))
    # get subset of pollutant (either sulfate or nitrate) of the id
    data_subset <- data[which(data$ID >= min(id) & data$ID <= max(id)), pollutant];
    # calculate the mean of subset
    mean(data_subset, na.rm=TRUE)
}