corr <- function(directory, threshold = 0) {
    # get completely ovserved data
    cdata <- xcomplete('specdata')
    out <- numeric(0)
    for (id in 1:332) {
        # get subset of complete case data by id
        xdata <- cdata[which(cdata$ID == id),]
        # compute correlation between nitrate and sulfate
        if (nrow(xdata) > threshold) {
            out <- c(out, cor(xdata$sulfate, xdata$nitrate))
        }
    }
    # return a numeric vector of correlations
    out
}