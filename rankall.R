rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    odata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    conditions <- c(11,17,23)
    names(conditions) <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that outcome is valid, read outcomes and states
    if (sum(names(conditions)==outcome) == 0) stop("invalid outcome")    
    fdata <- as.numeric(odata[,conditions[outcome]])
    names(fdata) <- odata$Hospital.Name
    states <- sort(unique(odata$State))
        
    ## For a given state, find the hospital of the given rank
    rankstate <- function(state, rnum) {
        rdata <- fdata[odata$State==state & !is.na(fdata)]
        rdata <- rdata[order(rdata, names(rdata))]
        if (rnum=="best") rnum <- 1
        else if (rnum=="worst") rnum <- length(rdata)
        else rnum <- as.numeric(rnum)
        names(rdata)[rnum]
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    ranks <- sapply(states, rankstate, num)
    data.frame(state=states,hospital=ranks)    
}

