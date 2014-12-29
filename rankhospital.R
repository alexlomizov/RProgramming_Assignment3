rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    odata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    conditions <- c(11,17,23)
    names(conditions) <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if (sum(odata$State==state) == 0) stop("invalid state")
    if (sum(names(conditions)==outcome) == 0) stop("invalid outcome")    
    
    ## Ranking hospitals
    hdata <- as.numeric(odata[odata$State==state,conditions[outcome]])
    names(hdata) <- odata$Hospital.Name[odata$State==state]
    rdata <- hdata[order(hdata, names(hdata))]
    rdata <- rdata[!is.na(rdata)]
    
    ## Returning result
    if (num=="best") num <- 1
    else if (num=="worst") num <- length(rdata)
    else num <- as.numeric(num)
    names(rdata)[num]
}
