best <- function(state, outcome) {
    ## Read outcome data
    odata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    conditions <- c(11,17,23)
    names(conditions) <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if (sum(odata$State==state) == 0) stop("invalid state")
    if (sum(names(conditions)==outcome) == 0) stop("invalid outcome")    
    
    ## Finding the best number
    hnames <- odata$Hospital.Name[odata$State==state]
    hdata <- as.numeric(odata[odata$State==state,conditions[outcome]])
    hmin <- min(hdata, na.rm=TRUE)
    
    ## Checking is there's any ties and returning result
    if (sum(hdata==hmin, na.rm=TRUE)==1) 
        best <- hnames[hdata==hmin & !is.na(hdata)]
    if (sum(hdata==hmin, na.rm=TRUE)>1) 
        best <- sort(hnames[hdata==hmin & !is.na(hdata)])[1]
    best 
}
