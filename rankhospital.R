rankhospital  <- function(state, outcome, num)
{
        data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE )
      
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        
        if (!(state %in% (data[,7])))
        {
                stop("invalid state") 
        }
        
        if ((outcome %in% validOutcomes) == FALSE)
        {
                stop("invalid outcome") 
        }
        
        colIndex <- (if (outcome == "heart attack") {11}
                     else if (outcome == "heart failure") {17}
                     else {23})
        
        filteredData <- data[,c(2,7,colIndex)]
        filteredData <- na.omit(object = filteredData, columns = 3)
        
        filteredStateData <- subset(filteredData, State == state)
        orderedData <- filteredStateData[order( filteredStateData[,3], filteredStateData[,1]),]
        
        if (num == "best")
        {
                num = 1
        }
        if (num == "worst")
        {
                num = nrow(orderedData)
        }
        
        selectedHospital <- orderedData[num,1]
        selectedHospital
}

