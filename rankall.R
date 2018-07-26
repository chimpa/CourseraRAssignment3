rankall   <- function(outcome, num = "best")
{
        data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE )
      
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")

        if ((outcome %in% validOutcomes) == FALSE)
        {
                stop("invalid outcome") 
        }
        
        colIndex <- (if (outcome == "heart attack") {11}
                     else if (outcome == "heart failure") {17}
                     else {23})
        
        filteredData <- data[,c(2,7,colIndex)]
        filteredData <- na.omit(object = filteredData, columns = 3)
        
        orderedData <- filteredData[order( filteredData[,2], filteredData[,3], filteredData[,1]),]
        
        splittedHospitals <- split(orderedData, orderedData$State)
        selectedHospitals <- lapply (splittedHospitals, function (x)
                                        {
                                                if (num == "best")
                                                {
                                                        num = 1
                                                }
                                                if (num == "worst")
                                                {
                                                        num = nrow(x)
                                                }
                
                                                return (x$Hospital.Name[num])
                                        }
                                     )
        
        data.frame(hospital=unlist(selectedHospitals), state=names(selectedHospitals))
}

