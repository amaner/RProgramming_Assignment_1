rankall <- function(outcome, num = "best"){
  
   
    if ( outcome == "heart attack" || outcome == "heart failure" ||
         outcome == "pneumonia"){ 
         ## keep going, no problem
    }
    else{
         ## not a legit outcome, so return an error and stop
         errmsg <- "invalid outcome"
         stop(errmsg)
    }
  
    ## state is in col 7, provider name is in col 2, heart attack mort rate is in col 11
    ## heart failure mort rate is in col 17, and pneumonia mort rate is in col 23
    stcol    <- 7
    pncol    <- 2
    hacol    <- 11
    hfcol    <- 17
    pneumcol <- 23
  
    ## read the frame from the csv file into a character data frame
    ## and convert columns 11, 17, & 23 to numeric type
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    suppressWarnings(data[ , hacol]    <- as.numeric(data[ , hacol]))
    suppressWarnings(data[ , hfcol]    <- as.numeric(data[ , hfcol]))
    suppressWarnings(data[ , pneumcol] <- as.numeric(data[ , pneumcol]))
    ## store the individual state names in a vector
    states <- data[!duplicated(data$State), stcol]
    ## create the data frame we will return later
    output <- data.frame(cbind(1:54 , 1:54) , row.names = states )
    colnames(output) <- cbind("hospital", "state")
    suppressWarnings(output[ , 1] <- as.character(output[ , 1]))
    suppressWarnings(output[ , 2] <- as.character(output[ , 2]))
    
    if (outcome == "heart attack") {
        for (i in 1:54){
             d.state       <- data[data$State == states[i] , ]
             d.state.good  <- d.state[!is.na(d.state[ , hacol]) , ]          
             d.state.good.min <- d.state.good[order( d.state.good[, hacol], as.character(d.state.good[ , pncol])),]
             d.state.good.max <- d.state.good[order(-d.state.good[, hacol], as.character(d.state.good[ , pncol])),]
             if (num == "best"){
                 out <- subset(d.state.good.min, !duplicated(d.state.good.min[ , hacol]))
                 output[i , 1] <- as.character( out[1, pncol])
                 output[i , 2] <- as.character( states[i] )
             }
             else if (num == "worst"){
                 out <- subset(d.state.good.max, !duplicated(d.state.good.max[ , hacol]))
                 output[i , 1] <- as.character( out[1, pncol])
                 output[i , 2] <- as.character( states[i] )
             } 
             else {
                 ## num is a number
                 if (num > length(d.state.good[ , hacol])){
                     output[i , 1] <- "<NA>"
                     output[i , 2] <- as.character( states[i] )
                 }
                 else {
                     output[i , 1] <- as.character( d.state.good.min[num , pncol] )
                     output[i , 2] <- as.character( states[i] )
                 }
             }
        }
        output <- output[order(output$state) , ]
        return(output)
    }
    else if (outcome == "heart failure"){
        for (i in 1:54){
          d.state       <- data[data$State == states[i] , ]
          d.state.good  <- d.state[!is.na(d.state[ , hfcol]) , ]          
          d.state.good.min <- d.state.good[order( d.state.good[, hfcol], as.character(d.state.good[ , pncol])),]
          d.state.good.max <- d.state.good[order(-d.state.good[, hfcol], as.character(d.state.good[ , pncol])),]
          if (num == "best"){
            out <- subset(d.state.good.min, !duplicated(d.state.good.min[ , hfcol]))
            output[i , 1] <- as.character( out[1, pncol])
            output[i , 2] <- as.character( states[i] )
          }
          else if (num == "worst"){
            out <- subset(d.state.good.max, !duplicated(d.state.good.max[ , hfcol]))
            output[i , 1] <- as.character( out[1, pncol])
            output[i , 2] <- as.character( states[i] )
          } 
          else {
            ## num is a number
            if (num > length(d.state.good[ , hfcol])){
              output[i , 1] <- "<NA>"
              output[i , 2] <- as.character( states[i] )
            }
            else {
              output[i , 1] <- as.character( d.state.good.min[num , pncol] )
              output[i , 2] <- as.character( states[i] )
            }
          }
          
        }
        output <- output[order(output$state) , ]
        return(output)
    }
    else {
         ## outcome is pneumonia
        for (i in 1:54){
          d.state       <- data[data$State == states[i] , ]
          d.state.good  <- d.state[!is.na(d.state[ , pneumcol]) , ]          
          d.state.good.min <- d.state.good[order( d.state.good[, pneumcol], as.character(d.state.good[ , pncol])),]
          d.state.good.max <- d.state.good[order(-d.state.good[, pneumcol], as.character(d.state.good[ , pncol])),]
          if (num == "best"){
            out <- subset(d.state.good.min, !duplicated(d.state.good.min[ , pneumcol]))
            output[i , 1] <- as.character( out[1, pncol])
            output[i , 2] <- as.character( states[i] )
          }
          else if (num == "worst"){
            out <- subset(d.state.good.max, !duplicated(d.state.good.max[ , pneumcol]))
            output[i , 1] <- as.character( out[1, pncol])
            output[i , 2] <- as.character( states[i] )
          } 
          else {
            ## num is a number
            if (num > length(d.state.good[ , pneumcol])){
              output[i , 1] <- "<NA>"
              output[i , 2] <- as.character( states[i] )
            }
            else {
              output[i , 1] <- as.character( d.state.good.min[num , pncol] )
              output[i , 2] <- as.character( states[i] )
            }
          }
          
        }
        output <- output[order(output$state) , ]
        return(output)
    }
      
}