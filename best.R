best <- function(state, outcome) {
  
  ## this function will take in a 2-letter state code and
  ## a health outcome (heart attack, heart failure, or pneumonia)
  ## and will search through a file to determine the hospital in
  ## that state with the lowest mortality due to the given outcome
  
  ## make sure the state passed is actually a legit state
  if ( state == "AK" || state == "AL" || state == "AR" ||
       state == "AZ" || state == "CA" || state == "CT" || 
       state == "DE" || state == "DC" || state == "FL" ||
       state == "GA" || state == "HI" || state == "ID" ||
       state == "IL" || state == "IN" || state == "IA" ||
       state == "KS" || state == "KY" || state == "LA" ||
       state == "ME" || state == "MD" || state == "MA" ||
       state == "MI" || state == "MN" || state == "MS" ||
       state == "MO" || state == "MT" || state == "NE" ||
       state == "NV" || state == "NH" || state == "NJ" ||
       state == "NM" || state == "NY" || state == "NC" ||
       state == "ND" || state == "OH" || state == "OK" ||
       state == "OR" || state == "PA" || state == "PR" ||
       state == "RI" || state == "SC" || state == "SD" ||
       state == "TN" || state == "TX" || state == "UT" ||
       state == "VT" || state == "VA" || state == "VI" ||
       state == "WA" || state == "WV" || state == "WI" || 
       state == "WY" ||  state == "GU" ) { 
       ## keep going, no problem
  }   
  else {
       ## not a legit state, so return an error and stop    
       errmsg <- "invalid state"
       stop(errmsg)
  }
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
  stcol <- 7
  pncol <- 2
  hacol <- 11
  hfcol <- 17
  pneumcol <- 23
  
  ## read the frame from the csv file into a character data frame
  ## and convert columns 11, 17, & 23 to numeric type
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  suppressWarnings(data[,hacol]    <- as.numeric(data[,hacol]))
  suppressWarnings(data[,hfcol]    <- as.numeric(data[,hfcol]))
  suppressWarnings(data[,pneumcol] <- as.numeric(data[,pneumcol]))
  
  ## subset the data frame to include only values relevant to
  ## the passed state
  d.state <- data[data$State == state,]
  
  if (outcome == "heart attack") {
      
      ## remove any rows with NAs in the heart attack column
      d.state.good <- d.state[!is.na(d.state[,hacol]),]
      ## sort entire data frame (low-high) w.r.t. heart attack
      ## mort rate
      d.state.good.min <- d.state.good[order(d.state.good[,hacol], as.character(d.state.good[,pncol])),]
      ## return the first entry in the ha column
      return(d.state.good.min[1,pncol])
      
   }
   else if (outcome == "heart failure") {
      
      ## remove any rows with NAs in the heart failure column
      d.state.good <- d.state[!is.na(d.state[,hfcol]),]
      ## sort entire data frame (low-high) w.r.t. heart failure
      ## mort rate
      d.state.good.min <- d.state.good[order(d.state.good[,hfcol], as.character(d.state.good[,pncol])),]
      ## return the first entry in the hf column
      return(d.state.good.min[1,pncol])
   }
   else {
     
      ## remove any rows with NAs in the pneumonia column
      d.state.good <- d.state[!is.na(d.state[,pneumcol]),]
      ## sort entire data frame (low-high) w.r.t. pneumonia
      ## mort rate
      d.state.good.min <- d.state.good[order(d.state.good[,pneumcol], as.character(d.state.good[,pncol])),]
      ## return the first entry in the pneumonia column
      return(d.state.good.min[1,pncol])     
      
   }
  
      
}