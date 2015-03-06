pollutantmean <- function(directory, pollutant, id = 1:332){
  
  ## check the pollutant
  if (pollutant == "sulfate" || pollutant == "nitrate"){
    ## keep on truckin'
  }
  else {
    ## bad pollutant or spelling error, bad monkey!
    errmsg <- "not a valid pollutant"
    stop(errmsg)
  }
  
  ## accumulator vector for later
  datax <- vector('numeric')
  
  for (j in 1:length(id)){
    
    ## build the file name from information given
    numid <- paste(ifelse(id[j]>=100,"",ifelse(id[j]>=10,"0","00")),id[j],sep="")
    fname <- paste(numid, ".csv", sep="")
    wdir <- paste(getwd(), "/", sep="")
    tdir <- paste(wdir, directory, sep = "")
    actdir <- paste(tdir, "/", sep="")
    actfile <- paste(actdir, fname, sep="")
        
    ## read the file into a temporary data frame
    data <- read.csv(actfile, colClasses = "character")
    
    ## dump the pollutant data into a vector
    if (pollutant == "sulfate"){
      pol <- as.numeric(data[,2])
    }
    else {
      ## pollutant == nitrate
      pol <- as.numeric(data[,3])
    }
    
    ## subset out the bad stuff in pol
    ## pol <- pol[!bad]
    pol <- pol[!is.na(pol)]
    
    ## append pol on the end of datax
    datax <- append(datax,pol)
    
  }
  ## m <- mean(datax)
  ## m <- round(m, digits = 3)
  ## print(m)
  accum = 0
  for (k in 1:length(datax)) {
    accum <- accum + datax[k]
  }
  accum <- accum / length(datax)
  accum <- round(accum, digits = 3)
  return(accum)
  ##return(m)
}