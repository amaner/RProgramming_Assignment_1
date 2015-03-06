complete <- function(directory, id){
	datax <- data.frame()
	for (j in 1:length(id)){
		
		## build the file name from information given
		numid <- paste(ifelse(id[j]>=100,"",ifelse(id[j]>=10,"0","00")),id[j],sep="")
		fname <- paste(numid, ".csv", sep="")
		wdir <- paste(getwd(), "/", sep="")
		tdir <- paste(wdir, directory, sep = "")
		actdir <- paste(tdir, "/", sep="")
		actfile <- paste(actdir, fname, sep="")
		
		## read the file into a temporary data frame
		data <- read.csv(actfile)
		
		## dump columns 2 and 3 into vectors
		x <- data[,2]
		y <- data[,3]
		
		## find out now long the vectors are
		len <- length(x)
		
		## make a new logical test vector
		## entries are TRUE if NA and FALSE if not 
		xbad <- is.na(x)
		ybad <- is.na(y)
		
		## count the number of good entries (FALSES)
		i <- 1
		count <- 0
		for (i in 1:len){
			if ((xbad[i] == FALSE) && (ybad[i] == FALSE)){
				count <- count + 1
			}
			else{
			}
			
		}
		
		## append the file id (as an int) into the first
		## column of datax, and the good counti into the
		## second colum of datax
		datax[j,1] <- id[j]
		datax[j,2] <- count
		
	}
	
	## give datax the proper column names
	colnames(datax)[1] <- "id"
	colnames(datax)[2] <- "nobs"
	print(datax)
}


