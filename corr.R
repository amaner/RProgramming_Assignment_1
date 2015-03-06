corr <- function(directory, threshold = 0){
		
		## this function reads all csv files in a specified
		## directory, counts how many data rows are non-empty,
		## and if this count is greater than threshold it
		## dumps the good values into an output numeric vector
		## if the count is <= threshold, it returns a vector
		## of 0 length
		
		## build actual directory name from input variable directory
		wdir <- paste(getwd(), "/", sep="")
		tdir <- paste(wdir, directory, sep = "")
		actdir <- paste(tdir, "/", sep="")
		
		## find out how many files are in the directory
		allfiles <- as.character(list.files(actdir))
		numfiles <- length(allfiles)
		
		## vector to contain correlation coefficients
		## and an index for the vector
		corvector <- numeric(numfiles)
		threshcount <- 0
		
		for (j in 1:numfiles){
			
			## build the complete file name
			actfile <- paste(actdir, allfiles[j], sep="")
			
			## read the file into a temporary data frame
			data <- read.csv(actfile)
						
			## dump columns 2 and 3 into vectors
			x <- data[,2]
			y <- data[,3]
			
			## make a new logical test vector
			## entries are TRUE if NA and FALSE if not 
			xbad <- is.na(x)
			ybad <- is.na(y)
			
			## make vectors for "good" rows
			xgood <- numeric(length(x))
			xgood[] <- NA
			ygood <- numeric(length(x))
			ygood[] <- NA
			
			## count the number of good entries (FALSES)
			i <- 1
			count <- 0
			for (i in 1:length(x)){
				if ((xbad[i] == FALSE) && (ybad[i] == FALSE)){
					## increment count of good values
					count <- count + 1
					## store good row in xgood and ygood
					xgood[count] <- x[i]
					ygood[count] <- y[i]
				}
				else{
				}
			}
			count2 <- 0
			if (count > threshold){
				## print(count)
				threshcount <- threshcount + 1
				## print(threshcount)
				xgoodgood <- numeric(count)
				ygoodgood <- numeric(count)
				for (count2 in 1:count){
					xgoodgood[count2] <- xgood[count2]
					ygoodgood[count2] <- ygood[count2]
				}
				corvector[threshcount] <- cor(xgoodgood, ygoodgood)
			}
			
		}
		if (threshcount == 0){
			return(corvector[0])
		}
		else{
			corvectorgood <- numeric(threshcount)
			for (i in 1:threshcount){
				corvectorgood[i] <- corvector[i]
			}
			return(corvectorgood)
		}
		
}
