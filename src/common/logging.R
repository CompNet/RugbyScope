#############################################################################################
# Defines functions used to log messages.
# 
# Usage:
# 	# start logging
# 	start.rec.log(text="mylogfile")
# 	
# 	# log some message
# 	tlog(<log offset>,"<log msg>")
#
#	# specific function for loops
# 	n <- 100
#	# specify the total number of iteration when logging
# 	tlog.start.loop(0,n,"Starting the loop")
#	for(i in 1:n)
#	{	Sys.sleep(runif(n=1,min=0,max=1))
#		# specify the current iteration when logging
#		# will log the estimated time before the loop ends
#		tlog.loop(2,i,"Iteration ",i,"/",n)
#	}
#	# will dislay the elapsed time for this loop
#	tlog.end.loop(0,"Finished the loop")
#   # if you use tlog.start.loop, then tlog.end.loop is compulsory
#	
# 	# close log file
# 	end.rec.log()
# 
# 05/2016 Vincent Labatut
#############################################################################################
# folder to store log files
FOLDER_LOG <- "log"
dir.create(path=FOLDER_LOG, showWarnings=FALSE, recursive=TRUE)

# start time of the log
START_TIME <- Sys.time()
# no opened log file 
CONNECTION <- NA

# loop start time
LOOP_START_TIME <- as.POSIXct(NA)
# total number of loop iterations
TOTAL_ITERATIONS <- NULL




#############################################################################################
# Properly formats a duration.
# Note: I tried first to use 
#	format(.POSIXct(duration,tz="GMT"),"%d:%H:%M:%S")
# But this command does not handle days properly (adds an extra day for no reason).
#
# duration: time duration to display, expressed in seconds.
#############################################################################################
format.duration <- function(duration)
{	# old version
	#res <- format(.POSIXct(duration,tz="GMT"),"%d:%H:%M:%S")
	
	# new version
	duration <- round(as.numeric(duration))
	days <- duration %/% 86400
	duration <- duration %% 86400
	hours <- duration %/% 3600
	duration <- duration %% 3600
	minutes <- duration %/% 60
	seconds <- duration %% 60
	if(days>0)
		res <- sprintf("%02d:%02d:%02d:%02d",days,hours,minutes,seconds)
	else
		res <- sprintf("%02d:%02d:%02d",hours,minutes,seconds)
	
	return(res)
}



#############################################################################################
# Start recording the logs in a text file.
#
# text: main name of the log file.
#############################################################################################
start.rec.log <- function(text=NA)
{	START_TIME <<- Sys.time()
	
	prefix <- format(START_TIME,"%Y%m%d_%H%M%S")
	log.file <- file.path(FOLDER_LOG,prefix)
	if(!is.na(text))
		log.file <- paste0(log.file,"_",text)
	log.file <- paste0(log.file,".txt")
	CONNECTION <<- file(log.file, encoding="UTF8")
	sink(CONNECTION, append=TRUE, split=TRUE)
}




#############################################################################################
# Stops recording the logs in a text file.
#############################################################################################
end.rec.log <- function()
{	end.time <- Sys.time()
	duration <- difftime(end.time, START_TIME, units="secs")
	tlog(0, "Total processing time: ", format.duration(duration))
	sink()
	close(CONNECTION)
}




#############################################################################################
# Computes the prefix used in log messages.
#
# offset: number of "." used to represent the hierarchical level of the message.
#############################################################################################
get.log.prefix <- function(offset=NA)
{	prefix <- paste0("[",format(Sys.time(),"%a %d %b %Y %X"),"] ")
	if(!is.na(offset))
	{	if(is.numeric(offset))
		{	os <- paste(rep(".",offset), sep="", collapse="")
			prefix <- paste0(prefix, os)
		}
		else
			prefix <- paste0(prefix, offset)
	}
	return(prefix)
}




#############################################################################################
# Logs the specified message on screen, adding current date and time, and possibly some
# offset (to represent the hierarchy of function calls).
#
# offset: number of "." used to represent the hierarchical level of the message.
# ...: parameters fetched to the cat function.
#############################################################################################
tlog <- function(offset=NA, ...)
{	prefix <- get.log.prefix(offset)
	cat(prefix, ..., "\n", sep="")
}




#############################################################################################
# Logs the specified message on screen, adding current date and time, and possibly some
# offset (to represent the hierarchy of function calls). Moreover, this specific function
# is designed to be used when starting a loop, in order to automatically display extra info.
#
# offset: number of "." used to represent the hierarchical level of the message.
# total.it: number of iterations of the loop.
# ...: parameters fetched to the cat function.
#############################################################################################
tlog.start.loop <- function(offset=NA, total.it, ...)
{	# init variables
	TOTAL_ITERATIONS <<- c(TOTAL_ITERATIONS, total.it)
	LOOP_START_TIME <<- c(LOOP_START_TIME, Sys.time())
	
	# display message
	tlog(offset, ...)
}




#############################################################################################
# Logs the specified message on screen, adding current date and time, and possibly some
# offset (to represent the hierarchy of function calls). Moreover, this specific function
# is designed to be used when iterating inside a loop, in order to automatically display extra 
# info.
#
# offset: number of "." used to represent the hierarchical level of the message.
# it: current iteration.
# ...: parameters fetched to the cat function.
#############################################################################################
tlog.loop <- function(offset=NA, it, ...)
{	# get prefix
	prefix <- get.log.prefix(offset)
	
	# set suffix
	cur.time <- Sys.time()
	el.duration <- as.numeric(difftime(cur.time, LOOP_START_TIME[length(LOOP_START_TIME)], units="secs"))
	avg.duration <- el.duration / it
	rem.duration <- as.difftime(max(0, avg.duration * TOTAL_ITERATIONS[length(TOTAL_ITERATIONS)] - el.duration), units="secs")
	suffix <- paste0(" [[ETA: ",format.duration(rem.duration),"]]")
	
	# display message
	cat(prefix, ..., suffix, "\n", sep="")
}




#############################################################################################
# Logs the specified message on screen, adding current date and time, and possibly some
# offset (to represent the hierarchy of function calls). Moreover, this specific function
# is designed to be used when exiting a loop, in order to automatically display extra info.
#
# offset: number of "." used to represent the hierarchical level of the message.
# ...: parameters fetched to the cat function.
#############################################################################################
tlog.end.loop <- function(offset=NA, ...)
{	# get prefix
	prefix <- get.log.prefix(offset)
	
	# set suffix
	end.time <- Sys.time()
	duration <- difftime(end.time, LOOP_START_TIME[length(LOOP_START_TIME)], units="secs")
	suffix <- paste0(" [[Total duration: ",format.duration(duration),"]]")
	
	# display message
	cat(prefix, ..., suffix, "\n", sep="")
	
	# update variables
	LOOP_START_TIME <<- LOOP_START_TIME[-length(LOOP_START_TIME)]
	TOTAL_ITERATIONS <<- TOTAL_ITERATIONS[-length(TOTAL_ITERATIONS)]
}
