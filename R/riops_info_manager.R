#' @export
#' @import Riops
#' @import dplyr
#' @import lubridate
#' @import stringr

#library(data.table)

riops_info_manager <- function(project, field= "", to= "", mindate= "yyyy-mm-dd", maxdate="yyyy-mm-dd", Boat=c("")) {

	L2 <- file.path(project, "L2")
	dirs = grep("/COPS(_[[:alpha:]]+)?$",list.dirs(L2), value = T)
	#dirdats <- scan(file = "directories.for.cops.dat", "", sep = "\n", comment.char = "#")

	# create a filter based on date
	#(would be useful on multi year project where differences occur in data collection)
	mindate <- NULL
	maxdate <- NULL

	# Check field paramter
	field <- as.character(field)
	if (!any(field == c("Tref.ACS", "scat.correction", "blank.ACS", "blank.BB9", "HS6.CALYEAR" , "blank.BB3"))) {
		stop("field does not match any exsisting field.
Only the following are accepted: blank.ACS, blank.BB9")
	}

	to <- as.character(to)

	if (Sys.getenv("R_IOPs_DATA_DIR") == ""){stop("R_IOPs_DATA_DIR from package Riops, is not set")}

	header.info.file <- file.path(Sys.getenv("R_IOPs_DATA_DIR"), "info.header.dat")

	for(i in dirs) {
		if(!file.exists(i)) {
			cat(i, "does not exist")
			next()
		}
		# message(paste0("PROCESSING DIRECTORY:", dirs))

		#look if absorption file empty, if so go to next iteration
		if(field=="chl" && to == "0"){

			abs <- data.table::fread(file= file.path(i, "absorption.cops.dat"), header= T)

			if(abs[[2]][1] == "a1"){
				message(paste(i, "No absorption values", sep= "   "))
				next()
			}
		}

		info.file <- file.path(i, "info.cops.dat")
		info.tab <- read.table(info.file,
						   colClasses = c(
						   	"character", "numeric", "numeric", "numeric",
						   	"character", "character", "character", "character",
						   	"character", "character", "character", "character"),
						   col.names = c("file", "lon", "lat", "chl", "timwin", "ssrm", "tiltm", "smoo", "b1", "b2", "b3", "b4"),
						   header = FALSE, fill = TRUE, sep = ";")

		info.tab[[field]] <- to
		message(paste(i, "modified", sep = "   "))

		info.tab
		file.copy(from = header.info.file, to = info.file, overwrite = T)
		write.table(info.tab, file = info.file , append = T, quote = F, sep = ";", dec = ".",
				  row.names = F, col.names = F)
	}
}
