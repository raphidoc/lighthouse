#' @export
#' @import dplyr
#' @import lubridate
#' @import stringr
#'

#TO DO : Create a report of coherence between Casts in L1 and LogTable

#Function to generate L2 COPS architecture from L1 and and a log table (ASCII)

#library(data.table)
#library(dplyr)
#library(stringr)
#library(lubridate)


#project <- "/home/raphael/TEST"
#setwd(project)

Generate.COPS.L2 <- function(project){


	# set L1 and L2 absolute path
	L1 = file.path(project,"L1","COPS")
	L2 <- file.path(project, "L2")

	# Check project before doing any manipulation
	CheckList <- Check.project(project,L1,L2,param="COPS")

	if (CheckList["Proot"][[1]] == F) {stop("project path is not set at a project root folder")}
	if (CheckList["L2exists"][[1]] == T) {
	stop("L2 structure for COPS is alredy present. You better be sure of what your doing ...\n
		take care of this by yourself !")}

	# Read LogTable file
	LogTable = list.files(path = project, pattern = "data_synthesis|Data_Synthesis", full.names = T)

	LogTable <- data.table::fread(LogTable, data.table = F)

	# Check if multiple boat where present
	if (any(names(LogTable) == "Boat") & length(unique(LogTable["Boat"][[1]])) > 1) {

		# Merge potential multiple COPS into one column (boolean)
		if (length(grep("COPS",names(LogTable))) > 1){
			COPS <- cbind(ifelse(LogTable[grep("COPS",names(LogTable))] == "T", T,F),
					    rowSums(ifelse(LogTable[grep("COPS",names(LogTable))] == "T", T,F)) > 0)

			LogTable <- LogTable %>% mutate(COPS = COPS[,3])
		}

		# Filter time search, done only for TRUE COPS station
		CopsTable <- LogTable %>% filter(COPS == T) %>%
			mutate(DateTime = ymd_hm(paste(Date, Time, sep = "T"))) %>%
			select(StationID, Boat, DateTime, Lat, Long, COPS)

		# Associate GPS file by station
		GPSpath <- file.path(L1, list.files(L1, pattern = "GPS", recursive = T))
		GPSdate <- ymd(str_extract(GPSpath, "[:digit:]{8}"))
		GPSboat <- str_extract(GPSpath, "(?<=/)[:alpha:]+(?=_[:digit:]{8})")
		GPSTable <- data.frame(Path = GPSpath, Date = GPSdate, Boat = GPSboat, stringsAsFactors=FALSE)

		IDlist <- c()
		for(i in 1:length(CopsTable[,1])){
			if(any(lubridate::date(CopsTable$DateTime)[i] == GPSTable$Date & CopsTable$Boat[i] == GPSTable$Boat)){
				IDlist[i] <- GPSTable$Path[which(lubridate::date(CopsTable$DateTime)[i] == GPSTable$Date & CopsTable$Boat[i] == GPSTable$Boat)]

			}
			else{
				IDlist[i] <- NA
			}
		}
		CopsTable <- CopsTable %>% mutate(GPS = IDlist)


		#Create L1Table of all URC casts and extract relevant information for association
		L1List <- list.files(L1, pattern = "URC", recursive = T)
		Name <- str_extract(L1List, "(?<=/).+$")
		Boat <- str_extract(L1List, "[:alpha:]+(?=_)")
		Date <- as.Date.character(gsub("^", "\\120" ,str_extract(L1List, "(?<=CAST_[:digit:]{3}_)[:digit:]{6}"), perl = T), format = "%Y%m%d")
		Time <- gsub("(\\d{2})(?=\\d{2})", "\\1:", str_extract(L1List, "[:digit:]{6}(?=_URC)"), perl = TRUE)
		Cast <- str_extract(L1List, "(?<=CAST_)[:digit:]{3}")
		L1Table <- data.frame(L1path = file.path(L1, L1List), Name, Boat, DateTime=ymd_hms(paste(Date, Time, sep = "T")), Cast)

		#function to create interval with boundary between (midle) each station time, first and last with 1800 seconds
		Station.interval <- function(x){
			int_diff(c(x[1]-1800,
					 (int_length(int_diff(x))/2)+x[-length(x)],
					 x[length(x)]+1800))
		}

		#Create column of time interval (group by craft (boat, ...) used on field)
		CopsTable <- CopsTable %>% group_by(Boat) %>% arrange(DateTime, .by_group = T) %>%
			mutate(start =  int_start(Station.interval(DateTime)),
				  end = int_end(Station.interval(DateTime)))

		#Tow steps because class "interval" is destroyed by row_bind() used in group_by() operation
		CopsTable <- CopsTable %>% ungroup() %>% mutate(inters = interval(start, end))

		#associate cast with station based on a time interval
		IDlist <- list()
		for(i in 1:length(L1Table[,1])){
			if(any(L1Table$DateTime[i] %within% CopsTable$inters & L1Table$Boat[i] == CopsTable$Boat)){
				IDlist[i] <- CopsTable$StationID[which(L1Table$DateTime[i] %within% CopsTable$inters & L1Table$Boat[i] == CopsTable$Boat)]
			}
			else{
				IDlist[i] <- NA
			}
		}
		L1Table <- L1Table %>% mutate(StationID = IDlist)

		#Create L2 structure
		L2COPS <- file.path(L2, paste0(gsub("-", "", as.character(lubridate::date(CopsTable$DateTime))),
									 "_Station", CopsTable$StationID),paste0("COPS_",CopsTable$Boat))
		for(i in L2COPS){
			dir.create(i, recursive = T)
			write(i, file = file.path(i, "directories.for.cops.dat"))
		}
		write(L2COPS, file = file.path(L2, "directories.for.cops.dat"))
		#return(dirdats = file.path(L2, "directories.for.COPS.dat"))

		#Copy GPS file in each station
		CopsTable <- CopsTable %>% mutate(L2path = L2COPS)
		file.copy(CopsTable$GPS, file.path(CopsTable$L2path, str_extract(CopsTable$GPS, "GPS_[:digit:]{6}\\.[:alpha:]{3}")))

		#create L2path for each matched cast and copy
		L1Table <- L1Table %>% filter(StationID != "NA") %>%
			mutate(L2path = file.path(L2, paste0(gsub("-", "", as.character(lubridate::date(DateTime))),
											"_Station", StationID),paste0("COPS_",Boat), Name))

		L1files <- as.character(L1Table$L1path)
		L2path <- L1Table$L2path
		file.copy(L1files, L2path, overwrite = F)

		# Generate report
		#report <- file(paste0("Gen_COPS_L2_report_",Sys.Date(),".txt"))
		#cat(paste0("Station : ",CopsTable$StationID,"\n"), file = report , append = F)
		#cat(paste0("Number of COPS copied = "), file = report , append = T)
	} else {
		print("To be implemented")
	}
}
