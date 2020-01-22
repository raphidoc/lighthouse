#TO DO : Create a report of coherence between Casts in L1 and LogTable

#Function to generate L2 COPS architecture from L1 and and a log table (ASCII)

library(data.table)
library(tidyverse)
library(lubridate)

Generate.L2 <- function(L1 = paste0(getwd(),"/L1/COPS"), LogTable = paste(getwd(),list.files(pattern = "LogTable"), sep = "/")){

	#Read LogTable file
	LogTable <- fread(LogTable, data.table = F)
	CopsTable <- LogTable %>% filter(COPS == "T") %>%
		mutate(DateTime = ymd_hm(paste(Date, Time, sep = "T"))) %>%
		select(StationID, Boat, DateTime, Lat, Long, COPS)

	#associate GPS file by station
	GPSpath <- file.path(L1, list.files(L1, pattern = "GPS", recursive = T))
	GPSdate <- ymd(str_extract(GPSpath, "[:digit:]{8}"))
	GPSboat <- str_extract(GPSpath, "(?<=/)[:alpha:]+(?=_[:digit:]{8})")
	GPSTable <- data.frame(Path = GPSpath, Date = GPSdate, Boat = GPSboat, stringsAsFactors=FALSE)

	IDlist <- c()
	for(i in 1:length(CopsTable[,1])){
		if(any(date(CopsTable$DateTime)[i] == GPSTable$Date & CopsTable$Boat[i] == GPSTable$Boat)){
			IDlist[i] <- GPSTable$Path[which(date(CopsTable$DateTime)[i] == GPSTable$Date & CopsTable$Boat[i] == GPSTable$Boat)]

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

	#Create column of time interval
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
	L2dir <- paste0(getwd(), "/L2")
	COPSpath <- file.path(L2dir, paste0(date(CopsTable$DateTime), "_Station", CopsTable$StationID), "COPS")
	for(i in 1:length(COPSpath)){
		dir.create(COPSpath[i], recursive = T)
		write(COPSpath[i], file = file.path(COPSpath[i], "directories.for.cops.dat"))
	}
	write(COPSpath, file = file.path(L2dir, "directories.for.cops.dat"))
	return(dirdats = file.path(L2dir, "directories.for.cops.dat"))

	#Copy GPS file in each station
	CopsTable <- CopsTable %>% mutate(L2path = COPSpath)
	file.copy(CopsTable$GPS, file.path(CopsTable$L2path, str_extract(CopsTable$GPS, "GPS_[:digit:]{6}\\.[:alpha:]{3}")))

	#create L2path for each matched cast and copy
	L1Table <- L1Table %>% filter(StationID != "NA") %>%
		mutate(L2path = file.path(L2dir, paste0(date(DateTime), "_Station", StationID), "COPS", Name))
	L1files <- as.character(L1Table$L1path)
	L2path <- L1Table$L2path
	file.copy(L1files, L2path)


}
