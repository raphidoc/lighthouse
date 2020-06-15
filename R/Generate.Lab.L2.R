#Function to generate Lab L2 from L1 and Log table

library(data.table)
library(tidyverse)
library(lubridate)


project <- "/media/raphael/D/Data/Chone"
setwd(project)

Generate.Lab.L2 <- function(L1 = paste0(getwd(),"/L1/Lab"), LogTable = file.path(getwd(),list.files(pattern = "Field_Log"))){

	#Read LogTable file
	LogTable <- fread(LogTable, data.table = F)
	CopsTable <- LogTable %>% filter(COPS == "T") %>%
		mutate(DateTime = ymd_hm(paste(Date, Time_UTC, sep = "T"))) %>%
		select(ID, DateTime, Lat, Long, )

	#Match experiment by station

}
