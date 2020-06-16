# Function to generate SQL databases from the L3 of a project
# locally a sqlite (minimum) remotely on PostgreSQL (optional, not implemented yet)

library(lubridate)
library(data.table)
library(DBI)
library(Cops)
library(tidyverse)

Gen.SQL.DB <- function(ppath="/mnt/D/Data/Chone", overw=F) {

	setwd(ppath)
	if (!dir.exists("./L3")) stop("No L3 directory in project ", last(last(str_split(ppath, "/"))))

	# SQLite connection -----------------------------------------------------

	con <- dbConnect(RSQLite::SQLite(), paste0("./L3/",last(last(str_split(ppath, "/"))),".sqlite"))

	if (!is_empty(dbListTables(con)) & overw==F) {
		stop("./L3/",last(last(str_split(ppath, "/"))),".sqlite is not empty and overwrite is set to F")
	}

	# Field log table ---------------------------------------------------------

	Field_Log <- fread(file = file.path(getwd(),list.files(pattern = "Field_Log")))

	# RspectroAbs table assemblage --------------------------------------------
	#take the output of RspectroAbs (Ag.DB, Ap.DB, Anap.DB, Aph.DB)
	#There where different ID for forestville station : replace(as.character(Ag.DB$Samples),
	#which(Ag.DB$Samples %in% c("FR1","FR2","FR3","FR5","FR6","FR7")),
	#c("109r","109s","109t","109v","109w","109x"))

	load(list.files(path=file.path(getwd(), "L3"),
				 full.names = T,recursive = T, pattern = "Ag.RData"))
	Ag <- data.table(ID=Ag.DB$ID, t(Ag.DB[["Ag.raw"]]))
	colnames(Ag) <- c("ID", paste0("Ag_",Ag.DB$waves))
	dbWriteTable(con, "Ag", Ag, overwrite = overw)

	load(list.files(path=file.path(getwd(), "L3"),
				 full.names = T,recursive = T, pattern = "Ap.Stramski.RData"))
	Ap <- data.table(ID=Ap.DB$ID, t(Ap.DB$Ap))
	colnames(Ap) <- c("ID", paste0("Ap_",Ap.DB$waves))
	dbWriteTable(con, "Ap", Ap, overwrite = overw)

	load(list.files(path=file.path(getwd(), "L3"),
				 full.names = T,recursive = T, pattern = "Anap.Stramski.RData"))
	Anap <- data.table(ID=Anap.DB$ID, t(Anap.DB$Anap))
	colnames(Anap) <- c("ID", paste0("Anap_",Anap.DB$waves))
	dbWriteTable(con, "Anap", Anap, overwrite = overw)

	load(list.files(path=file.path(getwd(), "L3"),
				 full.names = T,recursive = T, pattern = "Aph.Stramski.RData"))
	Aph <- data.table(ID=Aph.DB$ID, t(Aph.DB$Aph))
	colnames(Aph) <- c("ID", paste0("Aph_",Aph.DB$waves))
	dbWriteTable(con, "Aph", Aph, overwrite = overw)


	# ASPH table assemblage ---------------------------------------------------

	ASPH <- fread(file = list.files(path=file.path(getwd(), "L3"),
							  full.names = T,recursive = T, pattern = "ASPH_DB.csv"))
	ASPH <- na.omit(ASPH)
	#remove duplicated ID to take only the measurment closest to the surface
	ASPH <- ASPH[!duplicated(ASPH$ID),]
	dbWriteTable(con, "A", ASPH, overwrite = overw)

	# HS6 table assemblage ----------------------------------------------------

	HS6 <- fread(file = list.files(path=file.path(getwd(), "L3"),
							 full.names = T,recursive = T, pattern = "HS6_DB.csv"))
	HS6 <- na.omit(HS6)
	HS6 <- HS6[!duplicated(HS6$ID),]
	HS6 <- HS6[,c(-15,-16,-18)]
	dbWriteTable(con, "Bb", HS6, overwrite = overw)


	# COPS table assemblage ---------------------------------------------------

	load(list.files(path=file.path(getwd(), "L3"),
				 full.names = T,recursive = T, pattern = "BSI.RData"))

	Rrs <- data.table(ID=COPS.DB$stationID, Rrs=COPS.DB$Rrs.m)
	colnames(Rrs) <- c("ID", paste0("Rrs_",COPS.DB$waves))
	dbWriteTable(con, "Rrs", Rrs, overwrite = overw)

	Rrs_sd <- data.table(ID=COPS.DB$stationID, Rrs=COPS.DB$Rrs.sd)
	colnames(Rrs_sd) <- c("ID", paste0("Rrs_sd_",COPS.DB$waves))
	dbWriteTable(con, "Rrs_sd", Rrs_sd, overwrite = overw)

	#LwN <- data.table(ID=COPS.DB$stationID, LwN=COPS.DB$nLw.m)
	#colnames(LwN) <- c("ID", paste0("LwN_",COPS.DB$waves))
	#dbWriteTable(con, "LwN", LwN)

	Kd_1p <- data.table(ID=COPS.DB$stationID, Kd1p=COPS.DB$Kd.1p.m)
	colnames(Kd_1p) <- c("ID", paste0("Kd_1p_",COPS.DB$waves))
	dbWriteTable(con, "Kd_1p", Kd_1p, overwrite = overw)

	Kd_1p_sd <- data.table(ID=COPS.DB$stationID, Kd1p=COPS.DB$Kd.1p.sd)
	colnames(Kd_1p_sd) <- c("ID", paste0("Kd_1p_sd",COPS.DB$waves))
	dbWriteTable(con, "Kd_1p_sd", Kd_1p_sd, overwrite = overw)

	Kd_10p <- data.table(ID=COPS.DB$stationID, Kd10p=COPS.DB$Kd.10p.m)
	colnames(Kd_10p) <- c("ID", paste0("Kd_10p",COPS.DB$waves))
	dbWriteTable(con, "Kd_10p", Kd_10p, overwrite = overw)

	Kd_10p_sd <- data.table(ID=COPS.DB$stationID, Kd10p=COPS.DB$Kd.10p.sd)
	colnames(Kd_10p_sd) <- c("ID", paste0("Kd_10p_sd",COPS.DB$waves))
	dbWriteTable(con, "Kd_10p_sd", Kd_10p_sd, overwrite = overw)

	Ed0 <- data.table(ID=COPS.DB$stationID, Ed0=COPS.DB$Ed0.0p.m)
	colnames(Ed0) <- c("ID", paste0("Ed0_",COPS.DB$waves))
	dbWriteTable(con, "Ed0", Ed0, overwrite = overw)

	Ed0_sd <- data.table(ID=COPS.DB$stationID, Ed0=COPS.DB$Ed0.0p.sd)
	colnames(Ed0_sd) <- c("ID", paste0("Ed0__sd",COPS.DB$waves))
	dbWriteTable(con, "Ed0_sd", Ed0_sd, overwrite = overw)

	Ed0_f_diff <- data.table(ID=COPS.DB$stationID, Ed0=COPS.DB$Ed0.f.diff)
	colnames(Ed0_f_diff) <- c("ID", paste0("Ed0_f_",COPS.DB$waves))
	dbWriteTable(con, "Ed0_f_diff", Ed0_f_diff, overwrite = overw)

	Sunzen <- data.table(ID=COPS.DB$stationID, Sunzen=COPS.DB$sunzen)
	Field_Log <- left_join(Field_Log, Sunzen, by= "ID")

	# Biogeochimie tables assemblage ------------------------------------------
	SPM_table <- fread(file = list.files(path=file.path(getwd(), "L3"),
								  full.names = T,recursive = T, pattern = "SPM.csv"))

	SPM_table <- setDT(SPM_table)[,list(ID, Station, Replicate, SPM, PIM, POM=SPM-PIM)]

	SPMs <- setDT(SPM_table)[,list(SPM=as.numeric(median(SPM)),
			  	   			 PIM=as.numeric(median(PIM)),
							 POM=as.numeric(median(POM))), by=ID]
	dbWriteTable(con, "SPMs", SPMs, overwrite = overw)

	#!Should also compute SPM_Mean the way carlos does, with confidence interval!
	SPMs_Stats <- setDT(SPM_table)[,list(SPM_Mean=mean(SPM),
							SPM_Max=max(SPM),
							SPM_Min=min(SPM),
							SPM_Median=as.numeric(median(SPM)),
							SPM_Std=sd(SPM),
							PIM_Mean=mean(PIM),
							PIM_Max=max(PIM),
							PIM_Min=min(PIM),
							PIM_Median=as.numeric(median(PIM)),
							PIM_Std=sd(PIM),
							POM_Mean=mean(POM),
							POM_Max=max(POM),
							POM_Min=min(POM),
							POM_Median=as.numeric(median(POM)),
							POM_Std=sd(POM)), by=ID]

	dbWriteTable(con, "SPMs_Stats", SPMs_Stats, overwrite = overw)

	Chl_a_table <- fread(file = list.files(path=file.path(getwd(), "L3"),
								    full.names = T,recursive = T, pattern = "Chl-a.csv"))

	Pigments <- setDT(Chl_a_table)[,list(Chl_a=median(Chl_a),
								  Phaeo=median(Phaeopig)), by=ID]
	dbWriteTable(con, "Pigments", Pigments, overwrite = overw)

	Pigments_Stats <- setDT(Chl_a_table)[,list(Chl_a_Mean=mean(Chl_a),
									   Chl_a_Max=max(Chl_a),
									   Chl_a_Min=min(Chl_a),
									   Chl_a_Median=median(Chl_a),Chl_a_Std=sd(Chl_a),
									   Phaeo_Mean=mean(Phaeopig),
									   Phaeo_Max=max(Phaeopig),
									   Phaeo_Min=min(Phaeopig),
									   Phaeo_Median=median(Phaeopig),
									   Phaeo_Std=sd(Phaeopig)), by=ID]
	dbWriteTable(con, "Pigments_Stats", Pigments_Stats, overwrite = overw)

	dbWriteTable(con, "Field_Log", Field_Log, overwrite = overw)
	#RSQLite disconnect
	dbDisconnect(con)
}
