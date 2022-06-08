#' @name gen_sql_db
#' @title gen_sql_db
#' @description  Function to generate SQL database from the L3 of a project,
#' locally a SQLite, remotely on a PostgreSQL instalation (optional, not implemented yet)
#'
#' @import dplyr
#' @import DBI
#' @import stringr
#' @export

#library(lubridate)
#library(data.table::data.table)
#library(DBI)
#library(dplyr)

sql_gen <- function(project, mission="",overw=F) {

	L1 <- file.path(project,"L1")
	L3 <- file.path(project,"L3")

	if (!dir.exists(L3)) stop("No L3 directory in project: ", project)

	if (!exists("mission") || mission == "" ) {
		mission <- last(last(str_split(project, "/")))
		message("mission name empty, taking name of the project: ",mission)
	}


# SQLite connection -------------------------------------------------------

	con <- dbConnect(RSQLite::SQLite(), file.path(L3,paste0(mission,".sqlite")))

	if (!exists("con")) {stop("SQLite connection not established")}

	if (length(dbListTables(con)) != 0 & overw==F) {
		stop(mission,".sqlite is not empty and overwrite is set to F")
	}

# Params manager ----------------------------------------------------------

	handyParams <- list.files(L3, recursive = T, include.dirs = T, full.names = T)

	handyParams <- handyParams[str_detect(handyParams, "Report",negate = T)]

	# param_manager <- function(handyParams, param){
	# 	if (any(str_detect(handyParams, param))) {
	# 		message("Creating table for: ", param)
	# 		handyParams[str_detect(handyParams, param)]
	# 		TRUE
	# 	} else {FALSE}
	# }

	read_db <- function(handyParams, DB) {
		if (any(str_detect(handyParams, paste0(DB,"_DB")))) {
			read.csv(handyParams[str_detect(handyParams, paste0(DB,"_DB"))], colClasses = "character")
		} else {NULL}
	}



# data_synthesis ----------------------------------------------------------


	SyntheFile <- list.files(path = project, pattern = "data_synthesis", full.names = T)

	if (length(SyntheFile) == 0) {
		stop("No 'data_synthesis' found in: ",file.path(project))
	} else if (length(SyntheFile) > 1) {
		stop("Multiple 'data_synthesis' found in: ",file.path(project),
			"\n",str_c(SyntheFile, collapse = "\n"))
	}

	data_synthesis <- data.table::fread(file = SyntheFile, colClasses = "character")


# WaterSample assemblage --------------------------------------------------

	LabFile <- list.files(file.path(L1,"WaterSample"),pattern = "water_sample_log", full.names = T)

	if (length(LabFile) == 0) {
		stop("No water_sample_log found in: ",file.path(L1,"WaterSample"))
	} else if (length(LabFile) > 1) {
		stop("Multiple water_sample_log found in: ",file.path(L1,"WaterSample"),
			"\n",str_c(LabFile, collapse = "\n"))
	}

	LabLog <- data.table::fread(file = LabFile, data.table = F, colClasses = "character")
	if (is.data.frame(LabLog)) {dbWriteTable(con, "lab_log", LabLog, overwrite = overw)}

	Ap <- read_db(handyParams, "Ap")
	if (is.data.frame(Ap)) {dbWriteTable(con, "Ap", Ap, overwrite = overw)}

	Anap <- read_db(handyParams, "Anap")
	if (is.data.frame(Anap)) {dbWriteTable(con, "Anap", Anap, overwrite = overw)}

	Aph <- read_db(handyParams, "Aph")
	if (is.data.frame(Aph)) {dbWriteTable(con, "Aph", Aph, overwrite = overw)}

	Ag <- read_db(handyParams, "Ag")
	if (is.data.frame(Ag)) {dbWriteTable(con, "Ag", Ag, overwrite = overw)}

	SPMs <- read_db(handyParams, "SPMs")
	if (is.data.frame(SPMs)) {dbWriteTable(con, "SPMs", SPMs, overwrite = overw)}


# COPS assemblage ---------------------------------------------------------


	if (file.exists(list.files(path=file.path(L3,"COPS"),
						  full.names = T, pattern = ".RData"))) {

		load(list.files(path=file.path(L3,"COPS"),
					 full.names = T, pattern = ".RData"))

		Rrs <- data.table::data.table(ID=COPS.DB$ID, Rrs=COPS.DB$Rrs.m)
		colnames(Rrs) <- c("ID", paste0("Rrs_",COPS.DB$waves))
		dbWriteTable(con, "Rrs", Rrs, overwrite = overw)

		Rrs_sd <- data.table::data.table(ID=COPS.DB$ID, Rrs=COPS.DB$Rrs.sd)
		colnames(Rrs_sd) <- c("ID", paste0("Rrs_sd_",COPS.DB$waves))
		dbWriteTable(con, "Rrs_sd", Rrs_sd, overwrite = overw)

		nLw <- data.table::data.table(ID=COPS.DB$ID, LwN=COPS.DB$nLw.m)
		colnames(nLw) <- c("ID", paste0("nLw_",COPS.DB$waves))
		dbWriteTable(con, "nLw", nLw, overwrite = overw)

		nLw_sd <- data.table::data.table(ID=COPS.DB$ID, LwN=COPS.DB$nLw.sd)
		colnames(nLw_sd) <- c("ID", paste0("nLw_sd_",COPS.DB$waves))
		dbWriteTable(con, "nLw_sd", nLw_sd, overwrite = overw)

		Kd1p <- data.table::data.table(ID=COPS.DB$ID, Kd1p=COPS.DB$Kd.1p.m)
		colnames(Kd1p) <- c("ID", paste0("Kd1p_",COPS.DB$waves))
		dbWriteTable(con, "Kd1p", Kd1p, overwrite = overw)

		Kd1p_sd <- data.table::data.table(ID=COPS.DB$ID, Kd1p=COPS.DB$Kd.1p.sd)
		colnames(Kd1p_sd) <- c("ID", paste0("Kd1p_sd_",COPS.DB$waves))
		dbWriteTable(con, "Kd1p_sd", Kd1p_sd, overwrite = overw)

		Kd10p <- data.table::data.table(ID=COPS.DB$ID, Kd10p=COPS.DB$Kd.10p.m)
		colnames(Kd10p) <- c("ID", paste0("Kd10p_",COPS.DB$waves))
		dbWriteTable(con, "Kd10p", Kd10p, overwrite = overw)

		Kd10p_sd <- data.table::data.table(ID=COPS.DB$ID, Kd10p=COPS.DB$Kd.10p.sd)
		colnames(Kd10p_sd) <- c("ID", paste0("Kd10p_sd_",COPS.DB$waves))
		dbWriteTable(con, "Kd10p_sd", Kd10p_sd, overwrite = overw)

		Ed0 <- data.table::data.table(ID=COPS.DB$ID, Ed0=COPS.DB$Ed0.0p.m)
		colnames(Ed0) <- c("ID", paste0("Ed0_",COPS.DB$waves))
		dbWriteTable(con, "Ed0", Ed0, overwrite = overw)

		Ed0_sd <- data.table::data.table(ID=COPS.DB$ID, Ed0=COPS.DB$Ed0.0p.sd)
		colnames(Ed0_sd) <- c("ID", paste0("Ed0_sd",COPS.DB$waves))
		dbWriteTable(con, "Ed0_sd", Ed0_sd, overwrite = overw)

		Ed0_f_diff <- data.table::data.table(ID=COPS.DB$ID, Ed0=COPS.DB$Ed0.f.diff)
		colnames(Ed0_f_diff) <- c("ID", paste0("Ed0_f_",COPS.DB$waves))
		dbWriteTable(con, "Ed0_f_diff", Ed0_f_diff, overwrite = overw)

		Sunzen <- data.table::data.table(ID=COPS.DB$ID, Sunzen=COPS.DB$sunzen)
		data_synthesis <- left_join(data_synthesis, Sunzen, by= "ID")
	}


# IOP assemblage ----------------------------------------------------------

	# need to bind cols of same variable comming from different instruments
	# interpolate value to common wavelength ?
	# Think it's better not to modify data here, not the purpose of the fuction
	# A Tag like 'Instrument' should come with the data from here to specifies wich instrument was
	# Used to acquire varaible A or Bb: Nope, actually this is the job of data_synthesis table

	# A and C
	ASPH <- read_db(handyParams, "ASPH")
	if (is.data.frame(ASPH)) {
		ASPH_A <- ASPH[str_detect(names(ASPH), "ID|Depth|A_")]
		#ASPH_wl <- na.omit(str_extract(names(ASPH_A), "(?<=A_)[:digit:]{3}"))
	} else {
		ASPH_A <- NULL
	}

	ACS <- read_db(handyParams, "ACS")
	if (is.data.frame(ACS)) {
		ACS_A <- ACS[str_detect(names(ACS), "A_")]
		# ACS_wl <- na.omit(str_extract(names(ACS_A), "(?<=A_)[:digit:]{3}"))
		# ACS_A <- approx(ACS_wl,ACS[2,str_detect(names(ACS), "A_")],xout = ASPH_wl)
	} else {
		ACS_A <- NULL
	}

	A <- bind_rows(ASPH_A,ACS_A)

	ACS_C <- ACS[str_detect(names(ACS), "ID|Depth|C_")]

	if (is.data.frame(A)) {dbWriteTable(con, "A", A, overwrite = overw)}
	if (is.data.frame(ACS_C)) {dbWriteTable(con, "C", ACS_C, overwrite = overw)}

	# Bb and Bbp

	HS6 <- read_db(handyParams, "HS6")
	if (is.data.frame(HS6)) {
		HS6_Bb <- HS6[str_detect(names(HS6), "ID|Depth|Bb_")]
		HS6_Bbp <- HS6[str_detect(names(HS6), "ID|Depth|Bbp_")]
	} else {
		HS6_Bb <- NULL
		HS6_Bbp <- NULL
	}

	BB9 <- read_db(handyParams, "BB9")
	if (is.data.frame(BB9)) {
		BB9_Bb <- BB9[str_detect(names(BB9), "ID|Depth|Bb_")]
		BB9_Bbp <- BB9[str_detect(names(BB9), "ID|Depth|Bbp_")]
	} else {
		BB9_Bb <- NULL
		BB9_Bbp <- NULL
	}

	BB3 <- read_db(handyParams, "BB3")
	if (is.data.frame(BB3)) {
		BB3_Bb <- BB3[str_detect(names(BB3), "ID|Depth|Bb_")]
		BB3_Bbp <- BB3[str_detect(names(BB3), "ID|Depth|Bbp_")]
	} else {
		BB3_Bb <- NULL
		BB3_Bbp <- NULL
	}

	Bb <- bind_rows(HS6_Bb,BB9_Bb,BB3_Bb)
	Bbp <- bind_rows(HS6_Bbp,BB9_Bbp,BB3_Bbp)

	if (is.data.frame(Bb)) {dbWriteTable(con, "Bb", Bb, overwrite = overw)}
	if (is.data.frame(Bbp)) {dbWriteTable(con, "Bbp", Bbp, overwrite = overw)}

	CTD <- read_db(handyParams, "CTD")
	if (is.data.frame(CTD)) {dbWriteTable(con, "CTD", CTD, overwrite = overw)}

	# FLECO <- read_db(handyParams, "FLECO")
	# if (is.data.frame(FLECO)) {dbWriteTable(con, "FLECO", FLECO, overwrite = overw)}

# wrap up -----------------------------------------------------------------


	dbWriteTable(con, "data_synthesis", data_synthesis, overwrite = overw)

	dbDisconnect(con)
}
