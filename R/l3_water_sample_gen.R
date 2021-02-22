#' @name l3_water_sample_gen
#' @title l3_water_sample_gen
#' @author Raphael Mabit
#'
#' @description Generate a data base (one file by parameter) from L1 water sample files.
#' Different methods are aplicated depending on the type of parameter.
#' For Ag and Ap parameter, it takes the output of RspectroAbs package.
#' For SPM (Chl-a to be implemented) it read a .csv such as "link to example table"
#'
#' @param project The top level folder of the project
#'
#' @param mission Optional, name for data base file.
#' If none is providen, it is taken from the project name.
#'
#' @param  params Character vector used to select for which parameters data bases should be created
#'
#' @import dplyr
#' @import stringr
#' @export
#'

l3_water_sample_gen <- function(project,mission="",params=c("SPM","Ag","Ap")) {


# Setup -------------------------------------------------------------------

	if (!exists("mission") || mission == "" ) {
		mission <- str_split(project,"/")[[1]]
		mission <- mission[length(mission)]
		message("mission name empty, taking name of the project: ",mission)
	}

	L1 <- file.path(project,"L1")
	L3 <- file.path(project,"L3","WaterSample")

	CheckList <- lighthouse::check_project(project, L1)
	if (CheckList["Proot"][[1]] == F) {stop(project,"is not a project root folder")}

	SyntheFile = list.files(path = project, pattern = "data_synthesis|Data_Synthesis", full.names = T)

	Synthesis <- data.table::fread(SyntheFile, colClasses = "character", data.table = F)

	LogFile <- list.files(file.path(L1,"WaterSample"),pattern = "water_sample_log", full.names = T)

	if (length(LogFile) == 0) {
		stop("No water_sample_log found in: ",file.path(L1,"WaterSample"))
	} else if (length(LogFile) > 1) {
		stop("Multiple water_sample_log found in: ",file.path(L1,"WaterSample"),
			"\n",str_c(LogFile, collapse = "\n"))
	}

	LabLog <- data.table::fread(LogFile, colClasses = "character", data.table = F) %>% mutate(Zsample=as.numeric(Zsample))

	handyParams <- list.dirs(file.path(L1,"WaterSample"), recursive = F ,full.names = F)


# SPM ---------------------------------------------------------------------

	if (any(str_detect(params, "SPM")) && any(str_detect(handyParams, "^SPM$"))) {

		SPMfile <- list.files(file.path(L1,"WaterSample","SPM"), pattern = ".*SPM.*\\.csv$", full.names = T)

		if (length(SPMfile) > 1) {
			stop("Multiple SPM files foud: \n", SPMfile)
		} else if (length(SPMfile) == 0) {
			stop("No SPM file found, name must contain 'SPM' and end with '.csv'")
		} else {
			SPM_data <- data.table::fread(SPMfile, colClasses = "character", data.table = F)
		}

		# units detector to be extended
		handyUnit <- paste0("(?<=_)(mg|ml)$")

		SPMunits <- unique(na.omit(str_extract(names(SPM_data), handyUnit)))

		if (length(SPMunits) < 2) {
			weightUnit <- readline("missing units to calculate SPM, enter a unit for weight :")
			volUnit <- readline("missing units to calculate SPM, enter a unit for volume :")
		} else if (length(SPMunits) > 2) {
			stop("Too many units detected: ",SPMunits)
		} else {
			weightUnit <- na.omit(str_extract(SPMunits, "mg"))
			volUnit <- na.omit(str_extract(SPMunits, "ml"))
			names(SPM_data) <- str_remove(names(SPM_data), "_(mg|ml)")
		}

		if (all(weightUnit != c("mg"))) {
			stop(weightUnit,", not implemented")
		}
		if (all(volUnit != c("ml"))) {
			stop(volUnit,", not implemented")
		}

		# Calculate SPM, PIM and POM for replicates, in [mg.L-1]
		SPM_tbl <- SPM_data %>% select(SID, Replicate, V, Wbase, Wdry, Wburn) %>%
			mutate(V = as.numeric(V), Wbase = as.numeric(Wbase), Wdry = as.numeric(Wdry), Wburn= as.numeric(Wburn) ) %>%
			mutate(SPM = ((Wdry - Wbase) / V)*1000,
				  PIM = SPM-(Wdry-Wburn),
				  POM = SPM-PIM) %>%
			# Should do a mutate_if to avoid warning caused by NA?
			mutate(SPM= as.numeric(sprintf(SPM, fmt = '%#.2f')),
				  PIM= as.numeric(sprintf(PIM, fmt = '%#.2f')),
				  POM= as.numeric(sprintf(POM, fmt = '%#.2f')))

		# QC request if no QC file are found in ProLog/SPM
		QCfile <- list.files(file.path(project,"ProLog", "SPM"), pattern="QC[[:graph:]]+csv", full.names = T)

		if (length(QCfile) == 1) {
			QCSPM <- data.table::fread(QCfile, colClasses = "character", data.table = F) %>% mutate(QC=as.numeric(QC))

			SPM_tbl <- SPM_tbl %>% left_join(QCSPM %>% select(SID,Replicate,QC,Comment), by=c("SID","Replicate"))

			message()

			QCrequest <- askYesNo("SPM QC file found, update QC report ?")
			if (!is.na(QCrequest) && QCrequest) {
				# will update QC report with QC info and overwrite L3 SPM
				qc_spm(project, mission, LabLog, Synthesis, SPM_tbl, QCSPM)
			}

			SPM_tbl <- SPM_tbl %>% filter(QC>0)

		} else if (length(QCfile) > 1) {
			stop("more than one QC csv file found for SPM")

		} else if (purrr::is_empty(QCfile)) {
			message("\nNo QC files found, producing now ")
			qc_spm(project, mission, LabLog, Synthesis, SPM_tbl)
			# QCrequest <- readline(prompt = "")
			# if (QCrequest == "y") {
			# 	qc_spm(project, mission, LabLog, SPM_tbl)
			# } else {
			# 	message("No QC filter will be applied and all data will be used")
			# }
		}

		SPM_nest <- SPM_tbl %>% select(SID, SPM, PIM, POM) %>% group_by(SID) %>% tidyr::nest()

		SPMs <- SPM_nest %>% mutate(SPM= purrr::map_dbl(.x = data, ~ mean(.x$SPM, na.rm = T)),
							   PIM= purrr::map_dbl(.x = data, ~ mean(.x$PIM, na.rm = T)),
							   POM= purrr::map_dbl(.x = data, ~ mean(.x$POM, na.rm = T)))

		SPMs <- SPMs %>% mutate(SPM= as.numeric(sprintf(SPM, fmt = '%#.2f')),
						    PIM= as.numeric(sprintf(PIM, fmt = '%#.2f')),
						    POM= as.numeric(sprintf(POM, fmt = '%#.2f'))) %>%
			select(!data)

		# SPMs_table <- data.table::data.table(SPM_tbl)
		# SPMs_stats <- data.table::setDT(SPMs_table)[,list(SPM_Mean=mean(SPM),
		# 							  SPM_Max=max(SPM),
		# 							  SPM_Min=min(SPM),
		# 							  SPM_Median=as.numeric(median(SPM)),
		# 							  SPM_Std=sd(SPM),
		# 							  PIM_Mean=mean(PIM),
		# 							  PIM_Max=max(PIM),
		# 							  PIM_Min=min(PIM),
		# 							  PIM_Median=as.numeric(median(PIM)),
		# 							  PIM_Std=sd(PIM),
		# 							  POM_Mean=mean(POM),
		# 							  POM_Max=max(POM),
		# 							  POM_Min=min(POM),
		# 							  POM_Median=as.numeric(median(POM)),
		# 							  POM_Std=sd(POM)), by=SID]

		# Significant digit formating this way could be great !
		# SPMs_Stats %>% group_by(SID) %>% tidyr::nest() %>%
		# 	purrr::map_dbl(. = data , ~ as.numeric(sprintf(., fmt = '%#.2f')))

		lighthouse::check_l3(project, L3, set="SPM")
		write.csv(SPMs,
					  file = file.path(L3,"SPM",
					  			  paste0("SPMs_DB_",Sys.Date(),"_",mission,".csv")), row.names = F)

		# readr::write_csv(SPMs_stats,
		# 			  path = file.path(L3,"SPM",
		# 			  			  paste0("SPMs_stats_DB_",Sys.Date(),"_",mission,".csv")))

		write.csv(SPM_tbl,
					  file = file.path(L3,"SPM",
					  			  paste0("SPMs_table_DB_",Sys.Date(),"_",mission,".csv")), row.names = F)


	} else if (any(str_detect(params, "SPM")) && !any(str_detect(handyParams, "^SPM$"))) {
		warning("SPM data base requested but no 'SPM' folder found under WaterSample, skiping.\n",immediate. = T)
	}


# Ag ----------------------------------------------------------------------



	if (any(str_detect(params, "Ag")) && any(str_detect(handyParams, "^Ag$"))) {
		DataFolder <- file.path(L1,"WaterSample","Ag","RData")
		if (!dir.exists(DataFolder)) {stop("folder ", DataFolder," does not exist")}

		DataFiles <- list.files(DataFolder, pattern = ".RData$", full.names = T)

		Ag_table <- data.frame()

		for (i in seq_along(DataFiles)){
			load(DataFiles[i])

			ID <- str_extract(Ag$ID, "[:digit:]+")
			wl <- Ag$Lambda
			Agdata <- Ag$Ag.fitted

			temp <- data.frame(ID,t(Agdata))
			names(temp) <- c("SID",paste0("Ag_",wl))

			if(check_wl_consistency(Ag_table, temp, DataFiles, i)) {
				tryCatch(Ag_table <- bind_rows(Ag_table, temp))
			} else {
				Ag_table <- bind_rows(Ag_table, temp)
			}

		}

		lighthouse::check_l3(project, L3, set="Ag")
		write.csv(Ag_table,
					  file = file.path(L3,"Ag",
					  			  paste0("Ag_DB_",Sys.Date(),"_",mission,".csv")), row.names = F)

	} else if (any(str_detect(params, "Ag")) && !any(str_detect(handyParams, "^Ag$"))) {
		warning("Ag data base requested but no 'Ag' folder found under WaterSample, skiping.\n", immediate. = T)
	}


# Ap ----------------------------------------------------------------------

	if (any(str_detect(params, "Ap")) && any(str_detect(handyParams, "^Ap$"))) {

		DataFolder <- file.path(L1,"WaterSample","Ap","RData")
		if (!dir.exists(DataFolder)) {stop("folder ", DataFolder," does not exist")}

		DataFiles <- list.files(DataFolder, pattern = "[[:digit:]]{3}.RData$", full.names = T)

		# declare empty df to be filed by rowbind
		Ap_table <- data.frame()
		Anap_table <- data.frame()
		Aph_table <- data.frame()

		for (i in seq_along(DataFiles)){
			load(DataFiles[i])

			# Ap
			Ap_data <- A$Ap$Ap.Stramski.mean

			if (is.null(Ap_data)) {
				warning("No Ap data in: ", DataFiles[i],"\n")
			} else {
				ID <- A$Ap$ID
				wl <- A$Ap$Lambda

				temp <- data.frame(ID,t(Ap_data))
				names(temp) <- c("SID",paste0("Ap_",wl))

				if(check_wl_consistency(Ap_table, temp, DataFiles, i)) {
					tryCatch(Ap_table <- bind_rows(Ap_table, temp))
				} else {
					Ap_table <- bind_rows(Ap_table, temp)
				}
			}

			# Anap
			Anap_data <- A$Anap$Ap.Stramski.mean

			if (is.null(Anap_data)) {
				warning("No Anap data in: ", DataFiles[i],"\n")
			} else {
				ID <- A$Anap$ID
				wl <- A$Anap$Lambda

				temp <- data.frame(ID,t(Anap_data))
				names(temp) <- c("SID",paste0("Anap_",wl))

				if(check_wl_consistency(Anap_table, temp, DataFiles, i)) {
					tryCatch(Anap_table <- bind_rows(Anap_table, temp))
				} else {
					Anap_table <- bind_rows(Anap_table, temp)
				}
			}



			# Aph no ID and Lambda, take the last available (i.e, Ap or Anap)
			# ID <- A$Aph.Stramski$ID
			# wl <- A$Aph.Stramski$Lambda

			Aph_data <- A$Aph.Stramski$Aph

			if (is.null(Aph_data)) {
				warning("No Aph data in: ", DataFiles[i],"\n")
			} else {
				temp <- data.frame(ID,t(Aph_data))
				names(temp) <- c("SID",paste0("Aph_",wl))

				if(check_wl_consistency(Aph_data, temp, DataFiles, i)) {
					tryCatch(Aph_table <- bind_rows(Aph_table, temp))
				} else {
					Aph_table <- bind_rows(Aph_table, temp)
				}
			}
		}

		lighthouse::check_l3(project, L3, set="Ap")
		write.csv(Ap_table,
					  file = file.path(L3,"Ap",
					  			  paste0("Ap_DB_",Sys.Date(),"_",mission,".csv")), row.names = F)

		write.csv(Anap_table,
					  file = file.path(L3,"Ap",
					  			  paste0("Anap_DB_",Sys.Date(),"_",mission,".csv")), row.names = F)

		write.csv(Aph_table,
					  file = file.path(L3,"Ap",
					  			  paste0("Aph_DB_",Sys.Date(),"_",mission,".csv")), row.names = F)


	} else if (any(str_detect(params, "Ap")) && !any(str_detect(handyParams, "^Ap$"))) {
		warning("Ap data base requested but no 'Ap' folder found under WaterSample, skiping.\n",immediate. = T)
	}
}



#boot_median <- function(d,i) {median(d[i])}
#
# test <- SPMstats %>% mutate(booted = purrr::map(.x= data,
# 									   ~ boot::boot(data = .x$SPM,
# 									   		   statistic = boot_median,
# 									   		   R = 1000,
# 									   		   stype = "i")))
