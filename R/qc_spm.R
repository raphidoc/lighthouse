#' @title qc_spm
#' @name qc_spm
#' @author Raphael Mabit
#'
#' @description Quality Check SPM based on SPM, Anap, Bbp relationship (minimum requirement).
#' Optional: CTD, will be add to output QC plots if present, Other variable could be added as needed.
#' Data are searched for in L3.
#'
#' @inheritParams l3_water_sample_gen
#'
#' @import dplyr
#' @import stringr

qc_spm <- function(project, mission, LabLog, Synthesis, SPM_tbl, QCSPM=NULL) {

	qc_dir <- file.path(project,"ProLog", "SPM")

	if (is.null(QCSPM)) {
		SPM_tbl <- SPM_tbl %>% mutate(QC = 1, Comment ="")

		dir.create(qc_dir, recursive = T, showWarnings = F)

		# Create QC log file
		QClog <- SPM_tbl %>% select(SID,Replicate,QC,Comment)

		write.csv(QClog, file = file.path(qc_dir, paste0("QC_SPM_",mission,".csv")), row.names = F)
	}

# Bbp VS SPM --------------------------------------------------------------

	BB3file <- list.files(file.path(project,"L3","IOP"), recursive = T, pattern = "BB3_DB", full.names = T)
	BB9file <- list.files(file.path(project,"L3","IOP"), recursive = T, pattern = "BB9_DB", full.names = T)
	HS6file <- list.files(file.path(project,"L3","IOP"), recursive = T, pattern = "HS6_DB", full.names = T)

	if (any(!purrr::is_empty(BB3file),!purrr::is_empty(BB9file),!purrr::is_empty(HS6file))) {
		if (!purrr::is_empty(BB3file)) {
			BB3_Bbp <- read.csv(BB3file, colClasses = "character")
			BB3_Bbp <- BB3_Bbp[str_detect(names(BB3_Bbp), "ID|Depth|Bbp_")]
		} else {
			BB3_Bbp <- NULL
		}
		if (!purrr::is_empty(BB9file)){
			BB9_Bbp <- read.csv(BB9file, colClasses = "character")
			BB9_Bbp <- BB9_Bbp[str_detect(names(BB9_Bbp), "ID|Depth|Bbp_")]
		} else {
			BB9_Bbp <- NULL
		}
		if (!purrr::is_empty(HS6file)){
			HS6_Bbp <- read.csv(HS6file, colClasses = "character")
			HS6_Bbp <- HS6_Bbp[str_detect(names(HS6_Bbp), "ID|Depth|Bbp_")]
		} else {
			HS6_Bbp <- NULL
		}

		Bbp_tbl <- bind_rows(HS6_Bbp,BB9_Bbp,BB3_Bbp) %>% mutate_at(vars(!matches(c("ID"))),as.numeric)

		# Bbp_tbl %>% filter(across(matches("Bbp_"), ~ any(!is.na(.x))))
		# I used the supersede way, the above should be the new one
		Bbp_tbl <- Bbp_tbl %>% filter_at(vars(matches("Bbp_")), any_vars(!is.na(.)))

		Bbp_tbl <- SPM_tbl %>% left_join(LabLog %>% select(SID, ID, Depth), by="SID") %>%
			select(SID, ID, Replicate, Depth) %>%
			left_join(Bbp_tbl, by=c("ID")) %>% rename(Z_SPM = Depth.x, Z_Bbp = Depth.y)

		# Keep closest depth between SPM (discrete) and Bbp (continuous)
		Bbp <- Bbp_tbl %>% filter(near(Z_SPM, Z_Bbp, tol = 2)) %>% group_by(SID) %>%
			filter(abs(Z_SPM - Z_Bbp) == min(abs(Z_SPM - Z_Bbp)))

		# Add depth difference between Z_SPM and Z_Bbp
		Bbp <- Bbp %>% mutate(Z_dif = abs(Z_SPM-Z_Bbp))

	} else {
		Bbp <- data.frame(ID=NA,SID=NA,Bbp_555=NA)
	}


# Anap vs SPM -------------------------------------------------------------

	Anapfile <- list.files(file.path(project,"L3","WaterSample"), recursive = T, pattern = "Anap_DB", full.names = T)

	if (!purrr::is_empty(Anapfile)) {
		Anap <- read.csv(Anapfile, colClasses = "character")
		Anap <- Anap[str_detect(names(Anap), "SID|440|500|532|550|600|620")] %>% mutate_at(vars(!matches(c("SID"))),as.numeric)

	} else {
		Anap <- data.frame(SID=NA,Anap_532=NA)
	}


# Ap vs SPM ---------------------------------------------------------------

	Apfile <- list.files(file.path(project,"L3","WaterSample"), recursive = T, pattern = "Ap_DB", full.names = T)

	if (!purrr::is_empty(Apfile)) {
		Ap <- read.csv(Apfile, colClasses = "character")
		Ap <- Ap[str_detect(names(Ap), "SID|440|500|532|550|600|620")] %>% mutate_at(vars(!matches(c("SID"))),as.numeric)

	} else {
		Ap <- data.frame(SID=NA,Ap_532=NA)
	}

# Aph vs SPM --------------------------------------------------------------

	Aphfile <- list.files(file.path(project,"L3","WaterSample"), recursive = T, pattern = "Aph_DB", full.names = T)

	if (!purrr::is_empty(Aphfile)) {
		Aph <- read.csv(Aphfile, colClasses = "character")
		Aph <- Aph[str_detect(names(Aph), "SID|440|500|532|550|600|620")] %>% mutate_at(vars(!matches(c("SID"))),as.numeric)

	} else {
		Aph <- data.frame(SID=NA,Aph_532=NA)
	}

# CTD ---------------------------------------------------------------------

	CTDfile <- list.files(file.path(project,"L3","IOP"), recursive = T, pattern = "CTD_DB", full.names = T)

	if (!purrr::is_empty(CTDfile)) {
		CTD <- read.csv(CTDfile, colClasses = "character")
		CTD <- CTD %>% left_join(LabLog %>% select(ID,SID), by="ID") %>% mutate_at(vars(!matches(c("ID","SID"))),as.numeric)

	} else {
		CTD <- data.frame(ID=NA,SID=NA,Depth=NA,Temp=NA,PSU=NA)
	}


# HTML report -------------------------------------------------------------

	# Add Statistics per station
	Station_stats <- SPM_tbl %>% group_by(SID) %>% tidyr::nest() %>% mutate(
		CV_SPM = purrr::map_dbl(.x = data, ~ sd(.x$SPM , na.rm = T),
		CV_PIM = purrr::map_dbl(.x = data, ~ sd(.x$PIM , na.rm = T)
	) %>% select(!data)

	write.csv(Station_stats, file = file.path(qc_dir, paste0("SPM_",mission,"_samples_stats.csv")), row.names = F)

	Station <- Synthesis %>% dplyr::select(ID,Station,Depth) %>% dplyr::rename(Z_Station = Depth)

	SPM <- SPM_tbl %>% dplyr::left_join(LabLog %>% select(SID, ID, Depth), by="SID") %>%
		dplyr::rename(Z_Sample = Depth) %>% dplyr::left_join(Station, by ="ID")

	SPM_datatable <- SPM %>% select(ID,SID,Station,Z_Station,Z_Sample,Replicate,QC,SPM,PIM,POM,V)

	GLOB <- SPM %>% left_join(Bbp %>% select(!ID), by=c("SID","Replicate")) %>%
		left_join(Anap, by="SID") %>% left_join(Ap, by="SID") %>% left_join(Aph, by="SID")

	report = paste0("QC_SPM_",Sys.Date(),"_",str_c(mission,collapse = "_"))

	rmarkdown::render(file.path(Sys.getenv("R_lighthouse_rmd_dir"), "QC_SPM.Rmd"),
				   output_dir = file.path(project,"ProLog", "SPM"),
				   output_file = report)

# QC files ----------------------------------------------------------------

	if (is.null(QCSPM)) {
		message("QC files produced, terminated")
		stop_quietly()
	} else {
		QCrequest <- askYesNo(msg = "QC report updated, write SPMs in L3 ?")
		if (!is.na(QCrequest) && !QCrequest) {
			# will update QC report with QC info and overwrite L3 SPM
			message("Terminated")
			stop_quietly()
		}
	}
}
