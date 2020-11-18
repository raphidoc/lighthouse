#' @title qc_spm
#' @name qc_spm
#' @author Raphael Mabit
#'
#' @description Quality Check SPM based on SPM, Bbp relationship (minimum requirement).
#' Optional: CTD, Anap will be add to output QC plots if present, Other variable could be added as needed.
#' Data are serached for in L3.
#'
#' @inheritParams l3_water_sample_gen
#'
#' @import dplyr
#' @import stringr
#'
#' @export

qc_spm <- function(project, mission, LabLog, SPM_tbl){


# SPM vs Bbp --------------------------------------------------------------

	BB3file <- list.files(file.path(project,"L3","IOP"), recursive = T, pattern = "BB3_DB", full.names = T)
	BB9file <- list.files(file.path(project,"L3","IOP"), recursive = T, pattern = "BB9_DB", full.names = T)
	HS6file <- list.files(file.path(project,"L3","IOP"), recursive = T, pattern = "HS6_DB", full.names = T)

	if (any(!purrr::is_empty(BB3file),!purrr::is_empty(BB9file),!purrr::is_empty(HS6file))) {
		if (!purrr::is_empty(BB3file)) {
			BB3_Bbp <- readr::read_csv(BB3file)
			BB3_Bbp <- BB3_Bbp[str_detect(names(BB3_Bbp), "ID|Depth|Bbp_")]
		} else {
			BB3_Bbp <- NULL
		}
		if (!purrr::is_empty(BB9file)){
			BB9_Bbp <- readr::read_csv(BB9file)
			BB9_Bbp <- BB9_Bbp[str_detect(names(BB9_Bbp), "ID|Depth|Bbp_")]
		} else {
			BB9_Bbp <- NULL
		}
		if (!purrr::is_empty(HS6file)){
			HS6_Bbp <- readr::read_csv(HS6file)
			HS6_Bbp <- HS6_Bbp[str_detect(names(HS6_Bbp), "ID|Depth|Bbp_")]
		} else {
			HS6_Bbp <- NULL
		}

		Bbp_tbl <- bind_rows(HS6_Bbp,BB9_Bbp,BB3_Bbp)

		# I used the supersede way, The above should be the new one
		# Bbp_tbl %>% filter(across(matches("Bbp_"), ~ any(!is.na(.x))))

		Bbp_tbl <- Bbp_tbl %>% filter_at(vars(matches("Bbp_")), any_vars(!is.na(.)))

		Bbp_tbl <- SPM_tbl %>% left_join(LabLog, by="SID") %>%
			select(SID, ID, Replicate, Depth, SPM, PIM, POM) %>%
			left_join(Bbp_tbl, by=c("ID")) %>% rename(Depth_SPM = Depth.x, Depth_Bbp = Depth.y)

		# Keep closest depth between SPM (discrete) and Bbp (continous)
		Bbp_SPM <- Bbp_tbl %>% filter(near(Depth_SPM, Depth_Bbp, tol = 2)) %>% group_by(SID) %>%
			filter(abs(Depth_SPM - Depth_Bbp) == min(abs(Depth_SPM - Depth_Bbp))) %>% ungroup()

		# Add depth difference between Depth_SPM and Depth_Bbp
		Bbp_SPM <- Bbp_SPM %>% mutate(Ddiff = abs(Depth_SPM-Depth_Bbp))

		require(rmarkdown)

		report = paste0("QC_SPM_",Sys.Date(),"_",str_c(mission,collapse = "_"),".Rmd")

		cat(paste0("---\ntitle: '<center>QC SPM for __",mission,"__ mission'\n",
				"author: ''\n",
				"header-includes:\n",
				"output:\n\x20html_document:\n\x20\x20toc: true\n\x20\x20toc_float: true\n\x20\x20toc_depth: 5\n\x20\x20number_sections: true\n---\n\n"),
				file=report, append = F)

		cat("<style>\n\ntable, td, th {\n\tborder: none;\n\tpadding-left: 1em;\n\tpadding-right: 1em;\n\tmargin-left: auto;\n\tmargin-right: auto;\n\tmargin-top: 1em;\n\tmargin-bottom: 1em;\n}\n\n</style>\n\n",
			   file=report, append = T)

		cat(paste0("```{r setup, include=FALSE, echo=TRUE, message=FALSE}\n",
				"require(dplyr)\n",
				"require(tidyr)\n",
				"require(ggplot2)\n",
				"require(plotly)\n",
				"require(stargazer)\n",
				"require(DT)\n",
				"require(crosstalk)\n",
				"```\n"), file = report, append = T)

		cat(paste0("<center><font size='5'> Generated with lighthouse package __version: ",packageVersion("lighthouse"),"__ \n  \n",
				"Date: __",Sys.time(),"__ GMT</font></center>\n"), file = report, append=T)

		cat("\n# SPM vs Bbp \n\n", file = report, append=T)
		cat(paste0("```{r,echo=FALSE, message=FALSE}\n",
				 "Bbp_SPM_sd <- SharedData$new(Bbp_SPM %>% select(SID,ID,Replicate,Ddiff,SPM,PIM,POM,Bbp_532), key = ~SID)\n",
				 "ggplotly(Bbp_SPM_sd %>% ggplot(aes(log(SPM), log(Bbp_532), group=Replicate, color=SID)) +\n",
				 "geom_point(alpha=1) + ylab('log(Bbp(532))[m-1]'))\n",
				 "datatable(Bbp_SPM_sd)\n",
				 "```\n"), file = report, append = T)

		dir.create(file.path(project,"ProLog", "SPM"), recursive = T, showWarnings = F)

		# Create QC log file
		QClog <- Bbp_SPM %>% select(ID,SID,Replicate,Depth_SPM,Depth_Bbp) %>%
			mutate(QC = 1,
				comment ="")

		readr::write_csv(QClog, file = file.path(project,"ProLog", "SPM",paste0("QC_SPM_",mission,".csv")))
	} else {
		stop("No Bbp found in L3, cannot produce QC files for SPM")
	}



# Other helpful variables -------------------------------------------------

	Anapfile <- list.files(file.path(project,"L3","WaterSample"), recursive = T, pattern = "Anap_DB", full.names = T)
	CTDfile <- list.files(file.path(project,"L3","IOP"), recursive = T, pattern = "CTD_DB", full.names = T)

	if (!purrr::is_empty(Anapfile)) {
		Anap <- readr::read_csv(Anapfile)
		Anap <- Anap[str_detect(names(Anap), "ID|440|500|532|550|600|620")]

		Anap_Bbp_SPM <- Bbp_SPM %>% left_join(Anap, by="SID")

		cat("\n# SPM vs Anap \n\n", file = report, append=T)
		cat(paste0("```{r,echo=FALSE, message=FALSE}\n",
				 "ggplotly(Anap_Bbp_SPM %>% ggplot(aes(log(SPM), log(Anap_532), group=Replicate, color=SID)) + geom_point(alpha=1))\n",
				 "```\n"), file = report, append = T)

		cat("\n# Bbp vs Anap \n\n", file = report, append=T)
		cat(paste0("```{r,echo=FALSE, message=FALSE}\n",
				 "ggplotly(Anap_Bbp_SPM %>% ggplot(aes(log(Bbp_532), log(Anap_620), group=Replicate, color=SID)) + geom_point(alpha=1))\n",
				 "```\n"), file = report, append = T)
	}

	if (!purrr::is_empty(CTDfile)) {
		CTD <- readr::read_csv(CTDfile)

		CTD_SID <- CTD %>% left_join(LabLog %>% select(ID,SID), by="ID")

		cat("\n# CTD profile \n\n", file = report, append=T)
		cat("\n## Temperature \n\n", file = report, append=T)
		cat(paste0("```{r,echo=FALSE, message=FALSE}\n",
				 "maxDepth <- max(CTD_SID['Depth'])\n",
				 "ggplotly(CTD_SID %>% ggplot(aes(y=Depth, group=ID, color=SID)) + \n",
				 "geom_path(aes(x=Temp)) + scale_y_reverse(breaks = seq.int(0,maxDepth,5)))\n",
				 "```\n"), file = report, append = T)

		cat("\n## PSU \n\n", file = report, append=T)
		cat(paste0("```{r,echo=FALSE, message=FALSE}\n",
				 "ggplotly(CTD_SID %>% ggplot(aes(y=Depth, group=ID, color=SID)) + \n",
				 "geom_path(aes(x=PSU)) + scale_y_reverse(breaks = seq.int(0,maxDepth,5)))\n",
				 "```\n"), file = report, append = T)
	}

	render(report, output_dir = file.path(project,"ProLog", "SPM"))
	file.remove(report)

	stop("QC files produced, Terminated")
}
