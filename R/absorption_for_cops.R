#' @export
#' @import dplyr
#' @import Riops
#' @import RspectroAbs
#' @import stringr

#Fuction wich match all Cops Station with avalible cdom and particular measured absorption
#Dependancies: function compute.discrete.aTOT.for.COPS from Rspectro abs library

#project <- "/home/raphael/TEST"
#setwd(project)

absorption_for_cops <- function(project){

  # Initiate log for tool report,  append new section
  #report <- file(file.path(project,"Absorption_for_COPS.log"))
  #cat(paste0("\nSTART:",Sys.time(),"\n"), file = report, append = T)

  L2 <- file.path(project, "L2")

  Pcops = grep("/COPS(_[[:alpha:]]+)?$",list.dirs(L2), value = T)

  # Check if boat information is present
  if (any(str_detect(Pcops,"(?<=/COPS)_[:alnum:]+$"))) {

  	COPSframe <- data.frame(Pcops)
  	COPSframe <- COPSframe %>%
  		mutate(Station = paste0(str_extract(Pcops, "(?<=Station)[[:alnum:]-\\.]+(?=/)"),
  							   str_extract(Pcops,"(?<=/COPS)_[:alnum:]+$")))

  	PAg = list.files(L2, pattern = "Ag", recursive = T, full.names = T)

  	PAp = list.files(L2, pattern = "Ap", recursive = T, full.names = T)

  	Aframe <- data.frame(PAg, PAp)
  	Aframe <- Aframe %>%
  		mutate(Station = paste0(str_extract(PAg, "(?<=Station)[[:alnum:]-\\.]+(?=/)"),
  							   str_extract(PAg,"(?<=/Lab)_[:alnum:]+(?=/)")),
  			  Depth = str_extract(PAg, "(?<=_)[\\d\\.]+(?=Z)"))

  	Aframe <- Aframe %>% group_by(Station) %>% filter(Depth == min(Depth)) %>% ungroup()

  	Batch <- COPSframe %>% inner_join(Aframe, by="Station")

  } else {

  COPSframe <- data.frame(Pcops)
  COPSframe <- COPSframe %>% mutate(Station = str_extract(Pcops, "(?<=Station)[[:alnum:]-\\.]+(?=/)"))

  PAg = list.files(L2, pattern = "Ag", recursive = T, full.names = T)

  PAp = list.files(L2, pattern = "Ap", recursive = T, full.names = T)

  Aframe <- data.frame(PAg, PAp)
  Aframe <- Aframe %>% mutate(Station = str_extract(PAg, "(?<=Station)[[:alnum:]-\\.]+(?=/)"),
  					   Depth= str_extract(PAg, "(?<=_)[\\d\\.]+(?=Z)"))

  Aframe <- Aframe %>% group_by(Station) %>% filter(Depth == min(Depth)) %>% ungroup()

  Batch <- COPSframe %>% inner_join(Aframe, by="Station")
  }

  for (i in 1:length(Batch$Station)) {
  	print(as.character(Batch$Pcops[i]))
  	print(as.character(Batch$PAg[i]))
  	print(as.character(Batch$PAp[i]))

  	tryCatch(
  	  expr = {
  	    RspectroAbs::compute.discrete.aTOT.for.COPS(cops.path = as.character(Batch$Pcops[i]),
  	                                   ag.RData = as.character(Batch$PAg[i]),
  	                                   ap.RData = as.character(Batch$PAp[i]))
  	    message("Succes \\o/ \n")
  	  },
  	  error = function(e){
  	    message("error:")
  	    message(e,"\n")
  	  },
  	  warning = function(w){
  	    message("warning:")
  	    message(w,"\n")
  	    }
  	)
  }
  #cat(paste0("END"), file = report , append = T)
}

