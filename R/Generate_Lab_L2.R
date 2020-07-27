#' @export
#' @import dplyr
#' @import lubridate
#' @import stringr
#'

# Function to generate Lab L2 from L1 and Log table

#library(data.table)
#library(dplyr)
#library(stringr)
#library(lubridate)

# Think about how to define absolute project path in a user point of view ?
# some generic settings to define
# User should be responsible to set project directory as working directory (setwd()) before calling function
# Or manually enter desired project directory

#project <- "/home/raphael/TEST"
#setwd(project)

Generate.Lab.L2 <- function(project, Var = c("Ag","Ap")) {

	# Set L1 and L2 absolute path
	L1 = file.path(project,"L1","Lab")
	L2 = file.path(project,"L2")

	# Check project before doing any manipulation
	CheckList <- Check.project(project,L1,L2,param="Lab")

	if (CheckList["Proot"][[1]] == F) {stop("project path is not set at a project root folder")}
	if (CheckList["L2exists"][[1]] == T) {
		stop("L2 structure for Lab is alredy present. You better be sure of what your doing ...\n
			take care of this by yourself !")}

	# Read Lab sampling log
	LabLog <- data.table::fread(list.files(path = L1, pattern = "Lab_Log", full.names = T),
				colClasses = c(ID = "character"),
				data.table = F)

	if (any(str_detect(Var, "Ag"))){

		# Read Ag_Log file
		#AgLog <- fread(file.path(getwd(),list.files(pattern = "Ag_log", recursive = T)),
					#colClasses = c(ID = "character"),
					#data.table = F)

		# Match sample ID, station name along with L1path
		AgFiles <- list.files(path = file.path(L1,"Ag","RData"), "*.RData", full.names = T)

		AgLog <- LabLog %>% mutate(L1path = AgFiles)

		# Create corresponding L2 path

		if (any(names(AgLog) == "Boat")) {
			AgLog <- AgLog %>% mutate(L2path = file.path(L2,
												paste0(gsub("-", "", as.character(lubridate::date(Date))),"_Station",Station),
												paste0("Lab_",Boat),paste0("Ag_",ID,"_",Depth,"Z",".RData")))
		} else {
			AgLog <- AgLog %>% mutate(L2path = file.path(L2,
												paste0(gsub("-", "", as.character(lubridate::date(Date))),"_Station",Station),
												"Lab",paste0("Ag_",ID,"_",Depth,"Z",".RData")))
		}

		L2Ag <- unique(str_extract(AgLog$L2path, ".*(?=/[:alpha:]{2,5}_[:digit:]{3}.*.RData)"))

		for (i in L2Ag){
			dir.create(i, recursive = T)
		}

		file.copy(AgLog$L1path, AgLog$L2path, recursive = F)
	}
	if (any(str_detect(Var, "Ap"))) {
		# Match sample ID, station name along with L1path
		ApFiles <- list.files(path = file.path(L1,"Ap_NAp","RData"), "[[:digit:]]{3}.RData", full.names = T)

		ApLog <- LabLog %>% mutate(L1path = ApFiles)

		# Create corresponding L2 path

		if (any(names(ApLog) == "Boat")) {
			ApLog <- ApLog %>% mutate(L2path = file.path(L2,
												paste0(gsub("-", "", as.character(lubridate::date(Date))),"_Station",Station),
												paste0("Lab_",Boat),paste0("Ap_",ID,"_",Depth,"Z",".RData")))
		} else {
			ApLog <- ApLog %>% mutate(L2path = file.path(L2,
												paste0(gsub("-", "", as.character(lubridate::date(Date))),"_Station",Station),
												"Lab",paste0("Ap_",ID,"_",Depth,"Z",".RData")))
		}

		L2Ap <- unique(str_extract(ApLog$L2path, ".*(?=/[:alpha:]{2,5}_[:digit:]{3}.*.RData)"))

		for (i in L2Ap){
			dir.create(i, recursive = T)
		}

		file.copy(ApLog$L1path, ApLog$L2path, recursive = F)
	}
}
