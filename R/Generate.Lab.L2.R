# Function to generate Lab L2 from L1 and Log table

#library(data.table)
#library(dplyr)
#library(stringr)
#library(lubridate)

# Think about how to define absolute project path in a user point of view ?
# some generic settings to define
# User should be responsible to set project directory as working directory (setwd()) before calling function
# Or manually enter desired project directory

project <- "/home/raphael/TEST"
setwd(project)

Generate.Lab.L2 <- function(project = getwd(),
					   Var = c("CDOM","Anap")) {
	setwd(project)

	# Set L1 and L2 absolute path
	L1 = file.path(project,"L1","Lab")
	L2 = file.path(project,"L2")

	if (any(str_detect(Var, "CDOM"))){

		# Read Ag_Log file
		AgLog <- fread(file.path(getwd(),list.files(pattern = "Ag_log", recursive = T)),
					colClasses = c(ID = "character"),
					data.table = F)

		# Match sample ID, station name along with L1path
		AgFiles <- list.files(path = file.path(L1,"CDOM","RData"), "*.RData", full.names = T)

		AgLog <- AgLog %>% mutate(L1path = AgFiles)

		# Create corresponding L2 path

		if (any(names(AgLog) == "Boat")) {
			AgLog <- AgLog %>% mutate(L2path = file.path(L2,
												paste0(gsub("-", "", as.character(lubridate::date(Date))),"_Station",Station),
												Boat,"Lab",paste0("Ag_",ID,"_",Depth,"Z",".RData")))
		} else {
			AgLog <- AgLog %>% mutate(L2path = file.path(L2,
												paste0(gsub("-", "", as.character(lubridate::date(Date))),"_Station",Station),
												"Lab",paste0("Ag_",ID,"_",Depth,"Z",".RData")))
		}

		L2CDOM <- unique(str_extract(AgLog$L2path, ".*(?=/[:alpha:]{2,5}_[:digit:]{3}.*.RData)"))

		for (i in L2CDOM){
			dir.create(i, recursive = T)
		}

		file.copy(AgLog$L1path, AgLog$L2path, recursive = F)
	}
	if (any(str_detect(Var, "CDOM"))) {

	}
}
