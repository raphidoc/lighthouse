# .onLoad <- function(lib,pkg) {
# 	if (file.exists(file.path(lib,pkg,"data","pkgEnv"))) {
# 		load(file.path(lib,pkg,"data","pkgEnv"))
#
# 		if (!file.exists(pkgEnv$dataPath)) {
# 			dataPath <- readline("Write absolute path of your Data folder where your projects can be found: ")
# 			if (file.exists(dataPath)) {
# 				pkgEnv$dataPath <- dataPath
# 				message("Data path is set to: ", pkgEnv$dataPath)
# 				save(pkgEnv, file = file.path(lib,pkg,"data","pkgEnv"))
# 			} else {message("Data path does not exists")}
# 		} else {
# 			dataPath <- pkgEnv$dataPath
# 			YN <- readline(paste0("Do you want to change Data path: '", dataPath,"' ? y/n "))
# 			if (YN == "y") {
# 				dataPath <- readLine("Write absolute path of your Data folder where your projects can be found: ")
# 				if (file.exists(dataPath)) {
# 					pkgEnv$dataPath <- dataPath
# 					message("Data path is set to: ", pkgEnv$dataPath)
# 					save(pkgEnv, file = file.path(lib,pkg,"data","pkgEnv"))
# 				} else {message("Data path does not exists")}
# 			}
# 		}
# 	} else {
# 		assign("pkgEnv", new.env())
#
# 		dataPath <- readline("Write absolute path of your Data folder where your projects can be found: ")
# 		if (file.exists(dataPath)) {
# 			pkgEnv$dataPath <- dataPath
# 			message("Data path is set to: ", pkgEnv$dataPath)
# 			save(pkgEnv, file = file.path(lib,pkg,"data","pkgEnv"))
# 		}else {message("Data path does not exists")}
# 	}
#
# 	if (exists("dataPath")) {
# 		projects <- list.files(get, full.names = T)
#
# 		checkList <- c()
# 		for (project in projects) {
# 			checkList <- append(checkList,
# 							check_project(project, L1=file.path(project,"L1"))["Proot"][[1]])
# 		}
# 		message("Available projects are: \n",stringr::str_c(projects[checkList], sep = "\n"))
# 	}
#
# }
