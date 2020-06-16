DirDat <- function(path=getwd(), what=c("COPS","IOP"), upInDir=F){
	# fuction to create directories.for.*.dat

	for (i in seq_along(what)) {
		file.remove(paste0("directories.for.",tolower(what[i]),".dat"))
		fileConn <- file(paste0("directories.for.",tolower(what[i]),".dat"))
		dirs <- grep(stringr::str_c("/",what[i],"$"),list.dirs(path), value = T)
		if(upInDir==T){
			for (i2 in seq_along(dirs)) {
				write(dirs[i2], file = file.path(dirs, paste0("directories.for.",tolower(what[i]),".dat"))[i2])
			}
		}
		writeLines(dirs, fileConn)
		close(fileConn)
	}
}
