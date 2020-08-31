#' @export
#'
#'
#'

dirlist <- function(path=getwd(), what=c("COPS","IOP"), boat=c(""), upInDir=T){
	# fuction to create directories.for.*.dat
	# path is the path

	for (i in seq_along(what)) {
		if (file.exists(paste0("directories.for.",tolower(what[i]),".dat"))) {
			file.remove(paste0("directories.for.",tolower(what[i]),".dat"))
		}

		fileConn <- file(file.path(path,paste0("directories.for.",tolower(what[i]),".dat")))
		dirs <- grep(stringr::str_c("/",what[i],"(_[[:alpha:]]+)?$"),list.dirs(path), value = T)

		if (boat != "") {
			dirs <- dirs[str_detect(dirs, boat)]
		}

		if(upInDir==T){
			for (i2 in seq_along(dirs)) {
				write(dirs[i2], file = file.path(dirs, paste0("directories.for.",tolower(what[i]),".dat"))[i2])
				message(paste(what[i], " directory :", str_extract(dirs[i2], "(?<=Station).*(?=/)")))
			}
			message(paste(what[i], " = ", length(dirs)))
		}
		writeLines(dirs, fileConn)
		close(fileConn)
	}
}
