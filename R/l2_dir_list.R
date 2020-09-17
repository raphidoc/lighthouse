#' Fuction to create directories.for.*.dat requierd by Cops and Riops packages
#'
#' @param path is the L2 folder path (string) where uniquely identifie stations can be found.
#' @param what is a character vector of what to search for, default c("COPS","IOP")
#' @param boat is a character vector of to filter by boat, default c("")
#' @param upInDir logical indicating if update of file path append also in L2 subdirectories.
#'
#' @return nothing
#'
#' @export
#'
#'
#'

l2_dir_list <- function(path=getwd(), what=c("COPS","IOP"), boat=c(""), upInDir=T){

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
				#message(paste(what[i], " directory :", str_extract(dirs[i2], "(?<=Station).*(?=/)")))
			}
			message(paste(what[i], " = ", length(dirs)))
		}
		writeLines(dirs, fileConn)
		close(fileConn)
	}
}
