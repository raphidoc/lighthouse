Start.Tools.Log <- function(project){

	if (!dir.exists(file.path(project,"LighthouseLog"))) {
		dir.create(file.path(project,"LighthouseLog"))
	}
}
