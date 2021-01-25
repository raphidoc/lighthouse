.onLoad <- function(lib, pkg) {
	Sys.setenv(R_lighthouse_rmd_dir = file.path(lib, pkg, "rmd"))
}
