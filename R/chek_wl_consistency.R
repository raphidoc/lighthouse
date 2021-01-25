#' @name check_wl_consistency
#' @title check that the wavelength between data files are the same, if not throw a warning and use bind_rows.
#'
#' @author Raphael Mabit

check_wl_consistency <- function(df,temp,DataFiles,i) {
	if (length(names(df)) != 0 && (length(names(df)) != length(names(temp)) ||
							 names(df) != names(temp))) {
		warning("Different wavelengths detected between:\n",
			   DataFiles[i-1],"\nand\n",DataFiles[i],"\n\n Trying bind_rows on: ",DataFiles[i],"\n", immediate. = T)
		T
	} else {F}

}
