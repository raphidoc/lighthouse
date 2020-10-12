#' @description check_project role is to make sure the code wont do any harm to
#' any data alredy processed, it implment a protective approach by checking
#' that data is not alredy present in projected L2 structure for a specific
#' parameter. This function is to be extended for L3 processing level
#' to ensure uniqueness of data bases.
#'
#' @inheritParams project
#' @inheritParams L1
#' @inheritParams L2
#' @param set The set for which to check.
#' @export

check_project <- function(project, L1="", L2="", set="") {

	Pobjects <- list.files(project, recursive = F, full.names = T)

	# Check if project path is trully a project folder root
	if (dir.exists(L1)) {

		Proot <- T
	} else { Proot <- F }

	if (file.exists(grep("data_synthesis", Pobjects, value = T))) {

		Proot <- T
	} else { Proot <- F }

	# Check if L2 exist (for set) and is not empty
	if (dir.exists(L2) &&
	    length(dir(L2, pattern = set, recursive = T ,include.dirs = T)) != 0) {

		L2exist <- T
	} else { L2exist <- F
		}

	list("Proot" = Proot, "L2exists"=L2exist)

}
