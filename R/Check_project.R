

Check.project <- function(project, L1, L2, param = ""){

	Pobjects <- list.files(project, recursive = F, full.names = T)

	# Check if project path is trully a project folder root
	if (exists("L1") & dir.exists(L1)) {

		Proot <- T
	} else { Proot <- F }

	if (file.exists(grep("data_synthesis|Data_Synthesis", Pobjects, value = T))) {

		Proot <- T
	} else { Proot <- F }

	# Check if L2 exist (for param) and is not empty
	if (exists(c("L2","param")) & dir.exists(L2) &
	    length(dir(L2, pattern = param, recursive = T ,include.dirs = T)) != 0) {

		L2exist <- T
	}  else { L2exist <- F }

	list("Proot" = Proot, "L2exists"=L2exist)

}
