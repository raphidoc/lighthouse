#' @author Raphael Mabit
#' @description Check if databases are present for a specific set in L3.
#' If so the function will create an archive to ensure no data is accidentally erased
#' @export

check_l3 <- function(project,L3,set) {

  # Check if L3 exist (for set) and is not empty, if not create an archive
  if (exists("L3") && exists("set") && dir.exists(file.path(L3,set)) &&
      length(list.files(file.path(L3,set))) != 0) {

    message("L3/",set," dir exist and is not empty, creating an archive.")
    db <- list.files(file.path(L3,set), pattern = set)
    db <- db[stringr::str_detect(db, "\\.tar", negate = T)]
    # change directory to create proper archive
    setwd(file.path(L3,set))
    tar(tarfile = paste0(set,"_archive_",Sys.Date(),".tar"), files=db)

  } else if (!dir.exists(file.path(L3,set))) {
    dir.create(file.path(L3,set), recursive = T)
  }
  setwd(project)
}
