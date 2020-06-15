#Modify Info file based on directories.for.Cops (to be be changed, as sys.getenv)
library(data.table)


Info.Manager <- function(chfield= "", to= "", mindate, maxdate){
  mindate <- NULL
  maxdate <- NULL
  chfield <- as.character(chfield)
  to <- as.character(to)
  print(chfield)
  print(to)

  header.info.file <- paste(Sys.getenv("R_COPS_DATA_DIR"), "info.header.dat", sep = "/")
  dirdats <- scan(file = "directories.for.cops.dat", "", sep = "\n", comment.char = "#")

  for(dirdat in dirdats) {
    if(!file.exists(dirdat)) {
        cat(dirdat, "does not exist")
        next()
    }
    print(paste0("PROCESSING DIRECTORY:", dirdat))

    #look if absorption file empty
    if(chfield=="chl" && to == "0"){
      abs.tab <- fread(file= paste(dirdat, "absorption.cops.dat", sep = "/"), header= T)
      if(abs.tab[[2]][1] == "a1"){
        print(paste(dirdat, "No absorption values", sep= "   "))
        next()
      }
    }
    info.file <- paste(dirdat, "info.cops.dat", sep = "/")
    info.tab <- read.table(info.file,
                           colClasses = c(
                             "character", "numeric", "numeric", "numeric",
                             "character", "character", "character", "character",
                             "character", "character", "character", "character"),
                           col.names = c("file", "lon", "lat", "chl", "timwin", "ssrm", "tiltm", "smoo", "b1", "b2", "b3", "b4"),
                           header = FALSE, fill = TRUE, sep = ";")

    info.tab[[chfield]] <- to
    print(paste(dirdat, "modified", sep = "   "))

    info.tab
    file.copy(from = header.info.file, to = info.file, overwrite = T)
    write.table(info.tab, file = info.file , append = T, quote = F, sep = ";", dec = ".",
                row.names = F, col.names = F)
  }
}
