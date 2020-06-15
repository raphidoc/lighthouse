#Function to change remove.cops.dat to select.cops.dat
#library(tidyverse)

RemToSelect <- function(){
  dirs <- scan("directories.for.cops.dat", what = "character", comment.char = "#")
  for (i in seq_along(dirs)) {
    if(file.exists(file.path(dirs[i],"select.cops.dat"))){
      remove.file <- file.path(dirs[i],"select.cops.dat")
      remove.tab <- read.table(remove.file, header = FALSE, colClasses ="character", sep = ";")
      select.tab <- remove.tab %>% mutate(V3 = "Rrs.0p.linear", V4 = "NA")
      write.table(select.tab, file = file.path(dirs[i],"select.cops.dat"), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = ";")
    }
  }
}
