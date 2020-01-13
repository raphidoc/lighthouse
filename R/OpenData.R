#Intended to be the basis of the futur workflow with Cops data:
#Autoffil of init.cops.dat (instruments(Ed0, EuZ, LuZ, EdZ), date/time format of casts and GPS, ...)
#and info.cops.dat (keep only descending point, adjust tilt?, remove surface layer?, Loess?)

library(data.table)

OpenCast <- function(path = ""){
  Casts_in_dir <- list.files(path, pattern = "CAST")
  print(Casts_in_dir)
  for (Cast in Casts_in_dir){
    id <- substring(Cast, regexpr("CAST_\\d{3}", Cast), regexpr("CAST_\\d{3}", Cast) +7)
    print(id)
    assign(id, fread(file = Cast), envir = .GlobalEnv)
  }
}

OpenGPS <- function(path = ""){
  GPS_ind_dir <- list.files(path, pattern = "GPS")
  print(GPS_ind_dir)
  assign("GPS_data", fread(file  = GPS_ind_dir), envir = .GlobalEnv)
}
