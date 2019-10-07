OpenCast <- function(){
  library(data.table)
  Casts_in_dir <- list.files(path = (getwd()), pattern = "CAST")
  print(Casts_in_dir)
  for (Cast in Casts_in_dir){
    id <- substring(Cast, regexpr("CAST_\\d{3}", Cast), regexpr("CAST_\\d{3}", Cast) +7)
    print(id)
    assign(id, fread(file = Cast), envir = .GlobalEnv)
  }
}

OpenGPS <- function(){
  library(data.table)
  GPS_ind_dir <- list.files(path = getwd(), pattern = "GPS")
  print(GPS_ind_dir)
  assign("GPS_data", fread(file  = GPS_ind_dir), envir = .GlobalEnv)
}
