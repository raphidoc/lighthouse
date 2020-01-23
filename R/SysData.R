init.file <- scan(paste0(Sys.getenv("R_COPS_DATA_DIR"), "/init.parameters.dat"), what = "", sep = "\n")
info.header <- scan(paste0(Sys.getenv("R_COPS_DATA_DIR"), "/info.header.dat"), what = "", sep = "\n")

use_data(init.file, info.header, internal = T)
