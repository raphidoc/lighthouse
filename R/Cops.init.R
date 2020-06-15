#function to create and auto fill init.file and info.file

library(data.table)
library(tidyverse)

Cops.init.R <- function(dirdats){

	#str_subset(init.file, "#", negate = T)

	dirdats <- scan(file = "directories.for.COPS.dat", "", sep = "\n", comment.char = "#")
	for(dirdat in dirdats) {
		if(!file.exists(file.path(dirdat, list.files(dirdat, pattern = "CAST")[1]))) {
			cat(dirdat, "> No cast\n")
			next()
		}
		cast <- fread(file=file.path(dirdat, list.files(dirdat, pattern = "CAST")[1]))

		instruments.optics <- unique(str_extract(str_subset(names(cast), "^[:alnum:]{2,}(?=\\d{3})"), "^[:alnum:]{2,}(?=\\d{3})"))
		tiltmax.optics <- c(10,5,5,5)[0:length(instruments.optics)]
		time.interval.for.smoothing.optics <- c(40, 20,20,20)[0:length(instruments.optics)]
		sub.surface.removed.layer.optics <- c(0, 0.1, 0, 0)[0:length(instruments.optics)]
		delta.capteur.optics <- c(0, -0.05, 0.238, -0.05)[0:length(instruments.optics)]
		radius.instrument.optics <- c(0.035, 0.035, 0.035, 0.035)[0:length(instruments.optics)]

		number.of.fields.before.date <- str_count(list.files(dirdat, pattern = "CAST")[1], "_(?![:digit:]{6})")

		init.params <- init.file
		init.params[16] <- str_replace(init.params[16], "(?<=[:graph:]{0,100};[:graph:]{0,100};).+", str_c(instruments.optics, collapse = ","))
		init.params[17] <- str_replace(init.params[17], "(?<=[:graph:]{0,100};[:graph:]{0,100};).+", str_c(tiltmax.optics, collapse = ","))
		init.params[18] <- str_replace(init.params[18], "(?<=[:graph:]{0,100};[:graph:]{0,100};).+", str_c(depth.interval.for.smoothing.optics, collapse = ","))
		init.params[19] <- str_replace(init.params[19], "(?<=[:graph:]{0,100};[:graph:]{0,100};).+", str_c(sub.surface.removed.layer.optics, collapse = ","))
		init.params[20] <- str_replace(init.params[20], "(?<=[:graph:]{0,100};[:graph:]{0,100};).+", str_c(delta.capteur.optics, collapse = ","))
		init.params[21] <- str_replace(init.params[21], "(?<=[:graph:]{0,100};[:graph:]{0,100};).+", str_c(radius.instrument.optics, collapse = ","))

		init.params[27] <- str_replace(init.params[27], "(?<=[:graph:]{0,100};[:graph:]{0,100};).+", str_c(number.of.fields.before.date))

		write(init.params, file = file.path(dirdat, "init.cops.dat"))
	}

}


