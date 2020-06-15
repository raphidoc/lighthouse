library(data.table)

dirdats <- scan(file = "directories.for.cops.dat", "", sep = "\n", comment.char = "#")

for(dirdat in dirdats) {
	print(paste0("PROCESSING DIRECTORY:", dirdat))

	select_file <- fread(file.path(dirdat, "select.cops.dat"), select = c(2,3,4,5))
	copy <- paste("select.cops.dat",Sys.time()[1],sep = "_")
	write.table(select_file, file.path(dirdat, copy), sep = ";", col.names=F, quote = F, row.names = F)

	select_file[,2] <- 1
	select_file[,3] <- "Rrs.0p.linear"
	write.table(select_file, file.path(dirdat, "select.cops.dat"), sep = ";",
			  col.names=F, quote = F, row.names = F)
}
