library(tidyverse)
library(data.table)

intercops <- function(){

	waves <- seq(COPS.DB$waves[4],COPS.DB$waves[length(COPS.DB$waves)],1)

	for(i in 1:length(COPS.DB$Rrs.m[,1])){
		tempdf <- data.frame(t(approx(COPS.DB$waves, COPS.DB$Rrs.m[i,], xout = waves, method = "linear")$y))
		if (!exists("interpol") && !is.data.frame("interpol")){
			interpol <- data.frame(matrix(ncol = length(waves), nrow = 0))
		}
		interpol <- rbind(interpol, tempdf)
	}
	colnames(interpol) <- waves
}

simulcops <- function(band = c("aer", "blue", "green", "red")){

	load(file = "/home/raphael/R/lighthouse/data/RSRoli.RData")
	for(i in 1:length(band)){
		#Create the match DF of RSR wavelenght and COPS
		dfx <- interpol[,match(colnames(RSRoli[[band[i]]]), colnames(interpol))]

		#create a DF of same size as dfx with RSR values
		sizey <- do.call("rbind", replicate(length(dfx[,1]), RSRoli[[band[i]]], simplify = F))

		#do the math sum(f(x)*a(x))/sum(f(x))
		tempdf <- dfx * sizey
		tempdf <- data.frame(rowSums(tempdf)/rowSums(sizey))
		if (!exists("simul") && !is.data.frame("simul")){
			simul <- data.frame(matrix(nrow = length(tempdf[,1]), ncol = 0))
		}
		simul <- cbind(simul, tempdf)
	}
	colnames(simul) <- band
	rownames(simul) <- COPS.DB$stationID
}

#function to create final dataset for intercomparaison between in-situ and remote

#read pixel information from pixex output
#dfsat <- fread(file = "./Sat/Landsat/pixex/pixEx1h_Level 2_measurements.txt", colClasses = c(Name="character"), )
#row.names(dfsat) <- dfsat$Name


#TO ADD verif of same length and perfect match, if so do nothing (nest the two above inside)
#if(length(dfsat[,1])>length(simul[,1])){
	#dfsat <- dfsat[match(rownames(simul), rownames(dfsat)),]
#}
#if(length(dfsat[,1])<length(simul[,1])){
	#simul <- simul[match(rownames(dfsat), rownames(simul)),]
#}

#DF1h <- cbind(simul, dfsat[,10:13])
#DF1h <- na.omit(DF1h)

#remove(simul)
#remove(dfsat)
