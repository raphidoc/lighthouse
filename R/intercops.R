library(tidyverse)
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
