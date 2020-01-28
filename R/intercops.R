#function to simulate Relative spectral response of a sensor
#Do the math sum(f(x)*a(x))/sum(f(x))
#TO DO : add multiple sensors

SRF <- function(COPS.DB, band = c("B1", "B2", "B3", "B4", "B5"), Sensor = c("oli")){
	temp.data <- COPS.DB %>% group_by(ID) %>% nest()

	for(i in 1:length(band)){
		#Create the match DF of RSR wavelength and COPS, bandwise
		SRF.DF <- RSRoli %>% filter(RSRoli$Wavelength %in% COPS.DB$Lambda)  %>% filter(Band == band[i])

		COPS.DF <- COPS.DB %>% filter(COPS.DB$Lambda %in% SRF.DF$Wavelength) %>% select(ID:Rrs.m)
		#mutate(RSR = rep(RSR.DF[which(RSR.DF$Wavelength %in% COPS.DB$Lambda),]$RSR))

		MATH.D <- COPS.DF %>% group_by(ID) %>% nest() %>%
			mutate(data2 = map(data, ~ select(., Rrs.m))) %>%
			mutate(data3 = list(data2[[1]]*SRF.DF$RSR)) %>%
			mutate(!!band[i] := sum(reduce(data3[[1]], `+`)/reduce(SRF.DF$RSR, `+`))) %>% #tidying back
			ungroup() %>%
			mutate(data = map(temp.data$data, `[`, names(temp.data$data[[1]]))) %>%
			select(-data2, -data3) %>% unnest(cols = data)


		if (!exists("COPS.RSR.DF") && !is.data.frame("COPS.RSR.DF")){
			COPS.RSR.DF <- data.frame(COPS.DB)
		}
		COPS.RSR.DF <- cbind(COPS.RSR.DF, MATH.D[[15]]) #five if not tidy
		colnames(COPS.RSR.DF)[14+i] <- paste0("COPS_", band[i]) #4 ""
		rm(COPS.DF, MATH.D, SRF.DF)
	}
	rm(temp.data)
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

#function to interpolate COPS, add error, better not to use it
