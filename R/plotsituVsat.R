library(data.table)

dfSSP <- fread(file = "./SSP/pixExSSP_Level 2_measurements.txt")
row.names(dfSSP) <- dfSSP$Name
dfSSP$ProdID <- "SSP"

dfseadas <- fread(file = "./SeaDas/pixExSeaDas_Level 2_measurements.txt")
row.names(dfseadas) <- dfseadas$Name
dfseadas$ProdID <-"SeaDas"

geom_text(aes(label=row.names(DF)),hjust=0, vjust=0)+

dfcops <- data.frame(COPS.DB$Rrs.m, row.names = COPS.DB$stationID)
colnames(dfcops) <- paste("Rrs.m_",COPS.DB$waves, sep = "")

DF <- data.frame(COPS = dfcops$Rrs.m_555, SeaDas = dfseadas$Rrs_561, SSP = dfSSP$Rrs_561, row.names = dfseadas$Name )

#df <- reshape2::melt(DF)


m <- lm(DF$COPS ~ DF$SSP)
a <- signif(coef(m)[1], digits = 2)
b <- signif(coef(m)[2], digits = 2)
textlab <- paste("y = ",b,"x + ",a,sep ="")
r2 <- format(summary(m)$r.squared, digits = 3)
eq <- substitute(italic(r)^2~"="~r2)
as.expression(eq)
r2Se <- paste("R2_Seadas",r2, sep="= ")

ggplot(DF, aes(COPS, y = SeaDas))+
	geom_point(aes(y = SSP, col = "SSP"))+geom_text(aes( y = SSP,label=row.names(DF)),hjust=0, vjust=0)+
	geom_smooth(method=lm, se=F, aes(y=SSP))+
	geom_point(aes(y = SeaDas, col = "SeaDas"))+geom_text(aes(y = SeaDas, label=row.names(DF)),hjust=0, vjust=0)+
	annotate("text", x = 0.01, y = 0.009, label = r2, parse = T)+
	geom_smooth(method=lm, se=F, aes(y=SeaDas))+
	annotate("text", x = 0.01, y = 0.005, label = r2Se, parse = T)


