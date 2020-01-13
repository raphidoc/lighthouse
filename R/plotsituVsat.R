plotsituvsat <-function(df1 = "", df2 = "", df3 = ""){
	library(data.table)
	library(tidyverse)

	dfcops <- data.frame(COPS.DB$Rrs.m, row.names = COPS.DB$stationID)
	colnames(dfcops) <- paste("Rrs.m_",COPS.DB$waves, sep = "")

	df1 <- fread(file = "./Sat/Landsat/pixex/SSP1/pixExSSP_Level 2_measurements_1pix.txt")
	row.names(df1) <- df1$Name
	df1$ProdID <- "SSP1"

	df2 <- fread(file = "./Sat/Landsat/pixex/pixEx1h_Level 2_measurements.txt", colClasses = c(Name="character"))
	row.names(df2) <- df2$Name
	df2$ProdID <- "SSP2"

	simul <- simul[match(rownames(simul), rownames(df2), nomatch = 0),]

	DF <- cbind(simul, df2[,10:13])
	DF <- na.omit(DF)

	#df <- reshape2::melt(DF)

	#calcul y = ax+b and R2
	m <- lm(DF24h[,4] ~ DF24h[,8])
	a <- signif(coef(m)[1], digits = 2)
	b <- signif(coef(m)[2], digits = 2)
	textlab <- paste("y = ",b,"x + ",a,sep ="")
	r2 <- format(summary(m)$r.squared, digits = 3)
	eq <- paste("italic(R)^2","(SSP1)","==" , r2)
	eq2 <- paste("italic(R)^2","(SeaDas)","==" , r2)
	as.expression(eq)
	#r2 <- paste("R2_Seadas",r2, sep="= ")
	#geom_text(aes(y = SeaDas, label=row.names(DF)),hjust=0, vjust=0)

	xmin <- 0
	ymin <- 0


p1 <- ggplot(DF)+xlab(bquote('COPS '*R[rs]* '('*sr^{-1}*')'))+ylab(bquote('OLI '*R[rs]* '('*sr^{-1}*')'))+xmin+ymin
p2 <- labs(title = "Rrs relationship COPS vs OLI", subtitle = "COPS value simulated with OLI Relative Spectral Response")
p3 <- geom_point(aes(x = aer, y = Rrs_443, col = "B1"))
p4 <- geom_point(aes(x = blue, y = Rrs_482,  col = "B2"))
p5 <- geom_point(aes(x = green, y = Rrs_561,  col = "B3"))
p6 <- geom_point(aes(x = red, y = Rrs_655,  col = "B4"))

p1+p2+p3+p4+p5+p6

P4 <- geom_smooth(method=lm, se=F, aes(y=SSP))
p5 <- annotate("text", x = 0.01, y = 0.009, label = eq, parse = T)

p7 <- geom_smooth(method=lm, se=F, aes(y=SeaDas))
p8 <- annotate("text", x = 0.01, y = 0.004, label = eq2, parse = T)

}
