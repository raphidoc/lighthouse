
MRE <-colMeans((DF1h[,1:4]-DF1h[,5:8])/DF1h[,1:4])

RMSE <- sqrt(colSums((DF1h[,1:4]-DF1h[,5:8])^2)/length(DF1h[,1]))
RMSE <- RMSE/(max(DF1h[,1:4])-min(DF1h[,1:4]))
