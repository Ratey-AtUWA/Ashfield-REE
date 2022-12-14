dataHo <- afs1922surf[,c("La","Ce","Nd","Gd","Y")]
dataHo$Ho_from_La <- dataHo$La*(1/38)
dataHo$Ho_from_Ce <- dataHo$Ce*(1/80)
dataHo$Ho_from_Nd <- dataHo$Nd*(1/32)
dataHo$Ho_from_Gd <- dataHo$Gd*(1/4.7)
# inspect
numSummary(dataHo[,c("Ho_from_La","Ho_from_Ce","Ho_from_Nd","Ho_from_Gd")])
plot(dataHo[,c("Ho_from_La","Ho_from_Ce","Ho_from_Nd","Ho_from_Gd")])
# use the mean
dataHo$Ho_est <- with(dataHo, (Ho_from_La+Ho_from_Ce+Ho_from_Nd+Ho_from_Gd)/4)
# make the ratio
dataHo$Y_Ho <- with(dataHo, Y/Ho_est)
# plot it
boxplot(dataHo$Y_Ho,ylab="Y/Ho estimated for Ashfield sediments", cex.lab=1.5)
