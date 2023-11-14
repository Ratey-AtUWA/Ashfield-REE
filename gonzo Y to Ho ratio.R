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

# just swan coastal plain sites near Ashfield in Bassendean Formation
dataHo2 <- prakongkep[c(17,105,108:111),c("La","Ce","Nd","Gd","Y")]
dataHo2$Ho_from_La <- dataHo2$La*(1/38)
dataHo2$Ho_from_Ce <- dataHo2$Ce*(1/80)
dataHo2$Ho_from_Nd <- dataHo2$Nd*(1/32)
dataHo2$Ho_from_Gd <- dataHo2$Gd*(1/4.7)
# inspect
RcmdrMisc::numSummary(dataHo2[,c("Ho_from_La","Ho_from_Ce","Ho_from_Nd","Ho_from_Gd")])
plot(dataHo2[,c("Ho_from_La","Ho_from_Ce","Ho_from_Nd","Ho_from_Gd")])
# use the mean
dataHo2$Ho_est <- with(dataHo2, (Ho_from_La+Ho_from_Ce+Ho_from_Nd+Ho_from_Gd)/4)
# make the ratio
dataHo2$Y_Ho <- with(dataHo2, Y/Ho_est)

# Darling Range data from Du Xin's PhD thesis
duxin$Y_Ho <- with(duxin, Y/Ho)
duxin_regolith <- subset(duxin, duxin$Type=="Regolith")

# plot it
par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(4,4,1,1), mgp=c(1.7,0.3,0),
    tcl=0.25, font.lab=2)
boxplot(dataHo$Y_Ho,ylab="Y/Ho ratio (estimated)",
        cex.lab=1.5, xlim=c(0.5,3.5), ylim=c(5,40))
axis(1, at=1:3, labels = c("Ashfield","Bassendean","Darling Rg."),
     font.axis=2, cex.axis=1.2)
with(dataHo2, boxplot(Y_Ho, add=T, at=2))
duxin_regolith <- subset(duxin, duxin$Type=="Regolith")
duxin_regolith$Y_Ho <- with(duxin_regolith, Y/Ho)
with(duxin_regolith, boxplot(Y_Ho, add=T, at=3))
