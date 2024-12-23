library(PMCMRplus)
library(multcompView)
library(rcompanion)
par(mfrow=c(1,1),mar = c(4.5,4.5,0.5,0), oma=c(0,0,0,0), mgp=c(2.9,0.4,0), tcl=0.25, font.lab=2,
    xpd=T, las=1, lend=2, ljoin=1)
layout(matrix(c(1,1,1,2),nrow = 1,byrow = T))
palette(c("black",colorRampPalette(c("#000060","#d0e8ff"))(7),"grey","white"))
with(afs1923_PAAS, boxplot(REE ~ Zone, cex.axis=1.6, cex.lab=2.2, cex = 1.5,
                      col = c(3,3,10,6,6,6,10,6,8,8,6,3), xlab="Sampling Zone",
                      ylab="Normalized \u2211REE = sample/PAAS"))
m0 <- tapply(afs1923_PAAS$REE,afs1923_PAAS$Zone,median, na.rm=T)
lines(c(.65,1.35,NA,1.65,2.35,NA,11.65,12.35),
      c(m0[1],m0[1],NA,m0[2],m0[2],NA,m0[12],m0[12]),col="white",lwd=3,lend=2)
pwc0 <- with(afs1923_PAAS, kwAllPairsConoverTest(afs1923_PAAS$REE ~ Zone))
mcl0 <- multcompLetters(fullPTable(pwc0$p.value))
text(seq(1,12), rep(-0.042,12), labels = paste0("(",mcl0$Letters,")"),
     col = "blue3", cex = 1.5, family = "serif", font = 3)
plot(c(0,1),c(0,1),type="n",ann=F,xaxt="n",yaxt="n",bty="n",xlab="",ylab="",xaxs="i")
legend(0,1, bty = "n", inset = 0.01, cex = 1.8,
       legend = c("Drain","Pond","Saltmarsh","Mixed/other"), pch = 22,
       pt.bg = c(3,6,8,10), pt.cex = 4.5)
text(-0.1, 0.25, pos=4, cex=1.4,
     labels=paste0("CMD = Chapman Drain\n",
         "KMD = Kitchener Drain\n", "LP = Limestone path\n",
         "N = North wetland pond\n", "NE = North-east wetland pond\n",
         "NW = North-west wetland pond\n", "S = South wetlands / side drain\n",
         "SE = South-east wetland pond\n", "SM = Saltmarsh (east of CMD)\n",
         "SM_SW = Saltmarsh (SW of CMD)\n", "SW = South-west wetland pond\n",
         "WD = Woolcock Drain"))



afs1923clr2ree <- afs1923ree[,c(1:18,24:47)]
for(i in 19:45){
  afs1923clr2ree[which(afs1923clr2ree[,i]<1e-6),i] <- rep(NA, length(which(afs1923clr2ree[,i]<1e-6)))
}
afs1923clr2ree[,19:42] <- t(apply(afs1923clr2ree[,19:42], MARGIN = 1,
                              FUN = function(x){log(x) - mean(log(x),na.rm=T)}))
cat("Untransformed:\n")
head(signif(afs1923ree[,c("Al","Ca","Fe","P","S","REE","Cu","Pb","Th","Zn")],3))
cat("\nCLR-transformed:\n")
head(signif(afs1923clr2ree[,c("Al","Ca","Fe","P","S","REE","Cu","Pb","Th","Zn")],3))

afs1923$REE <- with(afs1923, La+Ce+Nd+Gd)

with(afs1923, plot(REE~log10(EC), pch=c(21,22,23,24)[Type], bg=c(2,4,6,8)[Type]))


# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
par(mfrow = c(4,3), mar = c(4,4,0.5,0.5), mgp = c(1.7,0.2,0),
    cex.lab = 1.5, cex.axis = 1.25, font.lab = 2, tcl = 0.2, las = 1)
clrz <- scico(12, pal="batlow", end=0.85, alp=0.5,dir=-1) # c("#4040FF80","#40c0c080","#b000ff80","#CCA66680")
for(j in 1:nlevels(afs1923$Zone)){
  pdat0 <- subset(afs1923, afs1923$Zone==levels(afs1923$Zone)[j])
  with(pdat0, plot(Al/1000., REE, log="xy", bg = clrz[j], pch = 21,
                   xlim=c(0.4, 65), ylim=c(2,500),
     ylab = "∑REE (mg/kg)", cex = 1.4, xlab = "Al (g/kg)") )
  lm0 <- with(pdat0, lm(log10(REE) ~ log10(Al/1e3)))
  print(lm0)
  r0 <- log10(range(pdat0$Al/1000., na.rm=T))
  y0 <- c(lm0$coef[1]+lm0$coef[2]*r0[1],lm0$coef[1]+lm0$coef[2]*r0[2])
  lines(10^r0, 10^y0, col = clrz[j], lwd = 3)
  legend("topleft", bty="o", cex = 1, bg=clrz[j], box.col="#00000000",
         legend=c(paste(levels(afs1923$Zone)[j],"    "),
                  paste("Slope =",signif(lm0$coef[2],3)),
                  paste("Intercept =",signif(lm0$coef[1],3))))
}

