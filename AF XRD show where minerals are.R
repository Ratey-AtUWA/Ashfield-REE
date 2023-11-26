library(sf)
library(maptiles)
library(prettymapr)
library(viridis);library(scico)
library(stringr)
afxrd2123 <- read.csv("afxrd2123corr.csv", stringsAsFactors = T)
afxrd2123$Sample <- as.character(afxrd2123$Sample)
names(afxrd2123)

miner <- "illite"
miner <- str_to_title(miner)
plot_tiles(aftiles)
palette(c("black", scico(9, pal="hawaii", alpha=0.6)))
with(afxrd2123, points(Easting, Northing, pch=22, bg=c(2:10)[Zone], cex=1.6))
with(subset(afxrd2123, afxrd2123[,miner]>0.001), points(Easting, Northing,
                                                        pch=3, lwd=2))
legend("topleft", legend=c(levels(afxrd2123$Zone),paste(miner,"detected")),
       pch=c(rep(22,nlevels(afxrd2123$Zone)),3), pt.bg=c(2:10),
       pt.cex=c(rep(1.6,nlevels(afxrd2123$Zone)),1), inset=0.02)

summary(afxrd2123$Zone)

RcmdrMisc::numSummary(afxrd2123[,8:47])
