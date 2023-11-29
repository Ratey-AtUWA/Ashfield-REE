library(sf)
library(maptiles)
library(prettymapr)
library(viridis);library(scico)
library(stringr)
afxrd2123 <- read.csv("afxrd2123corr.csv", stringsAsFactors = T)
afxrd2123$Sample <- as.character(afxrd2123$Sample)

miner <- "pyrite"
miner <- str_to_title(miner)
par(oma=c(4,4,1,1), mar=c(0,0,0,0), mgp=c(1.6,0.3,0), font.lab=2)
plot_tiles(aftiles)
axis(1); axis(2); box()
mtext("Easting (m)", 1, 1.6, font=2)
mtext("Northing (m)", 2, 1.6, font=2)
addscalebar(plotepsg=2750, label.cex=1.2)
addnortharrow("topleft",padin=c(0.15,0.8))
with(afr_map, polygon(wetland_E, wetland_N, border="lightblue", col="#c0f0ffa0"))
with(afr_map, lines(drain_E, drain_N, col="dodgerblue"))
palette(c("black", scico(9, pal="hawaii", alpha=0.6)))
with(afxrd2123, points(Easting, Northing, pch=22, bg=c(2:10)[Zone], cex=1.6))
with(subset(afxrd2123, afxrd2123[,miner]>0.001), points(Easting, Northing,
                                                        pch=20, cex=0.6))
legend("top", legend=c(levels(afxrd2123$Zone),paste0(miner," here")),x.int=0.8,
       ncol=5, pch=c(rep(22,nlevels(afxrd2123$Zone)),20), pt.bg=c(2:10),
       pt.cex=c(rep(1.6,nlevels(afxrd2123$Zone)),0.6), bty="n")
legend("bottomright", leg="EPSG: 32750 (UTM Zone 50 WGS84)", text.c="#603800",
       bty="n", cex=0.8, y.int=0.5)
names(afxrd2123)

summary(afxrd2123$Zone)

RcmdrMisc::numSummary(afxrd2123[,8:47])
