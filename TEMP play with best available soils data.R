library(sf)
library(maptiles)
library(viridis)
library(scico)

extent <- st_as_sf(x = data.frame(x = c(115.849,115.995),
                                  y = c(-31.98,-31.91)),
                   coords = c("x", "y"), crs = LongLat)

PerthRast <- get_tiles(extent, provider="Thunderforest.Neighbourhood",
                       apikey="03b0375e8e7c47d0a03018a9001de439",
                       crop = TRUE, zoom=14)

WAsoilLL <- st_read("../shapefiles/soils/Best_Available_DPIRD_027.shp") |>
  st_transform(crs=st_crs(4326))
(Bassendean <- WAsoilLL[grep("S8",WAsoilLL$mu_name),])
(Spearwood <- WAsoilLL[grep("Spearwood",WAsoilLL$mu_name),])
(LakeSed <- WAsoilLL[grep("lake", ignore.case = TRUE, WAsoilLL$mu_name),])
(Swan <- WAsoilLL[c(grep("Sw3",WAsoilLL$mu_name),
                    grep("Sw1",WAsoilLL$mu_name)),])
AlluvSand <- WAsoilLL[c(grep("S14",WAsoilLL$mu_name)),]
AlluvSilt <- WAsoilLL[c(grep("Ms4",WAsoilLL$mu_name),
                        grep("Ms2",WAsoilLL$mu_name)),]
AlluvClay <- WAsoilLL[c(grep("Mc1",WAsoilLL$mu_name)),]
Clay <- WAsoilLL[c(grep("C1",WAsoilLL$mu_name)),]
(S10 <- WAsoilLL[grep("S10",WAsoilLL$mu_name),])
(Sm1 <- WAsoilLL[grep("Sm1",WAsoilLL$mu_name),])
(Peaty <- WAsoilLL[c(grep("Cps",WAsoilLL$mu_name),
                     grep("Spc",WAsoilLL$mu_name)),])
(Pinjarra <- WAsoilLL[grep("Pinjarra",WAsoilLL$mu_name),])
(Made <- WAsoilLL[grep("MADE",WAsoilLL$mu_name),])
(Pebbly <- WAsoilLL[grep("Mgs1",WAsoilLL$mu_name),])
(YelSand <- WAsoilLL[grep("S7",WAsoilLL$mu_name),])
(Limestone <- WAsoilLL[grep("LS1",WAsoilLL$mu_name),])
(CalcSand <- WAsoilLL[grep("S2",WAsoilLL$mu_name),])

palette(c("black","#FAEBD780",scico::scico(19, pal="romaO",alpha = 0.5, dir=1)))
par(oma=c(3,3,1,1), mar=c(0,0,0,0), tcl=0.25)
plot_tiles(PerthRast)
axis(1, cex.axis=1.*s0, mgp=c(2.2,0.45,0)*s0, tcl = 0.25*s0, lwd=s0)
axis(2, cex.axis=1.*s0, mgp=c(2.2,0.2,0)*s0, tcl = 0.25*s0, lwd=s0)
box(which="plot", lwd=s0)
mtext("Longitude (\u00B0E)",1,2.*s0,font=2,cex = 1.2*s0)
mtext("Latitude (\u00B0S)",2,2.*s0,font=2,cex = 1.2*s0)
plot(Bassendean[8], add=TRUE, border="#202020", lwd=0.5, col=2)
# plot(LakeSed[8], add=TRUE, border="#202020", lwd=0.5, col=19)
plot(Swan[8], add=TRUE, border="#202020", lwd=0.5, col=4)
plot(AlluvSilt[8], add=TRUE, border="#202020", lwd=0.5, col=7)
plot(AlluvClay[8], add=TRUE, border="#202020", lwd=0.5, col=11)
plot(Clay[8], add=TRUE, border="#202020", lwd=0.5, col=15)
plot(Peaty[8], add=TRUE, border="#202020", lwd=0.5, col=19)
plot(S10[8], add=TRUE, border="#202020", lwd=0.5, col=13)
plot(Sm1[8], add=TRUE, border="#202020", lwd=0.5, col=6)
plot(Pinjarra[8], add=TRUE, border="#202020", lwd=0.5, col=12)
plot(Pebbly[8], add=TRUE, border="#202020", lwd=0.5, col=14)
plot(YelSand[8], add=TRUE, border="#202020", lwd=0.5, col=10)
plot(Limestone[8], add=TRUE, border="#202020", lwd=0.5, col=16)
plot(AlluvSand[8], add=TRUE, border="#202020", lwd=0.5, col=3)
plot(CalcSand[8], add=TRUE, border="#202020", lwd=0.5, col="#e0e0e080")
plot(Made[8], add=TRUE, border="#202020", lwd=0.5, col="#ffffffb0")
polygon(afr_map$bound_lon, afr_map$bound_lat,
        border = "forestgreen", col = "#FFFF00A0", lwd = 2)
polygon(afr_map$bound_lon, afr_map$bound_lat,
        border = "forestgreen", col = 1, lwd = s0, density = 16/s0)
legend("bottom", ncol=3, inset=0.02, box.col = "grey65", bg="grey92", cex=1.2,
       legend=c("Bassendean","Swan", "Alluvial silty",
                "Alluvial clay","Clay","Peaty","S10","Sm1","Pinjarra",
                "Pebbly sand","Yellow sand","Limestone","Alluvial sand",
                "Made ground"),
       pch=22, pt.cex=3, pt.bg=c(2,4,7,11,15,19,13,6,12,14,10,16,3,
                                 "#ffffffb0"))

# for(i in 1:200){
#   plot(WAsoilCrop[i], add=TRUE, border="#202020", lwd=0.5, col=i+1)
# }
