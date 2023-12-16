library(sf)
library(maptiles)
library(viridis)
library(scico)
library(prettymapr)

SwanLanduseLL <- st_read("../shapefiles/SwanLanduse/WA_CLUM_August2018_Swan.shp")
# |>  st_transform(crs=st_crs(4326)) # don't transform, keep GDA94 / EPSG:4283
str(SwanLanduseLL); unique(SwanLanduseLL$SECONDARY_)
SwanLanduseLL[,c(2:7,11:13)] <- NULL

# i <- 7
# names(LU_conserv)[i];head(LU_conserv[,i]);summary(LU_conserv[,i])
# unique(LU_conserv$Shape_Area)

LU_conserv <- subset(SwanLanduseLL,
                     SwanLanduseLL$PRIMARY_V8 == "1 Conservation and natural environments")

LU_silv <- subset(SwanLanduseLL,
                  SwanLanduseLL$SECONDARY_ == "3.1 Plantation forests" | SwanLanduseLL$SECONDARY_ == "4.1 Irrigated plantation forests")

LU_transition <- subset(SwanLanduseLL,
                        SwanLanduseLL$SECONDARY_ == "3.6 Land in transition" | SwanLanduseLL$SECONDARY_ == "4.6 Irrigated land in transition")

LU_intensHort <- subset(SwanLanduseLL,
                        SwanLanduseLL$SECONDARY_ == "5.1 Intensive horticulture")

LU_intensAnim <- subset(SwanLanduseLL,
                        SwanLanduseLL$SECONDARY_ == "5.2 Intensive animal production")

LU_industry <- subset(SwanLanduseLL,
                      SwanLanduseLL$SECONDARY_ == "5.3 Manufacturing and industrial")

LU_residential <- subset(SwanLanduseLL,
                         SwanLanduseLL$SECONDARY_ == "5.4 Residential and farm infrastructure")

LU_services <- subset(SwanLanduseLL,
                      SwanLanduseLL$SECONDARY_ == "5.5 Services")

LU_util <- subset(SwanLanduseLL,
                  SwanLanduseLL$SECONDARY_ == "5.6 Utilities")

LU_transpComms <- subset(SwanLanduseLL,
                         SwanLanduseLL$SECONDARY_ == "5.7 Transport and communication")

LU_waste <- subset(SwanLanduseLL,
                         SwanLanduseLL$SECONDARY_ == "5.9 Waste treatment and disposal")

LU_wetland <- subset(SwanLanduseLL,
                         SwanLanduseLL$SECONDARY_ == "6.5 Marsh/wetland")

LU_water <- subset(SwanLanduseLL,
                  SwanLanduseLL$PRIMARY_V8 == unique(SwanLanduseLL$PRIMARY_V8)[6])

extent <- st_as_sf(x = data.frame(x = c(115.849,115.995),
                                  y = c(-31.965,-31.895)),
                   coords = c("x", "y"), crs = st_crs(4283))

PerthRast <- get_tiles(extent, provider="Thunderforest.Neighbourhood",
                       apikey="03b0375e8e7c47d0a03018a9001de439",
                       crop = TRUE, zoom=14)

par(mar=c(0,0,0,0), oma=c(3,3,1,1), mgp=c(1.7,0.3,0), tcl=0.25)
# layout(matrix(c(1,1,2),nrow=1))
plot_tiles(PerthRast)
axis(1); axis(2); box()
mtext("Longitude (\u00b0E)", 1, 1.7, font=2, cex = 1.2)
mtext("Latitude (\u00b0S)", 2, 1.7, font=2, cex = 1.2)

plot(LU_conserv[10], add=T, col="#00802080")
plot(LU_wetland[9], add=T, col="#00606080")
plot(LU_industry[9], add=T, col="#80808080")
plot(LU_residential[9], add=T, col="#80300080")
plot(LU_transition[9], add=T, col="#30800080")
plot(LU_services[9], add=T, col="#90609080")
plot(LU_transpComms[9], add=T, col="#c0c03080")
plot(LU_util[9], add=T, col="#40404080")
plot(LU_waste[9], add=T, col="#EE9A0080")

# png(file="Swan-primary-landuse.png", width=2200, height=1920)
# par(mar=c(0,0,0,0), oma=c(3,3,1,12))
plot(SwanLanduseLL[10], add=T, lwd=0.1, border="#00000080", main="", axes=T,
     pal=c(scico::scico(5, pal="bamako", alp=0.7),"#ffffff80"))
prettymapr::addscalebar(plotepsg=4283, label.cex=2, padin=c(1,1))
legend
# dev.off()

# lupal <- c(rep("#00802080",3),rep("#30800080",4),rep("#50700080",4),"#30800080",
#            rep("#50700080",5),"#80808080",rep("#d0b02080",2),"#80808080","#c0c03080",
#            "#90609080","#40404080","#80300080","#80808080","#ee9a0080",
#            rep("#ffffff00",5),"#00606080","#ffffff00")
pal_order <- c(2,2,2,3,3,3,3,5,5,5,5,3,5,5,5,5,5,7,10,10,7,9,8,4,6,7,11,12,12,12,12,12,1,12)
(lupal <- c(scico(11, pal="bamako", alp=0.75),"#90DAF9C0")[pal_order])

s0 <- 2
png(file="Ashfield-landuse.png", width=2400, height=1500)
par(mar=c(0,0,0,0)*s0, oma=c(3,3,1,1)*s0, mgp=c(1.7,0.3,0)*s0, tcl=0.25*s0, lwd=1*s0)
# layout(matrix(c(1,1,2),nrow=1))
plot_tiles(PerthRast)
axis(1); axis(2); box(lwd=s0)
mtext("Longitude (\u00b0E)", 1, 1.7*s0, font=2, cex = 1.2*s0)
mtext("Latitude (\u00b0S)", 2, 1.7*s0, font=2, cex = 1.2*s0)
plot(SwanLanduseLL[9], add=T, lwd=0.1*s0, border="#00000080",
     pal=lupal)

legend("topleft", inset=0.01, box.col="gray50", y.int=1.05,
       legend=c("Conservation","Forests/grazing","Utilities","Crops/horticulture",
                "Transport/comms","Industrial","Services","Residential",
                "Intensive hort/agric","Waste management","Marsh/wetland","Water"),
       pch=22, pt.cex=3*s0, cex=1.4*s0,
       pt.bg= c(scico(11, pal="bamako", alp=0.75),"#90DAF9C0")[c(2:11,1,12)])
addscalebar(plotepsg=4283, label.cex=2, padin=c(1,1))
dev.off()
# plot(0:1,0:1,type="n",axes=F,bty="n",xlab="",ylab="",ann=F)
