library(prettymapr)
library(ggmap)
library(sf)
library(maptiles)
library(TeachingDemos)
library(geosphere)
library(RcmdrMisc)
library(car)

prakongkep_BS <- read.csv("prakongkep_BS.csv", stringsAsFactors = TRUE)
prakongkep_sites <- read.csv("Prakongkep_sites.csv", stringsAsFactors = TRUE)

# Perth area Map ####
palette(pal4lite)
# SCPextent <- st_as_sf(x = data.frame(x = c(115.5,116.1),
#                                   y = c(-32.4,-31.6)),
#                    coords = c("x", "y"), crs = LongLat)
SCPRast <- get_tiles(SCPextent, provider = "Stamen.TonerLite",
                       crop = TRUE, zoom=11)
# wa_soil <-
#   st_read(dsn=paste0(getwd(),
#                      "/soils/Best_Available_DPIRD_027.shp"))
# perth_soil <- st_crop(wa_soil, xmin=115.488, ymin=-32.431,
#                       xmax=116.131, ymax=-31.569); rm(wa_soil)
extent <- st_as_sf(x = data.frame(x = c(115.849,115.995),
                                  y = c(-31.98,-31.9)),
                   coords = c("x", "y"), crs = st_crs(4283)) # GDA94-LongLat
PerthRast <- get_tiles(extent, provider = "Esri.WorldImagery",
                       crop = TRUE, zoom=13)

s0 <- 1; par(oma=c(4,4,1,1)*s0, mar=c(4,4,1,1)*s0, tcl = 0.25*s0,
    lend = "square", ljoin = "mitre", lheight=0.7)
plot_tiles(PerthRast)
axis(1, cex.axis=1.2*s0, mgp=c(2.2,0.9,0)*s0, tcl = -0.25*s0, lwd=s0)
axis(2, cex.axis=1.2*s0, mgp=c(2.2,0.7,0)*s0, tcl = -0.25*s0, lwd=s0)
box(which="plot", lwd=s0)
mtext("Longitude (\u00B0E)",1,2.6*s0,font=2,cex = 1.2*s0)
mtext("Latitude (\u00B0S)",2,2.6*s0,font=2,cex = 1.2*s0)
with(prakongkep_sites, shadowtext(Longitude, Latitude, cex=1, font=2,
                      bg=1, col=c("cyan","lemonchiffon","pink")[MapUnit],
                      labels=substr(as.character(prakongkep_sites[,1]),8,10)))
plot(st_geometry(perth_soil), add=T, border="grey95");box(lwd=2)
addnortharrow(border = 1, text.col=1, scale = 1.*s0, lwd=s0,
              padin = c(0.165,0.165)*s0, pos = "topleft")
addnortharrow(border = 10, text.col=10, scale = 1.*s0, lwd=s0,
              padin = c(0.15,0.15)*s0, pos = "topleft")
addscalebar(plotepsg = 4326, linecol=1, label.col=1, htin=0.15*s0,
            label.cex=1.5*s0, widthhint = 0.3, lwd=s0, bar.cols=c(1,1),
            padin = c(0.165,0.135)*s0)
addscalebar(plotepsg = 4326, linecol=10, label.col=10, htin=0.15*s0,
            label.cex=1.5*s0, widthhint = 0.3, lwd=s0,
            padin = c(0.15,0.15)*s0)
legend("bottomright", bg="#FFFFFFC0", cex=0.8*s0, text.col=1, y.intersp=0.5,
       legend = "Map tiles: ESRI World Imagery.   Projection: WGS84 (EPSG:4326)",
       box.col="#FFFFFF00", x.intersp = 0.5)
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

par(new=TRUE, mfrow=c(1,1), fig=c(0.01,0.25,0.7,0.98),
    mar=c(1,1,.5,.5), font.lab=2, mgp=c(2,0.2,0),
    tcl=0.2)
plot(aust_map, asp=1.1, type = "l", xaxt="n", xlab="",
     yaxt="n", ylab="", xlim=c(102,154))
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "lightblue", border = "white", lwd=0.5*s0)
polygon(aust_map, col = "khaki", border="darkgreen", lwd=s0)
points(115.8567, -31.941, pch = 21, col = 1,
       bg = "gold", cex = 0.85*s0, lwd=1.5*s0, ljoin="mitre")
text(115,-31, labels = "Perth", cex=s0, pos = 2,
     offset = 0.2, font = 3)
text(133,-25, labels = "Australia", cex=1.1*s0, font=2, col="sienna")
