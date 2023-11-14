library(prettymapr)
library(ggmap)
library(sf)
# library(sp)
library(maptiles)
library(TeachingDemos)
library(geosphere)
library(RcmdrMisc)
library(flextable)
set_flextable_defaults(font.family = 'sans', theme_fun = "theme_zebra",
                       font.size = 11, text.align = "right",padding.left = 1,
                       padding.right = 1, padding.top = 2, padding.bottom = 2)
library(car)
library(diptest)
library(PMCMRplus)
library(rcompanion)
library(multcompView)
library(factoextra)
library(ggpubr)
library(lctools)
library(viridis)
library(plotrix)
extent <- st_as_sf(x = data.frame(x = c(115.849,115.995),
                                  y = c(-31.98,-31.91)),
                   coords = c("x", "y"), crs = LongLat)
# "OpenStreetMap.MapnikBW", "OpenStreetMap", "OpenStreetMap.DE", "OpenStreetMap.France", "OpenStreetMap.HOT",
# "Esri.WorldStreetMap", "Esri.DeLorme", "Esri.WorldTopoMap", "Esri.WorldImagery", "Esri.WorldTerrain", "Esri.WorldShadedRelief", "Esri.OceanBasemap", "Esri.NatGeoWorldMap", "Esri.WorldGrayCanvas",
# "CartoDB.Positron", "CartoDB.PositronNoLabels", "CartoDB.PositronOnlyLabels", "CartoDB.DarkMatter", "CartoDB.DarkMatterNoLabels", "CartoDB.DarkMatterOnlyLabels", "CartoDB.Voyager", "CartoDB.VoyagerNoLabels", "CartoDB.VoyagerOnlyLabels",
# "Thunderforest.OpenCycleMap", "Thunderforest.Transport", "Thunderforest.TransportDark", "Thunderforest.SpinalMap", "Thunderforest.Landscape", "Thunderforest.Outdoors", "Thunderforest.Pioneer", "Thunderforest.MobileAtlas", "Thunderforest.Neighbourhood",
# "OpenTopoMap",
# "HikeBike",
# "Wikimedia",
PerthRast <- get_tiles(extent, provider="Thunderforest.Transport",
             apikey="03b0375e8e7c47d0a03018a9001de439",
             crop = TRUE, zoom=14) # Esri.WorldTopoMap,CartoDB.Voyager,Thunderforest.Transport
s0 <- 4
png(filename = "Perth_area_map_nophoto.png", width=768*s0, height=486*s0)
par(oma=c(4,4,1,1)*s0, mar=c(4,4,1,1)*s0, tcl = 0.25*s0,
    lend = "square", ljoin = "mitre", lheight=0.7)
plot_tiles(PerthRast)
axis(1, cex.axis=1.2*s0, mgp=c(2.2,0.9,0)*s0, tcl = -0.25*s0, lwd=s0)
axis(2, cex.axis=1.2*s0, mgp=c(2.2,0.7,0)*s0, tcl = -0.25*s0, lwd=s0)
box(which="plot", lwd=s0)
mtext("Longitude (\u00B0E)",1,2.2*s0,font=2,cex = 1.2*s0)
mtext("Latitude (\u00B0S)",2,2.2*s0,font=2,cex = 1.2*s0)
addnortharrow(border = 1, text.col=1, scale = 1.25*s0, lwd=s0,
              padin = c(0.135,0.165)*s0)
# addnortharrow(border = 10, text.col=10, scale = 1.25*s0, lwd=s0,
#               padin = c(0.15,0.15)*s0)
addscalebar(plotepsg = 4326, linecol=1, label.col=1, htin=0.2*s0,
            label.cex=1.5*s0, widthhint = 0.25, lwd=s0,
            padin = c(0.165,0.135)*s0)
# addscalebar(plotepsg = 4326, linecol=10, label.col=10, htin=0.2*s0,
#             label.cex=1.5*s0, widthhint = 0.25, lwd=s0,
#             padin = c(0.15,0.15)*s0)
polygon(afr_map$bound_lon, afr_map$bound_lat,
        border = 1, col = "gold", lwd = 2)
polygon(afr_map$bound_lon, afr_map$bound_lat,
        border = 1, col = 1, lwd = s0, density = 16/s0)
shadowtext(115.939,-31.92,pos=2,labels="Ashfield\nFlats\nReserve",
           col="gold", bg=1, font=2, cex=1.3*s0)
shadowtext(115.97,-31.93, labels="Perth\nAirport",
           col=1, bg=10, font=2, cex=1.3*s0)
shadowtext(115.865,-31.955, labels="Perth\nCBD",
           col=1, bg=10, font=2, cex=1.3*s0)
text(115.863,-31.965, labels="Derbarl Yerrigan",
     col=4, font=3, cex=1.2*s0)
text(115.863,-31.968, labels="(Swan River)",
     col=4, font=3, cex=1.*s0)
legend("bottomright", bg="#FFFFFFC0", cex=0.8*s0, text.col=1, y.intersp=0.5,
       legend = "Map tiles: Thunderforest Transport.   Projection: WGS84 (EPSG:4326)",
       box.col="#FFFFFF00", x.intersp = 0.5)
par(new=TRUE, mfrow=c(1,1), fig=c(0.01,0.25,0.7,0.98),
    mar=c(1,1,.5,.5), font.lab=2, mgp=c(2,0.2,0),
    tcl=0.2)
plot(aust_map, asp=1.1, type = "l", xaxt="n", xlab="",
     yaxt="n", ylab="", xlim=c(102,154))
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "lightblue", border = 1, lwd=0.5*s0)
polygon(aust_map, col = "khaki", border="darkgreen", lwd=s0)
points(115.8567, -31.941, pch = 21, col = 1,
       bg = "gold", cex = 0.85*s0, lwd=1.5*s0, ljoin="mitre")
text(115,-31, labels = "Perth", cex=s0, pos = 2,
     offset = 0.2, font = 3)
text(133,-25, labels = "Australia", cex=1.1*s0, font=2, col="sienna")
dev.off()
