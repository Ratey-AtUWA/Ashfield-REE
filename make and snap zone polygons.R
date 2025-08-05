suppressPackageStartupMessages(
  {
library(sf)
library(maptiles)
library(prettymapr)
library(TeachingDemos)}
)
afs1925 <- read.csv("afs1925.csv", stringsAsFactors = TRUE)
afs1925$Year <- as.factor(afs1925$Year)
afs1925$Type <- factor(afs1925$Type, levels=c("Drain_Sed", "Pond_Sed", "Saltmarsh","Other"))
afs1925$Type2 <- factor(afs1925$Type2, levels=c("Drain_Sed", "Pond_Sed", "Saltmarsh W","Saltmarsh E","Other"))
afs1925$Zone <- as.character(afs1925$Zone)
afs1925$Zone <- rep(NA,length(afs1925$Zone))
afs1925utm <- st_as_sf(afs1925, coords=c("Easting","Northing"), crs=32750, remove=F)

afr_zones <- read.csv("afr_zones.csv", stringsAsFactors = T)
levels(afr_zones$zone)
#  1     2     3    4   5    6    7   8    9      10      11     12   13
# "CMD" "KMD" "LP" "N" "NE" "NW" "S" "SE" "SM-E" "SM-SW" "SM-W" "SW" "WD"

(CMDpoly <- afr_zones[afr_zones$zone=="CMD",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750))
(KMDpoly <- afr_zones[afr_zones$zone=="KMD",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750))
(LPpoly <- afr_zones[afr_zones$zone=="LP",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750))
(Npoly <- afr_zones[afr_zones$zone=="N",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750))
(NEpoly <- afr_zones[afr_zones$zone=="NE",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750))
(NWpoly <- afr_zones[afr_zones$zone=="NW",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750))
(Spoly <- afr_zones[afr_zones$zone=="S",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750))
(SEpoly <- afr_zones[afr_zones$zone=="SE",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750))
(SM_Epoly <- afr_zones[afr_zones$zone=="SM-E",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750))
(SM_SWpoly <- afr_zones[afr_zones$zone=="SM-SW",2:3]|>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750))
(SM_Wpoly <- afr_zones[afr_zones$zone=="SM-W",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750))
(SWpoly <- afr_zones[afr_zones$zone=="SW",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750))
(WDpoly <- afr_zones[afr_zones$zone=="WD",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750))


afs1925$Zone[which(st_intersects(afs1925utm, CMDpoly, sparse = F)==TRUE)] <- "CMD"
afs1925$Zone[which(st_intersects(afs1925utm, KMDpoly, sparse = F)==TRUE)] <- "KMD"
afs1925$Zone[which(st_intersects(afs1925utm, LPpoly, sparse = F)==TRUE)] <- "LP"
afs1925$Zone[which(st_intersects(afs1925utm, Npoly, sparse = F)==TRUE)] <- "N"
afs1925$Zone[which(st_intersects(afs1925utm, NEpoly, sparse = F)==TRUE)] <- "NE"
afs1925$Zone[which(st_intersects(afs1925utm, NWpoly, sparse = F)==TRUE)] <- "NW"
afs1925$Zone[which(st_intersects(afs1925utm, Spoly, sparse = F)==TRUE)] <- "S"
afs1925$Zone[which(st_intersects(afs1925utm, SEpoly, sparse = F)==TRUE)] <- "SE"
afs1925$Zone[which(st_intersects(afs1925utm, SM_Epoly, sparse = F)==TRUE)] <- "SM-E"
afs1925$Zone[which(st_intersects(afs1925utm, SM_SWpoly, sparse = F)==TRUE)] <- "SM-SW"
afs1925$Zone[which(st_intersects(afs1925utm, SM_Wpoly, sparse = F)==TRUE)] <- "SM-W"
afs1925$Zone[which(st_intersects(afs1925utm, SWpoly, sparse = F)==TRUE)] <- "SW"
afs1925$Zone[which(st_intersects(afs1925utm, WDpoly, sparse = F)==TRUE)] <- "WD"
afs1925$Zone <- as.factor(afs1925$Zone)

plot(SM_Wpoly, axes=T, mar=c(3,3,1,1))
plot(st_union(CMDpoly,st_union(KMDpoly,st_union(LPpoly,Npoly))),
     axes=T, mar=c(3,3,1,1))

KMDpoly <- st_snap(KMDpoly, SM_Wpoly, tolerance = 5)
Spoly <- st_snap(Spoly, SM_Wpoly, tolerance = 5)
SM_SWpoly <- st_snap(SM_SWpoly, SM_Wpoly, tolerance = 5)
SWpoly <- st_snap(SWpoly, SM_SWpoly, tolerance = 3)
LPpoly <- st_snap(LPpoly, SM_Wpoly, tolerance = 5)
Npoly <- st_snap(Npoly, SM_Wpoly, tolerance = 5)
SEpoly <- st_snap(SEpoly, SM_Epoly, tolerance = 25)





par(oma=c(0,0,0,0), mar=c(3,3,1,1), mgp=c(1.7,0.3,0), tcl = -0.2,
    lend = "round", ljoin = "round", lheight=0.7, font.lab=2); s0 <- 1
# palette(c("black", turbo(nlevels(afr_zones$zone), alpha=0.15)))
palette(c("black",scico(4,pal="batlow",alpha=0.65, end=0.75),"grey","white"))
plot(st_coordinates(extent), asp=1, xaxs="i", yaxs="i", type="n",
     xlab="Easting (m)", ylab="Northing (m)")
plot_tiles(aftiles, add=TRUE)
box(which="plot")
plot(afmask,col="#00800040", add=TRUE)
addnortharrow(border = 1, text.col=1, scale = 0.9*s0)
addscalebar(plotepsg = 32750, linecol=1, label.col=1, htin=0.15*s0,
            label.cex=1.2*s0, widthhint = 0.15)
# for (i in 1:nlevels(afr_zones$zone)){
#   zdata <- subset(afr_zones,
#                   afr_zones$zone == levels(afr_zones$zone)[i])
#   polygon(zdata$zonesE, zdata$zonesN, border = "#00000040",
#           col=c(2,2,5,3,3,3,5,3,4,4,4,3,2)[i])
# }
polz <- gsub("-","_",paste0(levels(afs1925$Zone),"poly"))
clrz <- c(2,2,5,3,3,3,5,3,4,4,4,3,2)
for(i in 1:length(polz)){
  plot(get(polz[i])[1], add=TRUE, col=clrz[i])
}
shadowtext(c(400219, 399934, 400214, 400267, 400382, 400051, 400082,
             400205, 400352, 399954, 400149, 399948, 400050),
           c(6468149,6468125,6468329,6468239,6468233,6468174,6468073,
             6468066,6468101,6467984,6468199,6468020,6468242),
           labels=levels(afr_zones$zone),font=1,,bg=7,
           col="navy")
# with(afs1925, points(Easting, Northing, pch=21, cex=0.65, lwd=1.5,
#                      bg="black", col="white"))
with(afs1925, points(Easting, Northing, pch=c(21:25,21:25,21:23)[afs1925$Zone],
                     cex=1.2, lwd=1.5,
                     bg=turbo(nlevels(afs1925$Zone), alp=0.75)[afs1925$Zone], col="white"))
# with(afs1925, points(Easting, Northing, pch=3, cex=0.6,
#            col="black"))
legend("topleft", bty = "n", inset = 0.01, cex = 1.2, ncol=2,
       legend = c("Drain","Pond","Saltmarsh","Mixed/other"), pch = 22,
       pt.bg = c(2,3,4,5), pt.cex = 2.5)
legend("bottomright", legend=levels(afs1925$Zone), pch=c(21:25,21:25,21:23), pt.cex=1.2,
       pt.bg=turbo(nlevels(afs1925$Zone), alp=0.75), ncol=2, bty="n")
