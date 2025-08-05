afr_zones <- read.csv("afr_zones.csv", stringsAsFactors = T)
CMDpoly <- afr_zones[afr_zones$zone=="CMD",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750)
KMDpoly <- afr_zones[afr_zones$zone=="KMD",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750)
LPpoly <- afr_zones[afr_zones$zone=="LP",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750)
Npoly <- afr_zones[afr_zones$zone=="N",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750)
NEpoly <- afr_zones[afr_zones$zone=="NE",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750)
NWpoly <- afr_zones[afr_zones$zone=="NW",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750)
Spoly <- afr_zones[afr_zones$zone=="S",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750)
SEpoly <- afr_zones[afr_zones$zone=="SE",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750)
SM_Epoly <- afr_zones[afr_zones$zone=="SM-E",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750)
SM_SWpoly <- afr_zones[afr_zones$zone=="SM-SW",2:3]|>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750)
SM_Wpoly <- afr_zones[afr_zones$zone=="SM-W",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750)
SWpoly <- afr_zones[afr_zones$zone=="SW",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750)
WDpoly <- afr_zones[afr_zones$zone=="WD",2:3] |>
    na.omit() |> as.matrix() |> list() |> st_polygon() |> st_sfc(crs=32750)
afs1925$Zone <- as.character(rep(NA, nrow(afs1925)))
afs1925$Zone[which(st_intersects(afs1925utm, WDpoly, sparse = F)==TRUE)] <- "WD"
afs1925$Zone[which(st_intersects(afs1925utm, SWpoly, sparse = F)==TRUE)] <- "SW"
afs1925$Zone[which(st_intersects(afs1925utm, SM_Wpoly, sparse = F)==TRUE)] <- "SM-W"
afs1925$Zone[which(st_intersects(afs1925utm, SM_SWpoly, sparse = F)==TRUE)] <- "SM-SW"
afs1925$Zone[which(st_intersects(afs1925utm, SM_Epoly, sparse = F)==TRUE)] <- "SM-E"
afs1925$Zone[which(st_intersects(afs1925utm, SEpoly, sparse = F)==TRUE)] <- "SE"
afs1925$Zone[which(st_intersects(afs1925utm, Spoly, sparse = F)==TRUE)] <- "S"
afs1925$Zone[which(st_intersects(afs1925utm, NWpoly, sparse = F)==TRUE)] <- "NW"
afs1925$Zone[which(st_intersects(afs1925utm, NEpoly, sparse = F)==TRUE)] <- "NE"
afs1925$Zone[which(st_intersects(afs1925utm, Npoly, sparse = F)==TRUE)] <- "N"
afs1925$Zone[which(st_intersects(afs1925utm, LPpoly, sparse = F)==TRUE)] <- "LP"
afs1925$Zone[which(st_intersects(afs1925utm, KMDpoly, sparse = F)==TRUE)] <- "KMD"
afs1925$Zone[which(st_intersects(afs1925utm, CMDpoly, sparse = F)==TRUE)] <- "CMD"

afs1925$Zone[(st_intersects(afs1925utm[which(afs1925$Zone=="SM-W"),],
                            st_buffer(WDpoly,10), sparse = F))==TRUE] <- "WD"
afs1925$Zone[(st_intersects(afs1925utm[which(afs1925$Zone=="SM-W"),],
                            st_buffer(Npoly,10), sparse = F))==TRUE] <- "N"
afs1925$Zone[(st_intersects(afs1925utm[which(afs1925$Zone=="SM-W"),],
                            st_buffer(NWpoly,10), sparse = F))==TRUE] <- "NW"

afs1925$Zone <- as.factor(afs1925$Zone)
