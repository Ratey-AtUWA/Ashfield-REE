library(sf)
library(ggmap)
library(ggplot2)
library(ggspatial)
library(ggpubr)
library(viridis)

secret <- read.csv("../StadiaMaps-API.csv")[1,1]
register_stadiamaps(key=secret)

source("https://github.com/Ratey-AtUWA/Learn-R-web/raw/refs/heads/main/FUNCTION-pad_bbox.R")
(afpolsf <- st_as_sf(af_border, coords=c("Easting","Northing"),
      crs=st_crs(32750), remove=F) |> st_transform(crs=st_crs(4326)))
(l2 <- st_bbox(pad_bbox(afpolsf)) |> as.numeric())
sitegg <-
  get_stadiamap(bbox=c(left=l2[1], bottom=l2[2], right=l2[3], top=l2[4]),
                maptype = "stamen_terrain", zoom=17)
afs1923LL <- st_as_sf(afs1923, coords=c("Easting","Northing"),
       crs=st_crs(32750), remove=F) |> st_transform(crs=st_crs(4326))

theme_set(theme_bw() +
            theme(axis.title = element_text(face="bold", size=12),
                  axis.text = element_text(size=9),
                  legend.text = element_text(size=9),
                  legend.title = element_text(size=11,, face="bold"),
                  plot.margin = unit(c(0,0,0,0), "cm")))

ggREE <- ggplot(st_coordinates(afpolsf)) +
  coord_fixed(ratio = 1)  +
  annotation_raster(sitegg, xmin=l2[1], ymin=l2[2], xmax=l2[3], ymax=l2[4]) +
  scale_x_continuous(expand = c(0,0), limits=c(l2[1],l2[3]),
                     breaks=seq(115.942, 115.948,0.002)) +
  scale_y_continuous(expand = c(0,0), limits=c(l2[2],l2[4])) +
  geom_path(data=afr_map,
            mapping=aes(x=drain_lon, y=drain_lat), col="#8080b0", lwd=1.1) +
  geom_polygon(data=afr_map, mapping=aes(x=wetland_lon, y=wetland_lat),
               fill="#80e0ff80", col="steelblue", lwd=0.8) +
  geom_sf(data=afs1923LL, mapping=aes(size=REE, fill=REE),
          pch=21, color="#00000080") +
  scale_fill_viridis_c(alpha=0.7, option="plasma",
                       breaks=c(10,20,50,100,200,500),
                       guide="legend", name="REE (mg/kg)") +
  scale_size_continuous(range=c(0,6), breaks=c(10,20,50,100,200,500),
                        name="REE (mg/kg)") +
  labs(x="Longitude", y="Latitude") +
  annotation_scale(location="bl", width_hint=0.25, text_cex=0.9,
                   height = unit(0.25, "cm")) +
  annotation_north_arrow(location="tl", which_north="true",
                         pad_x=unit(0.2, "cm"), pad_y=unit(0.2, "cm"),
                         width=unit(1, "cm"), height=unit(1.2, "cm")) +
  theme(legend.margin = margin(0,0,0,0,unit="pt"))

ggAl <- ggplot(st_coordinates(afpolsf)) +
  coord_fixed(ratio = 1)  +
  annotation_raster(sitegg, xmin=l2[1], ymin=l2[2], xmax=l2[3], ymax=l2[4]) +
  scale_x_continuous(expand = c(0,0), limits=c(l2[1],l2[3]),
                     breaks=seq(115.942, 115.948,0.002)) +
  scale_y_continuous(expand = c(0,0), limits=c(l2[2],l2[4])) +
  geom_path(data=afr_map,
            mapping=aes(x=drain_lon, y=drain_lat), col="#8080b0", lwd=1.1) +
  geom_polygon(data=afr_map, mapping=aes(x=wetland_lon, y=wetland_lat),
               fill="#80e0ff80", col="steelblue", lwd=0.8) +
  geom_sf(data=afs1923LL, mapping=aes(size=Al, fill=Al),
          pch=21, color="#00000080") +
  scale_fill_viridis_c(alpha=0.7, option="plasma",
                       breaks=c(1000,2000,5000,10000,20000,50000,1e5),
                       guide="legend", name="Al (mg/kg)") +
  scale_size_continuous(range=c(0,6),
                        breaks=c(1000,2000,5000,10000,20000,50000,1e5),
                        name="Al (mg/kg)") +
  labs(x="Longitude", y="Latitude") +
  annotation_scale(location="bl", width_hint=0.25, text_cex=0.9,
                   height = unit(0.25, "cm")) +
  annotation_north_arrow(location="tl", which_north="true",
                         pad_x=unit(0.2, "cm"), pad_y=unit(0.2, "cm"),
                         width=unit(1, "cm"), height=unit(1.2, "cm"))

ggAs <- ggplot(st_coordinates(afpolsf)) +
  coord_fixed(ratio = 1)  +
  annotation_raster(sitegg, xmin=l2[1], ymin=l2[2], xmax=l2[3], ymax=l2[4]) +
  scale_x_continuous(expand = c(0,0), limits=c(l2[1],l2[3]),
                     breaks=seq(115.942, 115.948,0.002)) +
  scale_y_continuous(expand = c(0,0), limits=c(l2[2],l2[4])) +
  geom_path(data=afr_map,
            mapping=aes(x=drain_lon, y=drain_lat), col="#8080b0", lwd=1.1) +
  geom_polygon(data=afr_map, mapping=aes(x=wetland_lon, y=wetland_lat),
               fill="#80e0ff80", col="steelblue", lwd=0.8) +
  geom_sf(data=afs1923LL, mapping=aes(size=As, fill=As),
          pch=21, color="#00000080") +
  scale_fill_viridis_c(alpha=0.7, option="plasma",
                       breaks=c(5,15,35,60),
                       guide="legend", name="As (mg/kg)") +
  scale_size_continuous(range=c(0,6),
                        breaks=c(5,15,35,60),
                        name="As (mg/kg)") +
  labs(x="Longitude", y="Latitude") +
  annotation_scale(location="bl", width_hint=0.25, text_cex=0.9,
                   height = unit(0.25, "cm")) +
  annotation_north_arrow(location="tl", which_north="true",
                         pad_x=unit(0.2, "cm"), pad_y=unit(0.2, "cm"),
                         width=unit(1, "cm"), height=unit(1.2, "cm"))

ggCu <- ggplot(st_coordinates(afpolsf)) +
  coord_fixed(ratio = 1)  +
  annotation_raster(sitegg, xmin=l2[1], ymin=l2[2], xmax=l2[3], ymax=l2[4]) +
  scale_x_continuous(expand = c(0,0), limits=c(l2[1],l2[3]),
                     breaks=seq(115.942, 115.948,0.002)) +
  scale_y_continuous(expand = c(0,0), limits=c(l2[2],l2[4])) +
  geom_path(data=afr_map,
            mapping=aes(x=drain_lon, y=drain_lat), col="#8080b0", lwd=1.1) +
  geom_polygon(data=afr_map, mapping=aes(x=wetland_lon, y=wetland_lat),
               fill="#80e0ff80", col="steelblue", lwd=0.8) +
  geom_sf(data=afs1923LL, mapping=aes(size=Cu, fill=Cu),
          pch=21, color="#00000080") +
  scale_fill_viridis_c(alpha=0.7, option="plasma",
                       breaks=c(10,20,50,100,200,500,1000,2000),
                       guide="legend", name="Cu (mg/kg)") +
  scale_size_continuous(range=c(0,6),
                        breaks=c(10,20,50,100,200,500,1000,2000),
                        name="Cu (mg/kg)") +
  labs(x="Longitude", y="Latitude") +
  annotation_scale(location="bl", width_hint=0.25, text_cex=0.9,
                   height = unit(0.25, "cm")) +
  annotation_north_arrow(location="tl", which_north="true",
                         pad_x=unit(0.2, "cm"), pad_y=unit(0.2, "cm"),
                         width=unit(1, "cm"), height=unit(1.2, "cm"))

ggPb <- ggplot(st_coordinates(afpolsf)) +
  coord_fixed(ratio = 1)  +
  annotation_raster(sitegg, xmin=l2[1], ymin=l2[2], xmax=l2[3], ymax=l2[4]) +
  scale_x_continuous(expand = c(0,0), limits=c(l2[1],l2[3]),
                     breaks=seq(115.942, 115.948,0.002)) +
  scale_y_continuous(expand = c(0,0), limits=c(l2[2],l2[4])) +
  geom_path(data=afr_map,
            mapping=aes(x=drain_lon, y=drain_lat), col="#8080b0", lwd=1.1) +
  geom_polygon(data=afr_map, mapping=aes(x=wetland_lon, y=wetland_lat),
               fill="#80e0ff80", col="steelblue", lwd=0.8) +
  geom_sf(data=afs1923LL, mapping=aes(size=Pb, fill=Pb),
          pch=21, color="#00000080") +
  scale_fill_viridis_c(alpha=0.7, option="plasma",
                       breaks=c(10,20,50,100,200,500,800),
                       guide="legend", name=" Pb (mg/kg)") +
  scale_size_continuous(range=c(0,6),
                        breaks=c(10,20,50,100,200,500,800),
                        name=" Pb (mg/kg)") +
  labs(x="Longitude", y="Latitude") +
  annotation_scale(location="bl", width_hint=0.25, text_cex=0.9,
                   height = unit(0.25, "cm")) +
  annotation_north_arrow(location="tl", which_north="true",
                         pad_x=unit(0.2, "cm"), pad_y=unit(0.2, "cm"),
                         width=unit(1, "cm"), height=unit(1.2, "cm"))

ggZn <- ggplot(st_coordinates(afpolsf)) +
  coord_fixed(ratio = 1)  +
  annotation_raster(sitegg, xmin=l2[1], ymin=l2[2], xmax=l2[3], ymax=l2[4])+
  scale_x_continuous(expand = c(0,0), limits=c(l2[1],l2[3]),
                     breaks=seq(115.942, 115.948,0.002)) +
  scale_y_continuous(expand = c(0,0), limits=c(l2[2],l2[4])) +
  geom_path(data=afr_map,
            mapping=aes(x=drain_lon, y=drain_lat), col="#8080b0", lwd=1.1) +
  geom_polygon(data=afr_map, mapping=aes(x=wetland_lon, y=wetland_lat),
               fill="#80e0ff80", col="steelblue", lwd=0.8) +
  geom_sf(data=afs1923LL, mapping=aes(size=Zn, fill=Zn),
          pch=21, color="#00000080") +
  scale_fill_viridis_c(alpha=0.7, option="plasma",
                       breaks=c(10,100,1000,3000,7000),
                       guide="legend", name=" Zn (mg/kg)") +
  scale_size_continuous(range=c(0,6), transform="sqrt",
                        breaks=c(10,100,1000,3000,7000),
                        name=" Zn (mg/kg)") +
  labs(x="Longitude", y="Latitude") +
  annotation_scale(location="bl", width_hint=0.25, text_cex=0.9,
                   height = unit(0.25, "cm")) +
  annotation_north_arrow(location="tl", which_north="true",
                         pad_x=unit(0.2, "cm"), pad_y=unit(0.2, "cm"),
                         width=unit(1, "cm"), height=unit(1.2, "cm"))

ggarrange(ggREE, ggAl, ggAs, ggCu, ggPb, ggZn, nrow = 3, ncol = 2)
