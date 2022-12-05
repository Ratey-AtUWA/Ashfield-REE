afr_gg <- get_stamenmap(bbox=c(left=115.941, bottom = -31.9204,
                               right = 115.9487, top = -31.916),
                        zoom=15, maptype = "terrain", col = "bw")
p1 <- c(115.9414,-31.9161); p2 <- geosphere::destPoint(p = p1, b = 90, d = 100)
ggmap(afr_gg) +
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)") +
  geom_text(aes(x = 115.944, y = -31.918, label = "Ashfield\nFlats",
                fontface = "italic", family="sans"),
            size = 4, color="gray65", lineheight=0.8) +
  geom_text(aes(x = 115.942, y = -31.92, label = "Swan River",
                fontface="italic", family="sans"), size=4, color="gray45") +
  geom_path(aes(x = drain_lon, y=drain_lat), data=afr_map,
            color = "gray50", size = 1.25) +
  geom_polygon(aes(x=wetland_lon, y=wetland_lat), data = afr_map,
               color = "gray50", fill="gray63") +
  geom_text(aes(x = 115.94191, y = -31.91665, label = "N",
                fontface="bold", family="serif"), size=7) +
  annotate("rect", color=1, fill=1, xmin = p1[1], xmax = p2[1],
           ymin = p1[2]-8e-5, ymax = p2[2]) +
  geom_text(aes(x = (p1[1]+p2[1])/2, y = p1[2]-1.8e-4, label = "100 m"),
            size = 5, col = 1) +
  geom_sf(data=afs1922ll, aes(bg=REE,size=REE), shape=21,
          inherit.aes = FALSE, na.rm = TRUE) +
  ggsn::north(data=afs1922ll, location="topleft", scale = 0.25, symbol=14,
        anchor = c(x=115.941,y=-31.9168)) +
  scale_fill_viridis_c(alpha = 0.85, guide = "none", option="C") +
  scale_size_area(breaks=c(0.3,1,3,10,30,50,100,300,500,1e3,3e3,5e3,1e4,3e4,
                           5e4,1e5), max_size = 9, name="\u2211REE (mg/kg)") +
  theme(axis.title = element_text(size = 12, face = "bold"),
        panel.border = element_rect(colour = 1,fill=NA),
        legend.key = element_rect(colour=NA, fill=NA)) +
  coord_sf(crs = st_crs(4326))
