register_google(key=GoogleMapsKey)
afr.gg <- get_stamenmap(bbox=c(left=115.941, bottom = -31.9204,
                               right = 115.9485, top = -31.916),
                        zoom=17, col = "bw",maptype = "toner-lite")
p1 <- c(115.947,-31.9161); p2 <- geosphere::destPoint(p = p1, b = 90, d = 100)
xlabs <- seq(115.941,115.948,.001)
ylabs <- seq(-31.920,-31.916,.001)

library(prettymapr)
library(ggmap)
library(sf)
library(sp)
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

# -=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=-

# Sample Map ####
palette(c(pal4lite,"transparent"))
extent <- st_as_sf(x = data.frame(x = c(399860,400670),
                                  y = c(6467920,6468500)),
                   coords = c("x", "y"), crs = UTM50S)
aftiles <- get_tiles(extent, provider = "Esri.WorldImagery", crop=T, zoom=17)
s0 <- 4
png(filename = "afsed1922_sample_map.png", width=768*s0, height=464*s0)
par(oma=c(4,4,1,1)*s0, tcl = -0.25*s0,
    lend = "round", ljoin = "round", lheight=0.7)
layout(matrix(c(1,1,1,1,2),nrow = 1))
plot(aftiles)
axis(1, cex.axis=1.4*s0, mgp=c(2,0.8,0)*s0)
axis(2, cex.axis=1.4*s0);box(which="plot")
mtext("Easting (m)",1,2.6*s0,font=2,cex = 1.2*s0)
mtext("Northing (m)",2,2.6*s0,font=2,cex = 1.2*s0)
addnortharrow(border = 10, text.col=10, scale = 1.25*s0, lwd = s0,
              padin=c(0.15,0.15)*s0)
# addscalebar(plotepsg = 32750, linecol=11, label.col=1, htin=0.2*s0,
#             label.cex=1.8*s0, widthhint = 0.15, padin=c(0.12,0.12)*s0,
#             pos = "bottomright", bar.cols = c(11,11))
addscalebar(plotepsg = 32750, linecol=10, label.col=11, htin=0.2*s0,
    lwd=0.667*s0, widthhint = 0.15, padin=c(0.15,0.15)*s0, pos="bottomright")
TeachingDemos::shadowtext(400580,6467918,labels="100 m", pos=2, cex=1.8*s0)
text(400360,6468120, labels = "Saltmarsh",
     font = 4, cex=6*s0, srt=43, col="#FFFFFF30")
text(400145,6468189, labels = "Sedge\nmarsh",
     font = 4, cex=4.5*s0, srt=43, col="#FFFFFF30")
shadowtext(399920,6468100, labels = "Kitchener\nDrain",
           font = 3, cex=1.6*s0, srt=305, col="lightblue")
shadowtext(400060,6468240, labels = "Woolcock\nDrain", pos = 4,
           font = 3, cex=1.6*s0, col="lightblue")
shadowtext(400537,6468370, labels = "Chapman\nDrain",
           font = 3, cex=1.6*s0, srt=61, col="lightblue")
with(afr_map, lines(wetland_E, wetland_N, col="lightblue", lty="33",
                    lend="round", lwd=1.2*s0))
with(afr_map, lines(drain_E, drain_N, col = "#8080FFB0", lwd=2.4*s0))
with(afr_map, polygon(veg_E, veg_N, border = "#B0FFB040", lwd = 5*s0))
with(afs1922, points(Easting, Northing, col = 10, bg = c(2,3,7,1)[Year],
                     lwd=1.33*s0, pch = c(21,22,23,24)[Year],
                     cex = s0*c(2.2,1.8,1.8,1.8)[Year],
                     ljoin="mitre"))
plot(c(0,1),c(0,1),type="n",ann=F,xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
legend("top", bg="grey33", cex = 1.6*s0, text.col = 10, y.intersp = 1.4,
       legend = levels(afs1922$Year),
       title = expression(bold("     Sampling Year      ")),
       pch = c(21,22,23,24), pt.bg = c(2,3,7,1), col = 10,
       pt.cex = c(2.2,1.8,1.8,1.8)*s0, pt.lwd=1.33*s0)
text(0.0,0.6,pos=4,labels="Ashfield Flats Reserve", font=2, cex=1.3*s0)
text(rep(0.0,2),c(0.5,0.4),pos=4,labels=c("Map tiles:\nESRI World Imagery",
                                          "Projection: UTM Zone 50S,\nWGS84 (EPSG:32750)"), cex = 1.25*s0)
legend("bottom", bg="grey33", cex = 1.6*s0, text.col = 10, y.intersp = 1.4,
       legend = c("Seasonal\nwetland ponds","Drains",
                  "Extent of native\nvegetation"),
       title = expression(bold("Map key")),
       pch = c(NA,NA,NA), lty = c(2,1,1), lwd = c(1.2,2.4,5)*s0,
       col = c("lightblue","#8080FFB0","#B0FFB040"))
dev.off()

# -=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=-

# REE bubbles ####
# note this is annoying as degree symbols don't plot correctly with axis values,
# so needs manual image editing
png(filename="Fig_REE-ggmap-bubbles2.png", width=1920, height=1260)
ggmap(afr.gg) +
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)") +
  geom_text(aes(x = 115.944, y = -31.918, label = "Ashfield\nFlats",
                fontface = "italic", family="sans"),
            size = 12, color="gray65", lineheight=0.8) +
  geom_text(aes(x = 115.942, y = -31.9199, label = "Swan River",
                fontface="italic", family="sans"), size=12, color="gray65") +
  geom_path(aes(x = drain_lon, y=drain_lat), data=afr_map,
            color = "slategray2", size = 4) +
  geom_polygon(aes(x=wetland_lon, y=wetland_lat), data = afr_map,
               color = "slategray", fill="azure3") +
  geom_segment(aes(x=115.9415, y=-31.91615, xend=115.9415, yend=-31.91605),
               arrow = arrow(length = unit(3, "cm"), type="closed"),
               lineend = "round", linejoin = "mitre", size=3, col = "gray33") +
  geom_text(aes(x = 115.9415, y = -31.91625, label = "N",
                fontface="bold", family="serif"), size=14, col = "white") +
  annotate("rect", color=1, fill=1, xmin = p1[1], xmax = p2[1],
           ymin = p1[2]-5e-5, ymax = p2[2]) +
  geom_text(aes(x = (p1[1]+p2[1])/2, y = p1[2]-1.3e-4, label = "100 m"),
            size = 14, col = 1) +
  geom_sf(data=afs1922ll, aes(bg=REE,size=REE), shape=21, inherit.aes = FALSE) +
  scale_fill_viridis_c(alpha = 0.7, name="\u2211REE (mg/kg)", guide = "none") +
  scale_size_area(breaks=c(0.3,1,3,10,30,50,100,300,500,1e3,3e3,5e3,1e4,3e4,
                           5e4,1e5), max_size = 28, name="\u2211REE (mg/kg)") +
  theme(axis.title = element_text(size = 32, face = "bold"),
        panel.border = element_rect(colour = 1,fill=NA),
        legend.text = element_text(size=28),
        axis.text = element_text(size=28),
        legend.title = element_text(size=28, face = "bold"))
dev.off()

# -=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=-

# multiple boxplots by Type ####
png(filename="Fig_multiBoxplots-Type.png", width=1920, height=1080)
varz <- c("La","Ce","Gd","Nd","Al","Ca","Fe","S","Cu","Pb","Th","Zn")
s0 <- 1
par(mfrow = c(3,4), mar = c(3,6,0.5,0.5)*s0, mgp = c(4.5,0.8,0)*s0,
    cex.lab = 1.5*s0, cex.axis=1.4*s0, font.lab = 2, tcl = 0.2*s0)
for(i in 1:4){
  with(afs1922surf, boxplot(afs1922surf[,which(colnames(afs1922surf)==varz[i])] ~ Type,
                            varwidth=T, names=c("DS","LS","SM","FW"), xlab="",
                            yaxt="n", ylab = "", cex=1*s0,
                            col=c("royalblue","wheat","honeydew3","lightblue")))
  axis(2, mgp=c(1.6,0.3,0)*s0, las=1)
  mtext(paste(varz[i],"(mg/kg)"), side=2, line=3*s0, cex=1.*s0, font=2, las=0)
}
with(afs1922surf,
     boxplot(afs1922surf[,which(colnames(afs1922surf)==varz[5])] ~ Type,
             varwidth=T, names=c("DS","LS","SM","FW"), xlab="",
             yaxt="n", ylab = paste(varz[5],"(mg/kg)"), cex=1*s0,
             col=c("royalblue","wheat","honeydew3","lightblue")))
axis(2, mgp=c(1.6,0.3,0)*s0, las=1)
for(i in 6:8){
  with(afs1922surf,
       boxplot(log10(afs1922surf[,which(colnames(afs1922surf)==varz[i])]) ~ Type,
               varwidth=T, names=c("DS","LS","SM","FW"), xlab="",
               yaxt="n", ylab = paste("log10",varz[i],"(mg/kg)"), cex=1*s0,
               col=c("royalblue","wheat","honeydew3","lightblue")))
  axis(2, at=log10(c(10,20,50,100,200,500,1000,2000,5000,1e4,2e4,5e4,1e5)),
       labels=c("10","20","50","100","200",NA,"1000",NA,"5000",NA,"20000","50000","100000"),
       mgp=c(1.6,0.3,0)*s0, las=1)
}
for(i in 9:10){
  with(afs1922surf,
       boxplot(log10(afs1922surf[,which(colnames(afs1922surf)==varz[i])]) ~ Type,
               varwidth=T, names=c("DS","LS","SM","FW"), xlab="",
               yaxt="n", ylab = paste("log10",varz[i],"(mg/kg)"), cex=1*s0,
               col=c("royalblue","wheat","honeydew3","lightblue")))
  axis(2, at=log10(c(10,50,200,1000,5000)),
       labels=c(10,50,200,1000,5000),
       mgp=c(1.6,0.3,0)*s0, las=1)
}
with(afs1922surf,
       boxplot(afs1922surf[,which(colnames(afs1922surf)==varz[11])] ~ Type,
               varwidth=T, names=c("DS","LS","SM","FW"), xlab="",
               yaxt="n", ylab = "", cex=1*s0,
               col=c("royalblue","wheat","honeydew3","lightblue")))
axis(2, mgp=c(1.6,0.3,0)*s0, las=1)
mtext(paste(varz[11],"(mg/kg)"),side=2, line=2, font=2, cex=1*s0,las=0)
  with(afs1922surf,
       boxplot(log10(afs1922surf[,which(colnames(afs1922surf)==varz[12])]) ~ Type,
               varwidth=T, names=c("DS","LS","SM","FW"), xlab="",
               yaxt="n", ylab = "", cex=1*s0,
               col=c("royalblue","wheat","honeydew3","lightblue")))
  axis(2, at=log10(c(10,50,200,1000,5000)),
       labels=c(10,50,200,1000,5000),
       mgp=c(1.6,0.3,0)*s0, las=1)
  mtext(paste("log10",varz[12],"(mg/kg)"),side=2, line=3.2, font=2,
        cex=1*s0,las=0)
dev.off()

# -=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=-

# multiple box plots REE by Zone and Type ####
varz <- c("La","Ce","Gd","Nd","Y")
palette(c("black",colorRampPalette(c("navy","lemonchiffon"))(7),"grey","white"))
par(mfrow = c(2,3), mar = c(4,3.5,0.5,0.5), mgp = c(2,0.2,0),
    cex.lab = 1.5, font.lab = 2, tcl = 0.2,las=2, xpd=T)
for(i in 1:length(varz)){
  with(afs1922, boxplot(afs1922[,which(colnames(afs1922)==varz[i])] ~ ZoneSimp,
                        cex=1.2, xlab="Zone", ylab = paste(varz[i],"(mg/kg)"),
                        col = c(3,10,3,5,5,5,10,5,7,5,3)))
  m0 <- tapply(afs1922[,which(colnames(afs1922)==varz[i])],
               afs1922$ZoneSimp, median, na.rm=T)
  lines(c(.65,1.35,NA,2.65,3.35,NA,10.65,11.35),
        c(m0[1],m0[1],NA,m0[3],m0[3],NA,m0[11],m0[11]),
        col="white",lwd=2,lend=2)
}
plot(c(0,1),c(0,1),type="n",ann=F,xaxt="n",yaxt="n",bty="n",
     xlab="",ylab="",xaxs="i")
legend(-0.16,1, bty = "n", inset = 0, cex = 1.6,
       legend = c("Drain","Pond","Saltmarsh","Mixed/other"), pch = 22,
       pt.bg = c(3,5,7,10), pt.cex = 3)
text(-0.15,0.18,pos=4,cex=1.2,
     labels=paste0("CMD = Chapman Drain, ", "KMD = Kitchener Drain,\n",
        "FW = Flooded woodland, ", "N = North wetland lake,\n",
        "NE = North-east wetland lake,\n", "NW = North-west wetland lake,\n",
        "S = South wetlands / side drain,\n", "SE = South-east wetland lake,\n",
        "SM = Saltmarsh (east of CMD),\n","SW = South-west wetland lake,\n",
        "WD = Woolcock Drain"))

# -=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=-

# multiple box plots Al Ca Fe S P by Zone and Type ####
palette(c("black",colorRampPalette(c("navy","lemonchiffon"))(7),"grey","white"))
par(mfrow = c(2,3), mar = c(4,5.5,0.5,0.5), mgp = c(3.5,0.3,0),
    cex.lab = 1.5, cex.axis=1.2, font.lab = 2, tcl = 0.2,las=2, xpd=T)
# Al
with(afs1922, boxplot(Al ~ ZoneSimp, cex=1.2, xlab="", ylab = "Al (mg/kg)",
                      col = c(3,10,3,5,5,5,10,5,7,5,3)))
mtext("Zone", side=1, line=2.5, font=2, cex=1.1, las=0)
m0 <- tapply(afs1922$Al, afs1922$ZoneSimp, median, na.rm=T)
lines(c(0.7,1.36,NA,2.7,3.36,NA,10.7,11.36),
      c(m0[1],m0[1],NA,m0[3],m0[3],NA,m0[11],m0[11]),
      col="white",lwd=2,lend=2)

# Ca
with(afs1922, boxplot(log10(Ca) ~ ZoneSimp, cex=1.2, xlab="", ylab="Ca (mg/kg)",
                      col = c(3,10,3,5,5,5,10,5,7,5,3), log="y", yaxt="n"))
axis(2, at=log10(c(500,1000,2000,5000,10000,20000,50000)),
     labels=c(500,1000,2000,5000,10000,20000,50000))
mtext("Zone", side=1, line=2.5, font=2, cex=1.1, las=0)
m0 <- tapply(log10(afs1922$Ca), afs1922$ZoneSimp, median, na.rm=T)
lines(c(0.7,1.36,NA,2.7,3.36,NA,10.7,11.36),
      c(m0[1],m0[1],NA,m0[3],m0[3],NA,m0[11],m0[11]),
      col="white",lwd=2,lend=2)

# Fe
with(afs1922, boxplot(Fe ~ ZoneSimp, cex=1.2, xlab="", ylab = "Fe (mg/kg)",
                      col = c(3,10,3,5,5,5,10,5,7,5,3)))
mtext("Zone", side=1, line=2.5, font=2, cex=1.1, las=0)
m0 <- tapply(afs1922$Fe, afs1922$ZoneSimp, median, na.rm=T)
lines(c(0.7,1.36,NA,2.7,3.36,NA,10.7,11.36),
      c(m0[1],m0[1],NA,m0[3],m0[3],NA,m0[11],m0[11]),
      col="white",lwd=2,lend=2)

# P
with(afs1922, boxplot(log10(P) ~ ZoneSimp, cex=1.2, xlab="", ylab="P (mg/kg)",
                      col = c(3,10,3,5,5,5,10,5,7,5,3), log="y", yaxt="n"))
axis(2, at=log10(c(50,100,200,500,1000,2000,5000,10000)),
     labels=c(50,100,200,500,1000,2000,5000,10000))
mtext("Zone", side=1, line=2.5, font=2, cex=1.1, las=0)
m0 <- tapply(log10(afs1922$P), afs1922$ZoneSimp, median, na.rm=T)
lines(c(0.7,1.36,NA,2.7,3.36,NA,10.7,11.36),
      c(m0[1],m0[1],NA,m0[3],m0[3],NA,m0[11],m0[11]),
      col="white",lwd=2,lend=2)

# S
with(afs1922, boxplot(log10(S) ~ ZoneSimp, cex=1.2, xlab="", ylab="S (mg/kg)",
                      col = c(3,10,3,5,5,5,10,5,7,5,3), log="y", yaxt="n"))
axis(2, at=log10(c(200,500,1000,2000,5000,10000,20000,50000)),
     labels=c(200,500,1000,2000,5000,10000,20000,50000))
mtext("Zone", side=1, line=2.5, font=2, cex=1.1, las=0)
m0 <- tapply(log10(afs1922$S), afs1922$ZoneSimp, median, na.rm=T)
lines(c(0.7,1.36,NA,2.7,3.36,NA,10.7,11.36),
      c(m0[1],m0[1],NA,m0[3],m0[3],NA,m0[11],m0[11]),
      col="white",lwd=2,lend=2)

plot(c(0,1),c(0,1),type="n",ann=F,xaxt="n",yaxt="n",bty="n",
     xlab="",ylab="",xaxs="i")
legend(-0.15,1.1, bty = "n", inset = 0, cex = 1.6,
       legend = c("Drain","Pond","Saltmarsh","Mixed/other"), pch = 22,
       pt.bg = c(3,5,7,10), pt.cex = 3)
text(-0.15,0.21,pos=4,cex=1.2,
     labels=paste0("CMD = Chapman Drain,\n", "KMD = Kitchener Drain,\n",
        "FW = Flooded woodland,\n", "N = North wetland lake,\n",
        "NE = North-east wetland lake,\n", "NW = North-west wetland lake,\n",
        "S = South wetlands / side drain,\n", "SE = South-east wetland lake,\n",
        "SM = Saltmarsh (east of CMD),\n","SW = South-west wetland lake,\n",
        "WD = Woolcock Drain"))

# -=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=-

# multiple LISA-type maps ####
aftiles <- get_tiles(extent, provider = "OpenTopoMap",
                     crop = TRUE, zoom=17)
par(mfrow=c(1,2), mar=c(0,0,0,0), oma=c(3,3,0.5,1.5), mgp=c(1.6,0.2,0),
      tcl=0.3,lend="square", ljoin="mitre")
palette(c("black","#400000","blue3","orange","skyblue",
            "gold","#8080FF","#FFA50040","#87CEEB40","white"))
varz <- c("REE","Al") #,"Fe","S","As","Cu","Pb","Zn"
for (i in 1:length(varz)){
  data_temp <- na.omit(as.data.frame(cbind(afs1922$Easting,afs1922$Northing,
                                           afs1922[,varz[i]])))
  colnames(data_temp) <- c("Easting","Northing",varz[i])

  Coords <- cbind(data_temp$Easting, data_temp$Northing)
  bw <- 8
  locMI <- l.moransI(Coords,bw,data_temp[,varz[i]], scatter.plot = F)
  LISA <- as.data.frame(cbind(Coords[,1:2], log10(data_temp[,varz[i]]),
                                locMI$Ii,
                                locMI$p.value))
    colnames(LISA) <- c("Easting", "Northing", var0, "MoranI", "p_value")
    medREE <- median(LISA[,var0], na.rm = TRUE)
    HiHi <- subset(LISA, LISA$p_value<=0.05&LISA$MoranI>0&LISA[,var0]>=medREE)
    LoLo <- subset(LISA, LISA$p_value<=0.05&LISA$MoranI>0&LISA[,var0]<medREE)
    HiLo <- subset(LISA, LISA$p_value<=0.05&LISA$MoranI<0&LISA[,var0]>=medREE)
    LoHi <- subset(LISA, LISA$p_value<=0.05&LISA$MoranI<0&LISA[,var0]<medREE)

    plot(aftiles)
    with(afr_map, polygon(wetland_E, wetland_N, col="skyblue",
                          border="steelblue"))
    with(afr_map, lines(drain_E, drain_N, col="steelblue"))
    if(i > length(varz)-1.5) {
      axis(1, at = seq(399900,400500,100),
           labels = c("399900","400000","400100","400200",
                      "400300","400400","400500"))
      mtext(side=1, line=1.6, text="Easting (UTM Zone 50, m)", font=2, cex=1)
    }
    if(round(i/2,0) != (i/2)) {
      axis(2)
      mtext(side=2, line=1.6, text="Northing (UTM Zone 50, m)", font=2, cex=0.9)
    }
    axis(1, labels=F); axis(2, labels = F)
    addnortharrow(pos="topleft", border=1, lwd=1, text.col=1,
                  padin=c(0.05,0.05), scale = 0.7)
    addscalebar(plotepsg = 32750, label.cex = 1.2)
    box()
    with(data_temp, points(Easting, Northing, pch=3, cex=0.7, col="steelblue4"))
    with(HiHi, points(Northing ~ Easting, pch = 22,
                      col = 2, bg = 6, lwd = 2, cex = 1.8))
    with(LoLo, points(Northing ~ Easting, pch = 21,
                      col = 3, bg = 7, lwd = 2, cex = 2))
    with(HiLo, points(Northing ~ Easting, pch = 25,
                      col = 2, bg = 7, lwd = 2, cex = 1.4))
    with(LoHi, points(Northing ~ Easting, pch = 24,
                      col = 3, bg = 6, lwd = 2, cex = 1.4))
    mtext(varz[i],3,-1.8,adj=0.98,font=2,cex=1.5)
  }
# rect(400320,6467855,400590,6467930,col="#f0f0f0", border = "#e0e0e0")
legend("bottomright", bty = "n", cex = 1, ncol=1,
       legend = c("High-High", "Low-Low","High-Low","Low-High","Samples"),
       pch = c(22, 21, 25,24,3), pt.cex = c(1.8,2,1.4,1.4,0.7),
       pt.lwd = c(2,2,2,2,1), col=c(2,3,2,3,"steelblue"), pt.bg=c(6,7,7,6,NA),
       text.col=1, box.col = "#b0b0b0", box.lwd = 1.3, bg = "#E8E8E8",
       x.intersp = 0.8, y.intersp=1.1)
# text(400350,6468090,pos=3,labels="Spatial\nautocorrelation key", font=3,
#        cex=1.2, offset = 0.15)
par(mfrow=c(1,1), mar=c(3,3,0.5,0.5), oma=c(0,0,0,0), mgp=c(1.6,0.2,0),
      tcl=0.3,lend="square", ljoin="mitre")


# -=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=-

# sulfate-chloride ratio ####
require(viridisLite)
afwater1922fresh <-
  droplevels(subset(afwater1922,afwater1922$salinity=="Fresh"))
afwater1922fresh <- afwater1922fresh[-which(afwater1922fresh$Na>1000),]
par(mar = c(3,8,1,1), mgp=c(1.8,0.4,0), las = 1, tcl = 0.2)
boxplot(log10((S*(96/32))/(Na*1.799)) ~ Zone,
        data = afwater1922fresh, horizontal = T, xaxt="n",
        ylab="", cex.axis = 1.1,
        col = rev(viridis(nlevels(afwater1922fresh$Zone))),
        xlab=expression(bold(paste(log[10],"(3 × S/1.799 × Na)"))))
axis(1, at = log10(c(0.01,0.02,0.03,0.05,0.1,0.2,0.3,0.5,1,2)),
     labels = c(0.01,0.02,0.03,0.05,0.1,0.2,0.3,0.5,1,2))
abline(v=log10(0.14), col = "darkcyan", lty = 2, lwd = 1)
text(log10(0.14) ,8.5, pos=2, col = "darkcyan", cex = 1, offset = 0.15,
     labels="Mean seawater ratio")
text(log10(0.14) ,7.5, pos=2, col = "darkcyan", cex = 1, offset = 0.15,
     labels=expression(paste(SO[4],"/Cl=0.14")))
abline(v=log10(0.5), col = "red3", lty = 2, lwd = 2)
text(log10(0.5) ,7, pos=4, col = "red3", offset=0.15,
     labels = "Acid sulfate trigger")
text(log10(0.5) ,6, pos=4, col = "red3", cex = 1, offset = 0.15,
     labels=expression(paste(SO[4],"/Cl=0.5")))
par(mar = c(3,3,1,1), las = 1)

# -=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=-

# sulfate-chloride table ####
afwater1922fresh$SO4_Cl <- signif((afwater1922fresh$S*(96/32))/
                                    (afwater1922fresh$Na*1.799),3)
afw1922SO4Cl <- cbind(afwater1922fresh[which(((afwater1922fresh$S*(96/32))/
                                    (afwater1922fresh$Na*1.799))>0.5),
                                    c("Year","Sample","Easting","Northing",
                                      "Longitude","Latitude",
                                      "Drain","Zone","SO4_Cl","pH")])
afw1922SO4Cl_LL <- st_as_sf(afw1922SO4Cl, coords=c("Longitude","Latitude"),
                            crs=st_crs(4326))
afr_gg <- get_stamenmap(bbox=c(left=115.941, bottom = -31.9204,
                               right = 115.95, top = -31.914),
                        zoom=15, maptype = "terrain", col = "bw")
s0 <- 2.5
p1 <- c(115.9414,-31.9144); p2 <- geosphere::destPoint(p = p1, b = 90, d = 100)
png(filename = "afr_SO4_Cl.png", width = 768*s0, height = 592*s0)
ggmap(afr_gg) +
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)") +
  geom_text(aes(x = 115.944, y = -31.918, label = "Ashfield\nFlats",
                fontface = "italic", family="sans"),
            size = 6.4*s0, color="gray65", lineheight=0.8) +
  geom_text(aes(x = 115.942, y = -31.92, label = "Swan River",
              fontface="italic", family="sans"), size=6.4*s0, color="gray45") +
  geom_path(aes(x = drain_lon, y=drain_lat), data=afr_map[1:57,],
            color = "gray50", linewidth = 1*s0) +
  geom_polygon(aes(x=wetland_lon, y=wetland_lat), data = afr_map,
               color = "gray50", fill="gray63") +
  annotate("rect", color=1, fill=1, xmin = p1[1], xmax = p2[1],
           ymin = p1[2]-8e-5, ymax = p2[2]) +
  geom_text(aes(x = (p1[1]+p2[1])/2, y = p1[2]-1.8e-4, label = "100 m"),
            size = 6.4*s0, col = 1) +
  geom_sf(data=afw1922SO4Cl_LL, aes(bg=pH, size=SO4_Cl), shape=21,
          inherit.aes = FALSE, na.rm = TRUE) +
  geom_text(aes(x = 115.9419, y = -31.91515, label = "N",
                fontface="bold", family="serif"), size=11.2*s0) +
  ggsn::north(data=afw1922SO4Cl_LL, location="topleft", scale = 0.25,
              symbol=14, anchor = c(x=115.9409,y=-31.9153)) +
  scale_fill_viridis_c(alpha = 0.75, option="C", direction = -1) +
  scale_size_area(breaks=seq(0.5,2,0.5), max_size = 20*s0,
                  name="Sulfate/\nchloride\nratio") +
  theme(axis.title = element_text(size = 20*s0, face = "bold", color = "black"),
        axis.text = element_text(size = 16.8*s0, color = "black"),
        panel.border = element_rect(colour = 1,fill=NA),
        legend.key = element_rect(colour=NA, fill=NA),
        legend.key.size = unit(x=1.2*s0, units="cm"),
        legend.text = element_text(size=16.8*s0, color = "black"),
        legend.title = element_text(size=16.8*s0, color = "black")) +
  coord_sf(crs = st_crs(4326))
dev.off()

# alternative sulfate/chloride map ####
extent <- st_as_sf(x = data.frame(x=c(399860,400670), y=c(6467920,6468600)),
                   coords = c("x", "y"), crs = UTM50S)
afwtiles <- get_tiles(extent, provider = "OpenTopoMap",
                     crop = TRUE, zoom=17)
afw1922SO4Cl_utm <- st_as_sf(afw1922SO4Cl, coords=c("Easting","Northing"),
                            crs=st_crs(32750))
s0 <- 4
png(filename = "afs_SO4_Cl_ratio.png", width=768*s0, height=624*s0)
par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(4,6.4,0.5,1.5)*s0, mgp=c(1.6,0.2,0)*s0,
    tcl=0.3*s0,lend="square", ljoin="mitre", las=0)
plot(afwtiles);axis(1, cex.axis=1.4*s0, mgp=c(1.6,0.8,0)*s0)
axis(2, las=1, cex.axis=1.4*s0);box(lwd=s0)
mtext("Easting (UTM Zone 50, m)", side=1, line=2.2*s0, cex=1.6*s0, font=2)
mtext("Northing (UTM Zone 50, m)", side=2,line=5*s0, cex=1.6*s0, font=2)
text(400140,6468150, labels = "Ashfield\nFlats", col="#80a080",
     cex=1.4*s0, font=3)
text(400000,6467940, labels = "Swan River", col="#8090c0",
     cex=1.4*s0, font=3)
with(afr_map[20:60,], lines(drain_E, y=drain_N, col="skyblue", lwd=1*s0))
with(afr_map, polygon(wetland_E, wetland_N, col="lightblue",
                      border="steelblue", lwd=s0))
addscalebar(plotepsg = 32750, htin=0.15*s0, label.cex = 1.2*s0, pos="bottomleft",
            padin=c(0.15,0.15)*s0)
addnortharrow(pos = "topleft", scale=1.2*s0, padin=c(0.15,0.15)*s0)
afw1922SO4Cl$pH_rank <- order(afw1922SO4Cl$pH)
with(afw1922SO4Cl,symbols(Easting, Northing, add=T,
                circles=20*sqrt(SO4_Cl), inches=F,
                bg=viridis(30,alpha=0.75,direction=-1,option="C")[pH_rank]))
plotrix::gradient.rect(400550,6467910,400600,6468100, nslices=60, gradient="y",
               col=viridis::viridis(60,alpha=0.75,direction=-1,option="C"))
text(c(400575,400600,400600), c(6468105,6468100,6467910), pos=c(3,4,4),
     labels=c("pH",max(afw1922SO4Cl$pH,na.rm=T),min(afw1922SO4Cl$pH,na.rm=T)),
     offset = 0.4, cex=1.4*s0, font=c(2,1,1))
symbols(c(400425,400425), c(6468000,6467920), add=T,
        circles=20*sqrt(c(2,0.5)), bg="#80808080", inches=F)
text(c(400425,400450,400450), c(6468050,6468000,6467920), pos=c(3,4,4),
     labels=c("Sulfate/\nchloride\nratio","2.0","0.5"),
     offset = 0.4, cex=1.4*s0, font=c(2,1,1))
dev.off()

# -=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=-

# Ce, La, Nd, Gd vs. Al ####
varz <- c("La","Ce","Gd","Nd")
palette(c("black","navy","dodgerblue","gold","darkturquoise","transparent"))
par(mfrow = c(2,2), mar = c(4,4,0.5,0.5), mgp = c(2,0.2,0),
    cex.lab = 1.6, cex.axis = 1.4, font.lab = 2, tcl = 0.2, las = 1,
    lend="square", ljoin="mitre")
for(i in 1:length(varz)){
  plot(afs1922surf$Al/1e3,
       afs1922surf[,which(colnames(afs1922surf)==varz[i])],
       log="xy", bg = c(2,3,4,5)[afs1922surf$Type],
       pch = seq(21,24)[afs1922surf$Type],
       cex = c(1.5,1.4,1.4,1.4)[afs1922surf$Type],
       ylab = paste(varz[i],"(mg/kg)"), xlab = "Al (g/kg)")
  for(j in 1:nlevels(afs1922surf$Type)){
    lm0 <- lm(log10(afs1922surf[,which(colnames(afs1922surf)==varz[i])]) ~
                log10(afs1922surf$Al/1e3),
              subset = afs1922surf$Type == levels(afs1922surf$Type)[j])
    v0 <- subset(afs1922surf$Al/1e3,
                 subset = afs1922surf$Type == levels(afs1922surf$Type)[j])
    r0 <- log10(range(v0, na.rm=T))
    y0 <- c(lm0$coef[1]+lm0$coef[2]*r0[1],lm0$coef[1]+lm0$coef[2]*r0[2])
    lines(10^r0, 10^y0, col = j+1, lwd = 2)
  }
}
legend("topleft", bty="n", cex = 1.4,
       legend=c("Drain sediment","Lake sediment","Saltmarsh","Flooded woodland"),
       pch=c(21,23,24,22), pt.bg=c(2,4,5,3), pt.cex = c(1.6,1.4,1.4,1.4),
       lwd=2, pt.lwd = 1, col = c(2,4,5,3))
legend("topleft", bty="n", cex = 1.4, text.col = 6,
       legend=rep(" ",4),
       pch=c(21,23,24,22), pt.cex = c(1.5,1.4,1.4,1.4),
       lwd=NA, pt.lwd = 1, col = 1, pt.bg=6)
par(mfrow=c(1,1))

# -=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=-

# REE vs. Fe, P ####
palette(c("black","navy","dodgerblue","gold","darkturquoise","transparent"))
par(mfrow = c(1,2), mar = c(4,4,0.5,0.5), mgp = c(2.2,0.2,0),
    cex.lab = 1.4, cex.axis = 1.25, font.lab = 2, tcl = 0.2, las = 1,
    lend="square", ljoin="mitre")

plot(afs1922surf$Fe/1e3, afs1922surf$REE,
     log = "xy", bg = c(2, 4, 5, 3)[afs1922surf$Type],
     pch = c(21,24,23,22)[afs1922surf$Type],
     cex = c(1.5, 1.4, 1.4, 1.4)[afs1922surf$Type],
     ylab = "\u2211REE (mg/kg)", xlab = "Fe (g/kg)")
lcolz <- c(2,4,5,3)
for (j in 1:nlevels(afs1922surf$Type)){
  lm0 <- lm(log10(afs1922surf$REE) ~ log10(afs1922surf$Fe/1e3),
                  subset = afs1922surf$Type == levels(afs1922surf$Type)[j])
  v0 <- subset(afs1922surf$Fe/1e3,
               subset = afs1922surf$Type == levels(afs1922surf$Type)[j])
  r0 <- log10(range(v0, na.rm=T))
  y0 <- c(lm0$coef[1]+lm0$coef[2]*r0[1],lm0$coef[1]+lm0$coef[2]*r0[2])
  lines(10^r0, 10^y0, col = lcolz[j], lwd = 2)
}
legend("topleft", bty="n", cex = 1.2, y.intersp = .9,
  legend=c("Drain sediment","Lake sediment","Saltmarsh","Flooded","woodland"),
     pch=c(21,23,24,22,NA), pt.bg=c(2,4,5,3,NA), pt.cex = c(1.6,1.4,1.4,1.4,NA),
     lwd=2, pt.lwd = 1, col = c(2,4,5,3,NA))
legend("topleft", bty="n", cex = 1.2, y.intersp = .9, text.col = 6,
       legend=rep(" ",5),
       pch=c(21,23,24,22,NA), pt.cex = c(1.5,1.4,1.4,1.4,NA),
       lwd=NA, pt.lwd = 1, col = 1, pt.bg=6)

plot(afs1922surf$P, afs1922surf$REE,
     log = "xy", bg = c(2, 4, 5, 3)[afs1922surf$Type],
     pch = c(21,24,23,22)[afs1922surf$Type],
     cex = c(1.5, 1.4, 1.4, 1.4)[afs1922surf$Type],
     ylab = "\u2211REE (mg/kg)", xlab = "P (mg/kg)")
lcolz <- c(2,4,5,3)
for (j in 1:nlevels(afs1922surf$Type)){
  lm0 <- lm(log10(afs1922surf$REE) ~ log10(afs1922surf$P),
                  subset = afs1922surf$Type == levels(afs1922surf$Type)[j])
  v0 <- subset(afs1922surf$P,
               subset = afs1922surf$Type == levels(afs1922surf$Type)[j])
  r0 <- log10(range(v0, na.rm=T))
  y0 <- c(lm0$coef[1]+lm0$coef[2]*r0[1],lm0$coef[1]+lm0$coef[2]*r0[2])
  lines(10^r0, 10^y0, col = lcolz[j], lwd = 2)
}

par(mfrow=c(1,1))

# -=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=-

# REE normalised to PAAS by Type ####
par(mfrow=c(2,2), mar = c(0,0,0,0), oma = c(2,4,2,1), mgp=c(1.7,0.3,0),
    tcl=0.25, font.lab=2, lend=2, ljoin=1)
plotdata <-
  stack(st_drop_geometry(PAAS_DrainSed[,c("La","Ce","Nd","Gd","Y","REE")]))
meanz <- with(plotdata, tapply(values, ind, FUN=mean, na.rm=TRUE))
with(plotdata, boxplot(values ~ ind, xaxt="n", xlab="", ylim=c(0,4),
                       ylab="", col = mako(9, dir=1)[4:9]))
mtext("Normalized REE = sample/PAAS",2,2, font=2)
points(c(1:6), as.numeric(meanz), pch=3, col="white", lwd=4)
points(c(1:6), as.numeric(meanz), pch=3, lwd=2)
abline(h=0.99, lty=3,col="sienna4");abline(h=1, lty=3, col="gold")
axis(3, at=1:6, labels=c("La","Ce","Nd","Gd","Y","∑REE"), mgp=c(2,0.15,0),
     font=2, tcl=0, cex.axis=1.2, col.axis = mako(9, dir=1)[2])
mtext("(a) Drain sediment", side=3, line=-1.2, adj=0.05, font=2, cex = 1.1)
pw0 <- with(plotdata, pairwise.t.test(values, ind))
fpw0 <- fullPTable(pw0$p.value)
shadowtext(1:6, rep(3,6), labels=multcompLetters(fpw0)$Letters, font=4,
           cex=1.2, family="serif", col=mako(9, dir=1)[2:7], bg="white")

plotdata <-
  stack(st_drop_geometry(PAAS_LakeSed[,c("La","Ce","Nd","Gd","Y","REE")]))
meanz <- with(plotdata, tapply(values, ind, FUN=mean, na.rm=TRUE))
with(plotdata, boxplot(values ~ ind, xaxt="n", xlab="", yaxt="n", ylim=c(0,4),
                       ylab="", col = mako(9, dir=1)[4:9]))
points(c(1:6), as.numeric(meanz), pch=3, col="white", lwd=4)
points(c(1:6), as.numeric(meanz), pch=3, lwd=2)
abline(h=0.99, lty=3,col="sienna4");abline(h=1, lty=3, col="gold")
axis(3, at=1:6, labels=c("La","Ce","Nd","Gd","Y","∑REE"), mgp=c(2,0.15,0),
     font=2, tcl=0, cex.axis=1.2, col.axis = mako(9, dir=1)[2])
mtext("(b) Lake sediment", side=3, line=-1.2, adj=0.05, font=2, cex = 1.1)
pw0 <- with(plotdata, pairwise.t.test(values, ind))
fpw0 <- fullPTable(pw0$p.value)
shadowtext(1:6, rep(3.75,6), labels=multcompLetters(fpw0)$Letters, font=4,
           cex=1.2, family="serif", col=mako(9, dir=1)[2:7], bg="white")

plotdata <-
  stack(st_drop_geometry(PAAS_SM[,c("La","Ce","Nd","Gd","Y","REE")]))
meanz <- with(plotdata, tapply(values, ind, FUN=mean, na.rm=TRUE))
with(plotdata, boxplot(values ~ ind, xaxt="n", xlab="", ylim=c(0,4),
                       ylab="", col = mako(9, dir=1)[4:9]))
mtext("Normalized REE = sample/PAAS",2,2, font=2)
points(c(1:6), as.numeric(meanz), pch=3, col="white", lwd=4)
points(c(1:6), as.numeric(meanz), pch=3, lwd=2)
axis(1, at=1:6, labels=c("La","Ce","Nd","Gd","Y","∑REE"), font=2, cex.axis=1.2)
abline(h=0.99, lty=3,col="sienna4");abline(h=1, lty=3, col="gold")
mtext("(c) Saltmarsh sediment", side=3, line=-1.2, adj=0.05, font=2, cex = 1.1)
pw0 <- with(plotdata, pairwise.t.test(values, ind))
fpw0 <- fullPTable(pw0$p.value)
shadowtext(1:6, rep(3.75,6), labels=multcompLetters(fpw0)$Letters, font=4,
           cex=1.2, family="serif", col=mako(9, dir=1)[2:7], bg="white")

plotdata <-
  stack(st_drop_geometry(PAAS_Flooded[,c("La","Ce","Nd","Gd","Y","REE")]))
meanz <- with(plotdata, tapply(values, ind, FUN=mean, na.rm=TRUE))
with(plotdata, boxplot(values ~ ind, xaxt="n", xlab="", yaxt="n", ylim=c(0,4),
                       ylab="", col = mako(9, dir=1)[4:9]))
points(c(1:6), as.numeric(meanz), pch=3, col="white", lwd=4)
points(c(1:6), as.numeric(meanz), pch=3, lwd=2)
axis(1, at=1:6, labels=c("La","Ce","Nd","Gd","Y","∑REE"), font=2, cex.axis=1.2)
abline(h=0.99, lty=3,col="sienna4");abline(h=1, lty=3, col="gold")
mtext("(d) Flooded woodland sediment",
      side=3, line=-1.2, adj=0.05, font=2, cex = 1.1)
pw0 <- with(plotdata, pairwise.t.test(values, ind))
fpw0 <- fullPTable(pw0$p.value)
shadowtext(1:6, rep(2,6), labels=multcompLetters(fpw0)$Letters, font=4,
           cex=1.2, family="serif", col=mako(9, dir=1)[2:7], bg="white")

# -=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=--=+=-

# where are [drain] samples with REE/PAAS > 1.2 ? ####
extent <- st_as_sf(x = data.frame(x=c(399860,400620), y=c(6467920,6468400)),
                   coords = c("x", "y"), crs = UTM50S)
afstiles <- get_tiles(extent, provider = "OpenTopoMap",
                     crop = TRUE, zoom=17)
s0 <- 4
png(filename = "REE_anom_map_afs1922.png", width=768*s0, height=492*s0)
par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(4,6.4,0.5,1.5)*s0, mgp=c(1.6,0.2,0)*s0,
    tcl=0.3*s0,lend="square", ljoin="mitre", las=0)
plot(afstiles);axis(1, cex.axis=1.4*s0, mgp=c(1.6,0.8,0)*s0)
axis(2, las=1, cex.axis=1.4*s0);box(lwd=s0)
mtext("Easting (UTM Zone 50, m)", side=1, line=2.2*s0, cex=1.6*s0, font=2)
mtext("Northing (UTM Zone 50, m)", side=2,line=5*s0, cex=1.6*s0, font=2)
text(400140,6468150, labels = "Ashfield\nFlats", col="#80a080",
     cex=1.2*s0, font=3)
text(400000,6467940, labels = "Swan River", col="#8090c0",
     cex=1.2*s0, font=3)
with(afr_map[20:60,], lines(drain_E, y=drain_N, col="#8040d0", lwd=1*s0))
with(afr_map, polygon(wetland_E, wetland_N, col="lightblue",
                      border="steelblue", lwd=s0))
addscalebar(plotepsg = 32750, htin=0.15*s0, label.cex = 1.2*s0, pos="bottomleft",
            padin=c(0.15,0.15)*s0)
addnortharrow(pos = "topleft", scale=1.2*s0, padin=c(0.15,0.15)*s0)
DrainAnom <- which(afs1922_PAAS$REE>1.2 & afs1922_PAAS$Type=="Drain_Sed") #
REEAnom <- which(afs1922_PAAS$REE>1.2) #
plot(afs1922_PAAS[DrainAnom,"REE"], add=T, pch=1, lwd=3*s0, col="white", cex=1.6*s0)
plot(afs1922_PAAS[REEAnom,"REE"], add=T, pch=19, cex=1.2*s0)
rect(399845,6468015,399950,6468255, col="grey92", border="grey92")
plotrix::gradient.rect(399850,6468020,399880,6468220, nslices=60, gradient="y",
                       col=viridis::plasma(60,alpha=1,direction=1))
text(c(399840,399880,399880), c(6468240,6468030,6468210), pos=c(4,4,4),
     labels=c("\u2211REE/PAAS",round(min(afs1922_PAAS$REE[DrainAnom],na.rm=T),2),
              round(max(afs1922_PAAS$REE[DrainAnom],na.rm=T),2)),
     offset = 0.4, cex=1.2*s0, font=c(2,1,1), col = 1)
dev.off()

# end of code ####
