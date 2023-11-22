library(sf)
library(maptiles)
library(prettymapr)
library(viridis);library(scico)
library(stringr)

afxrd2123 <- read.csv("afxrd2123corr.csv", stringsAsFactors = T)
afxrd2123$Sample <- as.character(afxrd2123$Sample)

extent <- st_as_sf(x = data.frame(x = c(399880,400520),
                                  y = c(6467960,6468360)),
                   coords = c("x", "y"), crs = st_crs(32750))
aftiles <- get_tiles(extent, provider = "OpenStreetMap.HOT",
                     crop = TRUE, zoom=17)
par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(3,3,0.5,1.5), mgp=c(1.6,0.2,0),
    tcl=0.3,lend="square", ljoin="mitre")
plot_tiles(aftiles)
axis(1);axis(2);box()
mtext(side=1, line=1.6, text="Easting (UTM Zone 50, m)", font=2, cex=1)
mtext(side=2, line=1.6, text="Northing (UTM Zone 50, m)", font=2, cex=0.9)
addnortharrow(pos="topleft", border=1, lwd=1, text.col=1,
padin=c(0.05,0.05), scale = 0.7)
addscalebar(plotepsg = 32750, label.cex = 1.2)
with(afr_map, polygon(wetland_E, wetland_N, col="skyblue",
border="steelblue"))
with(afr_map, lines(drain_E, drain_N, col="steelblue"))
with(subset(afxrd2123,afxrd2123$Type!="Core"),
     points(Easting, Northing, cex=sqrt(Jarosite),
                       pch=22, bg="#fff08080"))
with(subset(afxrd2123,afxrd2123$Type!="Core"),
     points(Easting, Northing, cex=sqrt(Pyrite),
            pch=21, bg="#8000c080"))
legend("bottomright", legend = c("10%","1%",NA,"Pyrite","Jarosite"),
       bty="n", inset=0.02, title=expression(bold("Mineral content")),
       pt.cex=c(3.16,1,NA,2,2), pch=c(21,21,NA,21,22),
       pt.bg=c("#80808080","#80808080",NA,"#8000c080","#fff08080"))

# calcite & gypsum
plot_tiles(aftiles)
axis(1);axis(2);box()
mtext(side=1, line=1.6, text="Easting (UTM Zone 50, m)", font=2, cex=1)
mtext(side=2, line=1.6, text="Northing (UTM Zone 50, m)", font=2, cex=0.9)
addnortharrow(pos="topleft", border=1, lwd=1, text.col=1,
              padin=c(0.05,0.05), scale = 0.7)
addscalebar(plotepsg = 32750, label.cex = 1.2)
with(afr_map, polygon(wetland_E, wetland_N, col="skyblue",
                      border="steelblue"))
with(afr_map, lines(drain_E, drain_N, col="steelblue"))
with(subset(afxrd2123,afxrd2123$Type!="Core"),
     points(Easting, Northing, cex=sqrt(Calcite),
            pch=21, bg="#ffffff80"))
with(subset(afxrd2123,afxrd2123$Type!="Core"),
     points(Easting, Northing, cex=sqrt(Gypsum),
            pch=22, bg="#ff808080"))
legend("bottomright", legend = c("10%","1%",NA,"Calcite","Gypsum"),
       bty="n", inset=0.02, title=expression(bold("Mineral content")),
       pt.cex=c(3.16,1,NA,2,2), pch=c(21,21,NA,21,22),
       pt.bg=c("#b0b0b080","#b0b0b080",NA,"#ffffff80","#ff808080"))

# halite etc.
plot_tiles(aftiles)
axis(1);axis(2);box()
mtext(side=1, line=1.6, text="Easting (UTM Zone 50, m)", font=2, cex=1)
mtext(side=2, line=1.6, text="Northing (UTM Zone 50, m)", font=2, cex=0.9)
addnortharrow(pos="topleft", border=1, lwd=1, text.col=1,
              padin=c(0.05,0.05), scale = 0.7)
addscalebar(plotepsg = 32750, label.cex = 1.2)
with(afr_map, polygon(wetland_E, wetland_N, col="skyblue",
                      border="steelblue"))
with(afr_map, lines(drain_E, drain_N, col="steelblue"))
with(subset(afxrd2123,afxrd2123$Type!="Core"),
     points(Easting, Northing, cex=sqrt(Calcite),
            pch=21, bg="#ffffff80"))
with(subset(afxrd2123,afxrd2123$Type!="Core"),
     points(Easting, Northing, cex=sqrt(Gypsum),
            pch=22, bg="#ff808080"))
legend("bottomright", legend = c("10%","1%",NA,"Calcite","Gypsum"),
       bty="n", inset=0.02, title=expression(bold("Mineral content")),
       pt.cex=c(3.16,1,NA,2,2), pch=c(21,21,NA,21,22),
       pt.bg=c("#b0b0b080","#b0b0b080",NA,"#ffffff80","#ff808080"))

plot_tiles(aftiles)
axis(1);axis(2);box()
mtext(side=1, line=1.6, text="Easting (UTM Zone 50, m)", font=2, cex=1)
mtext(side=2, line=1.6, text="Northing (UTM Zone 50, m)", font=2, cex=0.9)
addnortharrow(pos="topleft", border=1, lwd=1, text.col=1,
              padin=c(0.05,0.05), scale = 0.7)
addscalebar(plotepsg = 32750, label.cex = 1.2)
with(afr_map, polygon(wetland_E, wetland_N, col="skyblue",
                      border="steelblue"))
with(afr_map, lines(drain_E, drain_N, col="steelblue"))
# with(subset(afxrd2123,afxrd2123$Type!="Core"),
#      points(Easting, Northing, cex=3,
#             pch=21, bg="#ffffff80"))
# with(afxrd2123, text(jitter(Easting,.09), jitter(Northing,.09), labels=row.names(afxrd2123), cex=0.75))
with(afxrd2123, text(Easting, Northing, labels=row.names(afxrd2123), cex=0.75))

afxrd_clr <- afxrd2123
afxrd_clr[,8:38] <- t(apply(afxrd_clr[,8:38], MARGIN = 1,
                            FUN = function(x){log(x) - mean(log(x),na.rm=T)}))
# replace NAs with nominal (low) value
for(i in 8:38){
  afxrd_clr[which(is.na(afxrd_clr[,i])),i] <- -7
}
summary(afxrd_clr)

af_min_pca <- prcomp(afxrd_clr[,c(8:47)], scale. = TRUE)
summary(af_min_pca)
plot(af_min_pca)
biplot(af_min_pca, choices = c(1,2))

x0 <- 1      # ■■■ CHOOSE THE PRINCIPAL COMPONENT TO BE MAPPED ■■■
varwts1 <- data.frame(PC1.var=row.names(af_min_pca$rotation),
                      PC1=af_min_pca$rotation[,x0],
                      PC1abs=abs(af_min_pca$rotation[,x0]))
varwts1 <- varwts1[rev(order(varwts1$PC1abs)),] ; row.names(varwts1) <- NULL
x0 <- 2      # ■■■ CHOOSE THE PRINCIPAL COMPONENT TO BE MAPPED ■■■
varwts2 <- data.frame(PC2.var=row.names(af_min_pca$rotation),
                      PC2=af_min_pca$rotation[,x0],
                      PC2abs=abs(af_min_pca$rotation[,x0]))
varwts2 <- varwts2[rev(order(varwts2$PC2abs)),] ; row.names(varwts2) <- NULL
x0 <- 3      # ■■■ CHOOSE THE PRINCIPAL COMPONENT TO BE MAPPED ■■■
varwts3 <- data.frame(PC3.var=row.names(af_min_pca$rotation),
                      PC3=af_min_pca$rotation[,x0],
                      PC3abs=abs(af_min_pca$rotation[,x0]))
varwts3 <- varwts3[rev(order(varwts3$PC3abs)),] ; row.names(varwts3) <- NULL
x0 <- 4      # ■■■ CHOOSE THE PRINCIPAL COMPONENT TO BE MAPPED ■■■
varwts4 <- data.frame(PC4.var=row.names(af_min_pca$rotation),
                      PC4=af_min_pca$rotation[,x0],
                      PC4abs=abs(af_min_pca$rotation[,x0]))
varwts4 <- varwts4[rev(order(varwts4$PC4abs)),] ; row.names(varwts4) <- NULL
summwts <- cbind(varwts1[,1:2],varwts2[,1:2],varwts3[,1:2],varwts4[,1:2])
head(summwts,8)

par(oma=c(0,0,0,0), mar=c(3.5,3.5,3,3), font.lab=2, font.axis=1)
biplot(af_min_pca, choices = c(1,2), col=c("#00000000","grey35"),
       scale=.4, xlab="PC1", ylab="PC2", cex.lab=1.4, font.lab=2,
       #xlim=c(-2,1), ylim=c(-0.6,2.3)
       )
ff <- 0.25
with(af_min_pca, points(x[,1]*ff, x[,2]*ff, cex=1.75,
                        pch=c(21,22,23,24,25,21,22,23,24)[afxrd_clr$Zone],
                        bg=plasma(9, alpha=0.7)[afxrd_clr$Zone]))
legend("topright", bty="n", inset=0.02, cex=1.2, ncol=2,
       title=expression(bold("Ashfield Zone")),
       legend=levels(afxrd_clr$Zone),
       pch=c(21,22,23,24,25,21,22,23,24), pt.cex=1.5,
       pt.bg=plasma(9,alp=0.7))

# VERY MANUAL BIPLOT 8^D
library(scico); library(TeachingDemos)
n0 <- nrow(af_min_pca$rotation)
varcols <- c(scico(n0/2, pal="oslo", end=0.5, alp=0.6, dir=1),
             scico(n0/2, pal="tokyo", end=0.5, alp=0.6, dir=1))
# varcols <- scico(n0, pal="oslo", end=0.5, alp=0.8, dir=1)
par(oma=c(0,0,0,0), mar=c(3.5,3.5,3,3), mgp=c(1.7,0.3,0), font.lab=2, font.axis=1, lend="square",
    ljoin="mitre")
ff <- 0.07
with(af_min_pca, plot(x[,1]*ff, x[,2]*ff, cex=2, asp=1,
                      pch=c(21,22,23,24,25,21,22,23,24)[afxrd_clr$Zone],
                      bg=plasma(9, alpha=0.7)[afxrd_clr$Zone],
                      xlim=c(-0.4,.7), ylim=c(-0.4,0.8),
                      xlab="PC1", ylab="PC2", cex.lab=1.4))
with(af_min_pca, arrows(0,0,rotation[,1],rotation[,2], len=0.1, ang=20, lwd=3,
                        col=varcols))
with(af_min_pca,
     text(jitter(rotation[,1]*1.2, 75),
                jitter(rotation[,2]*1.2, 75),
                cex=1, labels=row.names(af_min_pca$rotation),
                font=2, col=varcols, bg="#ffffff40")
     )
legend("bottomright", bty="o", inset=c(0.02,0.02), cex=1.2,
       box.col = 8, title=expression(bold("Ashfield Zone")),
       legend=levels(afxrd_clr$Zone),
       pch=c(21,22,23,24,25,21,22,23,24), pt.cex=2,
       pt.bg=plasma(9,alp=0.7))
