d1 <- 1; d2 <- 2

data0 <- droplevels(na.omit(afs1922clr[,c("Zone","ZoneSimp","Type",
                                          "pH","EC","Al","Ca","Fe","P","S",
                                          "La","Ce","Gd","Nd","Y",
                                          "As","Cr","Cu","Ni","Pb","Zn")]))
af_pca <- prcomp(data0[,c(4:NCOL(data0))], scale. = TRUE)


i <- 1 # for debugging
par(mar = c(4,4,4,4), mgp = c(1.7,0.3,0), tcl = 0.3, font.lab = 2,
    lend=1,ljoin=0)
biplot(af_pca, col = c("transparent","black"), 
       xlim=c(-0.12,0.16), ylim = c(-0.07,0.1), font=2, cex = 1.4)
arrows(rep(0,18), rep(0,18), 
       af_pca$rotation[,1]*18, af_pca$rotation[,2]*18, 
       length = 0.12)
text(af_pca$rotation[,1]*18, af_pca$rotation[,2]*18, 
     labels=row.names(af_pca$rotation))
ff <- 2
lws <- rep(c(3,1), each=5)
symz <- rep(21:25,2)
colz <- c(rainbow(5,s=0.9, v=0.7, end=0.35, alpha=0.25),
          rainbow(5,s=0.9, v=0.5, start=0.4, end=0.375))
palette(c("black", rainbow(nlevels(data0$ZoneSimp),s=0.9, v=0.7,
                           end=0.75, alpha=0.15)))
for (i in 1:nlevels(data0$ZoneSimp)){
  chrows <- which(data0$ZoneSimp==levels(data0$ZoneSimp)[i])
  zdata <- cbind(af_pca$x[chrows,d1], af_pca$x[chrows,d2],
                 subset(data0, data0$ZoneSimp==levels(data0$ZoneSimp)[i]))
  points(zdata[,1:2]*ff, pch=symz[i], 
         bg=c(rainbow(5,s=0.9, v=0.7, end=0.35, alpha=0.25),
              rainbow(5,s=0.9, v=0.5, start=0.4, end=0.375))[i])
  chpts <- chull(zdata[,1], zdata[,2])
  polygon(zdata[chpts,1:2]*ff, 
          border = c(rainbow(5,s=0.9, v=0.7, end=0.35, alpha=0.25),
                     rainbow(5,s=0.9, v=0.5, start=0.4, end=0.375))[i], 
          col=i+1,lwd = lws[i])
}
legend("topleft", bty="n",ncol = 2, cex = 1.2, legend = levels(data0$ZoneSimp),
       pch=symz, pt.bg = c(rainbow(5,s=0.9, v=0.7, end=0.35, alpha=0.25),
                           rainbow(5,s=0.9, v=0.5, start=0.4, end=0.375)))
rm(list=c("ff","lws","colz","i","symz","chrows","chpts","zdata"))
