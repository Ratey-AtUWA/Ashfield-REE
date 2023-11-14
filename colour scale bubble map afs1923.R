par(oma=c(3,3,1,1), mar=c(0,0,0,0), mgp=c(1.7,0.2,0), tcl=0.25)
plot_tiles(aftiles)
with(afr_map, polygon(wetland_E, wetland_N, border = "steelblue", col="#87CEEB80"))
axis(1);axis(2);box()
mtext("Easting (m)",1,1.7*s0,font=2,cex = 1.2*s0)
mtext("Northing (m)",2,1.7*s0,font=2,cex = 1.2*s0)
legend("bottomright", bty="o", cex=0.8,
  legend="Map tiles: CartoDB Positron, Projection: UTM Zone 50S, WGS84 (EPSG:32750)",
       box.col="#00000000", bg="#ffffff40")
addnortharrow()
addscalebar(plotepsg=32750, htin=0.12, label.cex=1.2)
brk0 <- pretty(afs1923$Al,20)
if(max(brk0)<max(afs1923$Al, na.rm=T)){
  brk0 <- append(brk0, 999999)
}
ff <- (0.03 * (par("usr")[4]-par("usr")[3]))/sqrt(max(afs1923$Al,na.rm=T))
afs1923$bubcats <- cut(afs1923$Al, breaks = brk0)
# ppal <- scico::scico(length(brk0)-1, pal="imola", alpha=0.7, dir=-1)
ppal <- rev(UWAcolgrad(length(brk0)-1, alpha=0.7, sat=TRUE))
with(afs1923, symbols(Easting, Northing, add=TRUE, circles = ff*sqrt(Al), inches=F,
     bg=ppal[bubcats], fg=ppal[floor(length(ppal)/1.5)]))
# with(afs1923, points(Easting, Northing, pch=21,
#      bg=viridis(length(brk0)-1, alpha=0.7)[bubcats],
#      cex=0.8+(as.numeric(bubcats)/7)))
xy <- par("usr")
if(pretty(afs1923$Al)[1]==0){
  bublo <- pretty(afs1923$Al)[2]/2
} else {
  bublo <- pretty(afs1923$Al)[1]
}
bubmd <- median(pretty(afs1923$Al))
bubhi <- tail(pretty(afs1923$Al),1)
rect(xy[1]+0.04*(xy[2]-xy[1]), xy[3]+0.75*(xy[4]-xy[3]),
     xy[1]+0.2*(xy[2]-xy[1]), xy[3]+0.99*(xy[4]-xy[3]),
     col="#ffffffc0", border = "grey50")
symbols(rep(xy[1]+0.1*(xy[2]-xy[1]), 3),
  c(xy[3]+0.9*(xy[4]-xy[3]), xy[3]+0.83*(xy[4]-xy[3]), xy[3]+0.78*(xy[4]-xy[3])),
        add=T, inches=F, circles=ff*sqrt(c(bubhi,bubmd,bublo)),
        bg=c(ppal[length(ppal)], ppal[floor(length(ppal)/2)], ppal[1]),
  fg=ppal[floor(length(ppal)/1.5)])
text(rep(xy[1]+0.1*(xy[2]-xy[1]), 3),
     c(xy[3]+0.9*(xy[4]-xy[3]), xy[3]+0.83*(xy[4]-xy[3]), xy[3]+0.78*(xy[4]-xy[3])),
     labels=c(bubhi,bubmd,bublo), pos=4, offset=1.2)
text(xy[1]+0.12*(xy[2]-xy[1]), xy[3]+0.96*(xy[4]-xy[3]),
     labels="Al (mg/kg)", font=2)
