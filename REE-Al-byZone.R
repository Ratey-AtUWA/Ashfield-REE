palette(c("black",viridis(11),"transparent"))
plot(afs1922surf$Al/1e3, afs1922surf$REE,
     log = "xy", bg = c(2:12)[afs1922surf$ZoneSimp],
     pch = c(21,24,23,22,21,24,23,22,21,24,23)[afs1922surf$ZoneSimp],
     cex = c(1.5,1.4,1.4,1.4,1.5,1.4,1.4,1.4,1.5,1.4,1.4)[afs1922surf$ZoneSimp],
     ylab = "\u2211REE (mg/kg)", xlab = "Al (g/kg)")
lcolz <- c(2:12)
for (j in 1:nlevels(afs1922surf$ZoneSimp)){
  lm0 <- lm(log10(afs1922surf$REE) ~ log10(afs1922surf$Al/1e3),
            subset = afs1922surf$ZoneSimp == levels(afs1922surf$ZoneSimp)[j])
  v0 <- subset(afs1922surf$Al/1e3,
               subset = afs1922surf$ZoneSimp == levels(afs1922surf$ZoneSimp)[j])
  r0 <- log10(range(v0, na.rm=T))
  y0 <- c(lm0$coef[1]+lm0$coef[2]*r0[1],lm0$coef[1]+lm0$coef[2]*r0[2])
  lines(10^r0, 10^y0, col = lcolz[j], lwd = 2)
}
legend("topleft", bty="n", cex = 1.2, y.intersp = .9,
       legend=levels(afs1922surf$ZoneSimp),
       pch=c(21,24,23,22,21,24,23,22,21,24,23),
       pt.bg=c(2:12), pt.cex = c(1.5,1.4,1.4,1.4,1.5,1.4,1.4,1.4,1.5,1.4,1.4),
       lwd=2, pt.lwd = 1, col = c(2:12))
legend("topleft", bty="n", cex = 1.2, y.intersp = .9, text.col = 6,
       legend=rep(" ",11),
       pch=c(21,24,23,22,21,24,23,22,21,24,23),
       pt.cex = c(1.5,1.4,1.4,1.4,1.5,1.4,1.4,1.4,1.5,1.4,1.4),
       lwd=NA, pt.lwd = 1, col = 1, pt.bg=length(palette()));box()
