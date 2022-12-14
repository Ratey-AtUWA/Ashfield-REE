showpal <- function(
    ncol = 2,
    cex=1.4,
    pt.cex=4
)
{  lp0 <- length(palette())
xspac <- pt.cex/5
yspac <- 2*(pt.cex/5)
if(lp0>12&ncol==2) {        # trying to fit longer palettes - needs work
  ncol <- ceiling(lp0/6)
  cex <- 24/lp0
  pt.cex <- 72/lp0
  xspac <- pt.cex/2
  yspac <- 1.25*(pt.cex/2.5)
}
par(mar=rep(0.5,4))
plot(c(0,1),c(0,1), type="n", bty="n", ann = F, 
     xaxt="n", yaxt="n", xlab="", ylab="")
legend("center", ncol = ncol, bty = "n", 
       legend=paste(paste0(seq(1,length(palette())),"."),
                    palette()),
       title = "Current colour palette",
       pt.bg = seq(1,length(palette())), 
       pch = 22, 
       pt.cex = pt.cex, 
       cex = cex,
       x.intersp = xspac,
       y.intersp = yspac)
par(mar = c(4,4,3,1))
}
