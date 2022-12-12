library(sf)

data0 <- data.frame(Easting=c(seq(399900,400000,20),
                              seq(400100,400200,20),
                              seq(400000,400100,20)),
                    Northing=c(seq(6468000,6468100,20),
                               seq(6468000,6468100,20),
                               seq(6468100,6468000,-20)),
                    Conc=runif(18,30,500),
                    Set = factor(rep(c("A","B","C"), each=6)))

with(data0, plot(Easting, Northing, asp=1, col=c(1,2,4)[Set],
                 lwd=2, cex=2, pch = c(1,3,5)[Set]))

sfdata0 <- st_as_sf(x=data0, coords=c("Easting","Northing"), crs=st_crs(32750))

# identify rows with duplicate coordinates
(duprows <- which(table(unlist(st_equals(sfdata0)))>1))
# add random noise to rows with duplicate coordinates
sfdata0[duprows,] <- st_jitter(sfdata0[duprows,], factor = 0.01)
