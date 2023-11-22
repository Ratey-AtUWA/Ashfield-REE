library(sf)
extbox <- matrix(c(115.845,115.845,115.999,115.999,115.845,
                   -31.997,-31.893,-31.893,-31.997,-31.997), ncol=2, byrow=F)
extpoly <- st_polygon(list(as.matrix(extbox))) |> st_sfc(crs=st_crs(4326))


coastLL <- st_read("Coastline_LGATE_070.shp")
coastLL <- st_transform(coastLL, crs=st_crs(4326))

coastCrop <- st_intersection(extpoly, coastLL)

for (i in 1:length(coastCrop)){
  write.table(st_coordinates(coastCrop[i])[,1:2],
              file=paste0("river",i,".tsv"), sep="\t", row.names=F)
}

for(i in 1:length(coastCrop)){
  assign(paste0("sr",i), read.table(paste0("river",i,".tsv"),
                                    sep="\t", header=T)) |> as.matrix()
}

# then there is a lot of faffing around in excel to make the
# main river outline into a closed polygon !!!

srpoly <- st_polygon(list(sr16,sr1,sr2,sr3,sr4,sr5,sr6,sr7,sr8,sr9,sr10,sr11,sr12,sr13,sr14,sr15)) |> st_sfc(crs=st_crs(4326))
# srpoly <- st_polygon(list(sr16)) |> st_sfc(crs=st_crs(4326)) # for testing the main outline

# Check it...
plot(srpoly,col="skyblue")
# ...and write to a shapefile
st_write(srpoly, "srpoly.shp", append=F)
