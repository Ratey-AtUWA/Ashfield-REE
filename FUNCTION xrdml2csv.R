xrdml2csv <- function(xfile,
                     xdir = "wd",
                     toscreen = FALSE
                     ) {
  if(xdir == "wd") {
    xdir <- getwd()
    }
  xrdmlRaw <- readLines(paste0(xdir, xfile))
  twothet0 <- grep("2Theta", xrdmlRaw)
  beg0 <- xrdmlRaw[twothet0 + 1]
  beg0 <- gsub("\t\t\t\t\t<startPosition>", "", beg0)
  beg0 <- gsub("</startPosition>", "", beg0)
  beg0 <- as.numeric(beg0)
  end0 <- xrdmlRaw[twothet0 + 2]
  end0 <- gsub("\t\t\t\t\t<endPosition>", "", end0)
  end0 <- gsub("</endPosition>", "", end0)
  end0 <- as.numeric(end0)
  c0 <- grep("<counts", xrdmlRaw)
  xrdmlRaw[c0] <- gsub('\t\t\t\t<counts unit="counts">', "", xrdmlRaw[c0])
  xrdmlRaw[c0] <- gsub("</counts>", "", xrdmlRaw[c0])
  counts <- as.numeric(unlist(strsplit(xrdmlRaw[c0], " ")))
  counts2theta <-
    data.frame(Angle = seq(beg0, end0, ((end0 - beg0) / (length(counts) - 1))),
    Counts = counts)
  cfile <- paste0(substr(xfile,1,str_locate(xfile,".xrdml")[1]),"csv")
  write.csv(counts2theta, file = cfile, row.names = FALSE)
  rm(list = c("xrdmlRaw","twothet0","beg0","end0","c0","counts"))
  if(toscreen==TRUE) return(counts2theta)
}

dir0<-"My Documents/aaTeaching/ENVT4461/Data ENVT4461/XRD-S2-2022/CP_xrdml_S2_2022/"

S_12_Mg <- xrdml2df(xfile = "ENVT4461 S12-Mg.xrdml", xdir = paste0(myPC,dir0))
plot(S_12_Mg, type="l", col = "red3", ylim=c(-1e3,4e4))
head(S_12_Mg); tail(S_12_Mg)

S_12_Gly <- xrdml2df(xfile = "ENVT4461 S-12 Mg+Gly.xrdml",xdir = dir0)
with(S_12_Gly, lines(Counts+4000 ~ Angle, col = "green3"))

S_12_400 <- xrdml2df(xfile = "ENVT4461 S-12 Heated-400.xrdml",xdir = dir0)
with(S_12_400, lines(Counts+8000 ~ Angle, col = "blue2"))

S_12_550 <- xrdml2df(xfile = "ENVT4461 S-12 Heated-550.xrdml",xdir = dir0)
with(S_12_550, lines(Counts+12000 ~ Angle, col = "grey40"))

S14_RP <- xrdml2df(xfile = "S14_RP.xrdml",xdir = dir0)
with(S14_RP, plot(Counts ~ Angle, type="l", col = "tomato"))
