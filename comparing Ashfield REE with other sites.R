library(RcmdrMisc)
# comparing Ashfield with other sites
# Perth region comparisons ####
prakongkep <- read.csv("prakongkep.csv", stringsAsFactors = T)
prakongkep$REE <- with(prakongkep, La+Ce+Nd+Gd)
prakongkep$REEY <- with(prakongkep, La+Ce+Nd+Gd+Y)
with(prakongkep, tapply(REE, Horizon, mean, na.rm=T))
with(prakongkep, tapply(REEY, Horizon, mean, na.rm=T))
with(prakongkep, tapply(Fe, Horizon, mean, na.rm=T))
with(prakongkep, tapply(Ca, Horizon, mean, na.rm=T))
with(prakongkep, tapply(S, Horizon, mean, na.rm=T))
with(prakongkep, tapply(P, Horizon, mean, na.rm=T))
numSummary(prakongkep[,c("REE","REEY","La","Ce","Nd","Gd","Y",
                                    "Fe","Ca","S","P")],
           statistics = c("mean","sd","quantiles"), quantiles=c(0,.5,1))

with(afs1922surf, tapply(REE, Type, mean, na.rm=T))
afs1922surf$REEY <- with(afs1922surf, (La+Ce+Nd+Gd+Y))
with(afs1922surf, tapply(REEY, Type, mean, na.rm=T))
numSummary(afs1922surf[c("REE","REEY","La","Ce","Nd","Gd","Y")],
           statistics = c("mean","sd","quantiles"), quantiles=c(0,.5,1))

numSummary(RobPark2018[,c("REE","REEY","La","Ce","Nd","Gd","Y",
                          "Fe","Ca","S","P")],
           statistics = c("mean","sd","quantiles"), quantiles=c(0,.5,1))
tapply(RobPark2018$La, RobPark2018$Type, mean, na.rm=TRUE)
tapply(RobPark2018$REE, RobPark2018$Type, mean, na.rm=TRUE)

smiths <- read.table("clipboard", header=T, sep="\t", stringsAsFactors = T)
str(smiths)
smiths$REE <- with(smiths, (La+Ce+Nd+Gd))
smiths$REEY <- with(smiths, (La+Ce+Nd+Gd+Y))
numSummary(smiths[,c("REE","REEY","La","Ce","Nd","Gd","Y",
                                "Fe","Ca","TS","P")])
tapply(smiths$REE, smiths$Type, mean, na.rm=TRUE)
save.image()
write.csv(smiths, file="smiths.csv", row.names = F)

# international ####
huelva <- read.csv(file="data from Fernández-Caliani etal 2021 SoilSystems.csv",
                   stringsAsFactors = TRUE)
huelva$Transect <- as.factor(huelva$Transect)
huelva$Core <- as.factor(huelva$Core)
str(huelva)
huelva$LaCeNdGd <- with(huelva, La+Ce+Nd+Gd)
ns0 <- numSummary(huelva[,c("REE","LaCeNdGd","La","Ce","Nd","Gd")],
                  statistics = c("mean","sd","quantiles"), quantiles=c(0,.5,1))
t(ns0$table) |> print(digits=3)
with(huelva, tapply(LaCeNdGd, Depth_cat, mean, na.rm=T))
with(subset(huelva, huelva$Transect=="1"), tapply(LaCeNdGd, Core, mean, na.rm=T))
with(subset(huelva, huelva$Transect=="2"), tapply(LaCeNdGd, Core, mean, na.rm=T))

patos <- read.csv(file="data from Costa etal 2021 ChemGeol.csv",
                  stringsAsFactors = TRUE)
patos$Depth <- factor(patos$Depth,
                      levels=c("1 cm","3 cm","5 cm","7 cm","9 cm",
                               "11 cm","13 cm","21 cm","27 cm","33 cm","39 cm"))
numSummary(patos[,c("REE","LaCeNdGd","La","Ce","Nd","Gd")],
           statistics = c("mean","sd","quantiles"), quantiles=c(0,.5,1))
with(patos, tapply(LaCeNdGd, Depth, mean, na.rm=TRUE))
with(patos, tapply(LaCeNdGd, list(Core,Depth), mean, na.rm=TRUE))

marabasco <- read.csv(file="data from Marmolejo-Rodríguez etal 2017.csv",
                      stringsAsFactors = TRUE)
marabasco$REE <- with(marabasco, La+Ce+Pr+Nd+Sm+Eu+Gd+Tb+Dy+Ho+Er+Tm+Yb+Lu)
marabasco$LaCeNdGd <- with(marabasco, La+Ce+Nd+Gd)
numSummary(marabasco[,c("LaCeNdGd","REE","La","Ce","Nd","Gd")],
           statistics = c("mean","sd","quantiles"), quantiles=c(0,.5,1))
with(marabasco, tapply(LaCeNdGd, Env, mean, na.rm=TRUE))

jaguaripe <- read.csv(file="data from de Freitas etal 2021.csv",
                      stringsAsFactors = TRUE)
jaguaripe$LaCeNdGd <- with(jaguaripe, La+Ce+Nd+Gd)
numSummary(jaguaripe[,c("LaCeNdGd","REE","La","Ce","Nd","Gd")],
           statistics = c("mean","sd","quantiles"), quantiles=c(0,.5,1))
with(jaguaripe, tapply(LaCeNdGd, Core, mean, na.rm=TRUE))
with(jaguaripe, tapply(LaCeNdGd, Position, mean, na.rm=TRUE))
