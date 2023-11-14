afs1923surf$Type <- as.character(afs1923surf$Type)

isdrain <- which(afs1923surf$Zone=="CMD" | afs1923surf$Zone=="KMD" | afs1923surf$Zone=="WD")
afs1923surf[isdrain,"Type"] <- rep("Drain_Sed", length(isdrain))

islake <- with(afs1923surf, which(Zone=="N" | Zone=="NE" | Zone=="NW" | Zone=="SE" | Zone=="SW"))
afs1923surf[islake,"Type"] <- rep("Lake_Sed", length(islake))

ismarsh <- with(afs1923surf, which(Zone=="S" | Zone=="SM" | Zone=="SM_SW"))
afs1923surf[ismarsh,"Type"] <- rep("Saltmarsh", length(ismarsh))

isother <- with(afs1923surf, which(Zone=="LP"))
afs1923surf[isother,"Type"] <- rep("Other", length(isother))

afs1923surf$Type <- as.factor(afs1923surf$Type)
afs1923surf$Type <- factor(afs1923surf$Type, levels=c("Drain_Sed", "Lake_Sed", "Saltmarsh","Other"))
