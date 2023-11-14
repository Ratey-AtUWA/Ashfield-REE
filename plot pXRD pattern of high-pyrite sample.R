
xrdS2023_4.4 <- xrdml2df("ENVT3361-G4-S4.xrdml")
plot(xrdS2023_4.4, type="l", font.lab=2)
pyr <- c(33.155,38.45,43.184,47.543,55.539)-0.4
abline(v = pyr, lty = 3, col = "tan")
