
RedDeerdata <- read.table("D:/rprojects/trappingmotion_Rpackage/trappingmotion/data/RedDeerdata.txt", sep = ";", dec=".", header=TRUE, as.is=TRUE)
save(RedDeerdata, file="data/RedDeerdata.RData")
