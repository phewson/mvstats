## setwd("d:/Local Data/phewson/flury/multidat")

setwd("C:/work/book/flury/multidat")


files <- dir()

for (i in 1:length(files) ){
flurydat <- paste("f", i, " <- read.table(files[", i, "], sep = \"\")", sep = "" )
try(eval(parse(text = flurydat)))
 f1 <- read.table(files[1], sep = "")

}

f1$V1 <- factor(f1$V1, levels = c(0,1), labels = c("Af", "Apf"))
names(f1) <- c("Species", "Ant.Length", "Wing.Length")
midge <- f1
rm(f1)

names(f2) <- c("MFB", "BAM", "TFH", "LGAN", "LTN", "LTG")
swiss.heads <- f2
rm(f2)

names(f3) <- c("Wing.Length", "Frequency")
pipits <- f3
rm(f3)

f4$V1 <- factor(f4$V1, levels = c(1,2), labels = c("Male", "Female"))
names(f4) <- c("Gender", "Length", "Width", "Height")
turtles <- f4
rm(f4)


names(f5) <- c("Wing.Length", "Frequency")
row.names(f5) <- c("1.0-1.2", "1.2-1.4", "1.4-1.6", "1.6-1.8", "1.8-2.0", "2.0-2.2", "2.2-2.4", "2.4-2.6", "2.6-2.8", "2.8-3.0")
angels <- f5
rm(f5)


f6$V1 <- factor(f6$V1, levels = c(1,2), labels = c("oleracea", "carduorum"))
names(f6) <- c("Species", "TG", "Elytra", "Second.Antenna", "Third.Antenna")
flea.beetles <- f6
rm(f6)

f7$V1 <- factor(f7$V1)
names(f7) <- c("Machine", "X1", "X2", "X3", "X4", "X5")
electrode <- f7
rm(f7)

f8$V1 <- factor(f8$V1, levels = c(1,2), labels = c("californicus", "ochrogaster"))
names(f8) <- c("Species", "Age", "L2.Condylo", "L9.Inc.Foramen", "L7.Alveolar", "B3.Zyg", "B4.Interorbital", "H1.Skull")
f.voles <- f8
rm(f8)

f9$V1 <- factor(f9$V1, levels = c(1,2,0), labels = c("multiplex", "subterraneus", "unknown"))
names(f9) <- c("Group", "M1Left", "M2Left", "M3Left", "Foramen", "Pbone", "Length", "Height", "Rostrum")
microtus <- f9
rm(f9)

names(f10) <- c("S1Length", "S1Breadth", "S2Length", "S2Breadth")
sibling.heads <- f10
rm(f10)

names(f11) <- c("North", "East", "South", "West")
cork <- f11
rm(f11)

names(f12) <- c("Diameter", "Height", "Volume")
treesf <- f12
rm(f12)

names(f13) <- c("Units", "Minutes")
computers <- f13
rm(f13)

f14$V1 <- factor(f14$V1, levels = c(1,2), labels = c("monozygotic", "dizygotic"))
names(f14) <- c("Type", "STA1", "HIP1", "CHE1", "STA2", "HIP2", "CHE2")
m.twins <- f14
rm(f14)


f15$V1 <- factor(f15$V1, levels = c(1,2), labels = c("monozygotic", "dizygotic"))
names(f15) <- c("Type", "STA1", "HIP1", "CHE1", "STA2", "HIP2", "CHE2")
f.twins <- f15
rm(f15)

f16$V1 <- factor(f16$V1, levels = c(1,2,3), labels = c("setosa", "versicolor", "virginica"))
names(f16) <- c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
irisf <- f16
rm(f16)

## missing f17 wines data

dot <- scan("tab73_2.dat")
wines <- t(matrix(dot, 17,26))
wines <- as.data.frame(wines)
wines[,1] <- factor(wines[,1], levels = c(1:3), labels = c("South Africa", "Germany", "Italy"))
wines <- wines[,-2]
names(wines) <- c("Country", "Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9", "Y10", "Y11", "Y12", "Y13", "Y14", "Y15")

f18$V1 <- factor(f18$V1)
names(f18) <- c("Rootstock", "Girth.4", "Growth.4", "Girth.15", "Weight.15")
apples <- f18
rm(f18)

names(f19) <- c("Distance", "Hit.or.Miss")
steve <- f19
steve$Hit.or.Miss <- as.numeric(unclass(steve$Hit.or.Miss)) * -1 + 2
##steve$Hit.or.Miss <- as.numeric(spot)
##steve$Hit.or.Miss <-  factor(steve$Hit.or.Miss, levels  = c(0,1), labels = c("Miss", "Hit"))
rm(f19)

dead.beetles <- f20[,-1]
names(dead.beetles) <- c("Dose", "tested", "died")
rm(f20)

names(f21) <- c("Distance", "Steve", "Andy", "Chris", "Bernard")
basketball <- f21
spot <- as.numeric(basketball$Bernard) - 2
spot[spot == -1] <- NA
basketball$Bernard <- spot
rm(f21)

f22$V1 <- factor(f22$V1, levels = c(0,1), labels = c("Male", "Female"))
names(f22) <- c("Gender", "GHQ", "Number", "PsychiatricT")
ghq <- f22
rm(f22)

names(f23) <- c("Temp", "Damage")
challenger <- f23
rm(f23)

names(f24) <- c("Volume", "Rate", "Y")
vasoc <- f24
rm(f24)


f25$V1 <- factor(f25$V1)
f25$V2 <- factor(f25$V2)
f25$V3 <- factor(f25$V3)
names(f25) <- c("Infected", "Juvenile", "Brooding.Female", "Time", "Found", "Collected")
snailsf <- f25
rm(f25)


names(f26) <- c("Aspect", "Total.Trees", "Flowering.Trees")
dogwood <- f26
rm(f26)

names(f27) <- c("Femur1", "Tibia1", "Femur2", "Tibia2", "Femur3", "Tibia3")
strider <- f27
rm(f27)



names(f28) <- c("MFB", "BAM", "TFH", "LGAN", "LTN", "LTG")
f.swiss.heads <- f28
rm(f28)


names(f29) <- c("abs")
mumps <- f29
mumps <- as.vector(mumps$abs)
rm(f29)

names(f30) <- c("molar")
robustus <- f30
robustus <- as.vector(robustus$molar)
rm(f30)

names(f31) <- c("Myo", "Scyllo", "DH.I", "DH.II")
wine.sugar <- f31
rm(f31)

names(f32) <- c("Wing.length", "Number")
pipits2 <- f32
rm(f32)

names(f33) <- c("Premolar")
pachyosteus <- f33
deer <- pachyosteus$Premolar
rm(pachyosteus)
rm(f33)

rm(files)

lookup <- read.csv("../../code/lookup.csv", row.name = 1, colClasses = "character")

rm(flurydat)
rm(i)
rm(spot)
rm(dot)
