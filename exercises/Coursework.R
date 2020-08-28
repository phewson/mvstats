require(MASS)

sigma <- matrix(c(1.0, 0.6328, 0.2412, 0.0586, 0.6328,1,-0.0553, 0.0655, 
0.2412, -0.0553, 1, 0.4248, 0.0586, 0.0655, 0.4248, 1),4,4)
var <- c(5,2,3,1)
spot <- mvrnorm(140, mu = c(40,55,63,62), Sigma = sigma * outer(var,var))
cor(spot)
var(spot)
spot <- as.data.frame(spot)
names(spot) <- c("ReadSpeed", "ReadPower", "ArithSpeed", "ArithPower")



command <- paste("write.table(spot, \"u:/teaching/mvm/", studentnames[i], "hotelling.csv\", sep = \",\", col.names =TRUE)")



studentnames <- c("ALLYGINAR", "BASSCALLUMJ", "BENSONAIMEE", "BOWERSMEGAN", "BRAYRICHARD", "BURYREBEKAHE", "CATODAVIDM", "CHENGWINGS", "CLEAVEJACQUELINE", "EATONDANIELJ", "HANNAMREBECCAL", "HUGHESSAMANTHAL", "JACKSONCLAREM", "KWANWAIM", "LANNONJENNIFERD", "LOWAIF", "MACDONALDPAULD", "NUTTALLRICHARDL", "POWELLRICHARDW", "WELCHDANIEL", "WONGHO", "WUKWONGH")


for (i in 1:22){
spot <- mvrnorm(140, mu = c(40,55,63,62), Sigma = sigma * outer(var,var))
spot <- as.data.frame(spot)
names(spot) <- c("ReadSpeed", "ReadPower", "ArithSpeed", "ArithPower")
 command <- paste("write.table(spot, \"u:/teaching/mvm/", studentnames[i], ".csv\", sep = \",\", col.names =TRUE)", sep = "")
eval(parse(text = command))
}

