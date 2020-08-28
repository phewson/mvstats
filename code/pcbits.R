hep <- read.csv("Heptatholon.csv")
hep.df$X100mHurdles.S. <- hep.df$X100mHurdles.S. * -1
hep.df$X200m.sec. <- hep.df$X200m.sec. * -1
hep.df$r800m.s. <- hep.df$r800m.s. * -1

library(pcaPP)
pc <- prcomp(scale(hep.df))
pcP <- PCAproj(ScaleAdv(hep.df)$x, 2)


par(mfrow = c(1,2))
biplot(pcP, cex = 0.5)
##windows()
biplot(pc, cex = 0.5)


summary(pc)
summary(pcP)


> pc$rotation
                       PC1         PC2         PC3          PC4         PC5
X100mHurdles.S. -0.4656494  0.28868133 -0.32883275  0.003922038 -0.32294832
HighJump.m.     -0.2455859 -0.56442826  0.10737271  0.610427608 -0.41590153
ShotPut.m.      -0.4195748 -0.07137064  0.52173345 -0.235891780  0.41044566
X200m.sec.      -0.4330174 -0.02204083 -0.51825157 -0.357022268 -0.10408281
LongJump.m.     -0.4630436  0.11516723 -0.12459693  0.480637087  0.58118970
Javelin.m.      -0.3228125  0.38161226  0.56832241 -0.091073036 -0.45330419
r800m.s.        -0.2017272 -0.65849286  0.03216966 -0.452710298  0.01285556
                        PC6         PC7
X100mHurdles.S.  0.03196360 -0.69740237
HighJump.m.     -0.24458106  0.06452585
ShotPut.m.      -0.52729762 -0.21095984
X200m.sec.      -0.30913986  0.55638107
LongJump.m.      0.39994501  0.16749107
Javelin.m.       0.32350107  0.32976035
r800m.s.         0.54857930 -0.13640838
> pcp$loadings
Error: object "pcp" not found
> pcP$loadings

Loadings:
                Comp.1 Comp.2
X100mHurdles.S. -0.584  0.189
HighJump.m.     -0.186 -0.663
ShotPut.m.      -0.419 -0.106
X200m.sec.      -0.432       
LongJump.m.     -0.438       
Javelin.m.      -0.246  0.305
r800m.s.        -0.100 -0.643

               Comp.1 Comp.2
SS loadings     1.000  1.000
Proportion Var  0.143  0.143
Cumulative Var  0.143  0.286
> 


I should do a qq plot on the scores!



ScaleAdv() and ScaleAdvR() carry out scaling with different defaults.   The package also introduces an l1median()

Some subtle differences can be seen with these data.
