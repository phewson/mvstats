# From Michael Friendly's pages
d.hair.eye<-matrix(c(68,119,26,7,20,84,17,94,15,54,14,10,5,29,14,16),
  ncol=4,byrow=T)
rownames(d.hair.eye)<-c("brown","blue","hazel","green")
colnames(d.hair.eye)<-c("BLACK","BROWN","RED","BLOND")

f.corresp.anal(d.hair.eye)
mosaic.plot(t(d.hair.eye))

# From the text book
d.hair.eye2<-matrix(c(688,116,584,188,4,326,38,241,110,3,343,84,909,412,
  26,98,48,403,681,81),ncol=5,byrow=T,dimnames=list(
  c("Light","Blue","Medium","Dark"),
  c("FAIR","RED","MEDIUM","DARK","BLACK")))

f.corresp.anal(d.hair.eye2)
mosaic.plot(t(d.hair.eye2))
