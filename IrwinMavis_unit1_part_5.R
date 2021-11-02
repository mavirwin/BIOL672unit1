print("Assignment 1, Part 5")

library("ggplot2")
library("reshape2")
library("dplyr")
library("ggpubr")
library("janitor")

data=read.table("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/penguins_lter.txt", header=TRUE, sep="\t")
setwd("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive")
getwd()

#remove empty rows and columns (nothing to remove)
data %>%
  remove_empty(which= c("rows","cols"), quiet = FALSE)

#remove NA observations
#data.na.out=filter(!is.na(data))
data.na.out2=na.omit(data)

studyName=data.na.out2$studyName
Species=data.na.out2$Species
Region=data.na.out2$Region
Island=data.na.out2$Island
Stage=data.na.out2$Stage
Date.Egg=data.na.out2$Date.Egg
Culmen.Length..mm=data.na.out2$Culmen.Length..mm.
Culmen.Depth..mm=data.na.out2$Culmen.Depth..mm.
Flipper.Length..mm=data.na.out2$Flipper.Length..mm.
Body.Mass..g=data.na.out2$Body.Mass..g.


penguin.data=data.frame(
  Species,
  Island,
  Culmen.Length..mm,
  Culmen.Depth..mm,
  Flipper.Length..mm,
  Body.Mass..g
  # )%>%
  # filter(!is.na(penguin.data)
)

print(penguin.data)

data2=(summary(penguin.data))
print(data2)

#one variance by Species
body.mass.data=data.frame(
  Species,
  Body.Mass..g
)

print(body.mass.data)

#two variance by Species
culmen.data=data.frame(
  Species,
  Culmen.Depth..mm,
  Culmen.Length..mm
)

print(culmen.data)

#Kruskal Wallis test
testKW=kruskal.test(Body.Mass..g~Species,data=body.mass.data)

#Correlations, Pearson
#Spec.fac=data.na.out2$Species=as.factor(data.na.out2$Species)
#testcorP=cor.test(Culmen.Depth..mm,Culmen.Length..mm,use="complete.obs",method="pearson")
testcorP=cor.test(Body.Mass..g,Species,use="complete.obs",method="pearson")
#testcorP=cor.test(Species, Body.Mass..g, method="pearson")
#corP=cor(Body.Mass..g, Species,use="complete.obs",method="pearson")

#Heatmap
# col=colorRampPalette(c("red", "white", "blue"))
# heatmap (x=corP, col=col, symm=TRUE)

#Correlations, Spearman

# testcorS=cor.test(Culmen.Depth..mm,Culmen.Length..mm,use="complete.obs",method="spearman")
# testcorP=cor.test(Body.Mass..g,Species,use="complete.obs",method="pearson")

#scatterplots
plot151=ggplot(x=testcorP, aes(x=Culmen.Depth..mm, y=Culmen.Length..mm)) +
                  geom_point()

plot152=ggplot(penguin.data,x="Culmen.Depth..mm", y="Culmen.Length..mm", 
                  add="reg.line", conf.int=TRUE, 
                  cor.coef= TRUE, cor.method = "pearson",
                  xlab="Culmen Depth", ylab="Culmen Length")
print(plot152)

#Sample KS test
testks=ks.test(Species, Body.Mass..g)
testks=ks.test(Culmen.Depth..mm,Culmen.Length..mm)


note1=cat("Does the assumption work? (needs to get the code working first...)")

#make an external file
sink(file="C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/significant_tests.txt")
print(testKW)
print(testcorP)
print(testcorS)
print(testks)
print(note1)
sink()

cat("Dear Window 10, \nYou are so helpful with writing part 5. \nLove, Mavis")
