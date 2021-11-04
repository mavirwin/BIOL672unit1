print("Assignment 1, Part 4")

library("ggplot2")
library("reshape2")
library("dplyr")

data=read.csv("https://raw.githubusercontent.com/netuohcs/BiBC_essentials_20200916/master/data/penguins_lter.csv")
#data=read.table("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/penguins_lter.txt", header=TRUE, sep="\t", )
#setwd("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive")
getwd()

data.na.out2=na.omit(data)

#name set ups
#studyName=data.na.out2$studyName
Species=as.factor(data.na.out2$Species)
#Region=data.na.out2$Region
#Island=data.na.out2$Island
#Stage=data.na.out2$Stage
#Date.Egg=data.na.out2$Date.Egg
Culmen.Length=data.na.out2$Culmen.Length..mm.
Culmen.Depth=data.na.out2$Culmen.Depth..mm.
Flipper.Length=data.na.out2$Flipper.Length..mm.
Body.Mass=data.na.out2$Body.Mass..g.

#one variance by Species
body.mass.data=data.frame(
  Species,
  Body.Mass
)

print(body.mass.data)

#scatter, colored
plot1=ggplot(body.mass.data, aes(x=Species, y=Body.Mass, shape=Species, color=Species)) +
  geom_point()
print(plot1)

#mean body mass by Specie
#BMname=tapply(Species,Species) #tried tricking, but doesnt work
BMmean=tapply(Body.Mass,Species,mean)
BMsd=tapply(Body.Mass,Species,sd)
#BMse=tapply(Body.Mass,Species,se) tapply function does not know what se is?

#oneway ANOVA
# CL.anova=oneway.test(Culmen.Length~Species)
# CD.anova=oneway.test(Culmen.Depth~Species)
BM.anova=oneway.test(Body.Mass~Species)
# print(CL.anova)
# print(CD.anova)
print(BM.anova)

#simple boxplot chart, colored

plot3=ggplot(body.mass.data, aes(x=Species, y=Body.Mass, color=Species, fill=Species)) +
  geom_boxplot()

plot5=plot3 + scale_color_manual(values=c("black","black","black")) +
  scale_fill_manual(values = c("red", "yellow","blue"))
plot5=plot3 + scale_fill_manual(breaks= body.mass.data$Species, values = c("red", "yellow","blue"))
print(plot5)

#pairwise t test, Bonferroni
#Error in factor(g) : argument "g" is missing, with no default
test1=pairwise.t.test(Body.Mass, Species, p.adjust.method = "bonf")

#pairwise t test, Benjamini-Hochberg test
test2=pairwise.t.test(Body.Mass, Species,p.adjust.method = "BH")

#Comments
VI4E=cat("For the body mass between Adelie vs. Chinstrap, the P value adjustment bonf method was 1, \n
#and the BH method was 0.7. The body mass of Adelie vs Chinstrap can be suggested to be statistally \n
nonsignificace. Besides those, the Adilie vs. Gentoo and Chinscrap vs. Gentoo pairings are well below p-value<0.05, \n
at about 2e-16 for both, suggesting that Gentoo may benefit being a statisally significance different size.")

#make an external file
sink(file="C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/penguin.txt")
print(body.mass.data)
print(BMmean)
print(BMsd)
print(BM.anova)
print(test1)
print(test2)
print(VI4E)
sink()

#save plots on same pdf page
#some problems making this right
library(grid)
pdf("Unit1Part4Plots.pdf")
pushViewport(viewport(layout = grid.layout(2, 2)))
print(plot3, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot5, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
#print(plot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
dev.off()

cat("Dear Window 10, \nYou are so helpful with writing part 4, especially the tapply function. \nLove, Mavis")
