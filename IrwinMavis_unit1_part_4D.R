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

BMmean=tapply(Body.Mass,Species,mean)
BMsd=tapply(factor(Body.Mass),Species,sd)

print(BMsd[1])
#SpID= c("Bob", "Ann", "Chad")

#BMdf=data.frame(SpID, BMmean, BMsd, header=TRUE, stringsAsFactors = TRUE)

#mean by aggregate function
# BMmean=aggregate(x=data.na.out2$Body.Mass, by=list(data.na.out2$Species), FUN= mean)
# BMsd=aggregate(x=data.na.out2$Body.Mass, by=list(data.na.out2$Species), FUN= sd)


# #mean culmen by Specie
# CLmean=tapply(Culmen.Length,Species,mean)
# CDmean=tapply(Culmen.Depth,Species,mean)
# 
# #sd culmen by Specie
# CLsd=tapply(Culmen.Length,Species,sd)
# CDsd=tapply(Culmen.Depth,Species,sd)

#oneway ANOVA
# CL.anova=oneway.test(Culmen.Length~Species)
# CD.anova=oneway.test(Culmen.Depth~Species)
BM.anova=oneway.test(Body.Mass~Species)
# print(CL.anova)
# print(CD.anova)
print(BM.anova)

#simple boxplot chart, colored

plot3=ggplot(body.mass.data, aes(x=Species, y=Body.Mass, color=Species)) +
  geom_boxplot()
print(plot3)

#boxplot and error bar chart, colored, detailed
#problems with using additional features. 

#1---Error in FUN(left) : invalid argument to unary operator

#2---when remove geom_errorbar, got this error: Error: geom_boxplot requires 
#the following missing aesthetics: lower, upper, middle, ymin and ymax or xlower, 
#xupper, xmiddle, xmin and xmax
#2's problem looks like the code "forgot" the information in first line?

#print(body.mass.data)


print(summary(body.mass.data))


plot4=ggplot(body.mass.data, aes(x=Species, y=Body.Mass, color=Species)) + 
  geom_boxplot(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(data = body.mass.data, aes(ymin=Body.Mass-BMsd, ymax=Body.Mass+BMsd), width=0.01, col="black")
print(plot4)

#pairwise t test, Bonferroni
#Error in factor(g) : argument "g" is missing, with no default
test1=pairwise.t.test(Body.Mass, Species, p.adjust.method = "bonf")

#pairwise t test, Benjamini-Hochberg test
test2=pairwise.t.test(Body.Mass, Species,p.adjust.method = "BH")

#Comments
cat("For the body mass between Adelie Penguin vs. Chinstrap penguin, the P value adjustment bonf method was 1, and the BH method was 0.7. Besides those, the other pairings were 2e-16 for both approaches.")

#make an external file
sink(file="C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/penguin.txt")
print(body.mass.data)
print(BMmean)
print(BMsd)
print(BM.anova)
print(test1)
print(test2)
sink()

#save plots on same pdf page
#some problems making this right
library(grid)
pdf("Unit1Part4Plots.pdf")
pushViewport(viewport(layout = grid.layout(2, 2)))
print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
#print(plot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
dev.off()

cat("Dear Window 10, \nYou are so helpful with writing part 4, especially the tapply function. \nLove, Mavis")
