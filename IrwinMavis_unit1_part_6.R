print("Assignment 1, Part 6")

library("ggplot2")
library("reshape2")
library("dplyr")
library("ggpubr")
library("janitor")
library("palmerpenguins")

#data=read.csv("https://raw.githubusercontent.com/netuohcs/BiBC_essentials_20200916/master/data/penguins_lter.csv")
data.int=read.csv("https://raw.githubusercontent.com/mavirwin/raw-data/main/penguins_lter.integer.csv")
#data=read.table("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/penguins_lter.txt", header=TRUE, sep="\t", )
#setwd("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive")
getwd()

#remove NA observations
#data.na.out=filter(!is.na(data))
data.na.out2=na.omit(data.int)


#listing of columns
#studyName=data.na.out2$studyName
Species=as.factor(data.na.out2$Species)
Species.int=as.numeric(data.na.out2$Sp.as.int)
# Region=data.na.out2$Region
# Island=data.na.out2$Island
# Stage=data.na.out2$Stage
# Date.Egg=data.na.out2$Date.Egg
Culmen.Length..mm=data.na.out2$Culmen.Length..mm.
Culmen.Depth..mm=data.na.out2$Culmen.Depth..mm.
Flipper.Length..mm=data.na.out2$Flipper.Length..mm.
Body.Mass..g=data.na.out2$Body.Mass..g.

penguin.data=data.frame(
  Species,
  Culmen.Length..mm,
  Culmen.Depth..mm,
  Flipper.Length..mm,
  Body.Mass..g
)

print(penguin.data)

data2=(summary(penguin.data))
print(data2)

#one variance by Species
body.mass.data=data.frame(
  Species.int,
  Species,
  Body.Mass..g
)

print(body.mass.data)

# #two variance by Species
# culmen.data=data.frame(
#   Species,
#   Culmen.Depth..mm,
#   Culmen.Length..mm
# )
# 
# print(culmen.data)

#simple linear regression

BM_lm=lm(Body.Mass..g~Species, data=body.mass.data)
summary(BM_lm)

#sink("myfile.txt")
print(summary(BM_lm))
#sink()
dev.off()

#scatterplot
plot161=ggplot(body.mass.data, aes(x=Species, y=Body.Mass..g))+
  geom_point()+
  geom_smooth(method=lm) #regression line

print(plot161)

# #make an external file
 #sink(file="C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/linear_regression.pdf")
 #pdf("linear_regression.pdf")
# sink()

cat("Dear Window 10, \nYou are so helpful with writing part 6. \nLove, Mavis")