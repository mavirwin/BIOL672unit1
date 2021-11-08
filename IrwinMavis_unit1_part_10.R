print("Assignment 1, Part 10")

library("ggplot2")
library("reshape2")
library("dplyr")
library("palmerpenguins")

data=read.csv("https://raw.githubusercontent.com/netuohcs/BiBC_essentials_20200916/master/data/penguins_lter.csv")
data.int=read.csv("https://raw.githubusercontent.com/mavirwin/raw-data/main/penguins_lter.integer.csv")
#data=read.table("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/penguins_lter.txt", header=TRUE, sep="\t", )
#setwd("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive")
getwd()

#remove NA observations
data.na.out2=na.omit(data)

#listing of columns
Sp=as.factor(data.na.out2$Species)
Is=as.factor(data.na.out2$Island)
CL=data.na.out2$Culmen.Length..mm.
CD=data.na.out2$Culmen.Depth..mm.
FL= data.na.out2$Flipper.Length..mm.
BM= data.na.out2$Body.Mass..g.
Sex=as.factor(data.na.out2$Sex)

#used to work, but now empty for both numeric and integer???
Sp.int=as.numeric(data.na.out2$Sp.as.int)

#multvariance by Species
penguin.data=data.frame(
  Sp,#categorical variable
  CL,
  CD,
  FL,
  BM
)

print(penguin.data)

data2=(summary(penguin.data))
print(data2)

penguin.data=data.frame(Sp,CL,CD,FL,BM)
sub.pen.data=data.frame(CL,CD,FL,BM)
print(sub.pen.data)

#aov test
test1=aov(formula = CL~CD*BM, data=data.na.out2)
test2=aov(formula=CL~CD*FL, data=data.na.out2)

print(test1)
print(test2)

bind1=cbind(CL,CD,FL,BM)
print(bind1)

#
#legend() how do we add legend of the color assigned species in plot1101?
plot1101=pairs(bind1, col=Sp)

#ANCOVA
theANCOVA= summary(test1)
theANCOVA2= summary(test2)
print(theANCOVA)
print(theANCOVA2)
print(plot1101)

note1=cat("The low F-values and extremely low p-values evaluated against body mass or flipper length using \n
ANCOVA test appears to be statistically significant.I wanted to try with Species as integer, \n
          something is up with that one being empty. It did work last week...")

#make an external file
sink(file="C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/ANCOVA.txt")
print(theANCOVA)
print(note1)
sink()

cat("All right! \nWhere did those birds hide?")
