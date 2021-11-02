print("Assignment 1, Part 10")

library("ggplot2")
library("reshape2")
library("dplyr")
library("palmerpenguins")

data=read.table("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/penguins_lter.txt", header=TRUE, sep="\t")
setwd("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive")
getwd()

#remove NA observations
data.na.out2=na.omit(data)

#listing of columns
Sp=data.na.out2$Species
Is=data.na.out2$Island
CL=data.na.out2$Culmen.Length..mm.
CD=data.na.out2$Culmen.Depth..mm.
FL= data.na.out2$Flipper.Length..mm.
BM= data.na.out2$Body.Mass..g.
Sex= data.na.out2$Sex

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

penguin.data=data.frame(Sp, CL,CD,FL,BM)
sub.pen.data=date.frame(CL,CD,FL,BM)
print(sub.pen.data)

#aov test
test1=aov(formula = CL~CD*BM, data=data.na.out2)
test2=aov(formula=CL~CD*Sp, data=data.na.out2)
bind1=cbind(CL,CD,FL,BM)
print(bind1)
plot171=pairs(bind1, col=Sp)

#ANCOVA
theANCOVA= summary(test1)
print(theANCOVA)
print(plot171)

#make an external file
sink(file="C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/ANCOVA.txt")
print(theANCOVA)
sink()

#Bye
