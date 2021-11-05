print("Assignment 1, Part 9")

library("ggplot2")
library("reshape2")
library("dplyr")
library("palmerpenguins")

data=read.csv("https://raw.githubusercontent.com/netuohcs/BiBC_essentials_20200916/master/data/penguins_lter.csv")
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
sub.pen.data=data.frame(CL,CD,FL,BM)
print(sub.pen.data)

test1 = lm(CL ~ CD + FL + BM, data = data.na.out2)
test1 = lm(formula = CL ~ CD + FL + BM, data = data.na.out2, subset=Sp=="Adelie Penguin (Pygoscelis adeliae)")

bind1= cbind(CL,CD,FL,BM)
print (bind1)
plot191 =pairs(bind1, col=Sp)
theMLR = summary(test1)
print(theMLR)
print (plot191)
sink("statsMLR_all.txt")
print (theMLR)

VI9=cat("Red is Adelie, Yellow is Chinstrap, Purple is Garoon. My "bonus" is how we can know which color is represented. \n
But that is beyond the purpose of this assignment. The MLR thing's 

#make an external file
sink(file="C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/MLR.txt")
print(theMLR)
print(plot191)
sink()


cat("All right! \nWhere did those birds hide?")
