print("Assignment 1, Part 8")

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
  Sp,
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

plot181 <- ggplot(data = penguin.data, mapping = aes(x = Sp, y = CL)) + geom_boxplot() + labs(x = 'Species', y = 'Culmen Length') + theme(axis.text.x=element_text(angle=15), panel.background = element_rect(fill = 'grey50'))
plot182 <- ggplot(data = penguin.data, mapping = aes(x = Sp, y = CD)) + geom_boxplot() + labs(x = 'Species', y = 'Culmen Depth') + theme(axis.text.x=element_text(angle=15), panel.background = element_rect(fill = 'grey50'))
plot183 <- ggplot(data = penguin.data, mapping = aes(x = Sp, y = FL)) + geom_boxplot() + labs(x = 'Species', y = 'Flipper Length') + theme(axis.text.x=element_text(angle=15), panel.background = element_rect(fill = 'grey50'))
plot184 <- ggplot(data = penguin.data, mapping = aes(x = Sp, y = BM)) + geom_boxplot() + labs(x = 'Species', y = 'Body Mass') + theme(axis.text.x=element_text(angle=15), panel.background = element_rect(fill = 'grey50'))

test181 <- oneway.test(CL~Sp)
test182 <- oneway.test(CD~Sp)
test183 <- oneway.test(FL~Sp)
test184 <- oneway.test(BM~Sp)


bind1 <- cbind(CL,CD,FL,BM)
print (bind1)
test185 <- manova(bind1~Sp, data = data.na.out2)

print (test181)
print (test182)
print (test183)
print (test184)
theMANOVA <- summary(test185, test="Pillai")
print(theMANOVA)

VI8=cat("The one-way test for each type of measurements, one dependent variable at a time, between Species to be statistically significance \n
Alas, the MANOVA's Pillai test looks for any differiences between multiple independent and dependent groups, resulting in a p-value<0.000-plus \n
being statistically significance. But I am not sure whether the F-value of 369 is considered low or not.") 

library('grid')
pushViewport(viewport(layout = grid.layout(2, 2)))
print(plot181, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot182, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(plot183, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot184, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

sink(file="C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/MANOVA.txt")
print (test181)
print (test182)
print (test183)
print (test184)
theMANOVA = summary(test185, test="Pillai")
print(theMANOVA) 
print(VI8)
sink()

cat("All right! \nWhere did those birds hide?")
