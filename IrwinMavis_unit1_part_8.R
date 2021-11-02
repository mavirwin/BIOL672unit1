print("Assignment 1, Part 8")

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
sub.pen.data=date.frame(CL,CD,FL,BM)
print(sub.pen.data)

plot181 <- ggplot(data = dataframe, mapping = aes(x = Sp, y = CL)) + geom_boxplot() + labs(x = 'Species', y = 'Culmen Length') + theme(axis.text.x=element_text(angle=15), panel.background = element_rect(fill = 'grey50'))
plot182 <- ggplot(data = dataframe, mapping = aes(x = Sp, y = CD)) + geom_boxplot() + labs(x = 'Species', y = 'Culmen Depth') + theme(axis.text.x=element_text(angle=15), panel.background = element_rect(fill = 'grey50'))
plot183 <- ggplot(data = dataframe, mapping = aes(x = Sp, y = FL)) + geom_boxplot() + labs(x = 'Species', y = 'Flipper Length') + theme(axis.text.x=element_text(angle=15), panel.background = element_rect(fill = 'grey50'))
plot184 <- ggplot(data = dataframe, mapping = aes(x = Sp, y = BM)) + geom_boxplot() + labs(x = 'Species', y = 'Body Mass') + theme(axis.text.x=element_text(angle=15), panel.background = element_rect(fill = 'grey50'))

test181 <- oneway.test(CL~Sp)
test182 <- oneway.test(CD~Sp)
test183 <- oneway.test(FL~Sp)
test184 <- oneway.test(BM~Sp)

penguin.data=data.frame(Sp, CL,CD,FL,BM)
sub.pen.data=date.frame(CL,CD,FL,BM)
print(sub.pen.data)


bind1 <- cbind(CL,CD,FL,BM)
print (bind1)
test185 <- manova(bind1~Sp, data = data.na.out2)
print (test181)
print (test182)
print (test183)
print (test184)
theMANOVA <- summary(test185, test="Pillai")
print(theMANOVA)

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
sink()

cat("Dear Window 10, \nYou are so helpful with writing part 8. \nLove, Mavis")