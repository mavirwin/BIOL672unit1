print("Assignment 1, Part 13")

library("ggplot2")
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

#Prepare dataframe 
#penguin.data=data.frame(Sp, CL,CD,FL,BM)
sub.pen.data=data.frame(CL,CD,FL,BM)  #categorical Variance not included
print(sub.pen.data)

test= kmeans(sub.pen.data, 3, nstart = 3)
plot1131=ggplot(data.na.out2, aes(CD, CL, color = Sp)) + geom_point()
plot1132=ggplot(data.na.out2, aes(FL, BM, color = Sp)) + geom_point() 

theKMEANS= summary(test)
print(theKMEANS)


test$cluster = as.factor(test$cluster)
plot1133 =ggplot(data.na.out2, aes(CL, CD, color = test$cluster)) + geom_point()                                 
plot1134 =ggplot(data.na.out2, aes(FL, BM, color = test$cluster)) + geom_point()   

note1=cat("For three penguin species, the question is whether two or three clusters make the most sense.")

library('grid')
pushViewport(viewport(layout = grid.layout(2, 2)))
print(plot1131, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot1132, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(plot1133, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot1134, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

cat("Since K-means is on the 3D scale, how can we rotate the 2-D figures as such? \n Good night.")