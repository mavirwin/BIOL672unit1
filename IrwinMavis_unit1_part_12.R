print("Assignment 1, Part 12")

library("ggplot2")
library("palmerpenguins")

#data=read.csv("https://raw.githubusercontent.com/netuohcs/BiBC_essentials_20200916/master/data/penguins_lter.csv")
data.int=read.csv("https://raw.githubusercontent.com/mavirwin/raw-data/main/penguins_lter.integer.csv")
#data=read.table("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/penguins_lter.txt", header=TRUE, sep="\t", )
#setwd("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive")
getwd()

#remove NA observations
data.na.out2=na.omit(data.int)

#listing of columns
Sp=as.factor(data.na.out2$Sp.as.int)
Sp=as.numeric(data.na.out2$Sp.as.int)
Is=as.factor(data.na.out2$Island)
CL=data.na.out2$Culmen.Length..mm.
CD=data.na.out2$Culmen.Depth..mm.
FL= as.numeric(data.na.out2$Flipper.Length..mm.)
BM= as.numeric (data.na.out2$Body.Mass..g.)
Sex=as.factor(data.na.out2$Sex)

#Prepare dataframes
penguin.data=data.frame(Sp, CL,CD,FL,BM)
sub.pen.data=data.frame(CL,CD,FL,BM)  #categorical Variance not included
head(sub.pen.data)

#determining the number of factors to extract
#install.packages("nFactors")
library("nFactors")
eigen=eigen(cor(sub.pen.data)) #get eigenvalues
setpar=parallel(subject=nrow(sub.pen.data), var=ncol(sub.pen.data), rep=100, cent=.05)
scree=nScree(x=eigen$values, aparallel = setpar$eigen$qevpea)
plotnScree(scree)

note1=cat("Based on the eigenvalues in the plotnScree, I would like to use... \n
         ")

factanal(penguin.data, 2, rotation = "varimax")

# 
# print(penguin.data)


