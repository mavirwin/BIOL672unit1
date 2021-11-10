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
#data.na.out2=na.omit(data)
data.na.out2=na.omit(data.int)

#listing of columns
Sp=as.factor(data.na.out2$Species)
Is=as.factor(data.na.out2$Island)
CL=data.na.out2$Culmen.Length..mm.
CD=data.na.out2$Culmen.Depth..mm.
FL= data.na.out2$Flipper.Length..mm.
BM= data.na.out2$Body.Mass..g.
Sex=as.factor(data.na.out2$Sex)

#used to work, but now empty for both numeric and integer???
Sp.int=data.na.out2$Sp.as.int
print(data.na.out2$Sp.as.int)

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

#normalize the data (because the measurements of these multivariates has different measuring ranges and
#we want for them to contribute equally to the analysis

#define min-max normalization
min_max_norm = function(x) {
  (x-min(x))/(max(x)-min(x))
}
#set normalize dataframe
norm.data=as.data.frame(lapply(sub.pen.data, min_max_norm))
head(norm.data)
#add Species
norm.data$Species=data.na.out2$Species
head(norm.data.sp)


#aov test
# test1=aov(formula = CL~CD*BM, data=data.na.out2)
# test2=aov(formula=Sp.int~CD*CL+FL, data=data.na.out2)
# test3=aov(formula=BM~CD*CL+FL, data=data.na.out2)
test4=aov(formula=Sp.int~CD*CL+FL+BM, data=norm.data)
# test5=aov(formula=BM~CD*FL, data=data.na.out2)
bind1=cbind(CL,CD,FL,BM)
print(bind1)

#plot that
plot1101=pairs(bind1, col=Sp)

#ANCOVA
# theANCOVA= summary(test1)
# theANCOVA2= summary(test2)
# theANCOVA3= summary(test3)
theANCOVA4= summary(test4)
# theANCOVA5= summary(test5)
# print(theANCOVA)
# print(theANCOVA2)
# print(theANCOVA3)
print(theANCOVA4)
# print(theANCOVA5)
print(plot1101)

note1=cat("The higher F-values and low p-values evaluated using the \n
ANCOVA test suggests that the CL and CD measurements grouped by species is statistical significance. FL between
species is somewhat significance, but not BM.")

# #make an external file
# sink(file="C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/ANCOVA.txt")
# print(theANCOVA)
# print(note1)
# sink()

cat("All right! \nWhere did those birds hide?")
