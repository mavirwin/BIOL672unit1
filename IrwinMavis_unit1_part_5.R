print("Assignment 1, Part 5")

library("ggplot2")
library("reshape2")
library("dplyr")
library("ggpubr")
library("janitor")
library("palmerpenguins")

data=read.csv("https://raw.githubusercontent.com/netuohcs/BiBC_essentials_20200916/master/data/penguins_lter.csv")
data.int=read.csv("https://raw.githubusercontent.com/mavirwin/raw-data/main/penguins_lter.integer.csv")
#data=read.table("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/penguins_lter.txt", header=TRUE, sep="\t", )
#setwd("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive")
getwd()

#remove empty rows and columns (result:nothing to remove)
# data %>%
#   remove_empty(which= c("rows","cols"), quiet = FALSE)

#remove NA observations
#data.na.out=filter(!is.na(data))
data.na.out2=na.omit(data.int)

#turn Species string into integers
# print(Species)
# Sp.int=strtoi(as.character(Species),30)
# print(Sp.int)
#result: cannot use strtoi because of no base


#Sp.int2=data.frame(c("Adelie

#install new column
#unable to figure via R, so added a column to raw data and rename file as "penguin_itel.int...." 

# studyName=data$studyName
# Species=data$Species
# Region=data$Region
# Island=data$Island
# Stage=data$Stage
# Date.Egg=data$Date.Egg
# Culmen.Length..mm=data$Culmen.Length..mm.
# Culmen.Depth..mm=data$Culmen.Depth..mm.
# Flipper.Length..mm=data$Flipper.Length..mm.
# Body.Mass..g=data$Body.Mass..g.

# studyName=data.na.out2$studyName
Species=as.factor(data.na.out2$Species)
# Species2=as.numeric(data.na.out2$Species)
# Region=data.na.out2$Region
# Island=data.na.out2$Island
# Stage=data.na.out2$Stage
# Date.Egg=data.na.out2$Date.Egg
Species.int=as.numeric(data.na.out2$Sp.as.int)
Culmen.Length..mm=data.na.out2$Culmen.Length..mm.
Culmen.Depth..mm=data.na.out2$Culmen.Depth..mm.
Flipper.Length..mm=data.na.out2$Flipper.Length..mm.
Body.Mass..g=data.na.out2$Body.Mass..g.


penguin.data=data.frame(
  Species,
  Species.int,
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

#print(culmen.data)

#Kruskal Wallis test
testKW=kruskal.test(Body.Mass..g~Species,data=body.mass.data)

#Correlations, Pearson
#Spec.fac=data.na.out2$Species=as.factor(data.na.out2$Species)
#testcorP=cor.test(Culmen.Depth..mm,Culmen.Length..mm,use="complete.obs",method="pearson")
testcorP=cor.test(Body.Mass..g,Species.int,use="complete.obs",method="pearson")
#testcorP=cor.test(Species, Body.Mass..g, method="pearson")
corP=cor(Body.Mass..g, Species.int,use="complete.obs",method="pearson")



#Heatmap
# col=colorRampPalette(c("red", "white", "blue"))
# heatmap (x=corP, col=col, symm=TRUE)

#Correlations, Spearman

# testcorS=cor.test(Culmen.Depth..mm,Culmen.Length..mm,use="complete.obs",method="spearman")
# testcorP=cor.test(Body.Mass..g,Species.int,use="complete.obs",method="pearson")

#scatterplots
plot151=ggplot(body.mass.data, aes(x=Species.int, y=Body.Mass..g)) +
                  geom_point()

print(plot151)

plot152=ggplot(body.mass.data,x="Species.int", y="Body.Mass..g", 
               add="reg.line", conf.int=TRUE, 
               cor.coef= TRUE, cor.method = "pearson",
               xlab="Species", ylab="Body Mass")

# plot152=ggplot(penguin.data,x="Culmen.Depth..mm", y="Culmen.Length..mm", 
#                   add="reg.line", conf.int=TRUE, 
#                   cor.coef= TRUE, cor.method = "pearson",
#                   xlab="Culmen Depth", ylab="Culmen Length")
print(plot152)

#Sample KS test
testks=ks.test(Species, Body.Mass..g)
testks=ks.test(Culmen.Depth..mm,Culmen.Length..mm)


note1=cat("Does the assumption work? (needs to get the code working first...)")

#make an external file
sink(file="C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/significant_tests.txt")
print(testKW)
print(testcorP)
print(testcorS)
print(testks)
print(note1)
sink()

cat("Dear Window 10, \nYou are so helpful with writing part 5. \nLove, Mavis")