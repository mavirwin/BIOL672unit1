print("Assignment 1, Part 11")

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

#Prepare dataframe for PCA
#penguin.data=data.frame(Sp, CL,CD,FL,BM)
sub.pen.data=data.frame(CL,CD,FL,BM)  #categorical Variance not included
print(sub.pen.data)

#PCA, using the princomp and loading functions similar to 
#PCA2 example, for the unrotated type of PCA function

PCA1111= princomp(sub.pen.data, cor=TRUE) 
test1= summary(PCA1111) # print variance accounted for 
theload= loadings(PCA1111) # pc loadings 
thescores= PCA1111$scores # the principal components

print(theload)
#scree plot
plot(PCA1111,type="lines") # shows the variances of principal components
biplot(PCA1111) #multi-dimersional PCA of four measurements.

print(PCA1111)
print(test1)
print(theload)
print(thescores)
#print(plot1) #in PCA2 example, 

#above is based on the PCA2 example, using princomp
#below is using factanal for maximum likelihood factor analysis (MLFA)

MLFA=factanal(sub.pen.data, 1, rotation = "varimax") 
#factors above 1 has factor error as being too many, a possible degree of freedom problem?
print(MLFA)

#idea? Use the principal function to extract and rotate PCs...
#install.packages("psych")
library("psych")
PCnum=principal(sub.pen.data, nfactors=2, rotate="varimax")
print(PCnum)

#try factor 1 by factor 2
#load2=sub.pen.data$loadings [,1:2] #empty
# plot(load2)
# print(load2)

#above is using factanal
#below is based on the in PCA1 example, using prcomp

#PCA
PCA1111=prcomp(sub.pen.data, scale = TRUE) #a function built in R
plot(PCA1111$x[,1], PCA1111$x[,2])
plot(PCA1111$x[,2], PCA1111$x[,3])
plot(PCA1111$x[,3], PCA1111$x[,4])


# make a scree plot
PCA1111.var= PCA1111$sdev^2  #standard error of the PCA variance
PCA1111.var.per= round(PCA1111.var/sum(PCA1111.var)*100, 1) #total variance of each PC multiplied by 100 

barplot(PCA1111.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
loading_scoresPC1= PCA1111$rotation[,1]
print(loading_scoresPC1)
loading_scoresPC2= PCA1111$rotation[,2]
print(loading_scoresPC2)
loading_scoresPC3= PCA1111$rotation[,3]
print(loading_scoresPC3)
loading_scoresPC4= PCA1111$rotation[,4]
print(loading_scoresPC4)

note1=cat("We have three PCA approaches done for four measurements, separated by princomp, factanal, and prcomp.\n
          With the groups without the charactor factor of species, yields as two components. \n
          According to the loading scores, PC1 and maybe PC3 appear to have equal strengths. \n
          PC2 appears to have the CL and CD driven impressions.\n
           PC4 appears to be FL driven.")

# #make an external file
# sink(file="")
# print()
# print(note1)
# sink()

cat("Is the multidimensional dataset now nevermore? \n Short answer: No. \n Have a nice day.")