print("Assignment 1, Part 15")

library("ggplot2")
library("palmerpenguins")
library("mixtools")
library("MASS")

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

#Prepare dataframes
penguin.data=data.frame(Sp, CL,CD,FL,BM)
sub.pen.data=data.frame(CL,CD,FL,BM)  #categorical Variance not included
print(sub.pen.data)

#model fitting prepare
PZ=(CL+CD+FL+BM)/4
print(PZ)

#set up dataframe: table
distributions = as.data.frame(c((fit.normal=fitdistr(PZ, densfun="normal")$estimate),
                  (fit.cauchy=fitdistr(PZ, densfun="cauchy")$estimate),
                  (fit.geom=fitdistr(PZ, densfun="geometric")$estimate),
                  (fit.logistic=fitdistr(PZ, densfun="logistic")$estimate),
                  (fit.gamma=fitdistr(PZ, densfun="gamma")$estimate)))

print(distributions)
#setup dataframe: header fix?
distributions = (c((fit.normal=fitdistr(PZ, densfun="normal")$estimate), 
                    (fit.cauchy=fitdistr(PZ, densfun="cauchy")$estimate),
                   (fit.geom=fitdistr(PZ, densfun="geometric")$estimate),
                   (fit.logistic=fitdistr(PZ, densfun="logistic")$estimate),
                    (fit.gamma=fitdistr(PZ, densfun="gamma")$estimate)))
print(distributions)

#fun= c(dnorm, dcauchy, dgamma) #function
fun1= c("dnorm", "dcauchy", "dgeom", "dlogis", "dgamma") #function
print(fun1)

current1 <- 3
current2 <- 4
current3 <- 3

for (function1 in fun1)
{
  
    measure1=distributions[current1]
    print(measure1)
    measure2=distributions[current2]
    print(measure2)
    
  
    function1=fun1[current3]
    print(function1)
  
    plot =ggplot(penguin.data, aes(x=PZ)) + geom_histogram(bins=30, aes(y=..density..)) +
    geom_density() + stat_function(fun=function1, color="red", args=list(measure1, measure2))
    print(plot)
  
  current1<- current1 + 2
  current2 <-current2 + 2
  current3 <-current3 + 1

}

BICfit <- BIC(fit.normal,fit.cauchy,fit.poisson,fit.geom,fit.logistic,fit.gamma)
print(BICfit)

note1=cat("I set a loop to make it possible to do fewer lines for as many distribution methods as one wants to explore. We, however,\n
          appears to have some problems. The inital three were newer stuff I added has the flat red line, indirecting something was not running like they\n
          could. I did have this similar problem for part 14, which I did fix. The error messages for the poisson and geomatic methods are suggesting that \n
          some methods (poisson) probably need scaling enlargements like the multiplying by 2 for GMM) and/or clear out NaNs (geomatic) (don't yet figure \n
          where those NaNs came from and what to do about them). Furthermore, it looks like we cannot have more than five\n 
          distribution methods running for loop set comparsions at a time? ")

#save
# sink("part15.stats.txt")
# print(distributions)
# print(BICfit)
# print(note1)
# sink()
