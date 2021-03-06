print("Assignment 1, Part 14")

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

#evaluate four models
#normal
fitNORM <- fitdistr(PZ, densfun="normal")
print(fitNORM)
fitNORM$estimate
#lognormal
fitLNORM <- fitdistr(PZ, densfun="log-normal")
print(fitLNORM)
#played with: exponential(rate; dexp), gamma (shape, rate; dgamma?), cauchy (location, scale, dcauchy?), weibull (shape, scale, dweibell?)

# fitTEST <- fitdistr(PZ, densfun="gamma")
# print(fitTEST)

fitTEST <- fitdistr(PZ, densfun="cauchy")
print(fitTEST)

#GMM
fitGMM <- normalmixEM(PZ)
print(fitGMM)

#BIC setup
fitGMM_loglik <- fitGMM$loglik
#BIC_GMM <- -2*fitGMM_loglik+4*log(150)  #result was 4416
BIC_GMM2 = -2*fitGMM_loglik-4*log(150)  #result was 4376
BICfit <- BIC(fitNORM,fitLNORM,fitTEST)
print(BICfit)

#plot the four models
#NOTE plotting problem: "`stat_bin()` using `bins = 30`. Pick better value with `binwidth`." 
print ("BIC for GMM")
print(BIC_GMM2)
plot1141 <-ggplot(penguin.data, aes(x=PZ)) + geom_histogram(bins=30, aes(y=..density..)) + geom_density() + stat_function(fun=dnorm, color="red", args=list(mean = fitNORM$estimate[1], sd = fitNORM$estimate[2])) 
plot1142 <-ggplot(penguin.data, aes(x=PZ)) + geom_histogram(bins=30, aes(y=..density..)) + geom_density() + stat_function(fun=dlnorm, color="red", args=list(meanlog = fitLNORM$estimate[1], sdlog = fitLNORM$estimate[2])) 
#exp
#plot1143 <-ggplot(penguin.data, aes(x=PZ)) + geom_histogram(aes(y=..density..)) + geom_density() + stat_function(fun=dexp, color="red", args=list(rate = fitEXP$estimate[1])) 
#try gamma
#plot1143 <-ggplot(penguin.data, aes(x=PZ)) + geom_histogram(bins=30, aes(y=..density..)) + geom_density() + stat_function(fun=dgamma, color="red", args=list(shape= fitTEST$estimate[1], rate= fitTEST$estimate[2])) 
#try cauchy
plot1143 <-ggplot(penguin.data, aes(x=PZ)) + geom_histogram(bins=30, aes(y=..density..)) + geom_density() + stat_function(fun=dcauchy, color="red", args=list(location = fitTEST$estimate[1], scale = fitTEST$estimate[2])) 
plot1144 <-ggplot(penguin.data, aes(x=PZ)) + geom_histogram(aes(y=2*(..density..))) + geom_density(aes(y=2*(..density..))) + stat_function(fun=dnorm, color="red", args=list(mean = fitGMM$mu[1], sd = fitGMM$sigma[1])) + stat_function(fun=dnorm, color="blue", args=list(mean = fitGMM$mu[2], sd = fitGMM$sigma[2])) 

print(fitTEST$estimate[1])
library('grid')

pushViewport(viewport(layout = grid.layout(2, 2)))
print(plot1141, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot1142, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(plot1143, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot1144, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

note1=cat("The GMM shows a possible two grouping distributions. I have messed around with other stuff /n
          as my poteitial choices. \n
          According to the BIC printouts, the BIC_GMM at 4416 has better fit than other three tested so far as fitNORM=2260, fitLNORM= 4440, and fitTEST=4444(this one \n
          used the gamma method). Update: Tried cauchy, which is not a good fit, by the way of 4612 BIC results. ")

#save
# sink("stats.txt")
# print(fitGMM)
# print(BICfit)
# print ("BIC for GMM")
# print(BIC_GMM)
# sink()



