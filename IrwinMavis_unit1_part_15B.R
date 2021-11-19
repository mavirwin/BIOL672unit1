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
                  (fit.gamma=fitdistr(PZ, densfun="gamma")$estimate)))

print(distributions)
#setup dataframe: header fix?
distributions = (c((fit.normal=fitdistr(PZ, densfun="normal")$estimate), 
                    (fit.cauchy=fitdistr(PZ, densfun="cauchy")$estimate), 
                    (fit.gamma=fitdistr(PZ, densfun="gamma")$estimate)))
print(distributions)

#fun= c(dnorm, dcauchy, dgamma) #function
fun1= c("dnorm", "dcauchy", "dgamma") #function
print(fun1)

current1 <- 1
current2 <- 2

for (function1 in fun1)
{
  
    measure1=distributions[current1]
    print(measure1)
    measure2=distributions[current2]
    print(measure2)
  

    print(function1)
  
    plot =ggplot(penguin.data, aes(x=PZ)) + geom_histogram(bins=30, aes(y=..density..)) +
    geom_density() + stat_function(fun=function1, color="red", args=list(measure1, measure2))
    print(plot)
  
  current1<- current1 + 2
  current2 <-current2 + 2

}


# print(df)

# adjust.dis=data.frame(distributions$estimate)
# print(distributions)
# print(adjust.dis)
# 
# measures1 = c(fit.normal$estimate[1], head= FALSE, 
#               fit.cauchy$estimate[1], 
#               fit.gamma$estimate[1]) #measurement units
# 
# print(measure1[1])
# fit.normal$estimate[1]
# print(fit.normal$estimate[1],)
#test miniloop layout
#set our index to 0
# current <- 0
# for(distribution in distributions)
# {
#   measure1 = measures1[current]
#    #move to the next one
#   current= current + 1
# }
# print(distribution)
# 
# measures2 = c(fit.normal$estimate[2], 
#               fit.cauchy$estimate[2], 
#               fit.gamma$estimate[2]) #measurement units
# 
# functions = c(dnorm, dcauchy, dgamma) #function
# 
# #dataframe
# fit.df= rbind.data.frame(distributions,measures1, measures2, functions)
# 
# print(fit.df)


#set our index to 0
current <- 0

#go through a loop and pick out each element one at a time
for(distribution in distributions)
{
  measure1 = measures1[current]
  measure2 = measures2[current]
  function1 = functions[current]
  
  fit= fitdistr(PZ, densfun=distribution)

plot =ggplot(penguin.data, aes(x=PZ)) + geom_histogram(bins=30, aes(y=..density..)) + 
  geom_density() + stat_function(fun=function1, color="red",
  args=list(measure1=fit$estimate[1], measure2=fit$estimate[2]))
print(plot)

#move to the next one
current= current + 1
}

note1=cat("")
