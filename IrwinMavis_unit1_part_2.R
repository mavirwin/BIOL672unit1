print("Assignment 1, Part 2")

library("ggplot2")
library("reshape2")

#group1 
x1=rnorm(n=5000, mean=20, sd=1)
dataframex1=data.frame(x1)
g1_summary=c(mean(x1), sd=1)
g1_labels= c("Meanx1","SDx1")

#dataframe of group1
dfg1=data.frame(g1_labels,g1_summary)
print (dfg1)

#ggplot histogram approach with density and normal curve
#NOTE: Unlike in Rmarkdown, the regular Rstudio is able to plot both density and normal curve in same picture using the following code: 

hist1=ggplot(dataframex1, aes(x=x1))+ 
  geom_histogram(bins = 100, aes(x=x1, y=..density..)) + 
  geom_density(alpha=0.2, color= "red") +
  stat_function(fun=dnorm, args=list(mean=mean(x1), sd=sd(x1)))
print(hist1)

cat("Dear Window 10, \nYou are so helpful with writing part 2. \nLove, Mavis")

#NOTE: Below is just a note of the code that fail to put density, but can do normal curve. hist2 is neutralized. 
# hist2=ggplot(x=x1)+
#   geom_histogram(bins = 100, aes(x=x1, y=..density..)) +
#   geom_density(alpha=0.2, color= "red") +
#   stat_function(fun=dnorm, args=list(mean=mean(x1), sd=sd(x1)))
# print(hist2)

