print("Assignment 1, Part 4")

library("ggplot2")
library("reshape2")
library("dplyr")
#library("plyr")

data=read.table("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/penguins_lter.txt", header=TRUE, sep="\t", )
setwd("C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive")
getwd()

#NA to blanks? 
#data2=

studyName=data$studyName
Species=data$Species
Region=data$Region
Island=data$Island
Stage=data$Stage
Date.Egg=data$Date.Egg
Culmen.Length..mm=data$Culmen.Length..mm.
Culmen.Depth..mm=data$Culmen.Depth..mm.
Flipper.Length..mm=data$Flipper.Length..mm.
Body.Mass..g=data$Body.Mass..g.

penguin.data=data.frame(
  Species,
  Island,
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
  Species,
  Body.Mass..g
)

print(body.mass.data)

#multi variance by Species
physical.data=data.frame(
  Species,
  Culmen.Depth..mm,
  Culmen.Length..mm,
  Flipper.Length..mm,
  Body.Mass..g
)

print(physical.data)

#scatter, colored
plot1=ggplot(body.mass.data, aes(x=Species, y=Body.Mass..g, shape=Species, color=Species)) +
  geom_point()
print(plot1)

#scatter plot, colored
plot2=ggplot(culmen.data, aes(x=, y=Culmen.Depth..mm, shape=Species, color=Species))+
  geom_point()
print(plot2)

#plot2=ggplot()

#mean by Specie
#BMmean=tapply(Body.Mass..g,Species,mean)
#BMsd=tapply(Body.Mass..g,Species,mean)

#tapply isnt working right...
#--------------

#mean by Specie using dplyr
# BMmean=data%>%
#   group_by(Species) %>%
#   summarise_at(vars(all_of(Body.Mass..g)),
#                list(name=mean))
#error: Selections can't have missing values.
#----------------

#mean by aggregate function
BMmean=aggregate(x=data$Body.Mass..g., by=list(data$Species), FUN= mean, na.action())
#same problem as tapply

penguin.means=data.frame(
  BMmean
)


#varname,Body.Mass
#groupname,Species
#BMmean=

#sd by Specie
# CLsd=tapply(Culmen.Length..mm,Species,sd)
# CDsd=tapply(Culmen.Depth..mm,Species,sd)

penguin.sds=data.frame(
  BMsd
)

print(penguin.means)
print(penguin.sds)

#oneway ANOVA
CL.anova=oneway.test(Culmen.Length..mm~Species)
CD.anova=oneway.test(Culmen.Depth..mm~Species)
BM.anova=oneway.test(Body.Mass..g~Species)
print(CL.anova)
print(CD.anova)
print(BM.anova)

#error bar chart, colored
#problems with mean and sd

plot3=ggplot(data=body.mass.data, aes(x=Species, y=Body.Mass..g)) + 
  geom_bar(stat="identity", fill="gold", position=position_dodge()) + 
  geom_errorbar(aes(ymin=-BMsd, ymax=+BMsd), width=0.01, col="navy")
print(plot3)

#pairwise t test, Bonferroni
test1=pairwise.t.test(Body.Mass..g~Species,p.adjust.method = "bonf")

#pairwise t test, Benjamini-Hochberg test
test2=pairwise.t.test(Body.Mass..g~Species,p.adjust.method = "BH")

#make an external file
sink(file="C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/archive/penguin.txt")
print(penguin.means)
print(summary(penguin.data))
print(CL.anova)
print(CD.anova)
print(test1)
print(test2)
sink()

#save plots on same pdf page
library(grid)
#pdf("Unit1Part4Plots.pdf")
pushViewport(viewport(layout = grid.layout(2, 2)))
print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(plot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
#print(plot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
#dev.off()

cat("Dear Window 10, \nYou are so helpful with writing part 4, especially the tapply function. \nLove, Mavis")
