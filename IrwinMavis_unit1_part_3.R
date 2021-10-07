print("Assignment 1, Part 3")

library("ggplot2")
library("reshape2")

# Code from part 2, which is already in enivornment
# #group1
# x1=rnorm(n=5000, mean=20, sd=1)
# dataframex1=data.frame(x1)
# g1_summary=c(mean(x1), sd=1)
# g1_labels= c("Meanx1","SDx1")
# 
# #dataframe of group1
# dfg1=data.frame(g1_labels,g1_summary)

#check if have table available in enivornment
print (dfg1)

sink(file="C:/Users/Videosystem/Desktop/RocASAsamples/Mavis_samples/desc.txt")
write.table(dfg1)
print(dfg1)
sink()

#saving rplots as histo.pdf

getwd()
print(hist1)
#problems dev.off(hist1)
file.rename("Rplot.pdf","histo.pdf")

cat("Dear Window 10, \nYou are so helpful with writing part 3. \nLove, Mavis")

