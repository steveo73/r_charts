# Scatterplot & Boxplot Charting - predominately GGPLOT
# typically 2 continuous variables - the relationship between one variable and the
# other variable
# 1. Set-up environment
rm(list=ls()) # remove everything from workspace
library(ggplot2) # load charting package
library(reshape) # load data munging package
library(gcookbook) # load data package
setwd("/home/steveo/learning/ggplot") # set working directory
# 2. Complete exploratory analysis on the data.
# test for data types and format
head(heightweight)
str(heightweight)
# 3. Plot the data
p <- ggplot(heightweight, aes(x=heightIn, y=weightLb,colour=sex))
p <- p + geom_point(shape=21,size=3) #  Shape default = 16, 21 hollow, 19 smoother size default =2
p <- p + theme_grey()
p <- p + scale_colour_manual(name = "Sex", values = c("blue", "red"), labels = c("Female", "Male"))
p <- p + labs(title="Weight/Height Distribution\n")
p <- p + ylab("Weight\n (Pounds)") 
p <- p + xlab("Height\n (Inches)") 
p <- p + geom_smooth(method=lm, level = .75) # se = FALSE  -> no shading, level = .75 -> adjusts confidence level
p

p <- ggplot(heightweight, aes(x=heightIn, y=weightLb, shape=sex))
p <- p + geom_point(size=3) # shape=21, Shape default = 16, 21 hollow, 19 smoother size default =2
p <- p + theme_bw()
p <- p + scale_shape_manual(name = "Sex", values = c(5,3), labels = c("Female", "Male"))
p <- p + labs(title="Weight/Height Distribution\n")
p <- p + ylab("Weight\n (Pounds)") 
p <- p + xlab("Height\n (Inches)") 
p

# 2nd data-set
# 1. test for data types and format
head(ChickWeight)
str(ChickWeight)
# 2. start plotting
p <- ggplot(ChickWeight, aes(x=Time,y=weight))
p <- p + geom_point()
p # shows a relationship between weight and time
p <- ggplot(ChickWeight, aes(x=Diet,y=weight))
p <- p + geom_point()
p # not a huge relationship - maybe 3 is highest
p <- ggplot(ChickWeight, aes(x=Diet,y=weight,group=Diet))
p <- p + geom_boxplot()
p # try a different view - boxplot shows 3 is the highest mean but not a huge factor
p <- ggplot(ChickWeight, aes(x=Time,y=weight))
p <- p + geom_point(position="jitter")
p # stops the values being too close
p <- ggplot(ChickWeight, aes(x=Time,y=weight,group=Time))
p <- p + geom_boxplot()
p # boxplot might show a better chart
# tidy this up
p <- ggplot(ChickWeight, aes(x=Time,y=weight,group=Time))
p <- p + geom_boxplot()
p <- p + theme_grey()
p <- p + labs(title="Weight Over Time\n")
p <- p + ylab("Weight\n (Gms)") 
p <- p + xlab("Height\n (Days)") 
p <- p + annotate("text",label="Chicks Data", x=1,y=320)
p 

