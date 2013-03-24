# Time Series Charting - predominately GGPLOT & line charts
# 1. Set-up environment
rm(list=ls()) # remove everything from workspace
library(ggplot2) # load charting package
library(reshape) # load data munging package
setwd("/home/steveo/learning/ggplot") # set working directory
fileLocal <- "./data/sbucks.csv" # point local file
sData <-read.csv(fileLocal) # load Data to R
# 2. Complete exploratory analysis on the data.
# test for data types and format
head(sData)
str(sData)
# 3. Data Prep
# store dates as dates and prep the data into a vertical fact like format
# firstly in this case we need to compute returns
# we will use an intermeditary data frame to do this
# amend the column names to make it cleaner
sDatadf <- data.frame("date" = as.Date(sData$Date) 
                      , "open" = sData$Open
                      , "high" = sData$High
                      , "low" = sData$Low
                      , "close" = sData$Close 
                      , "adj_close" = sData$Adj.Close 
                      , "volume" = sData$Volume )
head(sDatadf) # check data
str(sDatadf) # check data
# compute returns
# to do so we need to munge the data so we have the prior days close listed next to the 
# current days close
min(sDatadf$date)
max(sDatadf$date)
# sub-set the data
dfsbuxpc <- sDatadf [sDatadf$date < "2008-03-03",] # prior close data
head(dfsbuxpc) # check data
dfsbuxcc <- sDatadf [sDatadf$date > "1993-03-01",] # current close data
head(dfsbuxcc) # check data
# create required data-set
dfsbucksdata <- data.frame( dfsbuxcc, "prior_close" = dfsbuxpc$adj_close ) 
head(dfsbucksdata) # check data
tail(dfsbucksdata) # check data
# compute returns
dfsbucksdata$simple_rtn <- c((dfsbucksdata$adj_close - dfsbucksdata$prior_close)/dfsbucksdata$prior_close) # add simple return as a column
head(dfsbucksdata) # check data
dfsbucksdata$cont_rtn <- c(log(dfsbucksdata$adj_close) - log(dfsbucksdata$prior_close)) # continuous returns
head(dfsbucksdata) # check data
# now we have a complete data set that needs to be melted into a vertical fact type data-set
# melt the data
dfFinal <- melt(dfsbucksdata, id=c("date"))
head(dfFinal) # check data
str(dfFinal)
unique(dfFinal$variable)
# remove data not required
rm(sData)
rm(sDatadf)
rm(dfsbuxpc)
rm(dfsbuxcc)
rm(dfsbucksdata)
# 4. Plot the data
# plot all variables we want to see on seperate axes and charts
p <- ggplot(subset(dfFinal, variable != "volume" & variable != "prior_close")
              , aes(x=date, y =  value, group = 1))
p <- p + geom_line(aes(colour=variable, group=variable))
p <- p + facet_grid(variable ~ . , scales="free") # horizontal charts
# p <- p + facet_grid(.~variable , scales="free") # vertical charts
p <- p + theme_grey()
p <- p + scale_colour_manual(name = "Indicator", values = c("red","blue", "green","red","blue", "green","red"))
p <- p + labs(title="SBUX Stock Market Performance")
p <- p + ylab("Value") 
p <- p + xlab("Date") 
p
# plot all variables we want to see on same chart to compare
p <- ggplot(subset(dfFinal, variable == "simple_rtn" | variable == "cont_rtn")
            , aes(x=date, y =  value, group = 1))
p <- p + geom_line(aes(colour=variable, group=variable))
p <- p + theme_grey()
p <- p + scale_colour_manual(name = "Indicator", values = c("red","blue"))
p <- p + labs(title="SBUX Stock Market Performance")
p <- p + ylab("Value") 
p <- p + xlab("Date") 
p
# plot all variables we want to see on same chart to compare
p <- ggplot(data=subset(dfFinal, variable == "adj_close" | variable == "simple_rtn")
            , aes(x=date, y =  log(value), group = 1))
p <- p + geom_line(aes(colour=variable, group=variable))
p <- p + theme_grey()
p <- p + scale_colour_manual(name = "Indicator", values = c("red","blue"))
p <- p + labs(title="SBUX Stock Market Performance")
p <- p + ylab("Value") 
p <- p + xlab("Date") 
p
# plot ribbon chart - this requires min & max values as well as the default y value
# we re-munge the data into traditional fact like table for the required values to be considered as coupled data
# need to re-munge the data
df1 <- subset(dfFinal, variable == "close")
df1$variable <- NULL
colnames(df1) <- c("date", "close")
head(df1)
df2 <- subset(dfFinal, variable == "high")
df2$variable <- NULL
colnames(df2) <- c("date", "high")
head(df2)
df3 <- subset(dfFinal, variable == "low")
df3$variable <- NULL
colnames(df3) <- c("date", "low")
head(df3)
# merge data-sets
dfM <- merge(df3,df2,by="date")
head(dfM)
dfM <- merge(dfM,df1,by="date")
head(dfM)
# plot ribbon plot
p <- ggplot(data=dfM, aes(x=date, y=close, ymin=low, ymax=high))
p <- p + geom_ribbon(alpha=0.5, fill = "red")
p <- p + geom_line(colour = "dark blue")
p <- p + labs(title="SBUX Stock Market Performance")
p <- p + ylab("Value (Close/Low/High)") 
p <- p + xlab("Date") 
p
