library(ggplot2)

#Use this if have problems loading packages
options(download.file.method="libcurl")

#Set working directory
setwd("/users/pgrad/wangy12/Documents/Thesis")

#Load and clean data
#The data is downloaded from coindesk. The data has three variables: Data, Log Price, Close Price.
fileName <- './BitCoin Historical Price Data/Clean Price.csv' #Change it to your own name!
fileName <- './BitCoin Historical Price Data/BTC_Data.csv'
data <- read.csv(fileName, header=TRUE, sep=",")
colnames(data) = c('Date','Close','Log Price') # Change column names
data$Date <- as.Date(data$Date, format = "%d/%m/%Y") #Convert string dates into R dates
key_window = as.Date('2017-01-01') #set critical window
ticker <- subset(data,data$Date >= key_window)
summary(ticker)

#Setting t value for model
gtd <- as.Date("2016-12-31")
ticker$t <- 2017 + as.numeric(difftime(ticker$Date,gtd)/365)

#Plotting all price data
p <- ggplot(data = data,aes(x=Date,y=Close))
p + geom_line(size=1) + xlab('Date') + ylab('Price') +  theme_bw() 

#Plotting key window price data
p <- ggplot(data = ticker,aes(x=Date,y=Close))
p + geom_line(size=1) + xlab('Date') + ylab('Price') + theme_bw()

