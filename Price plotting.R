library(ggplot2)

#Plotting total price data
p <- ggplot(data = data,aes(x=Date,y=Close))
p + geom_line(size=1) + xlab('Date') + ylab('Close Price') +  theme_bw() 

#Plotting key window price data
p <- ggplot(data = ticker,aes(x=Date,y=Close))
p + geom_line(size=1) + xlab('Date') + ylab('Close Price') + theme_bw()
