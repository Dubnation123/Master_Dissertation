library('cmaes')
library('quantmod')
library('zoo')
library('nortest')
library('Metrics')
library('stats')
library('dplyr')
library('reshape2')
library('ggplot2')

#Time window for estimation
from <- as.Date("2017-09-02")
to <- as.Date("2017-12-01")
rTicker <- subset(ticker, ticker$Date >= from & ticker$Date <= to)

#Slaving Linear Variables
LPPL <- function(data, m=1, omega=1, tc=0) {
  data$X <- tc - data$t
  data$Xm <- data$X ** m #B
  data$Xm.cos <- data$X ** m * cos(omega * log(data$X)) #C1
  data$Xm.sin <- data$X ** m * sin(omega * log(data$X)) #C2
  data$logP <- log(data$Close)
  return(lm(logP ~ Xm + Xm.cos + Xm.sin, data=data))
}

#Initial Estimates of A, B, C1 and C2 through Least Squares
FittedLPPL <- function(data, lm.result, m=1, omega=1, tc=0) {
  data$X <- tc - data$t
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  result <- exp(A + B * (data$X ** m) + C1 * (data$X ** m) * cos(omega * log(data$X)) + C2 * (data$X ** m) * sin(omega * log(data$X))) 
  return(result)
}

FittedPrice <- function(data,paras){
  m = as.numeric(paras[1])
  omega = as.numeric(paras[2])
  tc = as.numeric(paras[3])
  A = as.numeric(paras[4])
  B = as.numeric(paras[5])
  C1 = as.numeric(paras[6])
  C2 = as.numeric(paras[7])
  data$X <- tc - data$t
  data <- subset(data,data$X >0)
  result <- exp(A + B * (data$X ** m) + C1 * (data$X ** m) * cos(omega * log(data$X)) + C2 * (data$X ** m) * sin(omega * log(data$X)))
  #return(as.data.frame(result))
  return(result)
}


#Rewritten for plotting
FittedLPPLwithexpected <- function(data, lm.result, x_vector, m=1, omega=1, tc=0) {
  tmp_vector <- tc - x_vector
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  result <- exp(A + B * (tmp_vector ** m) + C1 * (tmp_vector ** m) * cos(omega * log(tmp_vector)) + C2 * (tmp_vector ** m) * sin(omega * log(tmp_vector))) 
  return(result)
  
}

#Function for getting final values of A, B, C1 and C2 parameters
getlinear_param <- function(m, omega, tc) {
  lm.result <- LPPL(rTicker, m, omega, tc)
  getcoeff_regLPPL <- c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4])
}

#Plotting everything
tryParams <- function (m, omega, tc) {  
  lm.result <- LPPL(rTicker, m, omega, tc)
  plot(rTickerPlot$t, rTickerPlot$Close, typ='l') #base graph based on data
  generate_vector = seq(min(rTicker$t), tc-0.002, 0.002)
  lines(generate_vector, FittedLPPLwithexpected(rTicker, lm.result, generate_vector, m, omega, tc), col="red")
}

# Sum of squared residuals, to evaluate the fitness of m, omega, phi
residuals <- function(m, omega, tc) {
  lm.result <- LPPL(rTicker, m, omega, tc)
  return(sum((FittedLPPL(rTicker, lm.result, m, omega, tc) - rTicker$Close) ** 2))
}


residual_obj <- function(x) {
  return(residuals(x[1], x[2], x[3]))
}


#Function if wanting to return original LPPL coeffs
getcoeff_regLPPL <- function(m, omega, tc) {
  lm.result <- LPPL(rTicker, m, omega, tc)
  getcoeff_regLPPL <- c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4])
}

vec_control <- data.frame(maxit = c(100))  


result_Par <- as.data.frame(NULL)
result_Par1 <- as.data.frame(NULL)
result_Par2 <- as.data.frame(NULL)
Expect_Price <- as.data.frame(NULL)
n <- 50 # times for estimation

for(i in 1:n){
  test <- cma_es(c(0.01, 6, max(rTicker$t)+0.01), residual_obj, lower=c(0.01, 6, max(rTicker$t)+0.01), upper=c(1, 16, max(rTicker$t)+0.25), control=vec_control)
  result_Par1 <- rbind(result_Par1, test$par)
  lm.result <-LPPL(rTicker,test$par[1],test$par[2],test$par[3])
  result_Par2 <- rbind(result_Par2,c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4]))
}
colnames(result_Par1) <- c('m','omega','tc')
colnames(result_Par2) <- c('A','B','C1','C2')
result_Par <- cbind(result_Par1,result_Par2)
last_row <- tail(rTicker, 1)
result_Par$Date_to_Peak <- as.integer((result_Par$tc-last_row$t)/(1/365))
summary(result_Par)

#Plotting tc
p <- ggplot(data = result_Par,aes(x=Date_to_Peak)) 
p + geom_histogram(binwidth = 3, fill = "lightblue", colour = "black") + xlim(0,30) +  theme_bw() +
  xlab( "距离泡沫顶端时间") + ylab("频数")

for (i in 1:n){
  Expect_Price <- dplyr::bind_rows(Expect_Price,data.frame(t(FittedPrice(ticker,result_Par[i,]))))
}
Expect_Price <- data.frame(t(Expect_Price))
colnames(Expect_Price) <- c(1:n)

n_plot = nrow(Expect_Price)
ss_plot <- as.Date('2017-07-01')
from_plot <- seq(from = ss_plot, by = 1, length.out = n_plot)
Expect_Price$Date <- from_plot

Expect_Price_long <- subset(Expect_Price,Expect_Price$Date>= as.Date('2017-08-01'))
Expect_Price_long <- melt(Expect_Price_long,id='Date')
colnames(Expect_Price_long) <- c('Date','Number','Price')
#Expect_Price_long <- subset(Expect_Price_long, Expect_Price_long$Number == '1'|Expect_Price_long$StartDate == '2017-08-31'|Expect_Price_long$StartDate == '2017-08-25')

#Plot the expected price
ticker_plot <- subset(ticker,ticker$Date >= as.Date('2017-07-01'))
p  <- ggplot() + geom_line(aes(ticker_plot$Date,ticker_plot$`Log Price`),size=1)+
  geom_line(aes(Expect_Price_long$Date,log(Expect_Price_long$Price),colour = Expect_Price_long$Number),size=0.8) +
  xlab('日期')+ylab('ln[p(t)]')+ guides(color=FALSE) + theme_bw() 
p 





