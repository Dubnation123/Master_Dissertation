library('cmaes')
library('quantmod')
library('zoo')
library('nortest')
library('Metrics')
library('stats')
library('dplyr')
library('reshape2')
library('grid')


#Generate time series for starting date t1
ndays <- 20

ss <- as.Date('2017-07-01')
from <- seq(from = ss, by = 3, length.out = ndays)
to <- as.Date("2017-12-01")

result_Par <- as.data.frame(NULL)
result_Par1 <- as.data.frame(NULL)
result_Par2 <- as.data.frame(NULL)
Expect_Price <- as.data.frame(NULL)

n_run = 10

for(i in 1:ndays){
  rTicker <- subset(ticker, ticker$Date >= from[i] & ticker$Date <= to)
  result_Par2 <- as.data.frame(NULL)
  for(j in 1:n_run){
    test <- cma_es(c(0.01, 6, max(rTicker$t)+0.01), residual_obj, lower=c(0.01,6 , max(rTicker$t)+0.01), upper=c(1, 16, max(rTicker$t)+0.25), control=vec_control)
    result_Par2 <- rbind(result_Par2, test$par)
   
  }
  result_Par1 <- rbind(result_Par1,apply(result_Par2,2,mean))
  #test <- cma_es(c(0.01, 6, max(rTicker$t)+0.01), residual_obj, lower=c(0.01,6 , max(rTicker$t)+0.01), upper=c(1, 16, max(rTicker$t)+0.25), control=vec_control)
  #result_Par1 <- rbind(result_Par1, test$par)
  #lm.result <-LPPL(rTicker,test$par[1],test$par[2],test$par[3])
  #result_Par2 <- rbind(result_Par2,c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4]))
}
colnames(result_Par1) <- c('m','omega','tc')
#colnames(result_Par2) <- c('A','B','C1','C2')
#result_Par <- cbind(result_Par1,result_Par2)
result_Par <- result_Par1
last_row <- tail(rTicker, 1)
result_Par$Date_to_Peak <- as.integer((result_Par$tc-last_row$t)/(1/365))
View(result_Par)

plot_long <- melt(result_Par,by = '')

p1 <- ggplot(NULL,aes(x=from,y=result_Par[,1])) + geom_point()+geom_line() + scale_x_date(breaks=as.Date(c("2017-07-01","2017-07-15","2017-08-01","2017-08-15"))) +
ylim(0,1) + xlab('日期') + ylab('Beta') + theme_bw()
p1
p2 <- ggplot(NULL,aes(x=from,y=result_Par[,2])) + geom_point()+geom_line() + scale_x_date(breaks=as.Date(c("2017-07-01","2017-07-15","2017-08-01","2017-08-15"))) +
  ylim(0,10) + xlab('日期') + ylab('Omega') + theme_bw()
p2
p3 <- ggplot(NULL,aes(x=from,y=result_Par[,4])) + geom_point()+geom_line() + scale_x_date(breaks=as.Date(c("2017-07-01","2017-07-15","2017-08-01","2017-08-15"))) +
  ylim(0,40) + xlab('日期') + ylab('距离Tc时间') + theme_bw()
p3

grid.newpage()

pushViewport(viewport(layout = grid.layout(3,1)))
vplayout <- function(x,y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}
print(p1,vp = vplayout(1,1))
print(p2,vp = vplayout(2,1))
print(p3,vp = vplayout(3,1))
#Plotting tc
p <- ggplot(data = result_Par,aes(x=Date_to_Peak)) 
p + geom_histogram(binwidth = 3, fill = "lightblue", colour = "black") + xlim(0,30) +  theme_bw() +
    xlab( "距离泡沫顶端时间") + ylab("频数")

for (i in 1:ndays){
  Expect_Price <- dplyr::bind_rows(Expect_Price,data.frame(t(FittedPrice(ticker,result_Par[i,]))))
}
Expect_Price <- data.frame(t(Expect_Price))
colnames(Expect_Price) <- c(from)

ndays_plot = nrow(Expect_Price)
t <- ts(1:ndays_plot,frequency = 1,start = as.Date('2017-01-01'))
ss_plot <- as.Date('2017-01-01')
from_plot <- seq(from = ss_plot, by = 1, length.out = ndays_plot)
Expect_Price$Date <- from_plot

Expect_Price_long <- subset(Expect_Price,Expect_Price$Date>= as.Date('2017-06-01'))

Expect_Price_long <- melt(Expect_Price_long,id='Date')
colnames(Expect_Price_long) <- c('Date','StartDate','Price')
Expect_Price_long <- subset(Expect_Price_long, Expect_Price_long$StartDate == '2017-08-28'|Expect_Price_long$StartDate == '2017-08-31'|Expect_Price_long$StartDate == '2017-08-25')

#Plot the expected price
p  <- ggplot() + geom_line(aes(ticker$Date,ticker$`Log Price`),size=1)+
  geom_line(aes(Expect_Price_long$Date,log(Expect_Price_long$Price),colour = Expect_Price_long$StartDate),size=0.8) +
  xlab('日期')+ylab('ln[p(t)]')+ scale_color_discrete(name="样本起始时间") + theme_bw() 
p 
multiplot
Expect_Price_long$StartDate = '2017-08-28'

#Plot Setup
fromplot <- as.Date ("2017-01-01")
toplot <- as.Date("2018-01-20")

#Restrict Ticker for window of interest
rTicker <- subset(ticker, ticker$Date >= from & ticker$Date <= to)
rTickerPlot <- subset(ticker, ticker$Date >= fromplot & ticker$Date <= toplot)

plot(rTickerPlot$t, rTickerPlot$Close, typ='l')

#Slaving Linear Variables
LPPL <- function(data, m=1, omega=1, tc=0) {
  data$X <- tc - data$t
  data$Xm <- data$X ** m #B
  data$Xm.cos <- data$X ** m * cos(omega * log(data$X)) #C1
  data$Xm.sin <- data$X ** m * sin(omega * log(data$X)) #C2
  data$logP <- log(data$Close)
  return(lm(logP ~ Xm + Xm.cos + Xm.sin, data=data))
}

#LPPL function
LPPL_Func <- function(m,omega,tc,A,B,C1,C2){
  return()
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

#Optimisation Procedure using CMAES
vec_control <- data.frame(maxit = c(100))   
test <- cma_es(c(0.01, 5, max(rTicker$t)+0.02), residual_obj, lower=c(0.01, 5, max(rTicker$t)+0.02), upper=c(1, 16, max(rTicker$t)+0.25), control=vec_control)

test$par

#Quick Check of Results
m <- test$par[1]
omega <- test$par[2]
tc <- test$par[3]

tryParams(test$par[1], test$par[2], test$par[3])

#Printing Full Results
linear_param <- getlinear_param(test$par[1], test$par[2], test$par[3])
last_row <- tail(rTicker, 1)
df_result <- NULL
rbind(df_result, c(format(from, "%Y-%m-%d"), format(to, "%Y-%m-%d"), last_row$t, last_row$Close, test$par[3]-last_row$t, as.integer((test$par[3]-last_row$t)/(1/365)), test$par[1], test$par[2], test$par[3], linear_param[1], linear_param[2], linear_param[3], linear_param[4])) -> df_result
colnames(df_result) <- c("date_from", "date_to", "t", "price","t_until_critical_point", "days_before_critical_time", "beta", "omega", "tc", "A", "B", "C1", "C2")

