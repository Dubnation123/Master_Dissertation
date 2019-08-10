library('cmaes')
library('quantmod')
library('zoo')
library('nortest')
library('Metrics')
library('stats')
library('dplyr')
library('reshape2')
library('ggplot2')

#Window Setup
from <- as.Date("2017-12-17")
to <- as.Date("2018-02-28")

#Restrict Ticker for window of interest
rTicker <- subset(ticker, ticker$Date >= from & ticker$Date <= to)
rTickerPlot <- subset(ticker, ticker$Date >= fromplot & ticker$Date <= toplot)

plot(rTickerPlot$t, rTickerPlot$Close, typ='l')

#Determine the critical time
tc_anti = ticker$t[which.max(ticker$Close)]

#Decrease the number of parameters: from 4 to 3
LPPL_anti <- function(data, m=1, omega=1) {
  data$X <- data$t - tc_anti
  data$Xm <- data$X ** m #B
  data$Xm.cos <- data$X ** m * cos(omega * log(data$X)) #C1
  data$Xm.sin <- data$X ** m * sin(omega * log(data$X)) #C2
  data$logP <- log(data$Close)
  return(lm(logP ~ Xm + Xm.cos + Xm.sin, data=data))
}

#Initial Estimates of A, B, C1 and C2 through Least Squares
FittedLPPL_anti <- function(data, lm.result, m=1, omega=1) {
  data$X <- data$t - tc_anti
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  result <- exp(A + B * (data$X ** m) + C1 * (data$X ** m) * cos(omega * log(data$X)) + C2 * (data$X ** m) * sin(omega * log(data$X))) 
  return(result)
}


#Rewritten for plotting
FittedLPPLwithexpected_anti <- function(data, lm.result, x_vector, m=1, omega=1) {
  tmp_vector <-  x_vector - tc_anti
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  result <- exp(A + B * (tmp_vector ** m) + C1 * (tmp_vector ** m) * cos(omega * log(tmp_vector)) + C2 * (tmp_vector ** m) * sin(omega * log(tmp_vector))) 
  return(result)
  
}

#Function for getting final values of A, B, C1 and C2 parameters
getlinear_param_anti <- function(m, omega) {
  lm.result <- LPPL_anti(rTicker, m, omega)
  getcoeff_regLPPL <- c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4])
}

#Plotting everything
tryParams_anti <- function (m, omega) {  
  lm.result <- LPPL_anti(rTicker, m, omega)
  plot(rTickerPlot$t, rTickerPlot$Close, typ='l') #base graph based on data
  generate_vector = seq(tc_anti+1/365, max(rTicker$t),1/365)
  lines(generate_vector, FittedLPPLwithexpected_anti(rTicker, lm.result, generate_vector, m, omega), col="red")
}

FittedPrice_anti <- function(data,paras){
  m = as.numeric(paras[1])
  omega = as.numeric(paras[2])
  A = as.numeric(paras[3])
  B = as.numeric(paras[4])
  C1 = as.numeric(paras[5])
  C2 = as.numeric(paras[6])
  data$X <- data$t - tc_anti
  data <- subset(data,data$X >0)
  result <- exp(A + B * (data$X ** m) + C1 * (data$X ** m) * cos(omega * log(data$X)) + C2 * (data$X ** m) * sin(omega * log(data$X)))
  #return(as.data.frame(result))
  return(result)
}

# Sum of squared residuals, to evaluate the fitness of m, omega, phi
residuals_anti <- function(m, omega) {
  lm.result <- LPPL_anti(rTicker, m, omega)
  return(sum((FittedLPPL_anti(rTicker, lm.result, m, omega) - rTicker$Close) ** 2))
}


residual_obj_anti <- function(x) {
  return(residuals_anti(x[1], x[2]))
}

#Optimisation Procedure using CMAES
vec_control <- data.frame(maxit = c(100))   

result_Par <- as.data.frame(NULL)
result_Par1 <- as.data.frame(NULL)
result_Par2 <- as.data.frame(NULL)
Expect_Price <- as.data.frame(NULL)
ts_plot <- as.data.frame(NULL)
n <- 50 # times for estimation

for(i in 1:n){
  test <- cma_es(c(0.01, 10), residual_obj_anti, lower=c(0.01, 7), upper=c(1, 16), control=vec_control)
  result_Par1 <- rbind(result_Par1, test$par)
  lm.result <-LPPL_anti(rTicker,test$par[1],test$par[2])
  result_Par2 <- rbind(result_Par2,c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4]))
}
colnames(result_Par1) <- c('m','omega')
colnames(result_Par2) <- c('A','B','C1','C2')
result_Par <- cbind(result_Par1,result_Par2)
summary(result_Par)

n_ts = 150
ts_Date <- seq(from = from, by = 1, length.out = n_ts)
gtd <- as.Date("2016-12-31")
ts_t <- 2017 + as.numeric(difftime(ts_Date,gtd)/365)
ts_plot <- cbind(as.Date(ts_Date),ts_t)
ts_plot <- as.data.frame(ts_plot)
colnames(ts_plot) <- c('Date','t')
  
for (i in 1:n){
  Expect_Price <- dplyr::bind_rows(Expect_Price,data.frame(t(FittedPrice_anti(ts_plot,result_Par[i,]))))
}
Expect_Price <- data.frame(t(Expect_Price))
colnames(Expect_Price) <- c(1:n)

n_plot = nrow(Expect_Price)
ss_plot <- as.Date('2017-12-17')
from_plot <- seq(from = ss_plot, by = 1, length.out = n_plot)
Expect_Price$Date <- from_plot

#Expect_Price_long <- subset(Expect_Price,Expect_Price$Date>= as.Date('2017-08-01'))
Expect_Price_long <- melt(Expect_Price,id='Date')
colnames(Expect_Price_long) <- c('Date','Number','Price')
#Expect_Price_long <- subset(Expect_Price_long, Expect_Price_long$Number == '1'|Expect_Price_long$StartDate == '2017-08-31'|Expect_Price_long$StartDate == '2017-08-25')

#Plot the expected price
ticker_plot <- subset(ticker,ticker$Date >=as.Date('2017-12-01'))
p  <- ggplot() + geom_line(aes(ticker_plot$Date,ticker_plot$`Close`),size=1)+
  geom_line(aes(Expect_Price_long$Date,Expect_Price_long$Price,colour = Expect_Price_long$Number),size=0.8) +
  xlab('ÈÕÆÚ')+ylab('ln[p(t)]') + theme_bw() +guides(color = FALSE)
p 








test <- cma_es(c(0.01, 10), residual_obj_anti, lower=c(0.01, 10), upper=c(1, 16), control=vec_control)

test$par

#Quick Check of Results
m <- test$par[1]
omega <- test$par[2]

tryParams_anti(test$par[1], test$par[2])

#Printing Full Results
linear_param <- getlinear_param_anti(test$par[1], test$par[2], test$par[3])
last_row <- tail(rTicker, 1)
df_result <- NULL
rbind(df_result, c(format(from, "%Y-%m-%d"), format(to, "%Y-%m-%d"), last_row$t, last_row$Close, test$par[3]-last_row$t, as.integer((test$par[3]-last_row$t)/(1/365)), test$par[1], test$par[2], test$par[3], linear_param[1], linear_param[2], linear_param[3], linear_param[4])) -> df_result
colnames(df_result) <- c("date_from", "date_to", "t", "price","t_until_critical_point", "days_before_critical_time", "beta", "omega", "tc", "A", "B", "C1", "C2")
