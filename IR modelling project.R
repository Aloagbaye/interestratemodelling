#install packages and load libraries
#to install packages, click "Tools" above and "install packages..", check for "quantmod and install.
#closeAllConnections()
rm(list = ls())

library(quantmod)
library(data.table)
library(dplyr)
library(corrplot)
library(GGally)
library(tseries)
library(factoextra)
library(FactoMineR)
#start_date <- "2000-01-01"

#function to compute the pca and plot the loadings

pca <- function(df) {
  
  pc <- princomp(df)
  
  factor.loadings <- pc$loading[,1:3]
  legend.loadings <- c("First PC","Second PC","Third PC")
  par(xaxt="n")
  matplot(factor.loadings,type="o",
          lwd=3,lty=1,xlab = "Tenor", ylab = "PC Rotation")
  legend(3,max(factor.loadings),legend=legend.loadings,col=1:3,lty=1,lwd=3)
  par(xaxt="s")
  axis(1,1:4,tenors)
  return(pc)
}


#Compute rate gap, this function computes the rate jump and checks if it exceeds a threshold.
ratejump <- function(df,threshold,time_lag){
  df$rate_gap <- df$rate - lag(df$rate,time_lag, default = df$rate[1])
  return(df)
}

teststat <-function(df){
  box_p <- Box.test(df,lag = 20,type = "Ljung-Box")
  adf_p <-adf.test(df,alternative = "stationary")
}

ats <- function(data){
  ats_mean <- colMeans(data)
  plot(ats_mean, type="o",xaxt = "n", xlab="Tenors",ylab = "Average Interest rate", main = "Average Term Structure")
  
  }
ats_sd <- function(data){
  ats_sd <- apply(data, 2, sd)
  plot(ats_sd, type="o",xaxt = "n", xlab="Tenors",ylab = "Interest rate SDs", main = "Standard Deviation Term Structure")
  
}

#Comments during presentation
#diff
#Transform to a stationary variable, time series of daily increment. Autoincrements.
#More interested in fluctuations around the trend

# Download data and convert to a dataframe
tenors<-c("1MT","3MT","6MT","12MD")
symbols_usd<-c("USD1MTD156N","USD3MTD156N","USD6MTD156N","USD12MD156N")
symbols_cad<-c("CAD1MTD156N","CAD3MTD156N","CAD6MTD156N","CAD12MD156N")
getSymbols(Symbols=c(symbols_usd,symbols_cad),src ="FRED", from = '2000-01-01') #Downloads data.

for (i in 1:4){
  #DataFrame for each USD Tenor
  usd = eval(parse(text = symbols_usd[i]))
  name =  paste("USD", tenors[i], sep = "")
  #usd = usd[index(usd) >= "2000-01-04",]
  usd = assign(name,data.frame(date=index(usd), rate=coredata(usd)[,]))
  
  #Rate Jumps for each USD Tenor
  usd$rate = na.approx(usd$rate)
  usd = assign(name,ratejump(usd,0.1,30))
  
  #DataFrame for each CAD Tenor
  cad = eval(parse(text = symbols_cad[i]))
  name =  paste("CAD", tenors[i], sep = "")
  cad = assign(name,data.frame(date=index(cad), rate=coredata(cad)[,]))
  
  #Rate Jumps for each USD Tenor
  cad$rate = na.approx(cad$rate)
  cad = assign(name,ratejump(cad,0.1,30))
  
}
#functions
#insights
#open questions
#Maximum six slides.

ggplot() + 
  #geom_line(data = USDON, aes(x = date , y = rate,color="USDON")) +
  #geom_line(data = USD1WK, aes(x = date, y = rate,color="USD1WK")) +
  geom_line(data = USD1MT, aes(x = date, y = rate,color="USD1MT")) +
  #geom_line(data = USD2MT, aes(x = date, y = rate,color="USD2MT"))+
  geom_line(data = USD3MT, aes(x = date, y = rate,color="USD3MT"))+
  geom_line(data = USD6MT, aes(x = date, y = rate,color="USD6MT"))+
  geom_line(data = USD12MD, aes(x = date, y = rate,color="USD12MD"))+
  xlab('Dates') +
  ylab('Rates')+
  labs(color="Tenors")+ggtitle("USD LIBOR time series")

ggplot() + 
  #geom_line(data = USDON, aes(x = date , y = rate,color="USDON")) +
  #geom_line(data = USD1WK, aes(x = date, y = rate,color="USD1WK")) +
  geom_line(data = CAD1MT, aes(x = date, y = rate,color="CAD1MT")) +
  #geom_line(data = USD2MT, aes(x = date, y = rate,color="USD2MT"))+
  geom_line(data = CAD3MT, aes(x = date, y = rate,color="CAD3MT"))+
  geom_line(data = CAD6MT, aes(x = date, y = rate,color="CAD6MT"))+
  geom_line(data = CAD12MD, aes(x = date, y = rate,color="CAD12MD"))+
  xlab('Dates') +
  ylab('Rates')+
  labs(color="Tenors")+ggtitle("CAD LIBOR time series")

ggplot() + 
  #geom_line(data = USDON, aes(x = date , y = rate_gap,color="USDON")) +
  #geom_line(data = USD1WK, aes(x = date, y = rate_gap,color="USD1WK")) +
  geom_line(data = USD1MT, aes(x = date, y = rate_gap,color="USD1MT")) +
  #geom_line(data = USD2MT, aes(x = date, y = rate_gap,color="USD2MT"))+
  geom_line(data = USD3MT, aes(x = date, y = rate_gap,color="USD3MT"))+
  geom_line(data = USD6MT, aes(x = date, y = rate_gap,color="USD6MT"))+
  geom_line(data = USD12MD, aes(x = date, y = rate_gap,color="USD12MD"))+
  xlab('Dates') +
  ylab('Rates')+
  labs(color="Tenors")+ggtitle("USD LIBOR 30-day increments")

ggplot() + 
  #geom_line(data = USDON, aes(x = date , y = rate_gap,color="USDON")) +
  #geom_line(data = USD1WK, aes(x = date, y = rate_gap,color="USD1WK")) +
  geom_line(data = CAD1MT, aes(x = date, y = rate_gap,color="CAD1MT")) +
  #geom_line(data = USD2MT, aes(x = date, y = rate_gap,color="USD2MT"))+
  geom_line(data = CAD3MT, aes(x = date, y = rate_gap,color="CAD3MT"))+
  geom_line(data = CAD6MT, aes(x = date, y = rate_gap,color="CAD6MT"))+
  geom_line(data = CAD12MD, aes(x = date, y = rate_gap,color="CAD12MD"))+
  xlab('Dates') +
  ylab('Rates')+
  labs(color="Tenors")+ggtitle("CAD LIBOR 30-day increments")


#Compute Correlations USD
usd_rate <- cbind(USD1MT$rate_gap,USD3MT$rate_gap,USD6MT$rate_gap,USD12MD$rate_gap)
usd_rate = na.approx(usd_rate)
colnames(usd_rate) <- c("USD1MT","USD3MT","USD6MT","USD12MD")
ggcorr(usd_rate,label = T)+ggtitle("USD LIBOR correlations 30-day increments")
#usd_cor <- cor(usd_rate)
#corrplot(usd_cor, method = "color")

#Compute Correlations CADD
cad_rate <- cbind(CAD1MT$rate_gap,CAD3MT$rate_gap,CAD6MT$rate_gap,CAD12MD$rate_gap)
cad_rate = na.approx(cad_rate)
colnames(cad_rate) <- c("CAD1MT","CAD3MT","CAD6MT","CAD12MD")
ggcorr(cad_rate,label = T)+ggtitle("CAD LIBOR correlations 30-day increments")
#cad_cor <- cor(cad_rate)
#corrplot(cad_cor, method = "color")

#Average Term structure (ATS)
par(mfrow=c(1,2))
usd_rate <- cbind(USD1MT$rate,USD3MT$rate,USD6MT$rate,USD12MD$rate)
colnames(usd_rate) <- c("USD1MT","USD3MT","USD6MT","USD12MD")
cad_rate <- cbind(CAD1MT$rate,CAD3MT$rate,CAD6MT$rate,CAD12MD$rate)
colnames(cad_rate) <- c("CAD1MT","CAD3MT","CAD6MT","CAD12MD")
ats(usd_rate)
axis(1,at=1:4,labels =colnames(usd_rate))
ats(cad_rate)
axis(1,at=1:4,labels =colnames(cad_rate))

par(mfrow=c(1,2))
ats_sd(usd_rate)
axis(1,at=1:4,labels =colnames(usd_rate))
ats_sd(cad_rate)
axis(1,at=1:4,labels =colnames(cad_rate))




usd_list<-list(USD1MT,USD3MT,USD6MT,USD12MD)
cad_list<-list(CAD1MT,CAD3MT,CAD6MT,CAD12MD)

#jpeg('usdplot.jpg')
par(mfrow = c(2,2))
for (i in 1:4){
  
  hist(data.frame(usd_list[i])$rate_gap,main=paste("1-day inc. histogram for",colnames(usd_rate)[i]),xlab = "rates", freq = F)
  
}
#dev.off()

#jpeg('cadplot.jpg')
par(mfrow = c(2,2))
for (i in 1:4){
  
  hist(data.frame(cad_list[i])$rate_gap,main=paste("1-day inc. Histogram of",colnames(cad_rate)[i]),xlab = "rates", freq = F)
  
}



#PRINCIPAL COMPONENT ANALYSIS

usd_rate <- data.frame(USD1MT$rate_gap,USD3MT$rate_gap,USD6MT$rate_gap,USD12MD$rate_gap)
cad_rate <- data.frame(CAD1MT$rate_gap,CAD3MT$rate_gap,CAD6MT$rate_gap,CAD12MD$rate_gap)
#usd_rate = tail(usd_rate,1000)
par(mfrow=c(1,1))
pca_usd <- pca(usd_rate) #uses the function PCA
fviz_eig(pca_usd,addlabels=TRUE,ylim = c(0, 100))

pca_cad <- pca(cad_rate)
fviz_eig(pca_cad,addlabels=TRUE,ylim = c(0, 100))


#SCALE FACTOR COMPUTATION
a <- ratejump(USD12MD,0.1,30)$rate_gap
b <- ratejump(USD12MD,0.1,1)$rate_gap
linearMod <- lm(b~a)
print(linearMod)
summary(linearMod)

plot(a,b)
cor(a,b)



#VASICEK MODEL
mod1 = setModel(drift = "kappa*(mu-r)", diffusion = "sigma",state.variable = "r", time.variable = "t", solve.variable = "r" )
data = setYuima(model = mod1, data = setData(USD1MT$rate))
initials = list(mu =2.5, sigma=3,kappa = 1 )
lwb = list(mu = 0, sigma = 0,kappa =0)
upb = list(mu = 5, sigma = 5,kappa =1)
mle = qmle(data, start = initials, lower =lwb, upper =upb)
summary(mle)

prior <- list(mu =list(measure.type="code",df= "dgamma(z,0,5)"), 
              sigma=list(measure.type="code",df= "dgamma(z,0,5)"),
              kappa = list(measure.type="code",df= "dgamma(z,0,1)"))
bayes <- adaBayes(data, start = initials, prior = prior,lower =lwb, upper =upb)

#VASICEK MODEL WITH CORRELATED BROWNIAN MOTION

set.seed(123)
X <- simulate(mod1,true.parameter = list(kappa=0,mu =2.765,sigma =3.045))
plot(X)

var_ir <-apply(usd_rate,2,sd)
mu_ir <- colMeans(usd_rate)
mu <- mu_ir[1]
new_var <- var_ir*pca_usd$x[,1:3]
a1 <- new_var[1,1]
a2 <- new_var[1,2]
a3 <- new_var[1,3]

a <- "mu*r"
b <- matrix(c("a1","a2","a3"),1,3)
mod1 = setModel(drift = a, diffusion = b,state.variable = "r", time.variable = "t", solve.variable = "r" )
set.seed(123)
str(mod1)
my.dW <- matrix( rnorm(n , 0, sqrt(delta)), nrow=1, ncol=n)
X <- simulate(mod1,true.param=list(a1 =new_var[1,1],a2 = new_var[1,2], a3 = new_var[1,3]),delta=1/252,increment.W=my.dW)
plot(X)

par(mfrow=c(1,1))
#VarianceGamma Distribution
USD12MD_30_day_inc <- USD12MD$rate_gap
descdist(USD12MD_30_day_inc, boot = 1000)
par(mfrow=c(2,2))
vgFit(USD12MD_30_day_inc,plots = T)





