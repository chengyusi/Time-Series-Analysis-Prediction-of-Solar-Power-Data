

# predict by daily data
# 1a monthly seasonal    next day fcst?
# acf eacf ...
# r^2 risidual
# 2a next day fcst
# hourly differnece


rm(list = ls())

library(forecast)
library(readxl)
library(TSA)


################################ 1a

KSEG_ <- read_excel("KSEG_.xlsx")
# KSEG <- read_excel("C:/Users/scy_a/Dropbox/Fall2018/TSA/KSEG_.xlsx")
attach(KSEG_)

TEMPts = ts(KSEG_$TEMP,  start =1, frequency=24*365)
RelHumts = ts(KSEG_$RelHum,  start =1, frequency=24*365)
WindSpeedts = ts(KSEG_$WindSpeed,  start =1, frequency=24*365)

########## TEMP

# diff data
plot(TEMPts, xlab='time',,ylab='TEMP')
acf(as.vector(TEMPts),main=("Sample ACF of TEMP Levels"))
pacf(as.vector(TEMPts),main=("Sample PACF of TEMP Levels"))
decompose_temp=decompose(TEMPts)
plot(decompose_temp)

plot(diff(TEMPts),xlab='time',ylab='First Diff of TEMP')
acf(as.vector(diff(TEMPts)),main="ACF on First Difference")
pacf(as.vector(diff(TEMPts)),main=("Sample PACF of First Difference"))
decompose_difftemp=decompose(diff(TEMPts))
plot(decompose_difftemp)


plot(diff(diff(TEMPts,lag=24)),xlab='Time', ylab='First and Seasonal Difference of TEMPts')
acf(as.vector(diff(diff(TEMPts),lag=24)),lag.max=50,ci.type='ma')
pacf(as.vector(TEMP),main=("Sample ACF of Seasonal Difference of TEMPts"))
decompose_sdifftemp=decompose(diff(diff(TEMPts,lag=24)))
plot(decompose_sdifftemp)

# auto.arima(TEMPts)

# check AIC
m1.TEMPts=arima(TEMPts,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=24))
m1.TEMPts
m2.TEMPts=arima(TEMPts,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=24))
m2.TEMPts
m3.TEMPts=arima(TEMPts,order=c(0,1,0),seasonal=list(order=c(2,1,1),period=24))
m3.TEMPts
m4.TEMPts=arima(TEMPts,order=c(1,1,0),seasonal=list(order=c(1,1,1),period=24))
m4.TEMPts
m4.TEMPts=arima(TEMPts,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=24))
m4.TEMPts


# residuals of model

plot(rstandard(m4.TEMPts),ylab='Standardized residuals',type='l')
qqnorm(residuals(m4.TEMPts)); qqline(residuals(m4.TEMPts))
hist(window(rstandard(m4.TEMPts)),xlab='Standardized Residuals')

acf(residuals(m4.TEMPts))


fitted=TEMPts-resid(m4.TEMPts)    
r.TEMPts=cor(fitted,TEMPts)
r2.TEMPts= (r.TEMPts)^2
r2.TEMPts

# forecast
plot(m4.TEMPts,n.ahead=24*2,ylab='Series, Forecasts, Actuals & Limits',pch=19)
abline(h=coef(m4.TEMPts)[names(coef(m4.TEMPts))=='intercept'])




# RelHum


# diff data
plot(RelHumts, xlab='time',,ylab='RelHumts')
acf(as.vector(RelHumts),main=("Sample ACF of RelHumts Levels"))
pacf(as.vector(RelHumts),main=("Sample PACF of RelHumts Levels"))
decompose_RelHumts=decompose(RelHumts)
plot(decompose_RelHumts)

plot(diff(RelHumts),xlab='time',ylab='First Diff of RelHumts')
acf(as.vector(diff(RelHumts)),main="ACF on First Difference")
pacf(as.vector(diff(RelHumts)),main=("Sample PACF of First Difference"))
decompose_diffRelHumts=decompose(diff(RelHumts))
plot(decompose_diffRelHumts)


plot(diff(diff(RelHumts,lag=24)),xlab='Time', ylab='First and Seasonal Difference of RelHumts')
acf(as.vector(diff(diff(RelHumts),lag=24)),lag.max=50,ci.type='ma')
pacf(as.vector(RelHumts),main=("Sample ACF of Seasonal Difference of RelHumts"))
decompose_sdiffRelHumts=decompose(diff(diff(RelHumts,lag=24)))
plot(decompose_sdiffRelHumts)

# auto.arima(TEMPts)

# check AIC
m1.RelHumts=arima(RelHumts,order=c(1,1,1),seasonal=list(order=c(1,1,2),period=24))
m1.RelHumts
m2.RelHumts=arima(RelHumts,order=c(1,1,0),seasonal=list(order=c(1,1,1),period=24))
m2.RelHumts
m4.RelHumts=arima(RelHumts,order=c(1,1,0),seasonal=list(order=c(1,1,2),period=24))
m4.RelHumts
m5.RelHumts=arima(RelHumts,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=24))
m5.RelHumts

fitted=RelHumts-resid(m2.RelHumts)    
r.RelHumts=cor(fitted,RelHumts)
r2.RelHumts= (r.RelHumts)^2
r2.RelHumts

# residuals of model

plot(rstandard(m2.RelHumts),ylab='Standardized residuals',type='l')
qqnorm(residuals(m2.RelHumts)); qqline(residuals(m2.RelHumts))
hist(window(rstandard(m2.RelHumts)),xlab='Standardized Residuals')

acf(residuals(m2.RelHumts))

# forecast
plot(m2.RelHumts,n.ahead=24*2,ylab='Series, Forecasts, Actuals & Limits',pch=19)
abline(h=coef(m2.RelHumts)[names(coef(m2.RelHumts))=='intercept'])





# WindSpeedts


# diff data
plot(WindSpeedts, xlab='time',,ylab='WindSpeedts')
acf(as.vector(WindSpeedts),main=("Sample ACF of WindSpeedts Levels"))
pacf(as.vector(WindSpeedts),main=("Sample PACF of WindSpeedts Levels"))
decompose_WindSpeedts=decompose(WindSpeedts)
plot(decompose_WindSpeedts)

plot(diff(WindSpeedts),xlab='time',ylab='First Diff of WindSpeedts')
acf(as.vector(diff(WindSpeedts)),main="ACF on First Difference")
pacf(as.vector(diff(WindSpeedts)),main=("Sample PACF of First Difference"))
decompose_diffWindSpeedts=decompose(diff(WindSpeedts))
plot(decompose_diffWindSpeedts)


plot(diff(diff(WindSpeedts,lag=24)),xlab='Time', ylab='First and Seasonal Difference of WindSpeedts')
acf(as.vector(diff(diff(WindSpeedts),lag=24)),lag.max=50,ci.type='ma')
pacf(as.vector(WindSpeedts),main=("Sample ACF of Seasonal Difference of WindSpeedts"))
decompose_sdiffWindSpeedts=decompose(diff(diff(WindSpeedts,lag=24)))
plot(decompose_sdiffWindSpeedts)

# auto.arima(TEMPts)

# check AIC
m1.WindSpeedts=arima(WindSpeedts,order=c(1,1,3),seasonal=list(order=c(1,1,3),period=24))
m1.WindSpeedts
m2.WindSpeedts=arima(WindSpeedts,order=c(1,1,2),seasonal=list(order=c(1,1,2),period=24))
m2.WindSpeedts
m3.WindSpeedts=arima(WindSpeedts,order=c(1,1,2),seasonal=list(order=c(1,1,1),period=24))
m3.WindSpeedts


fitted=WindSpeedts-resid(m3.WindSpeedts)    
r.WindSpeedts=cor(fitted,WindSpeedts)
r2.WindSpeedts= (r.WindSpeedts)^2
r2.WindSpeedts

# residuals of model

plot(rstandard(m3.WindSpeedts),ylab='Standardized residuals',type='l')
qqnorm(residuals(m3.WindSpeedts)); qqline(residuals(m3.WindSpeedts))
hist(window(rstandard(m3.WindSpeedts)),xlab='Standardized Residuals')

acf(residuals(m3.WindSpeedts))
pacf(residuals(m3.WindSpeedts))
# forecast
plot(m3.WindSpeedts,n.ahead=24*2,ylab='Series, Forecasts, Actuals & Limits',pch=19)
abline(h=coef(m3.WindSpeedts)[names(coef(m3.WindSpeedts))=='intercept'])















##################### 1b

################ TEMPts_daily


library(readxl)
KSEG_daily <- read_excel("KSEG_daily.xls")

attach(KSEG_daily)

TEMPts_daily = ts(KSEG_daily$max_temp,  start =2010, frequency=365)
RelHumts_daily = ts(KSEG_daily$mean_hum,  start =2010, frequency=365)
WindSpeedts_daily = ts(KSEG_daily$mean_wind,  start =2010, frequency=365)



plot(TEMPts_daily, xlab='time',,ylab='TEMPts_daily')
acf(as.vector(TEMPts_daily),main=("Sample ACF of TEMPts_daily Levels"))
decompose_tempdl=decompose(TEMPts_daily)
plot(decompose_tempdl)

plot(diff(TEMPts_daily),xlab='time',ylab='First Diff of TEMPts_daily')
acf(as.vector(diff(TEMPts_daily)),main="ACF on First Difference")
pacf(as.vector(TEMPts_daily),main=("Sample PACF of First Difference"))
decompose_difftemp=decompose(diff(TEMPts_daily))
plot(decompose_difftemp)


plot(diff(diff(TEMPts_daily,lag=365)),xlab='Time', ylab='First and Seasonal Difference of TEMPts_daily')
acf(as.vector(diff(diff(TEMPts_daily),lag=365)),lag.max=50,ci.type='ma')
pacf(as.vector(TEMPts_daily),main=("Sample PACF of TEMPts_daily"))
decompose_sdifftemp=decompose(diff(diff(TEMPts_daily,lag=365)))
plot(decompose_sdifftemp)

auto.arima(TEMPts_daily)


m1.TEMPts_daily=arima(TEMPts_daily,order=c(1,1,3),seasonal=list(order=c(1,1,3),period=10))
m1.TEMPts_daily
m2.TEMPts_daily=arima(TEMPts_daily,order=c(1,1,2),seasonal=list(order=c(1,1,2),period=10))
m2.TEMPts_daily
m3.TEMPts_daily=arima(TEMPts_daily,order=c(1,1,2),seasonal=list(order=c(1,1,1),period=10))
m3.TEMPts_daily


fitted=TEMPts_daily-resid(m3.TEMPts_daily)    
r.TEMPts_daily=cor(fitted,TEMPts_daily)
r2.TEMPts_daily= (r.TEMPts_daily)^2
r2.TEMPts_daily



auto1 <-auto.arima(TEMPts_daily)
auto1

# residuals of model

plot(rstandard(m3.TEMPts_daily),ylab='Standardized residuals',type='l')
qqnorm(residuals(m3.TEMPts_daily)); qqline(residuals(m3.TEMPts_daily))
hist(window(rstandard(m3.TEMPts_daily)),xlab='Standardized Residuals')

acf(residuals(m3.TEMPts_daily))
pacf(residuals(m3.TEMPts_daily))
# forecast
plot(m3.TEMPts_daily,n.ahead=30,ylab='Series, Forecasts, Actuals & Limits',pch=19)
abline(h=coef(m3.TEMPts_daily)[names(coef(m3.TEMPts_daily))=='intercept'])



########
# RelHumts_daily
# diff data


plot(RelHumts_daily, xlab='time',,ylab='RelHumts_daily')
acf(as.vector(RelHumts_daily),main=("Sample ACF of RelHumts_daily Levels"))
pacf(as.vector(RelHumts_daily),main=("Sample PACF of RelHumts_daily Levels"))
decompose_WindSpeedts=decompose(RelHumts_daily)
plot(decompose_WindSpeedts)

plot(diff(RelHumts_daily),xlab='time',ylab='First Diff of RelHumts_daily')
acf(as.vector(diff(RelHumts_daily)),main="ACF on First Difference")
pacf(as.vector(diff(RelHumts_daily)),main=("Sample PACF of First Difference"))
decompose_diffWindSpeedts=decompose(diff(RelHumts_daily))
plot(decompose_diffWindSpeedts)


plot(diff(diff(RelHumts_daily,lag=365)),xlab='Time', ylab='First and Seasonal Difference of RelHumts_daily')
acf(as.vector(diff(diff(RelHumts_daily),lag=365)),lag.max=50,ci.type='ma')
pacf(as.vector(RelHumts_daily),main=("Sample ACF of Seasonal Difference of RelHumts_daily"))
decompose_sdiffWindSpeedts=decompose(diff(diff(RelHumts_daily,lag=365)))
plot(decompose_sdiffWindSpeedts)


# auto.arima(TEMPts)

# check AIC
m1.RelHumts_daily=arima(RelHumts_daily,order=c(1,1,2),seasonal=list(order=c(1,1,2),period=10))
m1.RelHumts_daily
m2.RelHumts_daily=arima(RelHumts_daily,order=c(2,1,2),seasonal=list(order=c(1,1,2),period=10))
m2.RelHumts_daily


fitted=RelHumts_daily-resid(m2.RelHumts_daily)    
r.RelHumts_daily=cor(fitted,RelHumts_daily)
r2.RelHumts_daily= (r.RelHumts_daily)^2
r2.RelHumts_daily



auto2<-auto.arima(RelHumts_daily)
auto2
# residuals of model

plot(rstandard(m2.RelHumts_daily),ylab='Standardized residuals',type='l')
qqnorm(residuals(m2.RelHumts_daily)); qqline(residuals(m2.RelHumts_daily))
hist(window(rstandard(m2.RelHumts_daily)),xlab='Standardized Residuals')

acf(residuals(m2.RelHumts_daily))
pacf(residuals(m2.RelHumts_daily))
# forecast
plot(m2.RelHumts_daily,n.ahead=30,ylab='Series, Forecasts, Actuals & Limits',pch=19)
abline(h=coef(m2.RelHumts_daily)[names(coef(m2.RelHumts_daily))=='intercept'])





########
# WindSpeedts_daily
# diff data


plot(WindSpeedts_daily, xlab='time',,ylab='WindSpeedts_daily')
acf(as.vector(WindSpeedts_daily),main=("Sample ACF of WindSpeedts_daily Levels"))
pacf(as.vector(WindSpeedts_daily),main=("Sample PACF of WindSpeedts_daily Levels"))
decompose_WindSpeedts=decompose(WindSpeedts_daily)
plot(decompose_WindSpeedts)

plot(diff(WindSpeedts_daily),xlab='time',ylab='First Diff of WindSpeedts_daily')
acf(as.vector(diff(WindSpeedts_daily)),main="ACF on First Difference")
pacf(as.vector(diff(WindSpeedts_daily)),main=("Sample PACF of First Difference"))
decompose_diffWindSpeedts=decompose(diff(WindSpeedts_daily))
plot(decompose_diffWindSpeedts)


plot(diff(diff(WindSpeedts_daily,lag=365)),xlab='Time', ylab='First and Seasonal Difference of RelHumts_daily')
acf(as.vector(diff(diff(WindSpeedts_daily),lag=365)),lag.max=50,ci.type='ma')
pacf(as.vector(WindSpeedts_daily),main=("Sample PACF of Seasonal Difference of WindSpeedts_daily"))
decompose_sdiffWindSpeedts=decompose(diff(diff(WindSpeedts_daily,lag=365)))
plot(decompose_sdiffWindSpeedts)


# auto.arima(TEMPts)

# check AIC
m1.WindSpeedts_daily=arima(WindSpeedts_daily,order=c(1,1,2),seasonal=list(order=c(1,1,2),period=10))
m1.WindSpeedts_daily
m2.WindSpeedts_daily=arima(WindSpeedts_daily,order=c(2,1,2),seasonal=list(order=c(1,1,2),period=10))
m2.WindSpeedts_daily



fitted=WindSpeedts_daily-resid(m2.WindSpeedts_daily)    
r.WindSpeedts_daily=cor(fitted,WindSpeedts_daily)
r2.WindSpeedts_daily= (r.WindSpeedts_daily)^2
r2.WindSpeedts_daily


auto3<-auto.arima(WindSpeedts_daily)
auto3

# residuals of model

plot(rstandard(m2.WindSpeedts_daily),ylab='Standardized residuals',type='l')
qqnorm(residuals(m2.WindSpeedts_daily)); qqline(residuals(m2.WindSpeedts_daily))
hist(window(rstandard(m2.WindSpeedts_daily)),xlab='Standardized Residuals')

acf(residuals(m2.WindSpeedts_daily))
pacf(residuals(m2.WindSpeedts_daily))
# forecast
plot(m2.WindSpeedts_daily,n.ahead=60,ylab='Series, Forecasts, Actuals & Limits',pch=19)
abline(h=coef(m2.WindSpeedts_daily)[names(coef(m2.WindSpeedts_daily))=='intercept'])
















############## 2a


# import PV and roll up to hourly
PVgen <- read_excel("SolarPowerGen_.xlsx")
# PVgen = read.csv("C:/Users/scy_a/Dropbox/Fall2018/TSA/SolarPowerGen_.csv")
PVgen$hour <- rep(1:8087, each=4) 
PVgen = aggregate(PVgen$UNITS_GENERATED, by = list(PVgen$hour), FUN=sum)
summary(PVgen)
PVts=ts(PVgen$x, start=1, frequency=8670)

write.csv(PVgen$x, file="PV.csv", row.names=F)


plot(PVts, xlab='time',ylab='PVgen')



plot(PVts, xlab='time',,ylab='PVts')
acf(as.vector(PVts),main=("Sample ACF of PVts Levels"))
pacf(as.vector(PVts),main=("Sample PACF of PVts Levels"))
decompose_WindSpeedts=decompose(PVts)
plot(decompose_WindSpeedts)

plot(diff(PVts),xlab='time',ylab='First Diff of PVts')
acf(as.vector(diff(PVts)),main="ACF on First Difference")
pacf(as.vector(diff(PVts)),main=("Sample PACF of First Difference"))
decompose_diffWindSpeedts=decompose(diff(PVts))
plot(decompose_diffWindSpeedts)

plot(diff(diff(PVts,lag=24)),xlab='Time', ylab='First and Seasonal Difference of PVts')
acf(as.vector(diff(diff(PVts),lag=24)),lag.max=50,ci.type='ma')
pacf(as.vector(PVts),main=("Sample ACF of Seasonal Difference of PVts"))
decompose_sdiffWindSpeedts=decompose(diff(diff(PVts,lag=24)))
plot(decompose_sdiffWindSpeedts)


# auto.arima(TEMPts)

# check AIC
m1.PVts=arima(PVts,order=c(1,1,1))
m1.PVts
m2.PVts=arima(PVts,order=c(2,1,2))
m2.PVts


# check AIC
m1.PVts=arima(PVts,order=c(2,1,2),seasonal=list(order=c(2,1,0),period=24))
m1.PVts
m2.PVts=arima(PVts,order=c(1,1,2),seasonal=list(order=c(2,1,1),period=24))
m2.PVts
m3.PVts=arima(PVts,order=c(1,1,2),seasonal=list(order=c(2,1,0),period=24))
m3.PVts


fitted=PVts-resid(m2.PVts)    
r.PVts=cor(fitted,PVts)
r2.PVts= (r.PVts)^2
r2.PVts

auto2<-auto.arima(RelHumts_daily)
auto2
# residuals of model

plot(rstandard(m2.PVts),ylab='Standardized residuals',type='l')
qqnorm(residuals(m2.PVts)); qqline(residuals(m2.PVts))
hist(window(rstandard(m2.PVts)),xlab='Standardized Residuals')

acf(residuals(m2.PVts))
pacf(residuals(m2.PVts))
# forecast
plot(m2.PVts,n.ahead=24*2,ylab='Series, Forecasts, Actuals & Limits',pch=19)
abline(h=coef(m2.PVts)[names(coef(m2.PVts))=='intercept'])



################# 2b
rm(list = ls())

data_2b <- read_excel("data_2b.xlsx")
# KSEG <- read_excel("C:/Users/scy_a/Dropbox/Fall2018/TSA/KSEG_.xlsx")
attach(data_2b)

TEMP = ts(data_2b$TEMP,  start =1, frequency=24*365)
RelHum = ts(data_2b$RelHum,  start =1, frequency=24*365)
WindSpeed = ts(data_2b$WindSpeed,  start =1, frequency=24*365)
ATMO = ts(data_2b$ATMOSPRESSURE,  start =1, frequency=24*365)
NORMAL = ts(data_2b$DIRECT_NORMAL_RADIATION,  start =1, frequency=24*365)
HOZ = ts(data_2b$DIFFUSE_HOZ_RADIATION,  start =1, frequency=24*365)
SOLAR = ts(data_2b$DOWNWARD_SOLAR_RADIATION,  start =1, frequency=24*365)
PV = ts(data_2b$x,  start =1, frequency=24*365)
LPV = log(PV)



data_ab_ts = ts.intersect(TEMP,RelHum,WindSpeed,ATMOSPRESSURE,DIRECT_NORMAL_RADIATION,DIFFUSE_HOZ_RADIATION,DOWNWARD_SOLAR_RADIATION,x)

input.ts = ts.intersect(TEMP,RelHum,WindSpeed,ATMOSPRESSURE)



plot(data_ab_ts, yax.flip = T)


# OLS
PV.ols = lm(PV~TEMP+RelHum+WindSpeed+ATMOSPRESSURE)
summary(PV.ols)


plot(rstandard(PV.ols),ylab='Standardized residuals',type='l')
qqnorm(residuals(PV.ols)); qqline(residuals(PV.ols))
hist(window(rstandard(PV.ols)),xlab='Standardized Residuals')


# Sample ACF of Residuals from OLS Regression
acf(residuals(PV.ols),ci.type='ma')
pacf(residuals(PV.ols))

PV.ols.step=step(PV.ols,direction="both")

# reg
PV.reg=arima(PV,order=c(1,1,2),seasonal=list(order=c(2,1,1),period=24),xreg=(input.ts))
PV.reg


plot(residuals(PV.reg),ylab='Standardized residuals',type='l')
qqnorm(residuals(PV.reg)); qqline(residuals(PV.reg))
hist(window(residuals(PV.reg)),xlab='Standardized Residuals')

acf(residuals(PV.reg),ci.type='ma')
pacf(residuals(PV.reg))

# forecast
plot(PV.reg,n.ahead=24*2,ylab='Series, Forecasts, Actuals & Limits',pch=19)
abline(h=coef(PV.reg)[names(coef(PV.reg))=='intercept'])


predict(PV.reg, PV, 24)





