library(readxl)
library(lmtest)
library(forecast)
library(DIMORA)
library(dplyr)
library("tidyr")
Nba_merge <- read.csv("C:/Users/UTENTE/Downloads/nba_stats_merge2021_1.csv",stringsAsFactors=F, na.strings=c(NA,"NA"," NA"))
Nba_merge <- select(Nba_merge,-X, -Unnamed..0, -blanl, -position_x, -position_y, -blank2 )
Nba_merge <- Nba_merge %>% distinct(name,season_end, .keep_all = TRUE)
na.omit(Nba_merge$name)
summary(Nba_merge)
str(Nba_merge)


Nba_merge <- Nba_merge[Nba_merge['season_end'] >= '1991',]
Nba_merge <- Nba_merge[Nba_merge['season_end'] < '2021',]
agg_nba <- aggregate(salary ~ season_end , data=Nba_merge, sum)
plot(agg_nba$season_end,agg_nba$salary, type='l', lwd=2, ylab="", xlab="")
agg_nba

plot(agg_nba$season_end,log(agg_nba$salary), type='l', lwd=2)
log.sal <- log(agg_nba$salary)
log.sal
plot(log.sal)


#Nba_merge$salary <- log(Nba_merge$salary)
#agg_nba <- aggregate(salary ~ season_end , data=Nba_merge, sum)
#plot(agg_nba, type='b')


agg_points <- aggregate( X3PA ~ season_end , data=Nba_merge, sum )
plot(agg_points, type='b')

acf(agg_nba$salary)
acf(log.sal)
fit1_nba <- lm(log.sal~agg_nba$season_end)
summary(fit1_nba)
abline(fit1_nba,col='3')
dwtest(fit1_nba)
checkresiduals(fit1_nba)




Nba_merge.cor = cor(Nba_merge[sapply(Nba_merge, is.numeric)],Nba_merge$salary,use="pairwise.complete.obs")
Nba_merge.cor
library(corrplot)
#corrplot(Nba_merge.cor, method="color", type='upper',order="hclust", sig.level = 0.1)



salary_cap <- read.csv("C:/Users/UTENTE/Downloads/salary_cap.csv")
colnames(salary_cap)[2]  <- "cap"
colnames(salary_cap)[1]  <- "year"
plot(salary_cap)
Nba_merge$salary_pct <- 0
for(i in 1:nrow(Nba_merge)) {
  Nba_merge$salary_pct[i] <- (Nba_merge$salary[i]/salary_cap$cap[salary_cap$year==Nba_merge$season_end[i]])*100
}

# Number of players in each season that gain more than 10% of the salary cap divided by role
n_players_FC <- Nba_merge %>%
  group_by(season_end) %>%
  summarize(count = sum(salary_pct >18 &  (Pos == "C" | Pos == "SF" | Pos == "PF")))
n_players_G <- Nba_merge %>%
  group_by(season_end) %>%
  summarize(count = sum(salary_pct >18 &  (Pos == "PG" | Pos == "SG")))


countries_USA <- c(
  'Alaska',
  'Alabama',
  'Arkansas',
  'Arizona',
  'California',
  'Colorado',
  'Connecticut',
  'District of Columbia',
  'Delaware',
  'Florida',
  'Georgia',
  'Hawaii',
  'Iowa',
  'Idaho',
  'Illinois',
  'Indiana',
  'Kansas',
  'Kentucky',
  'Louisiana',
  'Massachusetts',
  'Maryland',
  'Maine',
  'Michigan',
  'Minnesota',
  'Missouri',
  'Mississippi',
  'Montana',
  'North Carolina',
  'North Dakota',
  'Nebraska',
  'New Hampshire',
  'New Jersey',
  'New Mexico',
  'Nevada',
  'New York',
  'Ohio',
  'Oklahoma',
  'Oregon',
  'Pennsylvania',
  'Rhode Island',
  'South Carolina',
  'South Dakota',
  'Tennessee',
  'Texas',
  'Utah',
  'Virginia',
  'Vermont',
  'Washington',
  'Wisconsin',
  'West Virginia',
  'Wyoming'
)

#num players usa vs world


#SALARY USA VS WORLD
bm_nba.world <- BM(n_world$`n()`,prelimestimates = c(3000,
                                                     BM(n_world$`n()`, display=FALSE)$Estimate[2,1],
                                                     BM(n_world$`n()`, display=FALSE)$Estimate[3,1]),display = T)
summary(bm_nba.world)
bm_nba.usa <- BM(n_usa$`n()`,display = T)

bn <- boxplot(Nba_merge$salary_pct)
Summary<-boxplot(Nba_merge$salary_pct)$stats
colnames(Summary)<-c("x")
rownames(Summary)<-c("Min","First Quartile","Median","Third Quartile","Maximum")
Summary


Nba_merge_filtered <- Nba_merge %>% filter(salary_pct > 1.84 & salary_pct < 18.3)
world_df <- Nba_merge_filtered %>% filter(!(sub(".*, ", "", Nba_merge_filtered$birthPlace) %in% countries_USA ))
n_world <- world_df %>% group_by(season_end) %>% summarize(n())
agg_nba_world <- aggregate(salary ~ season_end , data=world_df, sum)
usa_df <- Nba_merge_filtered %>% filter(sub(".*, ", "", Nba_merge_filtered$birthPlace) %in% countries_USA )
n_usa <- usa_df %>% group_by(season_end) %>% summarize(n())
agg_nba_usa <- aggregate(salary ~ season_end , data=usa_df, sum)
plot(agg_nba_world$salary)
range_world <- agg_nba_world$salary[1:29]
ucrcd_nba <- UCRCD(n_world$`n()`,n_usa$`n()`, display=T)
summary(ucrcd_nba)



res_nba <- residuals(fit1_nba)
Acf(res_nba)
plot(res_nba, xlab="time",ylab="residuals")
Nba_ts <- ts(agg_nba$salary, frequency = 4) #quarterly data
Nba_ts
ts.plot(Nba_ts,type="o")
fit_nba_ts <- tslm(Nba_ts~trend) #trend stays for linear trend / modelling a time series object
summary(fit_nba_ts)
plot(forecast(fit_nba_ts))
res_nba <- residuals(fit_nba_ts)
checkresiduals(fit_nba_ts)
dwtest(fit_nba_ts)



bm_nba <- BM(agg_nba$salary,display = T)
summary(bm_nba)
checkresiduals(bm_nba)
pred_bm_nba <- predict(bm_nba, newx = c(0:50))
pred_inst_nba = make.instantaneous(pred_bm_nba)
plot(agg_nba$salary)
lines(pred_inst_nba, lwd=2, col=2)



GBMr1_NBA<- GBM(agg_nba$salary,shock='exp',nshock=1,prelimestimates = c(10e+09,
                                                                        BM(agg_nba$salary, display=FALSE)$Estimate[2,1],
                                                                        BM(agg_nba$salary, display=FALSE)$Estimate[3,1], 24, -0.1, 1.5), oos=2)
summary(GBMr1_NBA)
checkresiduals(GBMr1_NBA)
GGM_nba<- GGM(agg_nba$salary, prelimestimates = c(BM(agg_nba$salary, display=FALSE)$Estimate[1,1], 0.01,0.1,0.1,0.1),oos=5)
Acf(agg_nba$salary)
Pacf(agg_nba$salary)
summary(GGM_nba)
checkresiduals(GGM_nba)
GGM_nba_res <- residuals(GGM_nba)
plot(GGM_nba_res)
r.ad <- 1-(((1-GGM_nba$Rsquared)*28)/25)
ggmbn <- boxplot(GGM_nba_res)
Summary<-ggmbn$stats
hist(GGM_nba_res,col="orange", xlab="")
#median(GGM_nba$Estimate)


Acf(log.sal)
Pacf(log.sal)
plot(log.sal)
log.sal %>% diff() %>% ggtsdisplay(main="")
log.sal %>% diff() %>% diff() %>% ggtsdisplay(main="")
Pacf(agg_nba$salary)
ar_nba <- Arima(log.sal, order=c(0,1,0))
ar_nba2 <- Arima(log.sal, order=c(1,1,0))
auto.nba <-auto.arima(log(agg_nba$salary), xreg = Nba_merge$PTS)
auto_nba <- auto.arima(log.sal[1:29])
auto_nba
ar_nba
ar_nba2
plot(log.sal, type='b', lwd=2)
lines(fitted(auto_nba),col=2)
fitted(auto_nba)
log.sal
ar_nba
pacf(residuals(auto_nba))
checkresiduals((ar_nba))
pacf(residuals(auto_nba))
plot(residuals(auto_nba), type='p')
checkresiduals((auto_nba))
dwtest(ar_nba)
plot(forecast(auto_nba))
forecas.arima <- forecast(auto_nba)
forecas.arima

install.packages('forecast')

library('forecast')

nbases = HoltWinters(agg_nba$salary, beta=FALSE, gamma=FALSE)
summary(nbases)
plot(nbases)


arima_europe <- auto.arima(n_world$`n()`)
arima_europe
plot(n_world$`n()`)
lines(fitted(arima_europe), col=2)
checkresiduals(arima_europe)

GGM_res_nba <- residuals(GGM_nba)
plot(Acf(GGM_res_nba)) #strong corrleation among residuals needs to be understood
fit_GGM_nba <- fitted(GGM_nba)
#fit and predict produce the same thing for the observed time window
#predicted values are able to fit out of sample, fitted don't
fit_GM_inst_nba <- make.instantaneous(fit_GGM_nba)
#SARMAX consider ARMA and as external covariate the predictions
plot(agg_nba$salary)
s2_nba <- Arima(cumsum(agg_nba$salary), order=c(0,1,0), seasonal=list(order=c(0,1,0), period=4), xreg=fit_GGM_nba)
#I'm modelling the residuals of GGM with an arma model 
summary(s2_nba)
#yt = ARMA + c*y_hat (y_hat = fitted GGM)
#By considering the fitted GGM I'm already extracting the trend so the
#series of residuals that I'm creating is stationary 
#Beacause ARMA models don't care about trend, and we are removing it with fitted GGM
pres2_nba <- make.instantaneous((fitted(s2_nba)))
lines(pres2_nba)

install.packages("sm")
library(sm)
plot(agg_nba$season_end, agg_nba$salary, type='b')
sm.regression(agg_nba$season_end, agg_nba$salary,   h = 10, add = T)
sm.regression(agg_nba$season_end, agg_nba$salary, ngrid=30, col=2)

sm.regression(agg_nba$season_end, agg_nba$salary,  h = 30, ngrid=200, col=1)
sm.regression(agg_nba$season_end, agg_nba$salary,  h = 50, add = T, ngrid=200, col=2)
sm.regression(agg_nba$season_end, agg_nba$salary,   h = 5,  add = T, ngrid=200, col=3)
sm.regression(agg_nba$season_end, agg_nba$salary,   h = 1,  add = T, col=3, ngrid=200)

#sm.regression()

scatter.smooth(agg_nba$season_end, agg_nba$salary, span=0.2, evaluation=200)









library(gam)
g3_nba <- gam(salary~., data=Nba_merge)
par(mfrow=c(1,1))
plot(g3_nba, se=T) 


par(mfrow=c(3,3)) 
for(i in seq(from=2, to=20, length=20)){
  hist(Nba_merge[sapply(Nba_merge, is.numeric)][,i], col="orange", main=paste(colnames(Nba_merge[sapply(Nba_merge, is.numeric)])[i]), xlab="")
}

hist(Nba_merge$salary_pct,col="orange", xlab="")


set.seed(1)
train = sample (1:nrow(Nba_merge), 0.7*nrow(Nba_merge))
Nba_merge.train=Nba_merge[train ,]
Nba_merge.test=Nba_merge[-train ,]

num_cols <- unlist(lapply(Nba_merge, is.numeric))         # Identify numeric columns
num_cols
Nba_merge.train[,num_cols]
m1.nba <- lm(salary~., data=Nba_merge.train[,num_cols],na.action=na.omit)

summary(m1.nba) #
m1.1.nba <- lm(salary~., data=m1.nba$model)
summary(m1.1.nba)
m2.nba <- step(m1.1.nba, direction="both",na.action=na.omit)
summary(m2.nba)

nba.predict <- predict(m2.nba, newdata=Nba_merge.test)
Nba_merge.test$predict <- nba.predict
predictions <- select(Nba_merge.test, salary, predict)


dev.lm.nba <- sum(((nba.predict[sapply(nba.predict, is.numeric)])-(Nba_merge.test$salary[sapply(Nba_merge.test, is.numeric)]))^2)
dev.lm.nba


nba.predict <- predict(m2.nba, newdata=Nba_merge.train[30,])
nba.predict

install.packages("gbm")
library (gbm)

boost.nba=gbm(salary~ ., data=Nba_merge.train[,num_cols], 
                 distribution="gaussian", n.trees=5000, interaction.depth=1)

par(mfrow=c(1,1))
#
#plot of training error
plot(boost.nba$train.error, type="l", ylab="training error")

summary(boost.nba)


yhat.boost.nba=predict(boost.nba, newdata=Nba_merge.test, n.trees=1:5000, type='response')
