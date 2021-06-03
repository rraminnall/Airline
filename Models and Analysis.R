
library(readr)
final <- read.csv("C:/flights data/on time performance/final2.csv")

#plots

library(gplots)
plotmeans(CANCELLED ~ YEAR,n.label = FALSE ,main="Annual Cancellation", data=final, ylab="Cancellation rate", xlab="Year")
abline(h=mean(final$CANCELLED, na.rm=TRUE), col="red")
plotmeans(loadfactor ~ YEAR,n.label = FALSE ,main="Annual Load factor", data=final, ylab="Load factor", xlab="Year")
abline(h=mean(final$loadfactor, na.rm=TRUE), col="red")


plotmeans(loadfactor ~ MONTH,n.label = FALSE ,main="Monthly load factor 2000-2019", data=final, ylab="Load factor", xlab="Month", xaxt = "n")
abline(h=mean(final$loadfactor, na.rm=TRUE), col="red")
axis(1, at=1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


plotmeans(CANCELLED ~ MONTH,n.label = FALSE ,main="Monthly Cancellation rate 2000-2019", data=final, ylab="Cancellation rate", xlab="Month", xaxt="n")
abline(h=mean(final$CANCELLED, na.rm=TRUE), col="red")
axis(1, at=1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


plot(final$loadfactor, final$CANCELLED, xlab="loadfactor", ylab="cancellation", ylim=c(0,0.4))
abline(lm(final$CANCELLED~final$loadfactor),lwd=3, col="red")


library(car)
route1=final[final$route=="CVG.ORD",]
plot(route1$loadfactor, route1$CANCELLED, pch=19, xlab="loadfactor", ylab="cancellation")
abline(lm(route1$CANCELLED~route1$loadfactor),lwd=3, col="red")

route2=final[final$route=="GNV.ATL",]

plot(route2$loadfactor, route2$CANCELLED, pch=19, xlab="loadfactor", ylab="cancellation")
abline(lm(route1$CANCELLED~route1$loadfactor),lwd=3, col="red")

routetest=final[(final$route=="CVG.ORD")|(final$route=="GNV.ATL")|(final$route=="CLE.LGA")|(final$route=="ORD.LGA")|(final$route=="SFO.LAX"),]

library(ggplot2)
ggplot(routetest, aes(x=loadfactor, y=CANCELLED, color=factor(route, labels=c("Cleveland Hopkins to LaGuardia", "Cincinnati to Chicago O'Hare", "Gainesville to Hartsfield-Jackson Atlanta", "Chicago O'Hare to LaGuardia", "San Francisco to Los Angeles"))))  +geom_point()+
  geom_smooth(method="lm", se=FALSE)+ylab("Cancellation rate")+ xlab("Load factor")+labs(color = "Route") ##geom_text(aes(label=ifelse(CANCELLED>0.25,as.character(FL_DATE),'')),hjust=0,vjust=0)


ggplot(final, aes(x=dest_precipitaion, y=CANCELLED))  +geom_point()+
  geom_smooth(method="lm", se=FALSE, col="red")+ylab("Cancellation rate")+ xlab("Load factor")+ ylim(0,0.4)

test=final[(final$load_level=="high")|(final$load_level=="low"),]
ggplot(test, aes(x=dest_precipitaion, y=CANCELLED, col=load_level))+geom_point()+
  geom_smooth(method="lm", se=FALSE)+ylab("Cancellation rate")+ xlab("Origin precipitation")+ ylim(0,0.4)


#OLS
ols<-lm(CANCELLED~ loadfactor, data=final)
summary(ols)

ols<-lm(CANCELLED~ loadfactor+origin_precipitaion+dest_precipitaion, data=final)
summary(ols)

ols<-lm(CANCELLED~ loadfactor+origin_precipitaion+dest_precipitaion+loadfactor*origin_precipitaion+loadfactor*dest_precipitaion, data=final)
summary(ols)


final$load_com=(final$loadfactor)*(final$competetive)
ols<-lm(CANCELLED~ loadfactor+load_com, data=final)
summary(ols)


#fixed effects
fixed.dum <-lm(CANCELLED ~ loadfactor + factor(route), data=final)
summary(fixed.dum)

#time fixed effects
fixed.time <-lm(CANCELLED ~ loadfactor + factor(YEAR), data=final)
summary(fixed.time)

#time and entity
fixed.dum <-lm(CANCELLED ~ loadfactor + factor(YEAR)+factor(route), data=final)
summary(fixed.dum)

#fixed effects with plm
install.packages("plm")
library(plm)
fixed <- plm(CANCELLED ~ loadfactor, data=final, index=c("route", "FL_DATE"), model="within")
summary(fixed)

fixed <- plm(CANCELLED ~ loadfactor+origin_precipitaion+dest_precipitaion, data=final, index=c("route", "FL_DATE"), model="within")
summary(fixed)

fixed <- plm(CANCELLED ~ loadfactor+origin_precipitaion+dest_precipitaion+loadfactor*origin_precipitaion+loadfactor*dest_precipitaion, data=final, index=c("route", "FL_DATE"), model="within")
summary(fixed)

fixef(fixed)
##test if fixed effect is better than ols: null is OLS is better
pFtest(fixed, ols)
              
              
#random effects with plm
random <- plm(CANCELLED ~ loadfactor+origin_precipitaion+dest_precipitaion+loadfactor*origin_precipitaion+loadfactor*dest_precipitaion, data=final, index=c("route", "FL_DATE"), model="random")
summary(random)

##test if random effect is better than ols
plmtest(random, type=c("bp")) #null is random effect is not better than OLS

#fixed or random (Hausman test): null is random is better
phtest(fixed, random)


#testing if time is necessary
fixed <- plm(CANCELLED ~ loadfactor, data=final, index=c("route", "FL_DATE"), model="within", effect = "twoways")
summary(fixed)

fixed.time <- plm(CANCELLED ~ loadfactor+origin_precipitaion+dest_precipitaion, data=final, index=c("route", "FL_DATE"), model="within", effect = "twoways")
summary(fixed.time)

pFtest(fixed.time, fixed) #null hypothesis is no time effect

plmtest(fixed, c("time"), type=("bp"))

#The best model is a model with time and route fixed effects.
###distance, hub destination and competetiveness of the route (based on the number of flights) seem to not have moderating effects.


##adding distance
final$load_distance=(final$loadfactor)*(final$DISTANCE)
ols<-lm(CANCELLED~ loadfactor+load_distance+DISTANCE, data=final)
summary(ols)

fixed <- plm(CANCELLED ~ loadfactor+load_distance+DISTANCE, data=final, index=c("route", "FL_DATE"), model="within")
summary(fixed)

fixed.time <- plm(CANCELLED ~ loadfactor+load_distance+DISTANCE, data=final, index=c("route", "FL_DATE"), model="within", effect = "twoways")
summary(fixed.time)

###Distance seems to not have a moderating effect either.
