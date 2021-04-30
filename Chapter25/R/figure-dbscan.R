#
# DBSCAN
#

library(dbscan)
library(ggplot2)


if (!exists("acdat")) {
  accelerometer.data <- read.csv("data/accelerometer-all.dat")
}

acdat <- accelerometer.data
#acdat <- accelerometer.data[accelerometer.data$ACTIVITY %in% c("Walking","Sitting","Standing", "Eating Soup","Dribbling","Writing"),]
acdat <- accelerometer.data[
  accelerometer.data$ACTIVITY
  %in% c("Walking","Sitting","Standing", 
         "Eating Soup","Eating Pasta","Eating Chips"),]

acdat$ACTIVITY <- as.factor(as.character(acdat$ACTIVITY))

obs <- acdat[,c("XPEAK","XVAR")]
obsr<-obs
result_db <- dbscan(obs,eps=2.0, minPts=1500)

obsr <- cbind(obs, dbscan=factor(result_db$cluster))
names(obsr)[1:2]<-c("x","y")

ggplot(obsr)+
  geom_point(aes(x=x,y=y), alpha=.1)+
  geom_point(aes(x=x,y=y,color=dbscan),size=1,alpha=1)+
  theme_light()
NULL
