library(kernlab)

library(tidyverse)

#unique(accelerometer.data$ACTIVITY)

if (!exists("acdat")) {
  accelerometer.data <- read.csv("data/accelerometer-all.dat")
}

acdat <- accelerometer.data[accelerometer.data$ACTIVITY %in% c("Walking","Sitting","Standing", "Eating Soup","Dribbling","Writing"),]

acdat$ACTIVITY <- as.factor(as.character(acdat$ACTIVITY))

#kpc <- kpca(~.,data=acdat[,1:5])
set.seed(234)
train <- sample(1:450,200)
test <- sample(1:450,20)
ids <- 32:42



#print the principal component vectors
#pcv(kpc)

#plot the data projection on the components
#plot(rotated(kpc),col=as.integer(as.factor(acdat[train,]$ACTIVITY)),
#     xlab="1st Principal Component",ylab="2nd Principal Component")

#embed remaining points 
#emb <- predict(kpc,acdat[test,ids])
#points(emb,col=as.integer(as.factor(acdat[test,]$ACTIVITY)))

plotkpc <- function(kpc,title="") {

dfg <- data.frame(rotated(kpc), Activity=acdat[train,]$ACTIVITY)
#dfg <- as.data.frame(dfg)
#dfg$col <- factor(dfg$col)
names(dfg) <- c("x","y","Activity")
ggplot(data= dfg, aes(x=x,y=y,col=Activity))+geom_point(size=4)+
theme_light()+xlab("X")+ylab("Y")+ggtitle(title)

}


sigma1 <- .05
sigma2 <- .008

kpc1 <- kpca(~.,data=acdat[train,ids],kernel="rbfdot",
            kpar=list(sigma=sigma1),features=2)

k1<-plotkpc(kpc1,paste0("RBF ",sigma1))


kpc2 <- kpca(~.,data=acdat[train,ids],kernel="rbfdot",
            kpar=list(sigma=sigma2),features=2)


#kpc2 <- kpca(~.,data=acdat[train,ids],kernel="polydot",
#             kpar=list(degree=2,scale=5),features=2)

k2<-plotkpc(kpc2, paste0("RBF ",sigma2))

kpc3 <- kpca(~.,data=acdat[train,ids],kernel="vanilladot", kpar=list(),features=2)
k3<-plotkpc(kpc3, "Linear")

k1 <- k1+theme(legend.position = "none") 
k2 <- k2+theme(legend.position = "none") 

library(patchwork)
patched <- k1 | k2 | k3

plot(patched)