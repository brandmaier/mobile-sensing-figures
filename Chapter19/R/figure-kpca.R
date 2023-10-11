library(kernlab)
library(tidyverse)


if (!exists("acdat")) {
  accelerometer.data <- read.csv("data/accelerometer-all.dat")
}

acdat <- accelerometer.data[accelerometer.data$ACTIVITY %in% 
                              c("Walking","Sitting","Standing", "Eating Soup","Dribbling","Writing"),]

acdat$ACTIVITY <- as.factor(as.character(acdat$ACTIVITY))

set.seed(234)
train <- sample(1:nrow(acdat),200)
#train <- 1:nrow(acdat)
#test <- sample(1:450,20)
ids <- 32:42


plotkpc <- function(kpc,title="") {

dfg <- data.frame(rotated(kpc), Activity=acdat[train,]$ACTIVITY)

names(dfg) <- c("x","y","Activity")
ggplot(data= dfg, aes(x=x,y=y,shape=Activity))+
  geom_point(size=4)+
theme_light()+xlab("X")+
  ylab("Y")+ggtitle(title)+
  xlab("PC #1")+
  ylab("PC #2")

}


sigma1 <- .05
sigma2 <- .008

sigma1 <- .1
sigma2 <- .01

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