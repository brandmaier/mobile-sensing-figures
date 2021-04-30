set.seed(123)

library(ggplot2)

if (!exists("mytheme")) {
  mytheme <- geom_blank()
}

sim <- function(N=100,p=5) {
  
  X <- matrix(rnorm(N*p),nrow=N)
  Xnew <- matrix(rnorm(N*p),nrow=N)
  y <- sample( c(0,1),size=N,replace=TRUE)
  ynew <- sample( c(0,1),size=N,replace=TRUE)
 # y<-factor(y)
  dat <- data.frame(cbind(y=y,X))
  datnew<-data.frame(Xnew)
  names(datnew) <- names(dat)[-1]
  model <- MASS::lda(y~., data=dat)
  acc.in<-mean(predict(model)$class==y)
  acc.out<-mean(predict(model,newdata=datnew)$class==ynew)
  
  return(list(acc.in, acc.out))
}

msim <- function() {
  dd<-replicate(sim(),n=5)
  apply(dd,1,function(x){max(unlist(x))})
}

n.rep=200

dist.naive <- replicate(sim(),n = n.rep)
dist.max <- replicate(msim(),n=n.rep)


df <- data.frame(x=c(unlist(dist.naive[1,]), # in-sample
                     unlist(dist.naive[2,]), # out-sample
                     unlist(dist.max[1,]), # in-sample
                     unlist(dist.max[2,])), # out-sample
                 holdout=rep(c(1,2),each=n.rep),
                 type=rep(c(1,2),each=2*n.rep),
                 grp=rep(c(1,2,3,4),each=n.rep))

df$type <- factor(df$type)
df$grp <- factor(df$grp)
df$Estimation<-factor(df$holdout,labels = c("In-sample","Out-of-sample"))
df$Selection<-factor(df$type,labels = c("Single","Best of 5"))

pl <- ggplot(data=df,aes(x=x,fill=grp,group=grp))+
  geom_density(alpha=.7,n=100,adjust=2)+#+geom_histogram()
  xlab("Accuracy")+ylab("Density")+ mytheme

library(viridis)
library(ggsci)

pl <- ggplot(data=df,aes(x=x,fill=Selection,group=grp,linetype=Estimation))+
  geom_density(alpha=.7,n=100,adjust=2)+#+geom_histogram()
  xlab("Accuracy")+ylab("Density")+ mytheme+
  #scale_fill_viridis(discrete = TRUE, option="D")
  scale_fill_jco()+theme(legend.position="bottom")
(pl)
