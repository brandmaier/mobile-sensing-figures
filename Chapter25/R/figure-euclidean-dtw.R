set.seed(3294)
x <- seq(0,1,.1)
N <- length(x)
rawy <- rnorm(n = N)+3*sin(x*5) #sample(c(-1,0,+1),N,replace=TRUE)
idx <- 1:N
idx2 <- c(1:3,3,3,4:9)
y <- rawy[idx]
y2 <- rawy[idx2]+5
yc <- cumsum(y)
df <- data.frame(x,y,y2)







# Euclidean
dfv <- data.frame(x=x,xend=x,y=y,yend=y2)
library(ggplot2)
g1 <- ggplot(df, aes(x=x,y=y))+geom_line()+geom_line(df,mapping=aes(x=x,y=y2))+
geom_segment(dfv,mapping=aes(x=x,y=y,xend=xend,yend=yend),col="gray")+
  xlab("Time")+ylab("Value")+
theme_classic()


# DTW
dfv <- data.frame(x=x,xend=x[idx2],y=y,yend=y2[idx2])
dfv <- data.frame(x=x[idx2],xend=x,y=y[idx2],yend=y2)
library(ggplot2)
g2 <- ggplot(df, aes(x=x,y=y))+geom_line()+geom_line(df,mapping=aes(x=x,y=y2))+
  geom_segment(dfv,mapping=aes(x=x,y=y,xend=xend,yend=yend),col="gray")+
  xlab("Time")+ylab("Value")+
  theme_classic()

# arrange layout
library(patchwork)
par(mar=c(0,0,0,0))
plot(g1 | g2)
