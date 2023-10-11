library(ica)
library(tidyverse)

path <- "../data/wisdm-dataset/raw/watch/accel/data_1600_accel_watch.txt"
dat <- readr::read_csv(path,comment=";")
names(dat) <- c("Participant","Activity","Timestamp","X","Y","Z")

path <- "../data/wisdm-dataset/raw/watch/gyro/data_1600_gyro_watch.txt"
dat2 <- readr::read_csv(path,comment=";")
names(dat2) <- c("Participant","Activity","Timestamp","X","Y","Z")


start_time <- 1000
end_time <- 2000

#head(dat)

#dat <- dat[1:1000,]

subdat <- dat %>% select(X,Y,Z) 

res <- icaimax(subdat,nc=2)

plotdat <- subdat %>% mutate(Time=1:nrow(dat),R1= res$Y[,1], R2=res$Y[,2])

library(ggplot2)

n <- nrow(dat)
#noise1 <- seq(1:n)%%100 
#noise2 <- sin((1:n)*.001)^2 
#noise3 <- (10-(1:n)%%20)^2
noise1 <- ica::icasamp("e","rnd",nsamp=n)
noise2 <- ica::icasamp("a","rnd",nsamp=n)
noise3 <- ica::icasamp("g","rnd",nsamp=n)
signal <- dat$X / max(dat$X) 



noise1 <- scale(noise1)
noise2 <- scale(noise2)
noise3 <- scale(noise3)
signal <- scale(signal)

nc<-4

mix1 <- noise1 * .4 + noise2 * .2 + signal * .4 + noise3*.5 + rnorm(n,0,.1)
mix2 <- noise1 * .1 + noise2 * .3 + signal * .6+ noise3*.4+ rnorm(n,0,.1)
mix3 <- noise1 * .2 + noise2 * .7 + signal * .1+ noise3*.3+ rnorm(n,0,.1)
mix4 <- noise1 * .2 + noise2 * .7 + signal * .1+ noise3*.2+ rnorm(n,0,.1)

noveldat <- data.frame(mix1,mix2,mix3,mix4)



ica_unmix <- icafast(noveldat,nc=4, maxit=500, alpha=1)$Y
#ica_unmix <- icaimax(noveldat, nc=nc)$Y
colnames(ica_unmix)<- paste0("X",1:nc)
ica_unmix <- as_tibble(ica_unmix ) %>% mutate(Time=1:n)
iplot <- ica_unmix %>% pivot_longer(-Time) %>%filter(Time>start_time & Time< end_time)%>%
  ggplot(aes(x=Time,y=value,group=name))+geom_line()+facet_wrap(~name,nrow=nc,scales="free")
#ggplot2::ggplot(data=dat, aes(x=Timestamp,y=reading,group=Activity))+geom_line()

pca_unmix<-prcomp(noveldat)$x
colnames(pca_unmix)<- paste0("X",1:nc)
pca_unmix <- as_tibble(pca_unmix ) %>% mutate(Time=1:n)
pplot <- pca_unmix %>% pivot_longer(-Time) %>%filter(Time>start_time & Time< end_time)%>%
  ggplot(aes(x=Time,y=value,group=name))+geom_line()+facet_wrap(~name,nrow=nc,scales="free")

#plotdat %>% pivot_longer(-Time) %>%
#  filter(Time>1000 & Time< 2000) %>% ggplot(aes(x=Time,y=value,group=name,col=name))+geom_line()+
#  facet_wrap(~name,nrow=5,scales = "free" )

library(patchwork)

iplot | pplot


# components are perfectly identical:
cor(pca_unmix$X2 , ica_unmix$X2)
