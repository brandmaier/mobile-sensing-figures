library(ica)

path <- "~/Downloads/wisdm-dataset/raw/watch/accel/data_1600_accel_watch.txt"
dat <- readr::read_csv(path,comment=";")
names(dat) <- c("Participant","Activity","Timestamp","X","Y","Z")

path <- "~/Downloads/wisdm-dataset/raw/watch/gyro/data_1600_gyro_watch.txt"
dat2 <- readr::read_csv(path,comment=";")
names(dat2) <- c("Participant","Activity","Timestamp","X","Y","Z")

head(dat)

subdat <- dat %>% select(X,Y,Z)

res <- icaimax(subdat,nc=2)

plotdat <- subdat %>% mutate(Time=1:nrow(dat),R1= res$Y[,1], R2=res$Y[,2])

library(ggplot2)


#ggplot2::ggplot(data=dat, aes(x=Timestamp,y=reading,group=Activity))+geom_line()

plotdat %>% pivot_longer(-Time) %>%
  filter(Time>1000 & Time< 2000) %>% ggplot(aes(x=Time,y=value,group=name,col=name))+geom_line()+
  facet_wrap(~name,nrow=5,scales = "free" )
