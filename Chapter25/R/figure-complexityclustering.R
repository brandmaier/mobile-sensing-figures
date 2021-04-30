library(tidyverse)
path <- "~/Downloads/wisdm-dataset/raw/watch/accel/data_1600_accel_watch.txt"
dat <- readr::read_csv(path,comment=";")
names(dat) <- c("Participant","Activity","Timestamp","X","Y","Z")

#head(dat)

subdat <- dat %>% select(Activity,X)

labels <- unique(subdat$Activity)

result <- list()
for (ac in labels) {
#  result <- cbind(result, subdat[subdat$Activity==ac,]$X) 
  result[[ac]] <- subdat[subdat$Activity==ac,]$X
}

#
# ZIP compressibility
#

x<-sample(c(0,1),100000,replace=TRUE)
#x<-rep(c(0,1),100000)

zipsize <- function(x) {
 tf<-tempfile()
 zf<-paste0(tempfile(),".zip")
 writeBin(x,tf)
 zip(zf,tf)
# cat(zf)
 #file.info(tf)$size

 unlink(tf)
 return( file.info(zf)$size )
}

dist.zip <- function(x,y) {
  zx <- zipsize(x)
  zy <- zipsize(y)
  zxy <- zipsize(c(x,y))
  return ( (zxy-min(zx,zy))/max(zx,zy)  )
}

#
#
#

dists <- matrix(0, nrow=length(result),ncol=length(result))

for (i in 1:length(result)) {
  for (j in 1:length(result)) {
    if (j>i) next;
    dists[i,j]<-dists[j,i]<-dist.zip(result[[i]],result[[j]])
  }
}

dd<-as.dist(dists)
#cl<-hclust(d=as.dist(dists),method = "complete")
#cl<-hclust(d=as.dist(dists),method = "average")
cl<-hclust(d=as.dist(dists),method = "single")

labels.clear <- c("Walking","Jogging","Stairs","Sitting",
            "Standing","Typing","Brushing","Eating Soup",
            "Eating Chips","Eating Pasta","Drinking",
            "Eating Sandwich","Kicking","Catch",
            "Dribbling","Writing","Clapping","Folding")


plot(cl,labels = labels.clear,xlab=NA, sub=NA)
