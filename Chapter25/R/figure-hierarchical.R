path <- "~/Downloads/wisdm-dataset/raw/watch/accel/data_1600_accel_watch.txt"
dat <- readr::read_csv(path,comment=";")
names(dat) <- c("Participant","Activity","Timestamp","X","Y","Z")

head(dat)


library(tidyverse)

#subs <- dat %>% select("X","Timestamp","Activity")
#subs$Timestamp


#dat %>% group_by(Activity) %>% summarize( min=min(Timestamp))

#pdcmat <- spread(subs, Activity,X) %>% select(-Timestamp)

actA_x = dat$X[dat$Activity=="A"]
actB_x = dat$X[dat$Activity=="B"]
actG_x = dat$X[dat$Activity=="P"]

actA_pieces = split(actA_x,ceiling(seq_along(actA_x)/100))
actB_pieces = split(actB_x,ceiling(seq_along(actB_x)/100))
actG_pieces = split(actG_x,ceiling(seq_along(actG_x)/100))


#mydat <- cbind(actA_pieces)

mydat <-#cbind(
  cbind(do.call(cbind, actA_pieces[c(1,20,7,24,10)]), do.call(cbind, actB_pieces[c(22,12,23,19,10)]))
  #actG_pieces[[1]])
labels <- c( rep("Walking",5),rep("Jogging",5))
lb2 <- c(rep(1,5),rep(2,5))

library(pdc)

#labels <- c("Walking","Jogging","Stairs","Sitting",
#            "Standing","Typing","Brushing","Eating Soup",
#            "Eating Chips","Eating Pasta","Drinking",
#            "Eating Sandwich","Kicking","Catch",
#            "Dribbling","Writing","Clapping","Folding")


require(TSclust)
#result <- pdclust(pdcmat)
#plot(result)

par(mfrow=c(1,2))

#plot(result,timeseries.as.labels = FALSE,labels = labels,xlab=NA, sub=NA)


library(TSclust)
#TSclust::diss.EUCL(mydat)

dmat <- diss(t(mydat),"EUCL")
result <- hclust(dmat)

plot(result,labels = labels)
#mdsPlot(result,labels = labels)


xmdsPlot <- function (X, labels = NULL, col = "gray", groups = NULL) 
{
  if (is.null(labels)) {
    labels <- colnames(X$data)
  }
  mds <- cmdscale(X, eig = TRUE, k = 2)
  x <- mds$points[, 1]
  y <- mds$points[, 2]
  plot(x, y, xlab = "Coordinate #1", ylab = "Coordinate #2", 
       type = "n", main = "Multidimensional Scaling",
       xlim=c(-80,100))
  for (cn in unique(groups)) {
    rng <- which(groups== cn)
    idx <- chull(x[rng], y[rng])
    polygon(mds$points[c(rng[idx], rng[idx[1]]), ], col = col[which(unique(groups) == 
                                                                      cn)])
  }
  text(x, y, labels)
  invisible()
}

#lb2 <- c(1,1,1, 2,2,2, 3, 4,4,4,4,4, 1,1,1,2,5,1)

xmdsPlot(dmat, labels=labels ,groups=lb2, col=c("green", "lightblue"))
