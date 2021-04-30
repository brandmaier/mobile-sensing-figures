path <- "../data/wisdm-dataset/raw/watch/accel/data_1600_accel_watch.txt"
dat <- readr::read_csv(path,comment=";")
names(dat) <- c("Participant","Activity","Timestamp","X","Y","Z")

head(dat)


library(dplyr)

subs <- dat %>% dplyr::select("X","Timestamp","Activity")
subs$Timestamp


dat %>% group_by(Activity) %>% summarize( min=min(Timestamp))

pdcmat <- spread(subs, Activity,X) %>% dplyr::select(-Timestamp)

library(pdc)

labels <- c("Walking","Jogging","Stairs","Sitting",
            "Standing","Typing","Brushing","Eating Soup",
            "Eating Chips","Eating Pasta","Drinking",
            "Eating Sandwich","Kicking","Catch",
            "Dribbling","Writing","Clapping","Folding")


result <- pdclust(pdcmat)
#plot(result)

par(mfrow=c(1,2))

plot(result,timeseries.as.labels = FALSE,labels = labels,xlab=NA, sub=NA)



#mdsPlot(result,labels = labels)

xmdsPlot <- function (X, labels = NULL, col = "gray", groups = NULL) 
{
  if (is.null(labels)) {
    labels <- colnames(X$data)
  }
  mds <- cmdscale(X$D, eig = TRUE, k = 2)
  x <- mds$points[, 1]
  y <- mds$points[, 2]
  plot(x, y, xlab = "Coordinate #1", ylab = "Coordinate #2", 
       type = "n", main = "Multidimensional Scaling", xlim=c(-.12,0.17))
  for (cn in unique(groups)) {
    rng <- which(groups== cn)
    idx <- chull(x[rng], y[rng])
    polygon(mds$points[c(rng[idx], rng[idx[1]]), ], col = col[which(unique(groups) == 
                                                                      cn)])
  }
  text(x, y, labels)
  invisible()
}

lb2 <- c(1,1,1, 2,2,2, 3, 4,4,4,4,4, 1,1,1,2,5,1)
xmdsPlot(result, labels=labels ,groups=lb2, col=c("green", "lightblue","yellow","orange"))
