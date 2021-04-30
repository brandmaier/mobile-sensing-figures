require("caret")
library("gridExtra")
if (!exists("mytheme")) mytheme <- theme_light()

if (!exists("accelerometer.data"))
  accelerometer.data <- read.csv("data/accelerometer-all.dat")

kdata <- accelerometer.data
kdata$ACTIVITY <- as.character(kdata$ACTIVITY)
kdata$ACTIVITY[kdata$ACTIVITY %in% c("Jogging","Brushing")] 
kdata$ACTIVITY[kdata$ACTIVITY=="Brushing"]<-"Brushing teeth"

kdata$ACTIVITY <- factor(kdata$ACTIVITY)


completedata <- kdata
#knn.k1 <- knn3(ACTIVITY ~XAVG+ZAVG, data=kdata)
#knn.k3 <- knn3(ACTIVITY ~XAVG+ZAVG, data=kdata)
#knn.k15 <- knn3(ACTIVITY ~XAVG+ZAVG, data=kdata)

knnplot<-function(k=3) {
  set.seed(234)
  
  cl<-"Jogging"
  
  ids1 <- sample(which(completedata$ACTIVITY==cl),size = 100,replace=FALSE)
  ids2 <- sample(which(completedata$ACTIVITY!=cl),size = 100,replace=FALSE)
  ids <- c(ids1,ids2)
  
  dat <- completedata[ids,]
  x <- dat[,c(38,34)]
  g <- ifelse(dat[,1]==cl,cl,"Brushing teeth")
  
  px1<-seq(min(x[,1]),max(x[,1]),length.out=60)
  px2<-seq(min(x[,2]),max(x[,2]),length.out=60)
  xgrid <- expand.grid(x=px1, y=px2)
  
  mod15 <- class::knn(x, xgrid, g, k=k, prob=TRUE)
  prob <- attr(mod15, "prob") # predictions for 'xgrid'
  prob <- ifelse(mod15==cl, prob, 1-prob)
  
  prob15 <- matrix(prob, length(px1), length(px2))
  
  if (FALSE) {
  par(mar=rep(2,4))
  contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main=
            paste0(k,"-nearest neighbour"), axes=TRUE)
  points(x, col=ifelse(g==cl, "coral", "cornflowerblue")) # plot original points
  #gd <- expand.grid(x=px1, y=px2)
  points(xgrid, pch=".", cex=1.2,
         col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
  box()
  }
  
  dfg<-cbind(xgrid,prob=as.numeric(prob>.5))
  dfg$fillcol <- c("blue","red")[dfg$prob+1]
  gp <- ggplot()+

    #           xlab("Abs. Dev. X")+ylab("Avg Z")+
    xlab("Absolute deviation of sensor's x-axis")+
    ylab("Average of sensor's z-axis")+
    #stat_contour(geom="polygon",aes(fill=stat(prob))) +
    #scale_fill_distiller(palette = "Spectral", direction = -1)+
    #metR::geom_contour_fill(data=dfg,aes(x=x,y=y,z=prob,fill=prob,col=NULL),breaks=.5)+
  #  metR::geom_contour_fill(data=dfg,aes(x=x,y=y,z=prob))+
    geom_contour(data=dfg,aes(x=x,y=y,z=prob,col=NULL),
                 breaks=.5,color="black",lwd=.5)+
   # stat_contour(data=dfg,geom="polygon",aes(x=x,y=y,z=prob,fill=stat(level)))+
    geom_point(data=cbind(x,Activity=g),
               aes(x=XABSOLDEV,y=ZAVG,
                   #col=Activity,
                   shape=Activity, 
                   fill=Activity),size=3)+
    scale_fill_manual(name="Activity",values=c("white","black"))+
    scale_shape_manual(values=c(1,21))+
#    scale_color_manual(name="Activity", values=c(1,21))+
   # stat_contour(geom="polygon", aes(fill=..prob..)) +
   # scale_colour_distiller(palette = "YlGn", direction = 1)+
  #geom_bin2d(bins=30,data=cbind(xgrid,prob=as.numeric(prob>=.5)),aes(x=x,y=y,z=prob,col=NULL))
    ggtitle(paste0("k=",k))+
  mytheme+
    theme(text=element_text(size=16))
    #theme(legend.position = 'none') # to make "collect work
  NULL
  return(gp)
}

#par(mfrow=c(1,2))

p1 <- knnplot(1) + theme(legend.position="bottom")
p2 <- knnplot(15)

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
mylegend<-g_legend(p1)


library(patchwork)

#combined <- p1 + (p2 & theme(legend.position = "bottom"))
#combined <- p1+p2+plot_layout(guides = "collect") & theme(legend.position = 'bottom')

#plot(combined)

grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1))

#plot(pl)
