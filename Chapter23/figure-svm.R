require("e1071")

#
# low C => large margin
# high C => small margin
#


if (!exists("accelerometer.data"))
  accelerometer.data <- read.csv("data/accelerometer-all.dat")

if (!exists("mytheme")) {
  mytheme <- geom_blank()
}


library(ggplot2)
library(gridExtra)

if (!exists("mytheme")) {
  mytheme <- geom_blank()
  mytheme <- theme_apa()
}


treedat <- accelerometer.data
#treedat <- completedata
source("R/rename_activities.R")
treedat <- rename_activities(treedat)
treedat$ACTIVITY <- factor(treedat$ACTIVITY)
#treedat <- treedat[treedat$ACTIVITY %in% c("Folding","Clapping","Writing"),]
treedat <- treedat[treedat$ACTIVITY %in% c("Folding clothes","Brushing teeth"),]
treedat$ACTIVITY <- droplevels(treedat$ACTIVITY)



xvar.name <- "ZVAR"#"XAVG"
yvar.name <- "YVAR"

form <- as.formula(paste0("ACTIVITY~",xvar.name,"+",yvar.name))

#treedat <- treedat[,which(names(treedat) %in% c("ACTIVITIY",xvar.name,yvar.name))]

Clow <- .1
Chigh<-100
model1 <- svm(form, treedat, cost=Clow)
model2 <- svm(form, treedat, kernel="linear", cost=Clow)
model3 <- svm(form, treedat, cost=Chigh)
model4 <- svm(form, treedat, kernel="linear", cost=Chigh)

set.seed(34)
tuned <- e1071::tune.svm(form,data=treedat, cost=c(Clow, Chigh), kernel="linear")
tuned2 <- e1071::tune.svm(form,data=treedat, cost=c(Clow, Chigh), kernel="radial")
#predict(model)

# get weights
#svm.model <- model1

#tune(svm, ACTIVITY~., treedat)



plotdec <- function(model, treedat, C) {
  #  pr.lda <- predict(my)$class
  reso<-100
  
  xx <- seq(min(treedat[,xvar.name]),max(treedat[,xvar.name]),length.out = reso)
  yy <- seq(min(treedat[,yvar.name]),max(treedat[,yvar.name]),length.out=reso)
  
  xx <- seq(0.05, 0.85, length.out=reso)
  yy <- seq(0.1,0.7,length.out=reso)
  
  grd <- expand.grid(XAVG=xx,YVAR=yy)
  names(grd)[1:2] <- c(xvar.name,yvar.name)
  grd$ACTIVITY <- predict(model,newdata=grd)
  grd$alpha <- .3
  
  svs<-treedat[model$index,c(xvar.name,yvar.name)]
  
  svm.model<-model
  w <- t(svm.model$coefs) %*% svm.model$SV
  b <- -svm.model$rho
  line.a=-b/w[1,2]
  line.b=-w[1,1]/w[1,2]
  #print(w)
  
  
  # support vectors
  svdat<-treedat[svm.model$index, ]# show the support vectors
  # print(svdat)
  
  # pl <- ggplot()+
  #    geom_point(data=grd,aes(x=XAVG,y=YVAR,col=ACTIVITY))+
  #    geom_point(shape=21,size=3,col="black", data=treedat,aes(x=XAVG,y=YVAR,fill=ACTIVITY))+
  #    geom_point(data=svdat,size=3,col="black", fill="black", shape=21,aes(x=XAVG,y=YVAR,fill=ACTIVITY))
  
  pl <- ggplot()+
    # data grid
    geom_point(data=grd,aes_string(x=xvar.name,y=yvar.name,col="ACTIVITY"))+
    scale_color_manual(values=c("#BBBBBB","#FFFFFF"))+
    
    # observations
    geom_point(size=3,col="black", 
               data=treedat,aes_string(x=xvar.name,y=yvar.name,col="ACTIVITY",
                                       shape="ACTIVITY" ))+
    scale_shape_manual(values=c(21,22))+
    scale_fill_grey()+
    
    # support vectors
    geom_point(data=svdat,size=1,col="black",
               fill="black", aes_string(x=xvar.name,y=yvar.name,shape="ACTIVITY"))+
    
    xlab("Variance of sensor's z-axis")+
    ylab("Variance of sensor's y-axis")
  # geom_abline(a=line.a,b=line.b)
  
  return(pl)
}

p1<-plotdec(model1, treedat)+ theme(legend.position="bottom")
p2<-plotdec(model2, treedat)
p3<-plotdec(model3, treedat)
p4<-plotdec(model4, treedat)



#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mylegend<-g_legend(p1)

grid.arrange(arrangeGrob(p2 + mytheme+ theme(legend.position="none"),
                         p1 + mytheme+theme(legend.position="none"),
                         p4 + mytheme+theme(legend.position="none"),
                         p3 + mytheme+theme(legend.position="none"),
                         nrow=2),
             mylegend, nrow=2, ncol=1,heights=c(10,1))+
  mytheme

