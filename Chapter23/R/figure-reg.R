library(tidyverse)
library(ggplot2)
library(gridExtra)
theme <- theme_classic()

#
# compute the model
#
require("glmnet")

if (!exists("accelerometer.data"))
  accelerometer.data <- read.csv("../data/accelerometer-all.dat")

if (!exists("mytheme")) {
  mytheme <- geom_blank()
}

treedat<-accelerometer.data
treedat$ACTIVITY <- factor(treedat$ACTIVITY)
treedat <- treedat[treedat$ACTIVITY %in% c("Jogging","Walking"),]
treedat$ACTIVITY<-droplevels(treedat$ACTIVITY)


y <- treedat$ACTIVITY
y <- as.numeric(y)-1
x <- as.matrix(treedat[,32:52])

table(y)

x <- apply( x, MARGIN = 2, scale)
mat <- cbind(y,x)
mat <- data.frame(mat)
mat$y <- factor(mat$y)


lsq = exp(seq(-15,-1,.5))
myglm <- glmnet(x=x,y=y, family = "binomial", alpha=1,lambda=lsq)
cvfit <- cv.glmnet(x,y, family="binomial",alpha=1, lambda=lsq)

mincoef <- coef(cvfit, s = "lambda.min")

brs <- as.numeric(abs(mincoef))
cols <- rainbow(ncol(x)+1) # add icept

minl1n <- sum(abs(mincoef))

nms<- rownames(mincoef)





#
# create panel of estimates (bar plot)
#

bars_data <- data.frame(name=nms, value=brs)

bars_data_sub <- bars_data %>% filter(value!=0)

panel1 <- ggplot()+ geom_bar(data=bars_data_sub, aes(x=reorder(name, value),y=value,fill=name),stat="identity")+
  theme+coord_flip()+xlab("")+ylab("Coefficient")+theme(legend.position = "none") +
  scale_fill_grey()

#
# create line plot
#
lambda <- myglm$lambda
coef_data<-as_tibble( t(as.matrix(coef(myglm)))) 


coef_data <- coef_data[,(bars_data$value!=0)]

coef_data <- coef_data %>% rowwise() %>% mutate(norm = sum(abs(across(XAVG:YVAR)))) %>% pivot_longer(-norm)
  
panel2 <- coef_data %>% ggplot()+
  geom_line(aes(x=norm,y=value,group=name,color=name),lwd=2)+theme+
  xlab("Norm")+ylab("Coefficient")+theme(legend.position = "none") +
  geom_vline(xintercept=minl1n,lty=2)+
  scale_color_grey()

library(patchwork)
combined <- (panel2 + panel1)# & theme(legend.position = "bottom")
pl <- combined + plot_layout(guides = "keep")

plot(pl)
