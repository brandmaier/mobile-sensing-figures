#
# devtools::install_github("vqv/ggbiplot")
#
#if (!exists("mytheme"))
mytheme <- papaja::theme_apa()

accelerometer.data <- read.csv("../data/accelerometer-all.dat")
head(accelerometer.data)

unique(accelerometer.data$ACTIVITY)

# features
idxs <- 32:40


selector <- c("Walking","Jogging","Sitting","Standing","Brushing","Clapping")


my.dat <- accelerometer.data[accelerometer.data$ACTIVITY %in% selector, idxs]
labels <- accelerometer.data$ACTIVITY[accelerometer.data$ACTIVITY %in% selector]

labels[which(labels %in% "Clapping")] <- "Clapping hands"
labels[which(labels %in% "Brushing")] <- "Brushing teeth"

labels <- as.factor(as.character(labels))

my.dat <- apply(my.dat, 2, scale)

my.dat <- data.frame(my.dat)

dims <- length(idxs)



#
# compute principal components and create biplot
#
eg <- stats::prcomp(my.dat)
library(ggbiplot)


#
# compute ICA
#
egica <- ica::icaimax(my.dat,2)
egica <- ica::icafast(my.dat, nc=2)
g3 <- ggplot(data.frame(egica$S,labels))+
  geom_point(aes(x=X1,y=X2,col=labels,shape=labels))+
  mytheme


# plot

g2<- ggbiplot(eg,alpha=0,groups=labels,obs.scale=.2,var.scale=.2)+theme_minimal()+
  geom_point(size=3,aes(shape=labels))+
 # xlim(-2,2.8)
  scale_shape(name="Activities")+guides(color=FALSE)+
  scale_color_grey()+mytheme

plot(g2)
