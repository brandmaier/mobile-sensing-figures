#
# devtools::install_github("vqv/ggbiplot")
#

accelerometer.data <- read.csv("/Users/brandmaier/Documents/Manuscripts/Brandmaier-Machine-Learning-Chapter/machine-learning-chapter/Chapter25/data/accelerometer-all.dat")
head(accelerometer.data)

unique(accelerometer.data$ACTIVITY)

# features
idxs <- 32:40


selector <- c("Walking","Jogging","Sitting","Standing","Brushing","Clapping")


my.dat <- accelerometer.data[accelerometer.data$ACTIVITY %in% selector, idxs]
labels <- accelerometer.data$ACTIVITY[accelerometer.data$ACTIVITY %in% selector]
labels <- as.factor(as.character(labels))
#my.dat <- rbind(walking.data, soup.data, brush.data)
#labels <- c(rep("Walk",nrow(walking.data)),rep("Soup",nrow(soup.data)), rep("Brush",nrow(brush.data))  )

#my.dat <- my.dat[sample(1:nrow(my.dat)),]

my.dat <- apply(my.dat, 2, scale)

my.dat <- data.frame(my.dat)

dims <- length(idxs)



#
# compute principal components and create biplot
#
eg <- stats::prcomp(my.dat)
library(ggbiplot)
g2 <- ggbiplot(eg,alpha=0)+theme_minimal()+
  geom_point(size=3,aes(shape=labels))+
  xlim(-2,2.8)+
  scale_shape(name="Activities")+guides(color=FALSE)+
  scale_color_grey()

#
# compute ICA
#
egica <- ica::icaimax(my.dat,2)
egica <- ica::icafast(my.dat, nc=2)
g3 <- ggplot(data.frame(egica$S,labels))+
  geom_point(aes(x=X1,y=X2,col=labels,shape=labels))+
  theme_minimal()

plot(g2)


#library(patchwork)

#g2 | g3
