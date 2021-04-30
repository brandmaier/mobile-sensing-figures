#
# devtools::install_github("vqv/ggbiplot")
#

accelerometer.data <- read.csv("/Users/brandmaier/Documents/Manuscripts/Brandmaier-Machine-Learning-Chapter/machine-learning-chapter/Chapter25/data/accelerometer-all.dat")
head(accelerometer.data)

unique(accelerometer.data$ACTIVITY)

#idxs <- c(32:40,65:92)
idxs <- 32:40

#idxs <- 40:80
#idxs <- 32:45

#selector<- c("Walking","Catch","Eating Soup","Standing")
selector <- c("Walking","Jogging","Sitting","Standing","Brushing","Clapping")
#selector <- c("Walking","Jogging","Sitting","Standing","Brushing","Clapping","Eating Soup","Drinking")

#walking.data <- accelerometer.data[accelerometer.data$ACTIVITY=="Walking",idxs]

#walking.data <- accelerometer.data[accelerometer.data$ACTIVITY=="Catch",idxs]
#walking.data <- accelerometer.data[accelerometer.data$ACTIVITY=="Jogging",idxs]

#soup.data <- accelerometer.data[accelerometer.data$ACTIVITY=="Eating Soup",idxs]
#brush.data <- accelerometer.data[accelerometer.data$ACTIVITY=="Brushing",idxs]
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
g2 <- ggbiplot(eg)+theme_minimal()+geom_point(aes(color=labels))

#
# compute ICA
#
egica <- ica::icaimax(my.dat,2)
egica <- ica::icafast(my.dat, nc=2)
g3 <- ggplot(data.frame(egica$S,labels))+geom_point(aes(x=X1,y=X2,col=labels))+theme_minimal()

plot(g2)


#library(patchwork)

#g2 | g3
