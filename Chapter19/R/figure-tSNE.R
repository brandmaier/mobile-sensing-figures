library(tidyverse)

if (!exists("accelerometer.data")) {
  accelerometer.data <- read.csv("data/accelerometer-all.dat")
}

library(tsne)

set.seed(23540)

treedat <- accelerometer.data
#treedat <- completedata
treedat$ACTIVITY <- factor(treedat$ACTIVITY)

raw <- treedat %>% select(-ACTIVITY)

filt <- c(c(1:10),c(1:10)+2000,c(1:10)+3000)
cols <- c(rep("green",10),rep("red",10),rep("blue",10))

subraw <- raw[filt,]
labels <- treedat$ACTIVITY[filt]

ecb = function(x,y){ plot(x,t='n'); text(x, labels=labels, col=cols) }

#tresult <- tsne(subraw, epoch_callback = ecb,perplexity = 10)

par(mfrow=c(2,2))

subraw2 <- subraw %>% select(XAVG,YAVG,ZAVG,XVAR,YVAR,ZVAR)
#subraw2 <- subraw %>% select(XVAR,YVAR,ZVAR)
tresult1 <- tsne(subraw, perplexity = 2)

#ecb(tresult1)
#title("Perplexity = 2")

tresult2 <- tsne(subraw, perplexity = 20)

#ecb(tresult2)
#title("Perplexity = 20")

tresult3 <- tsne(subraw2, perplexity = 2)

#ecb(tresult3)
#title("Perplexity = 2")

tresult4 <- tsne(subraw2, perplexity = 20)

#ecb(tresult4)
#title("Perplexity = 20")

tresult1 <- cbind(data.frame(tresult1), perplexity=2, features="all", labels=labels)
tresult2 <- cbind(data.frame(tresult2), perplexity=20, features="all", labels=labels)
tresult3 <- cbind(data.frame(tresult3), perplexity=2, features="few", labels=labels)
tresult4 <- cbind(data.frame(tresult4), perplexity=20, features="few", labels=labels)


tr <- rbind(tresult1, tresult2, tresult3, tresult4)

ggplot(data=tr, aes(x=X1,y=X2,color=labels))+geom_point()+facet_wrap(~features+perplexity)+theme_light()
