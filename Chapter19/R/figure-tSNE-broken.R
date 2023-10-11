rr <- apply(pdcmat, 2, function(x) {  x<-x[!is.na(x)]; x[1:1000] })
rr <- t(rr)
#tresult <- tsne(t(rr))

labels <- colnames(pdcmat)

labels <- c("Walking","Jogging","Stairs","Sitting",
            "Standing","Typing","Brushing","Eating Soup",
            "Eating Chips","Eating Pasta","Drinking",
            "Eating Sandwich","Kicking","Catch",
            "Dribbling","Writing","Clapping","Folding")

ecb = function(x,y){ plot(x,t='n'); text(x,labels=labels, col="black") }

tresult <- tsne(rr, epoch_callback = ecb,perplexity = 10)

#plot(tresult)

x <- pdcmat$A
x[complete.cases(x) ]


#mat <- cbind(x[1:100],x[])