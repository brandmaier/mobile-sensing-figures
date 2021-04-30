library(igraph)

l1 <- 1:5
l2 <- 6:8
l3 <- (1:5)+8


g <- make_empty_graph(n = 5+3+5)
 # add_edges(c(1,2, 2,3, 3,4, 4,5))
 # set_edge_attr("color", value = "red") %>%
#  add_edges(c(5,1), color = "green")

for (i in l1) {
  for (j in l2) {
  g<-g %>% add_edges(c(i,j))
  }
}

for (i in l2) {
  for (j in l3) {
    g<-g %>% add_edges(c(i,j))
  }
}

E(g)[[]]
#plot(g)

coords <- layout_(g, as_star())
coords[l1,1] <- 10
coords[l1,2] <- 1:5
coords[l2,1] <- 100
coords[l2,2] <- 1:3+1
coords[l3,1] <- 200
coords[l3,2] <- 1:5
cl<-cluster_optimal(g)
cl$membership<-c(1,1,1,1,1,2,2,2,3,3,3,3,3)
par(mar=c(0,0,0,0))
plot(cl,g, layout = coords, vertex.size=30)
