# Dataset from: http://snap.stanford.edu/data/email-Enron.html
library(igraph)
net <- read.table("Email-Enron.txt")
#index <- sample(1:dim(net)[1], size = 1000)
#index <- sort(index)
net <- net[1:1000,]
count <- table(c(net$V1,net$V2))


# decide the color
internal <- seq(min(count), max(count), 30)

color <- rep(0,length(count))
j = 1
for(i in internal){
  ind <- which(count >= i)
  color[ind] = j
  j = j + 1
}
color[color>5]=6
# sampling
data <- net
weight <- rep(1,dim(net)[1])
data <- data.frame(data,weight = weight)
Col <- color
colchooses <- rainbow(1000)
colbar <- colchooses[seq(500,1000,(500/length(unique(color))))]
colbar <- sort(colbar)
# draw the network
g <- graph.data.frame(data, directed = F)
V(g)$time <- Col
plot(g, edge.width = E(g)$weight,layout=layout.fruchterman.reingold,
     vertex.color=colbar[V(g)$time],vertex.frame.color="#ff000033",
     edge.color="#555555",vertex.label=NA, edge.width=2, vertex.size=4)


colours = unique(colbar[V(g)$time])
labels = c("Con <= 30","Con <= 60",
           "Con <= 90","Con <= 120",
           "Con <= 150","Con <= 151")

legend("right",legend=labels, col=colours, pch=19,
       title="Email connection")



