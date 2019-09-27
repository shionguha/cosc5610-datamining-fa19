library(igraph)


davis <- read.csv(file.choose(), header=FALSE)

g <- graph.data.frame(davis, directed=FALSE)

bipartite.mapping(g)

V(g)$type <- bipartite_mapping(g)$type

plot(g)


#graphing and labeling
plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")


V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"
  
plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")


V(g)$label.color <- "black" 
V(g)$label.cex <- 1
V(g)$frame.color <-  "gray"
V(g)$size <- 18

plot(g, layout = layout_with_graphopt)

plot(g, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)


#analyzing bimodal networks
types <- V(g)$type 
deg <- degree(g)
bet <- betweenness(g)
clos <- closeness(g)
eig <- eigen_centrality(g)$vector

cent_df <- data.frame(types, deg, bet, clos, eig)

cent_df[order(cent_df$type, decreasing = TRUE),]

#some more operations
V(g)$size <- degree(g)
V(g)$label.cex <- degree(g) * 0.2

plot(g, layout = layout_with_graphopt)