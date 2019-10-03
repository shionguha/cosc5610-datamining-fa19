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

#tnet analysis
library(tnet)

tm<-get.edgelist(g, names=FALSE)
head(tm)  # check to make sure it worked

NodeLabels <- V(g)$name
head(NodeLabels)   # Again, check

mt <- tm[, c(2, 1)]
head(mt)

deg_tm <- degree_tm(tm)
deg_mt <- degree_tm(mt)

#converting from bimodal networks to unimodal networks
bipartite_matrix <- as_incidence_matrix(g)
bipartite_matrix

t(bipartite_matrix)


event_matrix_prod <- t(bipartite_matrix) %*% bipartite_matrix 
## crossprod() does same and scales better, but this is better to learn at first at first so you understand the method
diag(event_matrix_prod) <- 0
event_matrix_prod

person_matrix_prod <- bipartite_matrix %*% t(bipartite_matrix)
diag(person_matrix_prod) <- 0
person_matrix_prod


women_overlap <- graph_from_adjacency_matrix(person_matrix_prod, 
                                        mode = "undirected", 
                                        weighted = TRUE)
women_overlap


E(women_overlap)$weight

