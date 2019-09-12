#making random graphs with ER
er <- sample_gnm(n=100, m=40) 

plot(er, vertex.size=6, vertex.label=NA) 

#watts-strogatz
sw <- sample_smallworld(dim=2, size=10, nei=1, p=0.1)

plot(sw, vertex.size=6, vertex.label=NA, layout=layout_in_circle)

#reading data
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)

links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

#exploring
head(nodes)

head(links)

nrow(nodes); length(unique(nodes$id))

nrow(links); nrow(unique(links[,c("from", "to")]))

#wrangling dataset1
links <- aggregate(links[,3], links[,-3], sum)

links <- links[order(links$from, links$to),]

colnames(links)[4] <- "weight"

rownames(links) <- NULL

#dataset2
nodes2 <- read.csv("Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)

links2 <- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)

#explore
head(nodes2)

head(links2)

links2 <- as.matrix(links2)

dim(links2)

dim(nodes2)

#igraph objects
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

class(net)

net

#exploring this object

E(net)       # The edges of the "net" object

V(net)       # The vertices of the "net" object

E(net)$type  # Edge attribute "type"

V(net)$media # Vertex attribute "media"

#plotting
plot(net, edge.arrow.size=.4,vertex.label=NA)

#wrangling
as_edgelist(net, names=T)

as_adjacency_matrix(net, attr="weight")

#wrangling as data frames
as_data_frame(net, what="edges")

as_data_frame(net, what="vertices")

#dataset2
head(nodes2)
head(links2)

#wrangling
net2 <- graph_from_incidence_matrix(links2)

table(V(net2)$type)

net2.bp <- bipartite.projection(net2)

as_incidence_matrix(net2)  %*% t(as_incidence_matrix(net2)) 

t(as_incidence_matrix(net2)) %*%   as_incidence_matrix(net2)

#plotting
plot(net2.bp$proj1, vertex.label.color="black", vertex.label.dist=1,
     
     vertex.size=7, vertex.label=nodes2$media[!is.na(nodes2$media.type)])

plot(net2.bp$proj2, vertex.label.color="black", vertex.label.dist=1,
     
     vertex.size=7, vertex.label=nodes2$media[ is.na(nodes2$media.type)])

#plotting bimodal networks
V(net2)$color <- c("steel blue", "orange")[V(net2)$type+1]

V(net2)$shape <- c("square", "circle")[V(net2)$type+1]

V(net2)$label <- ""

V(net2)$label[V(net2)$type==F] <- nodes2$media[V(net2)$type==F] 

V(net2)$label.cex=.4

V(net2)$label.font=2



plot(net2, vertex.label.color="white", vertex.size=(2-V(net2)$type)*8) 
