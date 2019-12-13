install.packages("igraph")
library(igraph)
install.packages("data.table")
library(data.table)
install.packages("tidyverse")
library(tidyverse)
setwd("C:\\Users\\devan\\cosc5610-datamining-fa19\\Project")

#df <- read.csv("DF_NOV29.csv")
#df$newAuth <- gsub("\\s*\\([^\\)]+\\)","",as.character(df$Authors))
#data <- df[c(1,2,5,4)]
#names(data) <- c("ID", "Title", "Author", "Org")
#write.csv(data, file = "nov29_2.csv")

df <- read.csv("data_without_ands.csv")
df <- df[c(2,3,4,5)]

df2 <- df[order(df$Org),]
df3 <- df[order(df$Author),]

df4 <- df[!(is.na(df$Author) | df$Author==""), ]
df5 <- df4[!(is.na(df4$Org) | df4$Org==""), ]

df_org <- df5[c(1,4)]

fulldat <- unique(df_org[c("ID", "Org")])
o_counts <- as.data.frame(table(fulldat$Org))

#auth_counts <- as.data.frame(table(df4$Author))
#org_counts <- as.data.frame(table(df5$Org))

#==============================================================================
#BUILD SMALLER NETWORKS

smaller_df <- fulldat
smallnet <- smaller_df[smaller_df$Org %in% names(which(table(smaller_df$Org) > 10)), ]
u_names <- as.data.frame(unique(smallnet$Org))

smallnet$O_Name[smallnet$Org == "University of Washington"] <- "WashU"
smallnet$O_Name[smallnet$Org == "Google"] <- "Google"
smallnet$O_Name[smallnet$Org == "University of Nottingham"] <- "UNott"
smallnet$O_Name[smallnet$Org == "Newcastle University"] <- "Newcastle"
smallnet$O_Name[smallnet$Org == "University of Waterloo"] <- "Waterloo"
smallnet$O_Name[smallnet$Org == "Carnegie Mellon University"] <- "CMU"
smallnet$O_Name[smallnet$Org == "IBM Research"] <- "IBM"
smallnet$O_Name[smallnet$Org == "Korea Advanced Institute of Science and Technology"] <- "KAIST"
smallnet$O_Name[smallnet$Org == "Microsoft Research"] <- "MSR"
smallnet$O_Name[smallnet$Org == "Monash University"] <- "Monash"
smallnet$O_Name[smallnet$Org == "Queensland University of Technology"] <- "QueensUT"
smallnet$O_Name[smallnet$Org == "University of Toronto"] <- "UToronto"
smallnet$O_Name[smallnet$Org == "Tsinghua University"] <- "Tsinghua"
smallnet$O_Name[smallnet$Org == "University of London"] <- "ULondon"
smallnet$O_Name[smallnet$Org == "Simon Fraser University"] <- "SimonF"
smallnet$O_Name[smallnet$Org == "Eindhoven University of Technology"] <- "EindhovenUT"
smallnet$O_Name[smallnet$Org == "University of Colorado Boulder"] <- "Boulder"
smallnet$O_Name[smallnet$Org == "University of California Berkeley"] <- "Berkeley"
smallnet$O_Name[smallnet$Org == "Ludwig Maximilian University of Munich"] <- "LudwigM"
smallnet$O_Name[smallnet$Org == "University College London"] <- "UCLondon"
smallnet$O_Name[smallnet$Org == "Indiana University"] <- "IndianaU"
smallnet$O_Name[smallnet$Org == "Stanford University"] <- "Stanford"
smallnet$O_Name[smallnet$Org == "Adobe Research"] <- "Adobe"
smallnet$O_Name[smallnet$Org == "University of Melbourne"] <- "Melbourne"
smallnet$O_Name[smallnet$Org == "Massachusetts Institute of Technology"] <- "MIT"
smallnet$O_Name[smallnet$Org == "University of Michigan"] <- "UMich"
smallnet$O_Name[smallnet$Org == "Cornell University"] <- "CornellU"
smallnet$O_Name[smallnet$Org == "Northumbria University"] <- "Northumbria"
smallnet$O_Name[smallnet$Org == "Stony Brook University"] <- "StonyBrook"
smallnet$O_Name[smallnet$Org == "Swansea University"] <- "Swansea"
smallnet$O_Name[smallnet$Org == "Hong Kong University of Science and Technology"] <- "HKU"
smallnet$O_Name[smallnet$Org == "Georgia Institute of Technology"] <- "GTech"
smallnet$O_Name[smallnet$Org == "Lancaster University"] <- "Lancaster"
smallnet$O_Name[smallnet$Org == "University of Maryland College Park"] <- "MCollegePark"
smallnet$O_Name[smallnet$Org == "University of British Columbia"] <- "BColumbia"
smallnet$O_Name[smallnet$Org == "Northwestern University"] <- "Northwestern"
smallnet$O_Name[smallnet$Org == "University of Copenhagen"] <- "Copenhagen"
smallnet$O_Name[smallnet$Org == "RMIT University"] <- "RMIT"

smallnet <- smallnet[c(1,3)]

#==================================================================================

df_ab <- df

abstracts <- read.csv("Completed Abstracts.csv")
abstracts <- abstracts[c(1,4)]

mergedDF <- merge(df_ab, abstracts ,by="ID")
write.csv(mergedDF, file = "orgs-with-abstracts.csv")

small_tp1 <- mergedDF %>% filter(Org == "University of Washington" | Org == "University of Michigan")
write.csv(small_tp1, file="washu-michu.csv")

small_tp2 <- mergedDF %>% filter(Org == "Carnegie Mellon University" | Org == "Stanford University")
write.csv(small_tp2, file = "stanford-cmu.csv")

#==================================================================================

#Building the graph
g <- graph.data.frame(smallnet, directed=FALSE)

bipartite.mapping(g)

V(g)$type <- bipartite_mapping(g)$type

plot(g)

#graphing and labeling
plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")


V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "circle")
E(g)$color <- "lightgray"

plot(g, vertex.label.cex = 0, vertex.label.color = "black")


V(g)$label.color <- "black" 
V(g)$label.cex <- 0.0
V(g)$frame.color <-  "gray"
V(g)$size <- 6

#dev.new(width = 2000, height = 2000, unit = "px")

V(g)$color <- c("orange", "steel blue")[V(g)$type+1]
V(g)$shape <- c("circle", "square")[V(g)$type+1]

V(g)$label <- ""
V(g)$label[V(g)$type==T] <- V(g)$name[V(g)$type==T]
V(g)$label.cex=1
V(g)$label.font=2

#4+V(g)$type)*0.4
plot(g, vertex.label.color="black", vertex.size=(5-V(g)$type)*0.8)

#plot(g, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)

dev.new(width = 2000, height = 2000, unit = "px")

plot(g, vertex.shape="none", vertex.label=V(g)$name,
     vertex.label.color=V(g)$color, vertex.label.font=2,
     vertex.label.cex=1.1, edge.color="gray70", edge.width=2)

#=============================================================================================================
#USE FULL DATASET FOR THIS!
f2 <- fulldat
g2 <- graph.data.frame(fulldat, directed=FALSE)
bipartite.mapping(g2)
V(g2)$type <- bipartite_mapping(g2)$type

#analyzing bimodal networks
types <- V(g2)$type 
deg <- degree(g2)
bet <- betweenness(g2)
clos <- closeness(g2)
eig <- eigen_centrality(g2)$vector

cent_df <- data.frame(types, deg, bet, eig)

cent_df[order(cent_df$type, decreasing = TRUE),]

#some more operations
V(g)$size <- degree(g)
V(g)$label.cex <- degree(g) * 0.2

plot(g, layout = layout_with_graphopt)

#tnet analysis
install.packages("tnet")
library(tnet)

tm<-get.edgelist(g2, names=FALSE)
head(tm)  # check to make sure it worked

NodeLabels <- V(g2)$name
head(NodeLabels)   # Again, check

mt <- tm[, c(2, 1)]
head(mt)

deg_tm <- degree_tm(tm)
deg_mt <- degree_tm(mt)

#converting from bimodal networks to unimodal networks
bipartite_matrix <- as_incidence_matrix(g2)
bipartite_matrix

t_bi_matrix <- t(bipartite_matrix)

event_matrix_prod <- t(bipartite_matrix) %*% bipartite_matrix 
## crossprod() does same and scales better, but this is better to learn at first at first so you understand the method
diag(event_matrix_prod) <- 0
m1 <- event_matrix_prod
x1 <- colSums(m1 != 0)

x2 <- data.frame(x1)

x3 <- as.data.frame(as.table(m1))
names(x3) <- c("school1", "school2", "count")


cmu <- x3 %>% filter(school2 == "Carnegie Mellon University")
uclondon <- x3 %>% filter(school2 == "University College London")
utoronto <- x3 %>% filter(school2 == "University of Toronto")
standford <- x3 %>% filter(school2 == "Stanford University")
MSR <- x3 %>% filter(school2 == "Microsoft Research")

person_matrix_prod <- bipartite_matrix %*% t(bipartite_matrix)
diag(person_matrix_prod) <- 0
m2 <- person_matrix_prod


women_overlap <- graph_from_adjacency_matrix(person_matrix_prod, 
                                             mode = "undirected", 
                                             weighted = TRUE)
women_overlap


df_weights <- E(women_overlap)$weight
