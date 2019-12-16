install.packages("igraph")
library(igraph)
install.packages("data.table")
library(data.table)
install.packages("tidyverse")
library(tidyverse)
library(data.table)

setwd("C:\\Users\\devan\\cosc5610-datamining-fa19\\Project")

df <- read.csv("data_without_ands.csv")
df <- df[c(2,3,4,5)]

df2 <- df[order(df$Org),]
df3 <- df[order(df$Author),]

df4 <- df[!(is.na(df$Author) | df$Author==""), ]
df5 <- df4[!(is.na(df4$Org) | df4$Org==""), ]

df_auth <- df5[c(1,3)]

#Melt data for authors to get our boy Stevie
df_authy <- df_auth %>% filter(str_detect(Author, "Steve Benford"))

df_stevie <- df_auth[df_auth$ID == 5 | df_auth$ID == 30 | df_auth$ID == 148 | df_auth$ID == 398 | df_auth$ID == 474 | df_auth$ID == 569 | df_auth$ID == 579 | df_auth$ID == 698,]
#5, 30, 148, 398, 474, 569, 579, 698

authdat <- unique(df_auth[c("ID", "Author")])
a_counts <- as.data.frame(table(authdat$Author))
write.csv(a_counts, file="authycount.csv")

odat <- unique(df5[c("ID", "Org")])
o_counts <- as.data.frame(table(odat$Org))
write.csv(o_counts, file="orgycount.csv")


smaller_df <- authdat
smallnet <- smaller_df[smaller_df$Author %in% names(which(table(smaller_df$Author) > 3)), ]


smallnet <- smaller_df[smaller_df$Author %in% names(which(table(smaller_df$Author) < 5 & table(smaller_df$Author) > 2)), ]

#Building the graph
g <- graph.data.frame(smallnet, directed=FALSE)
bipartite.mapping(g)
V(g)$type <- bipartite_mapping(g)$type


V(g)$color <- c("orange", "steel blue")[V(g)$type+1]
V(g)$shape <- c("circle", "square")[V(g)$type+1]

V(g)$label <- ""
V(g)$label[V(g)$type==T] <- V(g)$name[V(g)$type==T]
V(g)$label.cex=1
V(g)$label.font=2

plot(g, vertex.label.color="black", vertex.size=(5-V(g)$type)*0.8)

dev.new(width = 2000, height = 2000, unit = "px")

plot(g, vertex.shape="none", vertex.label=V(g)$name,
     vertex.label.color=V(g)$color, vertex.label.font=2,
     vertex.label.cex=1.1, edge.color="gray70", edge.width=2)

#CREATE ADJACENCY MATRICES

f <- df_auth
g2 <- graph.data.frame(f, directed=FALSE)
bipartite.mapping(g2)
V(g2)$type <- bipartite_mapping(g2)$type

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
names(x3) <- c("auth1", "auth2", "count")

x3$auth1 <- as.character(x3$auth1)
x3$auth2 <- as.character(x3$auth2)

install.packages("dplyr")
library(dplyr)

##Go through each row and determine if a value is zero
row_sub = apply(x3, 1, function(count) all(count !=0 ))
##Subset as usual
newdf <- x3[row_sub,]

write.csv(newdf, file="pleasework.csv")

sttteve <- newdf %>% filter(auth2 == "Steve Benford")

stevy <- newdf[newdf$auth2 == 'Steve Benford',]

#=============================================================
write.csv(x3, file = "auth_matrix.csv")

stevy <- x3[x3$auth1 == 'Steve Benford',]
stevy <- x3 %>% filter(auth1 == "Steve Benford")





