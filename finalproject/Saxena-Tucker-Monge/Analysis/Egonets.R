install.packages("dplyr")
install.packages("reshape2")
install.packages("tidyr")
install.packages("igraph")
library(dplyr)
library(reshape2)
library(tidyr)
library(igraph)
library(data.table)

setwd("C:\\Users\\devan\\cosc5610-datamining-fa19\\Project")

df <- read.csv("data_without_ands.csv")
df <- df[c(2,3,4,5)]

df2 <- df[order(df$Org),]
df3 <- df[order(df$Author),]

df4 <- df[!(is.na(df$Author) | df$Author==""), ]
df5 <- df4[!(is.na(df4$Org) | df4$Org==""), ]

df_auth <- df5[c(1,3)]

#Melt data for authors to get our boy Danny
df_authy <- df_auth %>% filter(str_detect(Author, "John Vines"))

df_danny <- df_auth[df_auth$ID == 70 | df_auth$ID == 84 | df_auth$ID == 249 | df_auth$ID == 377 | df_auth$ID == 628 | df_auth$ID == 677,]

#GRAPHIE!
#Building the graph

smallnet <- df_danny

g <- graph.data.frame(smallnet, directed=FALSE)
bipartite.mapping(g)
V(g)$type <- bipartite_mapping(g)$type


V(g)$color <- c("orange", "steel blue")[V(g)$type+1]
V(g)$shape <- c("circle", "none")[V(g)$type+1]

V(g)$label <- ""
V(g)$label[V(g)$type==T] <- V(g)$name[V(g)$type==T]
V(g)$label.cex=1.1
V(g)$label.font=2

plot(g, vertex.label.color="black", vertex.size=(5-V(g)$type)*0.8, edge.color="gray70", edge.width=2)

dev.new(width = 2000, height = 2000, unit = "px")

plot(g, vertex.shape="none", vertex.label=V(g)$name,
     vertex.label.color=V(g)$color, vertex.label.font=2,
     vertex.label.cex=1.1, edge.color="gray70", edge.width=2)
