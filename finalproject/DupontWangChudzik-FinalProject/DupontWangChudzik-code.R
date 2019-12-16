library(igraph)
library(readtext)
library(tidyverse)
library(tnet)
library(cluster)    # clustering algorithms
library(readxl)
#library(mallet)
library(NLP)
library(tm)
library(SnowballC)
library(topicmodels)
library(tidyr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(scales)
library(broom)

emailEdges <-  read.csv("./data/DupontWangChudzik-2.csv", stringsAsFactors = FALSE)
emailAttributes <- read.csv("./data/DupontWangChudzik-1.csv", stringsAsFactors = FALSE)
network.df <- read.csv('./data/DupontWangChudzik-3.csv', stringsAsFactors = FALSE)
emails.df <- read.csv('./data/DupontWangChudzik-4.csv', stringsAsFactors = FALSE)
stopwords.df <- read.table('./data/DupontWangChudzik-5.txt', stringsAsFactors = FALSE)

wd <- getwd()
wdout <- paste(wd, "data", sep = "/")
setwd(wdout)
set.seed(1234)

###########################################################################
#   Initial Network Construction, Analysis
###########################################################################
network.directed <- graph.data.frame(emailEdges, directed = TRUE)

#Collapse network edges into weights
E(network.directed)$weight <- 1
network.directed.weighted <- igraph::simplify(network.directed, edge.attr.comb=list(weight='sum', email= 'concat', 'ignore'))

# centrality metrics = 
V(network.directed)$degree                <- degree(network.directed)
V(network.directed)$betweenness           <- betweenness(network.directed, weights = NULL)

V(network.directed.weighted)$degree       <- degree(network.directed.weighted)
V(network.directed.weighted)$strength     <- strength(network.directed.weighted)
V(network.directed.weighted)$betweenness  <- betweenness(network.directed.weighted, weights = NULL)

V(network.directed.weighted)$indegree     <- degree(network.directed.weighted, mode = c("in"))
V(network.directed.weighted)$outdegree    <- degree(network.directed.weighted, mode = c("out"))

#Top 10 most central nodes (#4 will shock you)
top10Degree <- head(order(V(network.directed.weighted)$degree, decreasing = TRUE), n = 10)
top10Strength <- head(order(V(network.directed.weighted)$strength, decreasing = TRUE), n = 10)
top10Betweenness <- head(order(V(network.directed.weighted)$betweenness, decreasing = TRUE), n = 10)

top10DegreeVertices <- V(network.directed.weighted)$name[top10Degree]
top10StrengthVertices <- V(network.directed.weighted)$name[top10Strength]
top10BetweennessVertices <- V(network.directed.weighted)$name[top10Betweenness]
top10CentralityIndices <- unique(c(top10Degree, top10Strength, top10Betweenness))

top10MetricVertices <- unique(c(top10DegreeVertices, top10StrengthVertices, top10BetweennessVertices))

V(network.directed.weighted)$isTop10Degree      <- V(network.directed.weighted) %in% top10Degree
V(network.directed.weighted)$isTop10Strength    <- V(network.directed.weighted) %in% top10Strength
V(network.directed.weighted)$isTop10Betweenness <- V(network.directed.weighted) %in% top10Betweenness
V(network.directed.weighted)$isTop10Metric      <- V(network.directed.weighted) %in% top10CentralityIndices

tidy(top10DegreeVertices)
tidy(top10StrengthVertices)
tidy(top10BetweennessVertices)

##The one address in the most important nodes list with no outdegree
subset(V(network.directed.weighted)$name[top10CentralityIndices], V(network.directed.weighted)$outdegree[top10CentralityIndices] == 0)

###########################################################################
#   Centrality Distributions, Edge weight distributions
###########################################################################
V(network.directed)$indegree <- degree(network.directed, mode = c("in"))
V(network.directed)$outdegree <- degree(network.directed, mode = c("out"))

sendersOnly = subset(V(network.directed), V(network.directed)$indegree == 0)
recipientsOnly = subset(V(network.directed), V(network.directed)$outdegree == 0)

degreeHistogram <- as.data.frame(table(V(network.directed.weighted)$degree))
degreeHistogram$Var1 <- as.numeric(levels(degreeHistogram$Var1))[degreeHistogram$Var1]

png(filename = 'Figure 1 - Degree Distribution.png', width = 400, height = 400)
ggplot(degreeHistogram, aes(x=Var1, y=Freq)) + geom_point() +
  scale_x_log10(breaks=c(0,1,2,5,10,20,50,100,200,500)) + scale_y_log10(breaks=c(0,1,5,10,50,100,500,750)) +
  xlab('Degree') + ylab('Frequency') + ggtitle('Degree Distribution') + theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
dev.off()


strengthHistogram <- as.data.frame(table(V(network.directed.weighted)$strength))
strengthHistogram$Var1 <- as.numeric(levels(strengthHistogram$Var1))[strengthHistogram$Var1] 

png(filename = 'Figure 2 - Strength Distribution.png', width = 400, height = 400)
ggplot(strengthHistogram, aes(x=Var1, y=Freq)) + geom_point() +
  scale_x_log10(breaks=c(0,1,2,5,10,20,50,100,200,500)) + scale_y_log10(breaks=c(0,1,5,10,50,100,500,750)) +
  xlab('Strength') + ylab('Frequency') + ggtitle('Strength Distribution') + theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
dev.off()


#inDegreeHistogram <- as.data.frame(table(V(network.directed.weighted)$indegree))
#inDegreeHistogram$Var1 <- as.numeric(levels(inDegreeHistogram $Var1))[inDegreeHistogram $Var1] 

#png(filename = 'InDegreeDistribution.png', width = 400, height = 400)
#ggplot(inDegreeHistogram, aes(x=Var1, y=Freq)) + geom_point() +
#  scale_x_log10(breaks=c(0,1,2,5,10,20,50,100,200,500)) + scale_y_log10(breaks=c(0,1,5,10,50,100,500,750)) +
#  xlab('InDegree') + ylab('Frequency') + ggtitle('InDegree Distribution') + theme_bw() +
#  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#dev.off()


#outDegreeHistogram <- as.data.frame(table(V(network.directed.weighted)$outdegree))
#outDegreeHistogram$Var1 <- as.numeric(levels(outDegreeHistogram$Var1))[outDegreeHistogram$Var1] 

#svg(filename = 'OutDegreeDistribution.svg', width = 10, height = 10)
#ggplot(outDegreeHistogram, aes(x=Var1, y=Freq)) + geom_point() +
#  scale_x_log10(breaks=c(0,1,2,5,10,20,50,100,200,500)) + scale_y_log10(breaks=c(0,1,5,10,50,100,500,750)) +
#  xlab('OutDegree') + ylab('Frequency') + ggtitle('OutDegree Distribution') + theme_bw() +
#  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#dev.off()


edgeWeightHistogram <- as.data.frame(table(E(network.directed.weighted)$weight))
edgeWeightHistogram$Var1 <- as.numeric(levels(edgeWeightHistogram$Var1))[edgeWeightHistogram$Var1]

png(filename = "Figure 3 - Edge Weight Distribution.png", height = 400, width = 400)
ggplot(edgeWeightHistogram, aes(x=Var1, y=Freq)) + geom_point() +
  scale_x_log10(breaks=c(0,1,2,5,10,20,50,100,200,500)) + scale_y_log10(breaks=c(0,1,5,10,50,100,500,750)) +
  xlab('Edge Weight') + ylab('Frequency') + ggtitle('Edge Weight Distribution') + theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

maxEdgeWeight <- max(E(network.directed.weighted)$weight)

###########################################################################
#                           Network Visualizations
###########################################################################

###########################Visualization parameters########################

E(network.directed.weighted)$width <- .5
E(network.directed.weighted)$arrow.size <- .1
E(network.directed.weighted)$arrow.width <- .5


# node color
V(network.directed.weighted)$color <- ifelse(V(network.directed.weighted)$degree>100, "red", "black")
V(network.directed.weighted)$frame.color <- V(network.directed.weighted)$color
# node size
V(network.directed.weighted)$size <- log(V(network.directed.weighted)$degree, 10)
# node label
V(network.directed.weighted)$label <- ifelse(V(network.directed.weighted)$degree>100, V(network.directed.weighted)$name, "")
V(network.directed.weighted)$label <- ""
V(network.directed.weighted)$label.cex <- .4

##########################################################################

network.directed.weighted.layout = layout_with_fr(network.directed.weighted, grid='nogrid')

V(network.directed.weighted)$color <- "white"	
V(network.directed.weighted)$frame.color <- "black"
V(network.directed.weighted)$size <- log(V(network.directed.weighted)$strength, base = 10)
#Write to file
png('Figure 4 - DNC Email Network.png', width = 1600, height = 1600)
plot(network.directed.weighted, layout = network.directed.weighted.layout)
dev.off()

#Node Color: Green nodes only input information, Red nodes only receive information.
V(network.directed.weighted)$color <- ifelse(V(network.directed.weighted)$outdegree==0, "red", 
                                             ifelse(V(network.directed.weighted)$indegree==0, "green", "yellow"))
V(network.directed.weighted)$frame.color <- "black"
V(network.directed.weighted)$frame.size <- .1
V(network.directed.weighted)$size <- 1


png('Figure 5 - DNC Email Network Sources And Sinks.png', width = 1600, height = 1600)
plot(network.directed.weighted, layout = network.directed.weighted.layout)
dev.off()

V(network.directed.weighted)$color = ifelse(V(network.directed.weighted)$name %in% top10MetricVertices, "white", "black")
V(network.directed.weighted)$frame.color = "black"
V(network.directed.weighted)$size = ifelse(V(network.directed.weighted)$name %in% top10MetricVertices, 3, 1)  	
for (i in 1:length(V(network.directed.weighted))) {	
  V(network.directed.weighted)$color[i] = rgb(ifelse(V(network.directed.weighted)$isTop10Degree[i], 1, 0),	
                                              ifelse(V(network.directed.weighted)$isTop10Strength[i], 1, 0),	
                                              ifelse(V(network.directed.weighted)$isTop10Betweenness[i], 1, 0),	
                                              1)	
}	
V(network.directed.weighted)$label = ifelse(V(network.directed.weighted)$isTop10Metric, V(network.directed.weighted)$name, "")
png('Figure 6 - DNC Email Network Important Nodes.png', width = 1600, height = 1600)	
plot(network.directed.weighted, layout = network.directed.weighted.layout)	
dev.off()

V(network.directed.weighted)$size = 1
V(network.directed.weighted)$label = ""
#########################################################
#                      Cluster Analysis
#########################################################

# cluster

numberClusters <- 12

graph.clusters <- clusters(network.directed.weighted)
cluster.vector <- graph.clusters$membership[which(graph.clusters$membership==1)]
cluster.vector <- names(cluster.vector)
graph.sub <- subgraph(network.directed.weighted, cluster.vector)
com <- cluster_spinglass(graph.sub, spins=numberClusters)

# Identify high edge weight pair outside of main centroid
externalPairs <- graph.clusters$membership[which(graph.clusters$membership!=1)]
externalPairs <- names(externalPairs)
graph.outliers <- subgraph(network.directed.weighted, externalPairs)
E(graph.outliers)
E(graph.outliers)$weight

# set cluster color
for (i in 1:length(V(network.directed.weighted)$name))
{
  idx <- which(com$name==V(network.directed.weighted)$name[i])
  if (length(idx) == 0) {
    V(network.directed.weighted)$membership[i] = 0
    V(network.directed.weighted)$color[i] = 1
  }
  else {
    V(network.directed.weighted)$membership[i] = com$membership[idx]
    V(network.directed.weighted)$color[i] = com$membership[idx] + 1
  }
}
V(network.directed.weighted)$frame.color <- V(network.directed.weighted)$color

minC <- rep(-Inf, vcount(network.directed.weighted))
maxC <- rep(Inf, vcount(network.directed.weighted))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(network.directed.weighted, minx=minC, maxx=maxC,
                     miny=minC, maxy=maxC, grid='nogrid')

V(network.directed.weighted)$highestDegreeSubgraphNode <- FALSE
# Look at central nodes of each cluster
for (i in 1:numberClusters){
  #i <- 4
  clusterNodes <- which(V(network.directed.weighted)$membership == i)
  clusterNodes
  highestStrengthNode <- max(V(network.directed.weighted)$strength[clusterNodes])
  highestStrengthNode <- which(V(network.directed.weighted)$strength[clusterNodes] == highestStrengthNode)
  V(network.directed.weighted)$highestDegreeSubgraphNode[clusterNodes[highestStrengthNode]] <- TRUE
}

V(network.directed.weighted)$color <- ifelse(V(network.directed.weighted)$outdegree==0, "red", 	
                                             ifelse(V(network.directed.weighted)$indegree==0, "green", "yellow"))
V(network.directed.weighted)$size <- ifelse(V(network.directed.weighted)$highestDegreeSubgraphNode, 3, 1)
V(network.directed.weighted)$frame.color <- V(network.directed.weighted)$membership

V(network.directed.weighted)$label = ifelse(V(network.directed.weighted)$highestDegreeSubgraphNode, paste(V(network.directed.weighted)$membership, ", ", V(network.directed.weighted)$name), "")
png(filename = "Figure 7 - DNC Email Network with Clusters.png", width = 1000, height = 1000)	
plot(network.directed.weighted, layout=network.directed.weighted.layout)	
dev.off()	
V(network.directed.weighted)$label = ""
##Extra New Stuff

outlierIndices <- which(V(network.directed.weighted)$membership == 0)
highestStrengthOfOutlier <- max(V(network.directed.weighted)$strength[outlierIndices])
highestStrengthOutlier <- which(V(network.directed.weighted)$strength == highestStrengthOfOutlier)
associatedEdges <- E(network.directed.weighted)[inc(V(network.directed.weighted)[strength == highestStrengthOfOutlier])]
associatedEdges$email

#for (thisCluster in 1:length(numberClusters))
#{
#  nodesOfThisCluster <- which(V(network.directed.weighted)$membership == thisCluster)
#  
#  for (node in 1:length(nodesOfThiscluster))
#  {
#    thisCluster <- 1
#    node <- 5
#    associatedEdges <- E(network.directed.weighted)[inc(V(network.directed.weighted)[node])]
#    allEmailsInThisCluster <- unlist(associatedEdges$email)
#    for (emailIdx in 1:length(allEmailsInThisCluster)){
#      emailId = allEmailsInThisCluster[emailIdx]
#      #emailId = "4841.eml"
#      timestamp <- c(timestamp, emailAttributes$timestamp[which(emailAttributes$email == emailId)])
#    }
#    timestamp
#  }
#}

##########################################################################
#                     Overall topic modeling - LDA
##########################################################################

emails <- emailAttributes

emails <- na.omit(emails)
remove <- which(nchar(emails$body) == 0)
if (0 != length(remove)) {
  emails <- emails[-remove,]
}

# convert to corpus
names(emails) = c("doc_id", "subject", "text")
emails$doc_id = as.character(emails$doc_id)
emails <- as.data.frame(emails)
docs <- Corpus(DataframeSource(emails))

# removing potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, ' ', x))})
docs <- tm_map(docs, toSpace, '-')
docs <- tm_map(docs, toSpace, '\'')
docs <- tm_map(docs, toSpace, "\"")
docs <- tm_map(docs, toSpace, '\\.')
docs <- tm_map(docs, toSpace, ':')
docs <- tm_map(docs, toSpace, '@')
docs <- tm_map(docs, toSpace, '/')
docs <- tm_map(docs, toSpace, '”')
docs <- tm_map(docs, toSpace, '“')
docs <- tm_map(docs, toSpace, '‘')
docs <- tm_map(docs, toSpace, '’')

# remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
# to lower case
docs <- tm_map(docs, tolower)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords.df[[1]])
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Stem document
docs <- tm_map(docs, stemDocument, 'english')

# create LDA model
topic.dtm <- DocumentTermMatrix(docs)
ui <- unique(topic.dtm$i)
topic.dtm.ui <- topic.dtm[ui, ]
ap_lda <- LDA(topic.dtm.ui, k = 4, control = list(seed = 1234))
# get term beta information
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
# plot top term
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
# write gamma information into files
#email.gamma <- tidy(ap_lda, matrix="gamma")
#gamma.df <- as.data.frame(email.gamma)
#write.csv(gamma.df, 'overall_gamma.csv')

##################################################################################
#                     cluster topic modeling - LDA
##################################################################################

data.graph <- graph.data.frame(network.df)

# create 12 clusters for the main subgraph
graph.clusters <- clusters(data.graph)
cluster.vector <- graph.clusters$membership[which(graph.clusters$membership==1)]
cluster.vector <- names(cluster.vector)
graph.sub <- subgraph(data.graph, cluster.vector)
com <- cluster_spinglass(graph.sub, spins=12)

# clean data
emailbodies <- emails.df$body
docs <- Corpus(VectorSource(emailbodies)) # create corpus from vector of email bodies
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, ' ', x))})
docs <- tm_map(docs, toSpace, '-')
docs <- tm_map(docs, toSpace, "\"")
docs <- tm_map(docs, toSpace, '\\.')
docs <- tm_map(docs, toSpace, ':')
docs <- tm_map(docs, toSpace, '@')
docs <- tm_map(docs, toSpace, '/')
docs <- tm_map(docs, toSpace, '”')
docs <- tm_map(docs, toSpace, '“')
docs <- tm_map(docs, toSpace, '‘')
docs <- tm_map(docs, toSpace, '’')
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords.df[[1]])
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument, 'english')
email.topic <- data.frame(text = sapply(docs, paste, collapse = " "), stringsAsFactors = FALSE)
#email.id <- str_split_fixed(emails$doc_id, '\\.', 2)
email.topic <- cbind(emails.df$index, email.topic)
colnames(email.topic) <- c("doc_id", "text")
email.topic$doc_id <- as.character(email.topic$doc_id)
email.topic <- na.omit(email.topic)
remove <- which(nchar(email.topic$text) == 0)
email.topic <- email.topic[-remove,]

# loop for the 12 clusters
for (j in 1:length(com$csize))
{
  # create data frame
  cluster.vector <- com$names[which(com$membership==j)]
  topic.df <- NULL
  for (i in 1:nrow(network.df))
  {
    if (!(network.df$node1[i] %in% cluster.vector))
      next()
    if (!(network.df$node2[i] %in% cluster.vector))
      next()
    email.list <- strsplit(network.df$emails[i], ',')
    topic.df <- rbind(topic.df, email.topic[email.topic$doc_id%in%email.list[[1]], ])
  }
  
  # create LDA model
  topic.dtm <- DocumentTermMatrix(Corpus(DataframeSource(topic.df)))
  rowSum <- apply(topic.dtm , 1, sum)
  topic.dtm <- topic.dtm[rowSum> 0, ]
  ap_lda <- LDA(topic.dtm, k = 2, control = list(seed = 1234))
  # get beta information for terms
  ap_topics <- tidy(ap_lda, matrix = "beta")
  ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  # plot top terms
  ap_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered() + 
    labs(title=paste("cluster ", j)) +
    ggsave(paste("cluster_",j,".png", sep = ""))
  # output gamma
  #email.gamma <- tidy(ap_lda, matrix="gamma")
  #gamma.df <- as.data.frame(email.gamma)
  #write.csv(gamma.df, paste('cluster', j, '_gamma.csv'))
}

#####################################################################################
