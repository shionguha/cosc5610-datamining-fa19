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

df <- read.csv("FINFINDAT.csv")
df_nov18 <- df[,c(1,2)]
write.csv(df_nov18, file = "Nov18_getAbstracts.csv")

df2 <- melt(df, id=(c("ID", "Title")))
df3 <- df2[order(df2$ID),]
df4 <- df3[-which(df3$value == ""),]
df5 <- df4[c(1,2,4)]
names(df5) <- c("ID", "Title", "Authors")

write.csv(df5, file = "NS_cleaneddata_Oct3.csv")
new <- read.csv("NS_cleaneddata_Oct3.csv")

#Helper function
new_fun <- function(x){
  out <- gsub("[\\(\\)]", "", regmatches(x, gregexpr("\\(.*?\\)", x))[[1]])
  out
}

new$org <- lapply(new$Authors,FUN = new_fun)

new$org[new$org == "character(0)"] <- NA

new <- new[c(-1)]

new$newAuth <- gsub("\\s*\\([^\\)]+\\)","",as.character(new$Authors))

new <- new[c(1,2,5,4)]
names(new) <- c("ID", "Title", "Author", "Organization")

fwrite(new, file = "dat_nov29.csv")


cleaned_df <- unnest(new, new$Organization)

savethis <- cleaned_df[, c(1,2,4)]
names(savethis) <- c("ID", "Title", "Org")

save2 <- na.omit(savethis, cols="Org")
write.csv(save2, file = "Nov14-title-org.csv")

org_counts <- as.data.frame(table(cleaned_df$`new$Organization`))
auth_counts <- as.data.frame(table(cleaned_df$Author))


write.csv(org_counts, file="org_counts.csv")
write.csv(auth_counts, file="auth_counts.csv")
write.csv(cleaned_df, file = "CLEANED_WITH_NA_ORGS.csv")
#####################################################################

edge_is_authors <- read.csv("Edge_is_authors.csv")
edge_is_authors <- edge_is_authors[c(2,3,4)]
names(edge_is_authors) <- c("ID", "Title", "Org")

edge_is_authors$Org_ID <- as.numeric(factor(edge_is_authors$Org, levels=unique(edge_is_authors$Org)))
write.csv(edge_is_authors, file="org_with_ID.csv")

small_df <- edge_is_authors[edge_is_authors$Org %in% names(which(table(edge_is_authors$Org) > 18)), ]

test2 <- edge_is_authors[edge_is_authors$Org %in% names(which(table(edge_is_authors$Org) < 18 & table(edge_is_authors$Org) > 10)), ]

unique_counts <- as.data.frame(unique(edge_is_authors$Org))

small2 <- small_df[c(1,3)]
small2$O_Name[small2$Org == "University of Washington"] <- "UDub"
small2$O_Name[small2$Org == "Carnegie Mellon University"] <- "CMU"
small2$O_Name[small2$Org == "Korea Advanced Institute of Science and Technology"] <- "Korea AI"
small2$O_Name[small2$Org == "Newcastle University"] <- "Newcastle"
small2$O_Name[small2$Org == "Microsoft Research"] <- "MR"
small2$O_Name[small2$Org == "University College London"] <- "UCLondon"
small2$O_Name[small2$Org == "University of Michigan"] <- "UMich"
small2$O_Name[small2$Org == "Stanford University"] <- "Stanford"
small2$O_Name[small2$Org == "Georgia Institute of Technology"] <- "Gtech"
small2$O_Name[small2$Org == "Eindhoven University of Technology"] <- "Eindhoven"
small2$O_Name[small2$Org == "University of Toronto"] <- "UToronto"
small2$O_Name[small2$Org == "Tsinghua University"] <- "Tsinghua"
small2$O_Name[small2$Org == "University of California Berkeley"] <- "Berkeley"

test <- as.data.frame(unique(small2$O_Name))
small2 <- small2[c(1,3)]

#Building the graph
g <- graph.data.frame(small2, directed=FALSE)

bipartite.mapping(g)

V(g)$type <- bipartite_mapping(g)$type

plot(g)

#graphing and labeling
plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")


V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "circle")
E(g)$color <- "lightgray"

#plot(g, vertex.label.cex = 0, vertex.label.color = "black")


V(g)$label.color <- "black" 
V(g)$label.cex <- 0.0
V(g)$frame.color <-  "gray"
V(g)$size <- 6

png("my_plot.png", 1000, 1000)

dev.off()
plot(g, layout = layout_with_graphopt)

plot(g, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)



#######################################################################
x<- as.data.frame(table(new$Author))
x2 <- unnest(new, new$Organization)

x3<- as.data.frame(table(x2$`new$Organization`))

paper_dat <- new[c(1,2,4)]
paper_dat <- paper_dat[!is.na(paper_dat$org),]

findat <- unnest(paper_dat, paper_dat$org)
names(findat) <- c("ID", "Title", "Org")
write.csv(findat, file="Edge_is_authors.csv")

findat2 <- unnest(new, new$org)
names(findat2) <- c("ID", "Title", "Authors", "Org")
write.csv(findat2, file="AUTHOR_ORG.csv")


#new$org <- ifelse(grepl('\\(', new$Authors), "yes", "no")
#abc <- "Gary Hsieh (University of Washington)"
#gsub("[\\(\\)]", "", regmatches(abc, gregexpr("\\(.*?\\)", abc))[[1]])