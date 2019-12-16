smaller_df <- test2[c(1,3)]
u_names <- as.data.frame(unique(smaller_df$Org))

smaller_df$O_Name[smaller_df$Org == "Google"] <- "Google"
smaller_df$O_Name[smaller_df$Org == "University of Nottingham"] <- "Nottingham"
smaller_df$O_Name[smaller_df$Org == "Monash University"] <- "Monash"
smaller_df$O_Name[smaller_df$Org == "University of Waterloo"] <- "Waterloo"
smaller_df$O_Name[smaller_df$Org == "Simon Fraser University"] <- "SimonFraser"
smaller_df$O_Name[smaller_df$Org == "University of Colorado Boulder"] <- "UCBoulder"
smaller_df$O_Name[smaller_df$Org == "Ludwig Maximilian University of Munich"] <- "LudwigM"
smaller_df$O_Name[smaller_df$Org == "Indiana University"] <- "Indiana"
smaller_df$O_Name[smaller_df$Org == "Adobe Research"] <- "Adobe"
smaller_df$O_Name[smaller_df$Org == "Aalto University"] <- "Aalto"
smaller_df$O_Name[smaller_df$Org == "Massachusetts Institute of Technology"] <- "MIT"
smaller_df$O_Name[smaller_df$Org == "Cornell University"] <- "CornellU"
smaller_df$O_Name[smaller_df$Org == "Stony Brook University"] <- "StonyB"
smaller_df$O_Name[smaller_df$Org == "Hong Kong University of Science and Technology"] <- "HKU"
smaller_df$O_Name[smaller_df$Org == "University of Bristol"] <- "Bristol"
smaller_df$O_Name[smaller_df$Org == "Lancaster University"] <- "Lancaster"
smaller_df$O_Name[smaller_df$Org == "University of Maryland College Park"] <- "MarylandCP"
smaller_df$O_Name[smaller_df$Org == "University of Stuttgart"] <- "Stuttgart"
smaller_df$O_Name[smaller_df$Org == "Northwestern University"] <- "Northwestern"
smaller_df$O_Name[smaller_df$Org == "University of Edinburgh"] <- "Edinburgh"
smaller_df$O_Name[smaller_df$Org == "KTH Royal Institute of Technology"] <- "KTH"

smaller_df <- smaller_df[c(1,3)]


#Building the graph
g <- graph.data.frame(smaller_df, directed=FALSE)

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

plot(g, layout = layout_with_graphopt)
