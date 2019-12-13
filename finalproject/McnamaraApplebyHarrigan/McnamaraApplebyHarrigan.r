library(tidyverse)
library(caret)
library(igraph)
library(readr)
library(knitr)
library(lubridate)
library(gridExtra)
library(reshape2)
library(topicmodels)
library(gutenbergr)
library(tm)
library(SnowballC)
library(ldatuning)
library(tidytext)
library(wordcloud)
library(corrplot)

# !diagnostics off

tweets <- read_csv("McnamaraApplebyHarrigan-data.csv")
users <- read_csv("McnamaraApplebyHarrigan-data2.csv")

data <- tweets


# Date and Data Parsing --------------------------

users$month<-sapply(users$created_at, function(x) match(strsplit(x," ")[[1]][2], month.abb))
users$year<-sapply(users$created_at, function(x) as.numeric(strsplit(x," ")[[1]][6]))
users$day<-sapply(users$created_at, function(x) as.numeric(strsplit(x," ")[[1]][3]))
users$DayTS<-as.Date(paste0(users$year,'-',users$month,'-',users$day), format="%Y-%m-%d")
#clean from empty creation date
#users<-data.frame(users %>% filter(created_at !=""))

tweets$DayTS<-as.Date(tweets$created_str,format="%Y-%m-%d")
tweets$year<-year(tweets$DayTS)
tweets$month<-month(tweets$DayTS)
tweets$day<-day(tweets$DayTS)
tweets$weekdays<-weekdays(tweets$DayTS)
tweets$week<-week(tweets$DayTS)
tweets$weekdays <- factor(tweets$weekdays, levels = rev(c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday")))

users$date <- as.POSIXct(users$DayTS)

# Graphing ------------------
# Plot by Year-Month
# https://stackoverflow.com/questions/17758006/time-series-plot-with-x-axis-in-year-month-in-r

time <- as.POSIXct(strptime(c("2009-01-07 00:00:00","2017-09-26 23:59:59"), format = "%Y-%m-%d %H:%M:%S"))

user.creation <- users %>% group_by(year,month) %>% summarise(count=n()) %>% mutate(dateTS = as.Date(paste0(year,'-',month,'-01'),format="%Y-%m-%d")) %>% 
  ggplot(aes(x=as.POSIXct(dateTS),y=count)) + geom_point(size=1) + geom_line(alpha=.5,size=1, color = "#ff0505") + 
  scale_x_datetime(limits =time) + labs(title="Users Created by Month") + xlab("Month and Year") + ylab("Number of Users Created") +
  theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#808484"), axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"), axis.title=element_text(colour="white"))

tweet.time <- tweets %>% group_by(year,month) %>% summarise(count=n()) %>% mutate(dateTS = as.Date(paste0(year,'-',month,'-01'),format="%Y-%m-%d")) %>% 
  ggplot(aes(x=as.POSIXct(dateTS),y=count)) + geom_point(size=1) + geom_line(alpha=.5,size=1, color = "#ff0505") + 
  scale_x_datetime(limits =time) + labs(title="Number of Tweets by Month") + xlab("Month and Year") + ylab("Number of Tweets") +
  theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#808484"), axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"), axis.title=element_text(colour="white"))


pe <- tweets[(tweets$DayTS> "2016-01-01" & tweets$DayTS < "2016-12-31"),]
petime <- as.POSIXct(strptime(c("2016-01-01 00:00:00","2016-12-31 23:59:59"), format = "%Y-%m-%d %H:%M:%S"))

pe.month <- pe %>% group_by(year,month) %>% summarise(count=n()) %>% mutate(dateTS = as.Date(paste0(year,'-',month,'-01'),format="%Y-%m-%d")) %>% 
  ggplot(aes(x=as.POSIXct(dateTS),y=count)) + geom_point(size=1) + geom_line(alpha=.5,size=1, color = "#ff0505") + 
  scale_x_datetime(limits =petime) + labs(title="Tweets in 2016 by Month") + xlab("") + ylab("Number of Tweets") +
  geom_vline(xintercept=as.numeric(as.POSIXct(as.Date('2016-11-08'))),color='blue') +
  ggplot2::annotate("text", x=as.POSIXct(as.Date("2016-10-11")), y = 20000, label = "Election Day", size=3, colour="blue") +
  theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#808484"), axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"), axis.title=element_text(colour="white"))


pe.day <- pe %>% group_by(DayTS) %>% summarise(count=n()) %>%
  ggplot(aes(x=DayTS,y=count)) + geom_point(size=1) + geom_line(alpha=.5,size=1, color = "#ff0505") + 
  labs(title="Tweets in 2016 by Day") + xlab("") + ylab("Number of Tweets") +
  geom_vline(xintercept=as.Date('2016-11-08'),color='blue') +
  ggplot2::annotate("text", x=as.Date("2016-11-08"), y = 4000, label = "Election Day", size=3, colour="blue") +
  geom_vline(xintercept=as.Date('2016-10-19'),color='dark green') +
  ggplot2::annotate("text", x=as.Date("2016-10-19"), y = 3500, label = "Access Hollywood", size=3, colour="dark green") +
  geom_vline(xintercept=as.Date('2016-10-07'),color='black') +
  ggplot2::annotate("text", x=as.Date("2016-10-07"), y = 3000, label = "Wikileaks emails \n release", size=3, colour="black") +
  geom_vline(xintercept=as.Date('2016-09-26'),color='purple') +
  ggplot2::annotate("text", x=as.Date("2016-09-26"), y = 2750, label = "First debate", size=3, colour="purple") +
  geom_vline(xintercept=as.Date('2016-09-17'),color='orange') +
  ggplot2::annotate("text", x=as.Date("2016-09-17"), y = 2625, label = "Unknown", size=3, colour="orange") +
  geom_vline(xintercept=as.Date('2016-09-11'),color='blue') +
  ggplot2::annotate("text", x=as.Date("2016-09-11"), y = 2500, label = "Clinton Overheats", size=3, colour="blue") +
  geom_vline(xintercept=as.Date('2016-07-21'),color='dark green') +
  ggplot2::annotate("text", x=as.Date("2016-07-21"), y = 2000, label = "Trump Nomination", size=3, colour="dark green") +
  geom_vline(xintercept=as.Date('2016-03-22'),color='black') +
  ggplot2::annotate("text", x=as.Date("2016-03-22"), y = 1500, label = "Additional Primaries", size=3, colour="black") +
  geom_vline(xintercept=as.Date('2016-02-02'),color='purple') +
  ggplot2::annotate("text", x=as.Date("2016-02-02"), y = 1000, label = "Iowa Caucuses", size=3, colour="purple") +
  theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#808484"), axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"), axis.title=element_text(colour="white"))

pdf("1_user_tweet_distribution_graphs.pdf", width = 20, height = 11)
grid.arrange(user.creation,tweet.time, pe.month, pe.day, ncol=1)
dev.off()



pe.day.limited <- pe %>% group_by(DayTS) %>% summarise(count=n()) %>%
  ggplot(aes(x=DayTS,y=count)) + geom_point(size=1) + geom_line(alpha=.5,size=1, color = "#ff0505") + 
  labs(title="Tweets in 2016 by Day") + xlab("") + ylab("Number of Tweets") +
  geom_vline(xintercept=as.Date('2016-11-08'),color='blue') +
  ggplot2::annotate("text", x=as.Date("2016-11-08"), y = 4000, label = "Election Day", size=3, colour="blue") +
  ggplot2::annotate("text", x=as.Date("2016-10-07"), y = 3000, label = "Wikileaks emails \n release", size=3, colour="black") +
  geom_vline(xintercept=as.Date('2016-09-17'),color='orange') +
  ggplot2::annotate("text", x=as.Date("2016-09-17"), y = 2625, label = "Unknown", size=3, colour="orange") +
  theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#808484"), axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"), axis.title=element_text(colour="white"))


pdf("2_tweets_by_day_limited.pdf", width = 20, height = 11)
plot(pe.day.limited)
dev.off()





#####################
# Taking the date tweet surges from above, target dats for additional analysis will be
# 2016-03-22 "Additional Primaries" As the oldest date, how are these tweets different from later?
# 2016-07-21 "Trump nomination" Does this mark a point of protrump interference?
## 2016-09-17 "No idea" What does this spike represent?
## 2016-10-07 "Wikileaks emails released"
# 2016-10-19 "Access Hollywood" What can you even say about this?
## 2016-11-08 "Election Day" What was the final message?

# a <- subset(tweets, DayTS == "2016-03-22")
# b <- subset(tweets, DayTS == "2016-07-21")
c <- subset(tweets, DayTS == "2016-09-17")
pre.c <- subset(tweets, DayTS <= "2016-09-17")
d <- subset(tweets, DayTS == "2016-10-07")
pre.d <- subset(tweets, DayTS <= "2016-10-07")
# e <- subset(tweets, DayTS == "2016-10-19")
f <- subset(tweets, DayTS == "2016-11-08")
pre.f <- subset(tweets, DayTS <= "2016-11-08")

z <- tweets



plotfunction <- function(df) {
  
  # RT Parsing --------------------------------------
  
  # Grep RT's 
  rt <- grep("^rt @[a-z0-9_]{1,15}", tolower(df$text), perl=T, value=T)
  
  # Select RT senders 
  rt.send <- tolower(as.character(df$user_key[grep("^rt @[a-z0-9_]{1,15}", tolower(df$text), perl=T)]))
  rt.rec <- tolower(regmatches(rt, regexpr("@(?U).*:", rt)))
  # Remove @ and :
  rt.rec <- (gsub(":", "", rt.rec))
  rt.rec <- (gsub("@", "", rt.rec)) 
  View(rt.rec)
  
  # Missing Values as NA
  rt.send[rt.send==""] <- "<NA>"
  rt.rec[rt.rec==""] <- "<NA>"
  
  # Create single df with all users
  users.all <- unique(as.data.frame(c(rt.send, rt.rec))) 
  #renaming the handle names variable
  users.all <- users.all %>% rename(user = "c(rt.send, rt.rec)")
  
  #Force global df
  rt.send <<- rt.send
  rt.rec <<- rt.rec
  
  
  df <- df %>% rename(user = user_key) #renaming user name variable
  tweets.user <- df %>% select(user) #selecting only the users from the data
  trolls <- users %>% select(screen_name)
  trolls <- trolls %>% rename(user = screen_name)
  
  trolls <- rbind(trolls, tweets.user)
  
  trolls.u <- unique(trolls) #removing duplicates
  trolls.u$troll <- "troll" #assigning all of these users a trolls
  ### matching trolls with the complete set of handle names in the retweet network
  
  nodes <- right_join(trolls.u, users.all)
  nodes <<- replace(nodes, is.na(nodes), "non-troll") 
  
  
  # Network graph creation ###########################
  
  # This is an edge list, who RTs who and how many times
  rt.df <<- data.frame(rt.send, rt.rec)
  ### creating the retweetnetwork based on the sender-receiver df and the node attributes (troll/non-troll)
  rt.g <<- graph.data.frame(rt.df, directed=T, vertices = nodes)
  
  
  # bipartite.mapping(rt.g)
  # V(rt.g)$type <- bipartite_mapping(rt.g)$type
  # plot(rt.g)
  
  ### removing self-ties
  rt.g.noloop <<-simplify(rt.g, remove.loops = T, remove.multiple = F)
  
  
  
  # Now we can compute basic centrality scores for each user and store it in a data frame.
  # removing multiple edges between users
  g <- simplify(rt.g.noloop, remove.multiple = T, remove.loops = T)
  # creating a data frame with weighted and unweighted degree centrality for each profile
  g.centrality <- data.frame(name =V(g)$name,
                             troll= V(g)$troll,indegree=degree(g,mode='in'),
                             indegree_weighted = degree(rt.g.noloop, mode ="in"),
                             outdegree=degree(g,mode='out'),
                             outdegree_weighted = degree(rt.g.noloop, mode = "out"))
  
  colnames(g.centrality)[colnames(g.centrality)=="name"] <- "user"
  g.centrality <<- g.centrality
  
  return(rt.g.noloop)
}

# If you're looking at this wondering why I didn't just create a loop...
# getting the analysis done took prioirity over efficiency.




plotfunction(c)
# data frame c -------------------
# Still Missing Closness and Types
# Reuse this for each dataframe C, D, F:

c.cent.df <- nodes
c.cent.rt.g <- rt.g.noloop
c.cent.df$deg <- degree(rt.g.noloop)
c.cent.df$bet <- betweenness(rt.g.noloop)
c.cent.df$eig <- eigen_centrality(rt.g.noloop)$vector
c.cent.df <- c.cent.df %>% left_join(g.centrality[,c("user","indegree","indegree_weighted","outdegree","outdegree_weighted")], by=c("user"))



# Write file to Gephi
write.graph(rt.g.noloop, file="functionoutput_c.graphml", format="graphml")


plotfunction(d)
# data frame d -------------------
# Reuse this for each dataframe C, D, F:

d.cent.df <- nodes
d.cent.rt.g <- rt.g.noloop
d.cent.df$deg <- degree(rt.g.noloop)
d.cent.df$bet <- betweenness(rt.g.noloop)
d.cent.df$eig <- eigen_centrality(rt.g.noloop)$vector
d.cent.df <- d.cent.df %>% left_join(g.centrality[,c("user","indegree","indegree_weighted","outdegree","outdegree_weighted")], by=c("user"))

# Write file to Gephi
write.graph(rt.g.noloop, file="functionoutput_d.graphml", format="graphml")

plotfunction(f)
# data frame f -------------------
# Reuse this for each dataframe C, D, F:

f.cent.df <- nodes
f.cent.rt.g <- rt.g.noloop
f.cent.df$deg <- degree(rt.g.noloop)
f.cent.df$bet <- betweenness(rt.g.noloop)
f.cent.df$eig <- eigen_centrality(rt.g.noloop)$vector
f.cent.df <- f.cent.df %>% left_join(g.centrality[,c("user","indegree","indegree_weighted","outdegree","outdegree_weighted")], by=c("user"))

# Write file to Gephi
write.graph(rt.g.noloop, file="functionoutput_f.graphml", format="graphml")


plotfunction(pre.c)
# data frame pre c (all dates <= c) -------------------
# Reuse this for each dataframe C, D, F:

pre.c.df <- nodes
pre.c.cent.rt.g <- rt.g.noloop
pre.c.df$deg <- degree(rt.g.noloop)
pre.c.df$bet <- betweenness(rt.g.noloop)
pre.c.df$eig <- eigen_centrality(rt.g.noloop)$vector
pre.c.df <- pre.c.df %>% left_join(g.centrality[,c("user","indegree","indegree_weighted","outdegree","outdegree_weighted")], by=c("user"))

plotfunction(pre.d)
# data frame pre d (all dates <= d) -------------------
# Reuse this for each dataframe C, D, F:

pre.d.df <- nodes
pre.d.cent.rt.g <- rt.g.noloop
pre.d.df$deg <- degree(rt.g.noloop)
pre.d.df$bet <- betweenness(rt.g.noloop)
pre.d.df$eig <- eigen_centrality(rt.g.noloop)$vector
pre.d.df <- pre.d.df %>% left_join(g.centrality[,c("user","indegree","indegree_weighted","outdegree","outdegree_weighted")], by=c("user"))

#pre.f <- subset(tweets, DayTS <= "2016-11-08")
plotfunction(pre.f)
pre.f.df <- nodes
pre.f.cent.rt.g <- rt.g.noloop
pre.f.df$deg <- degree(rt.g.noloop)
pre.f.df$bet <- betweenness(rt.g.noloop)
pre.f.df$eig <- eigen_centrality(rt.g.noloop)$vector
pre.f.df <- pre.f.df %>% left_join(g.centrality[,c("user","indegree","indegree_weighted","outdegree","outdegree_weighted")], by=c("user"))


plotfunction(z)
# data frame z (all data)-------------------
# Still Missing Closness and Types
# Reuse this for each dataframe C, D, F:

cent.df <- nodes
cent.rt.g <- rt.g.noloop
cent.df$deg <- degree(rt.g.noloop)
cent.df$bet <- betweenness(rt.g.noloop)
cent.df$eig <- eigen_centrality(rt.g.noloop)$vector
cent.df <- cent.df %>% left_join(g.centrality[,c("user","indegree","indegree_weighted","outdegree","outdegree_weighted")], by=c("user"))

# Write file to Gephi
write.graph(rt.g.noloop, file="functionoutput_z.graphml", format="graphml")


# cata frame pre f (all dates <= f) -------------------





# Begin Delta Analysis --------------------------------


# Left joinging centrality dataframes into a single data frame 
# pre c, d, f 9_17, 10_7, 11_8

z.cent.df <- cent.df
z.cent.df <- z.cent.df %>% left_join(pre.c.df[,c("user","deg","bet","eig","indegree","indegree_weighted","outdegree","outdegree_weighted")], by=c("user"), suffix = c("","_c_9_17"))
z.cent.df <- z.cent.df %>% left_join(pre.d.df[,c("user","deg","bet","eig","indegree","indegree_weighted","outdegree","outdegree_weighted")], by=c("user"), suffix = c("","_d_10_7"))
z.cent.df <- z.cent.df %>% left_join(pre.f.df[,c("user","deg","bet","eig","indegree","indegree_weighted","outdegree","outdegree_weighted")], by=c("user"), suffix = c("","_f_11_8"))


# Find delta between dates as percent change
# Delta 1 difference between 9-17 and 10-7
# Delta 2 difference between 10-7 and 11-8

# Create Delta 1 measuresments 9-17 and 10-7
z.cent.df$Delta1_deg <- (((z.cent.df$deg_d_10_7 - z.cent.df$deg_c_9_17)/ z.cent.df$deg_c_9_17)*100)
z.cent.df$Delta1_bet <- (((z.cent.df$bet_d_10_7 - z.cent.df$bet_c_9_17)/ z.cent.df$bet_c_9_17)*100)
z.cent.df$Delta1_eig <- (((z.cent.df$eig_d_10_7 - z.cent.df$eig_c_9_17)/ z.cent.df$eig_c_9_17)*100)
z.cent.df$Delta1_W_IN <- (((z.cent.df$indegree_weighted_d_10_7 - z.cent.df$indegree_weighted_c_9_17)/ z.cent.df$indegree_weighted_c_9_17)*100)
z.cent.df$Delta1_W_OUT <- (((z.cent.df$outdegree_weighted_d_10_7 - z.cent.df$outdegree_weighted_c_9_17)/ z.cent.df$outdegree_weighted_c_9_17)*100)


# Create Delta 2 measurements 10-7 to 11-8
z.cent.df$Delta2_deg <- (((z.cent.df$deg_f_11_8 - z.cent.df$deg_d_10_7)/ z.cent.df$deg_d_10_7)*100)
z.cent.df$Delta2_bet <- (((z.cent.df$bet_f_11_8 - z.cent.df$bet_d_10_7)/ z.cent.df$bet_d_10_7)*100)
z.cent.df$Delta2_eig <- (((z.cent.df$eig_f_11_8 - z.cent.df$eig_d_10_7)/ z.cent.df$eig_d_10_7)*100)
z.cent.df$Delta2_W_IN <- (((z.cent.df$indegree_weighted_f_11_8 - z.cent.df$indegree_weighted_d_10_7)/ z.cent.df$indegree_weighted_d_10_7)*100)
z.cent.df$Delta2_W_OUT <- (((z.cent.df$outdegree_weighted_f_11_8 - z.cent.df$outdegree_weighted_d_10_7)/ z.cent.df$outdegree_weighted_d_10_7)*100)

# graphing differences in troll degree change between the two dates
troll.z.cent.df <- z.cent.df %>% filter(troll == "troll")
#degree <- troll.z.cent.df[c("user","Delta1_deg","Delta2_deg")] %>% melt(degree, id.vars=c("user"))
#degree <- melt(degree, id.vars=c("user"))

degree.density <- troll.z.cent.df %>% select(user, Delta1_deg, Delta2_deg) %>% melt(id.vars=c("user")) %>% 
  ggplot(aes(x = value, fill = variable)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#119111","#ff0505"))

degree.box <- troll.z.cent.df %>% select(user, Delta1_deg, Delta2_deg) %>% melt(id.vars=c("user")) %>% 
  ggplot(aes(x=variable, y=value)) + 
  scale_y_log10() +
  geom_boxplot(aes(col=variable), alpha=0.5) + 
  theme(legend.position="none") + 
  scale_color_manual(values=c("#119111", "#ff0505")) + 
  labs(title="Delta 1 vs Delta 2 troll changes in degree") + 
  theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#808484"), axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"), axis.title=element_text(colour="white"))

w.in.box <- troll.z.cent.df %>% select(user, Delta1_W_IN, Delta2_W_IN) %>% melt(id.vars=c("user")) %>%
  ggplot(aes(x=variable, y=value)) + 
  scale_y_log10() +
  geom_boxplot(aes(col=variable), alpha=0.5) + 
  theme(legend.position="none") + 
  scale_color_manual(values=c("#119111", "#ff0505")) + 
  labs(title="Delta 1 vs Delta 2 troll Changes Indegree") + 
  theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#808484"), axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"), axis.title=element_text(colour="white"))

w.out.box <- troll.z.cent.df %>% select(user, Delta1_W_OUT, Delta2_W_OUT) %>% melt(id.vars=c("user")) %>% 
  ggplot(aes(x=variable, y=value)) + 
  scale_y_log10() +
  geom_boxplot(aes(col=variable), alpha=0.5) + 
  theme(legend.position="none") + 
  scale_color_manual(values=c("#119111", "#ff0505")) + 
  labs(title="Delta 1 vs Delta 2 troll Changes Outdegree") + 
  theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#808484"), axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"), axis.title=element_text(colour="white"))

bet.box <- troll.z.cent.df %>% select(user, Delta1_bet, Delta2_bet) %>% melt(id.vars=c("user")) %>% 
  ggplot(aes(x=variable, y=value)) + 
  scale_y_log10() +
  geom_boxplot(aes(col=variable), alpha=0.5) + 
  theme(legend.position="none") + 
  scale_color_manual(values=c("#119111", "#ff0505")) + 
  labs(title="Delta 1 vs Delta 2 troll changes in Betweenness") + 
  theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#808484"), axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"), axis.title=element_text(colour="white"))

eig.box <- troll.z.cent.df %>% select(user, Delta1_eig, Delta2_eig) %>% melt(id.vars=c("user")) %>% 
  ggplot(aes(x=variable, y=value)) + 
  scale_y_log10() +
  geom_boxplot(aes(col=variable), alpha=0.5) + 
  theme(legend.position="none") + 
  scale_color_manual(values=c("#119111", "#ff0505")) + 
  labs(title="Delta 1 vs Delta 2 troll changes in EIG") + 
  theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#808484"), axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"), axis.title=element_text(colour="white"))




# Degree increases as the campaign goes on
pdf("3_degree_density.pdf", width = 10, height = 5)
plot(degree.density)
dev.off()

pdf("4_degree_box.pdf", width = 10, height = 5)
plot(degree.box)
dev.off()

pdf("5_betweeness_box.pdf", width = 10, height = 5)
plot(bet.box)
dev.off()

pdf("5_eig_box.pdf", width = 10, height = 5)
plot(eig.box)
dev.off()

# primarily out degree:
pdf("6_in_out_box.pdf", width = 10, height = 10)
grid.arrange(w.in.box,w.out.box,ncol=1,nrow=2)
dev.off()




# Identfiy the top n% of trolls with the highest % change in degree --------------------------------------
n <- 5
top.trolls.delta.one <- subset(troll.z.cent.df, Delta1_deg > quantile(Delta1_deg, prob = 1 - n/100,na.rm=TRUE))
top.trolls.delta.two <- subset(troll.z.cent.df, Delta2_deg > quantile(Delta2_deg, prob = 1 - n/100,na.rm=TRUE))

# Users: followers to status = gaining more followers with less tweets (more effective?)
# Number of tweets in the dataset will be different from the overall counts in the user data
# So outside fo followers to status we'll use numbers from the tweet dataset itself for any calculations
#users$followers_to_status <- (users$followers_count / users$statuses_count)

# Total and Average tweets per user within the dataset, ignore NA values
tweet.calc.sub <- subset(tweets, select = c(user_key, retweet_count, favorite_count))#, weekdays, week,DayTS, created_str))
tweet.calc.sub$NumberofTweetsInDataSet <- 1 
tweet.calc.sub <- tweet.calc.sub %>%
  group_by(user_key) %>%
  summarise_all(funs(mean(., na.rm = TRUE),sum(., na.rm = TRUE)))
# remove this column that makes no sense
tweet.calc.sub$NumberofTweetsInDataSet_mean <- NULL
# Replace error NaN (divide by zero) columns with NA
tweet.calc.sub$retweet_count_mean[is.nan(as.numeric(tweet.calc.sub$retweet_count_mean))] <- NA
tweet.calc.sub$favorite_count_mean[is.nan(as.numeric(tweet.calc.sub$favorite_count_mean))] <- NA

# adding calculations back to z dataframe
colnames(tweet.calc.sub)[colnames(tweet.calc.sub)=="user_key"] <- "user"
z.cent.df <- z.cent.df %>% left_join(tweet.calc.sub[,c("user","retweet_count_mean","favorite_count_mean","NumberofTweetsInDataSet_sum")], by=c("user"))

# Just renaming the troll.z.cent.df df for easy of memory going forward
troll.subset <- troll.z.cent.df %>% left_join(tweet.calc.sub[,c("user","retweet_count_mean","favorite_count_mean","NumberofTweetsInDataSet_sum")], by=c("user"))

#pulling the troll with the highest degree in the complete dataset
#ameliebaldwin <- tweets %>% filter(user_key == "ameliebaldwin")

# Finding Retweeting in dataset ---------------------------------
# Parsing RT from tweet (looking at first two letters only) 
#Using the z dataframe, a copy of tweets, to reduce touching original df
z$RT <- substr(tweets$text, start = 1, stop = 2)
#Numerical value for RT = true
z$RT <- (ifelse(z$RT == 'RT',"1","0"))
# Updating column name of z dataframe from user key to user for joining
#colnames(z)[colnames(z)=="user"] <- "user_key"
colnames(z)[colnames(z)=="user_key"] <- "user"
# Change to numeric data type for summarization
z$RT <- as.numeric(z$RT)
#Sum total RT's found by user, store in an intermediate table
RTtotal <- aggregate( RT ~ user, z, sum)
#Joining to the tweet calc subset for calculation
tweet.calc.sub <- tweet.calc.sub %>% left_join(RTtotal[,c("user","RT")], by=c("user"))
# removing junk df
rm(RTtotal)
# calculating percent RT from total number of tweets in tweets/z df
tweet.calc.sub$percentRT <- (tweet.calc.sub$RT / tweet.calc.sub$NumberofTweetsInDataSet_sum) 
# Moving this number to the troll.z.cent.df with other measurements and metrics to be used in the model
troll.subset <- troll.subset %>% left_join(tweet.calc.sub[,c("user","percentRT","RT")], by=c("user"))


troll.bet.v.rt <- ggplot(troll.subset, aes(x=percentRT, y=bet)) + 
  geom_point() + 
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  scale_y_log10()

troll.deg.v.rt <- ggplot(troll.subset, aes(x=percentRT, y=deg)) + 
  geom_point() + 
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  scale_y_log10()

troll.eig.v.rt <- ggplot(troll.subset, aes(x=percentRT, y=eig)) + 
  geom_point() + 
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  scale_y_log10() 


# Not needed
# troll.deg.v.bet <- ggplot(troll.subset, aes(x=deg, y=bet)) + 
#                    geom_point() + 
#                    geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)

## Percent retweeted tweets and count of average retweets doesn't really work because of the low numbers of retweet counts
## It might otherwise seem that trolls with a higher percent of RT tweets have less of their tweets RT'd
#ggplot(troll.subset, aes(x=percentRT, y=retweet_count_mean)) + 
#   geom_point() +
#   scale_y_log10() +
#   geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)

pdf("7_troll_percentrt_v_metric.pdf", width = 11, height = 10)
grid.arrange(troll.bet.v.rt,troll.deg.v.rt,troll.eig.v.rt,ncol=1,nrow=3)
dev.off()



# Saving off the troll subset to another dataframe
troll.subset.save <- troll.subset

# Previous calculations left Nan and Inf values, will replace those with NA below:
# function to remove all nan values from troll subset 
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
#Replace NaN values with NA
troll.subset[is.nan(troll.subset)] <- NA
#Replace inf values with NA
troll.subset <- do.call(data.frame,lapply(troll.subset, function(x) replace(x, is.infinite(x),NA)))


# Only taking the factors I really care about along for the modeling ride
troll.subset.limited <- troll.subset[,c("deg","eig","bet","retweet_count_mean","favorite_count_mean","percentRT")]


numericVars <- which(sapply(troll.subset.limited, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')

# put numeric variables into the numeric data frame and calculate their correlations
all_numVar <- troll.subset.limited[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations 
cor_sorted <- as.matrix(sort(cor_numVar[,'percentRT'], decreasing = TRUE))
# #select only high corelations 
# Skip this, cause none of our variables correlate well... :-(
# CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
# cor_numVar <- cor_numVar[CorHigh, CorHigh]
# 
# corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")


pdf("8_correlation_plot_numeric_variables.pdf", width = 6, height = 5)
corrplot(cor_numVar)
dev.off()

#check and remove zero or near zero variance features 
nzv <- nearZeroVar(troll.subset.limited, saveMetrics = TRUE)
# There are none
#troll.subset.limited.nzv <- troll.subset.limited[,-nzv]

## Create correlation table to find highly correlated features. Remove those with > 0.75 correlation
## to prevent multicollinearity
# troll.subset.limited.cor <- cor(troll.subset.limited)
# summary(troll.subset.limited.cor[upper.tri(troll.subset.limited.cor)])
# glmdnc <- findCorrelation(troll.subset.limited.cor, cutoff = .75)
# troll.subset.limited.cor.nocor <- troll.subset.limited.cor[,-glmdnc]
# 
# # Center and Scale values
# glm.data.nzv.nocor.hv <- subset(glm.data.nzv.nocor, select = c(Highvalue))
# glm.data.nzv.nocor <- subset(glm.data.nzv.nocor, select = -c(Highvalue))


# Preprocess values by centering and scaling, remove any NA values
troll.subset.limited.preprocess <- preProcess(troll.subset.limited, method = c("center","scale"))
glm.preprocess <- predict(troll.subset.limited.preprocess,troll.subset.limited)
glm.preprocess <- na.omit(glm.preprocess)


# Create random 60/40 training and testing dataset
smp_size <- floor(0.60 * nrow(glm.preprocess))
train_ind <- sample(seq_len(nrow(glm.preprocess)), size = smp_size)
glm.train <- glm.preprocess[train_ind,]
glm.test <- glm.preprocess[-train_ind,]


# Below, testing several different models to find the best accuracy

# glm <- glm(percentRT~., data = glm.train, family = "gaussian")
# 
# sink("9_glm_summary.txt")
# print(summary(glm))
# sink()
# 
# confint(glm)
# predict(glm, type="response")


# Setting control and performance metric RMSE
control <- trainControl(method ="cv", number = 10)
metric <- "RMSE"

# Train KNN, SVM, RF models for performance comparison
set.seed(333)
fit.knn <- train(percentRT~., data = glm.train, method="knn", metric=metric, trControl=control)
set.seed(333)
fit.svm <- train(percentRT~., data = glm.train, method="svmRadial", metric=metric, trControl=control)
set.seed(333)
fit.rf <- train(percentRT~., data = glm.train, method="rf", metric=metric, trControl=control, importance=T)
set.seed(333)
fit.glm <- train(percentRT~., data = glm.train, method="glm", metric=metric, trControl=control)
results <- resamples(list(knn=fit.knn, svm=fit.svm,rf=fit.rf,glm=fit.glm))

#summary(results)
sink("10_model_results_summary.txt")
print(summary(results))
sink()

# Plot feature importances 
scales <- list(x=list(relation="free"), y=list(relation="free"))

pdf("11_model_importance_plot.pdf", width = 6, height = 3)
dotplot(results, scales=scales)
dev.off()


pdf("11_RF_variable_importance_plot.pdf", width = 6, height = 3)
plot(varImp(fit.rf, scale = FALSE))
dev.off()

#importance.svm <- varImp(fit.svm, scale = FALSE)
# plot(importance.svm)

# Random Forest showed the best performance
#knn.prediction <- predict(fit.knn, glm.test)
#svm.prediction <- predict(fit.svm, glm.test)
rf.prediction <- predict(fit.rf, glm.test)
#glm.prediction <- predict(fit.glm, glm.test)


#Evalution of random forest prediciton

sink("11_rf_performance.txt")
print(postResample(pred = rf.prediction, obs = glm.test$percentRT))
sink()


# Begin Text Analysis ---------------------------

#text <- tweets$text

textfunction <- function(text) {
  
  # Preprocessing to remove puncuation, upper case, numbers, white space, and stop words
  # https://compsocialscience.github.io/summer-institute/2018/materials/day3-text-analysis/basic-text-analysis/rmarkdown/Basic_Text_Analysis_in_R.html#tokenization
  # https://www.springboard.com/blog/text-mining-in-r/
  text <- as.data.frame(text)
  text <- data.frame(text %>% mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% filter(!is.na(text)))
  
  text.corp <- VCorpus(VectorSource(as.vector(text)))
  text.corp <- tm_map(text.corp, removePunctuation)
  text.corp <- tm_map(text.corp, content_transformer(tolower))
  text.corp <- tm_map(text.corp, removeNumbers)
  text.corp <- tm_map(text.corp, stripWhitespace)
  text.corp <- tm_map(text.corp, removeWords, stopwords("english"))
  
  # Removing additional words RT (retweet), amp (google amp mobile webpages?)
  # Untested code
  text.corp  <- tm_map(text.corp , removeWords, c(tidytext::stop_words$word,"rt","amp","http"))
  
  # Stemming
  text.corp <- tm_map(text.corp, stemDocument, language = "english")
  
  # Creat document term matrix and remove 0 word entries from the table
  text.dtm <- DocumentTermMatrix(text.corp, control = list(wordLengths = c(2, Inf)))
  
  # Another pass at taking out the trash then pushing it global for additional analysis
  text.corp <<- text.corp[!grepl('^http|amp|rt',text.corp)]
  text.corp.tdm <- TermDocumentMatrix(Corpus(VectorSource(text.corp)))
  text.corp.tdm.m <<- as.matrix(text.corp.tdm)
  
  # #Find the sum of words in each Document This method results in a vector of 370Gb, use method below
  # rowTotals <- apply(text.dtm, 1, sum) 
  # #remove all docs without words
  # text.dtm.one   <- text.dtm[rowTotals> 0, ]
  
  # A document-term-matrix created by the tm package contains the names i and j , 
  # which are indices for where entries are in the sparse matrix. 
  # If text.dtm$i does not contain a particular row index p, then row p is empty
  # ui contains all the non-zero indices, and since text.dtm$i is already ordered, text.dtm.n will be in the same order as text.dtm
  ui = unique(text.dtm$i)
  text.dtm.n = text.dtm[ui,]
  
  
  #Pushing non-zero document term matrixto global frame
  text.dtm.n <<- text.dtm.n
  
  return (text.dtm.n)
}






#c <- subset(tweets, DayTS == "2016-09-17")--------------------
# Converting function global frames to subset specific frames before additional processing
c.text <- c$text
textfunction(c.text)
c.text.corp <- text.corp
c.text.dtm.n <- text.dtm.n
c.text.tdm.m <- text.corp.tdm.m


cc.text.lda <- LDA(c.text.dtm.n, k = 2, control = list(seed = 333))
cc.text.topics <- tidy(cc.text.lda, matrix = "beta")

cc.text.topics.top <- cc.text.topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

cc.comm.text.topics.top <- cc.text.topics.top

cc.text.topics.plot <- cc.comm.text.topics.top %>% 
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

pdf("12_cc_text_topics.pdf", width = 6, height = 5)
plot(cc.text.topics.plot)
dev.off()


#d <- subset(tweets, DayTS == "2016-10-07")
d.text <- d$text
textfunction(d.text)
d.text.corp <- text.corp
d.text.dtm.n <- text.dtm.n
d.text.tdm.m <- text.corp.tdm.m


dd.text.lda <- LDA(d.text.dtm.n, k = 2, control = list(seed = 333))
dd.text.topics <- tidy(dd.text.lda, matrix = "beta")

dd.text.topics.top <- dd.text.topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

dd.comm.text.topics.top <- dd.text.topics.top

dd.text.topics.plot <- dd.comm.text.topics.top %>% 
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

pdf("13_dd_text_topics.pdf", width = 6, height = 5)
plot(dd.text.topics.plot)
dev.off()



#f <- subset(tweets, DayTS == "2016-11-08")
f.text <- f$text
textfunction(f.text)
f.text.corp <- text.corp
f.text.dtm.n <- text.dtm.n
f.text.tdm.m <- text.corp.tdm.m

ff.text.lda <- LDA(f.text.dtm.n, k = 2, control = list(seed = 333))
ff.text.topics <- tidy(ff.text.lda, matrix = "beta")

ff.text.topics.top <- ff.text.topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ff.comm.text.topics.top <- ff.text.topics.top

ff.text.topics.plot <- ff.comm.text.topics.top %>% 
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


pdf("14_ff_text_topics.pdf", width = 6, height = 5)
plot(ff.text.topics.plot)
dev.off()



# Top Topic Analysis --------------------------------------------
pdf("15_Word_clouds_we_shouldnt_use.pdf", width = 6, height = 5)

par(mfrow=c(1, 1),bg="#808484")
word.freq<-sort(rowSums(c.text.tdm.m), decreasing=T)
wordcloud(words=names(word.freq),
          freq = word.freq,
          random.order=F,
          colors=brewer.pal(8,"GnBu"),scale=c(8,1),max.words=150)
title('Most frequent words during September 17th 2016',col.main='black',cex.main=1.5)

word.freq<-sort(rowSums(d.text.tdm.m), decreasing=T)
wordcloud(words=names(word.freq),
          freq = word.freq,
          random.order=F,
          colors=brewer.pal(8,"GnBu"),scale=c(8,1),max.words=150)
title('Most frequent words during October 10th 2016',col.main='black',cex.main=1.5)

word.freq<-sort(rowSums(f.text.tdm.m), decreasing=T)
wordcloud(words=names(word.freq),
          freq = word.freq,
          random.order=F,
          colors=brewer.pal(8,"GnBu"),scale=c(8,1),max.words=150)
title('Most frequent words during November 11th 2016',col.main='black',cex.main=1.5)

dev.off()

# text.topics.top %>% ggplot(aes(term, beta, fill = factor(topic))) + 
#                     geom_col(show.legend = FALSE) + 
#                     facet_wrap(~ topic, scales = "free") +
#                     coord_flip()
#    
# 
# # gives comparison of a given word for a topic, statistical probability of word appearing
# beta_spread <- ap_topics %>%
#    mutate(topic = paste0("topic", topic)) %>%
#    spread(topic, beta) %>%
#    filter(topic1 > .001 | topic2 > .001) %>%
#    mutate(log_ratio = log2(topic2 / topic1))
#  
# beta_spread
# 



# Community detection for subset C ----------------------------------

# https://kateto.net/netscix2016.html
# clp <- cluster_label_prop(as.undirected(c.cent.rt.g))
# plot(clp, c.cent.rt.g)
# 
# cfg <- cluster_fast_greedy(as.undirected(c.cent.rt.g))
# kc <- coreness(c.cent.rt.g, mode="all")
# plot(c.cent.rt.g, vertex.size=kc*6, vertex.label=kc)

# High-betweenness edges are removed sequentially (recalculating at each step) 
# the best partitioning of the network is selected.
c.ceb <- cluster_edge_betweenness(as.undirected(c.cent.rt.g))
## Dendogram on communities

pdf("16_C_subset_community_dendogram.pdf", width = 24, height = 10)
dendPlot(c.ceb, mode="hclust")
dev.off()

## Network plot of the communities 
# plot(ceb, c.cent.rt.g) 

##number of communities
#length(c.ceb) 
c.clust.memb <- membership(c.ceb)
# assign community number back to user in the subset data frame 
c.clust.memb <- as.data.frame(as.numeric(c.clust.memb))
c.cent.df$membership <- c.clust.memb$`as.numeric(c.clust.memb)`

# By communitiy who has the highest betweeness? Find "community" of information broker trolls
# average bet by community membership for trolls
c.cent.df.troll <- c.cent.df %>% filter(troll == "troll")
c.comm.bet.mean <- aggregate( bet ~ membership, c.cent.df.troll, mean )

# GRoup 14 of the C subset has the highest betweenness
c.cent.df.comm.sub <- c.cent.df.troll %>% filter(membership == "14")
colnames(c)[colnames(c)=="user_key"] <- "user"
c.comm.text <- c.cent.df.comm.sub %>% left_join(c[,c("user","text")], by=c("user"))
c.comm.text <- c.comm.text$text

#c.comm.text
textfunction(c.comm.text)
c.comm.text.corp <- text.corp
c.comm.text.dtm.n <- text.dtm.n
c.comm.text.tdm.m <- text.corp.tdm.m

# Determine the "k" for LDA https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html
# DANGER
# DANGER
# DANGER
# LDA TUNING CAN HAVE SIGNIFICANT PERFORMANCE IMPACT
# Adjust mc.cores parameter as needed for your processor

c.result <- FindTopicsNumber(
  c.comm.text.dtm.n,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 333),
  mc.cores = 4L,
  verbose = TRUE
)

# plot the number of topics found for community 14 of the c subset, 
# this appears to be 3, so I'll modify the text function to produce 3 topics-- Maybe 5?
# I dont think there is enough data to really know, I'm going with three as it seems to be the best
# combination of extremum without having a large number of topics. 

# FindTopicsNumber_plot(c.result)

# Perform LDA on an K
c.text.lda <- LDA(c.comm.text.dtm.n, k = 5, control = list(seed = 333))
c.text.topics <- tidy(c.text.lda, matrix = "beta")

c.text.topics.top <- c.text.topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

c.comm.text.topics.top <- c.text.topics.top

# This corpus is likely far too small to be useful, but POC anyway
c.text.topics.plot <- c.comm.text.topics.top %>% 
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#plot(c.text.topics.plot)



# Community detection for subset D -----------------------------

# High-betweenness edges are removed sequentially (recalculating at each step) and the best partitioning of the network is selected.
d.ceb <- cluster_edge_betweenness(as.undirected(d.cent.rt.g))
## Dendogram on communities
# dendPlot(d.ceb, mode="hclust")
## Network plot of the communities 
# plot(d.ceb, d.cent.rt.g) 

##number of communities
#length(d.ceb) 
d.clust.memb <- membership(d.ceb)
# assign community number back to user in the subset data frame 
d.clust.memb <- as.data.frame(as.numeric(d.clust.memb))
d.cent.df$membership <- d.clust.memb$`as.numeric(d.clust.memb)`

# By communitiy who has the highest betweeness? Find "community" of information broker trolls
# average bet by community membership for trolls
d.cent.df.troll <- d.cent.df %>% filter(troll == "troll")
d.comm.bet.mean <- aggregate( bet ~ membership, d.cent.df.troll, mean )

# GRoup 1 of the D subset has the highest betweenness
d.cent.df.comm.sub <- d.cent.df.troll %>% filter(membership == "1")
colnames(d)[colnames(d)=="user_key"] <- "user"
d.comm.text <- d.cent.df.comm.sub %>% left_join(c[,c("user","text")], by=c("user"))
d.comm.text <- d.comm.text$text

#c.comm.text

textfunction(d.comm.text)
d.comm.text.corp <- text.corp
d.comm.text.dtm.n <- text.dtm.n
d.comm.text.tdm.m <- text.corp.tdm.m

# Determine the "k" for LDA https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html

d.result <- FindTopicsNumber(
  d.comm.text.dtm.n,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 333),
  mc.cores = 4L,
  verbose = TRUE
)

# plot the number of topics found for community 1 of the D subset, 

#FindTopicsNumber_plot(d.result)

# Perform LDA on an K
d.text.lda <- LDA(d.comm.text.dtm.n, k = 4, control = list(seed = 333))
d.text.topics <- tidy(d.text.lda, matrix = "beta")

d.text.topics.top <- d.text.topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

d.comm.text.topics.top <- d.text.topics.top

d.text.topics.plot <- d.comm.text.topics.top %>% 
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#plot(d.text.topics.plot)


# Community detection for subset F -----------------------------

# High-betweenness edges are removed sequentially (recalculating at each step) and the best partitioning of the network is selected.
f.ceb <- cluster_edge_betweenness(as.undirected(f.cent.rt.g))

##number of communities
f.clust.memb <- membership(f.ceb)
# assign community number back to user in the subset data frame 
f.clust.memb <- as.data.frame(as.numeric(f.clust.memb))
f.cent.df$membership <- f.clust.memb$`as.numeric(f.clust.memb)`

# By communitiy who has the highest betweeness? Find "community" of information broker trolls
# average bet by community membership for trolls
f.cent.df.troll <- f.cent.df %>% filter(troll == "troll")
f.comm.bet.mean <- aggregate( bet ~ membership, f.cent.df.troll, mean )

# GRoup 9 of the F subset has the highest betweenness
f.cent.df.comm.sub <- f.cent.df.troll %>% filter(membership == "9")
colnames(f)[colnames(f)=="user_key"] <- "user"
f.comm.text <- f.cent.df.comm.sub %>% left_join(c[,c("user","text")], by=c("user"))
f.comm.text <- f.comm.text$text

#c.comm.text

textfunction(f.comm.text)
f.comm.text.corp <- text.corp
f.comm.text.dtm.n <- text.dtm.n
f.comm.text.tdm.m <- text.corp.tdm.m

# Determine the "k" for LDA https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html

f.result <- FindTopicsNumber(
  f.comm.text.dtm.n,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 333),
  mc.cores = 4L,
  verbose = TRUE
)

# plot the number of topics found for community 9 of the F subset, 
#FindTopicsNumber_plot(f.result)

# Perform LDA on an K
f.text.lda <- LDA(f.comm.text.dtm.n, k = 5, control = list(seed = 333))
f.text.topics <- tidy(f.text.lda, matrix = "beta")

f.text.topics.top <- f.text.topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

f.comm.text.topics.top <- f.text.topics.top

f.text.topics.plot <- f.comm.text.topics.top %>% 
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#plot(f.text.topics.plot)

# Printing all the graphs and topics from this failed effort
pdf("17_CDF_subset_info_broker_community_topics.pdf", width = 7, height = 10)
FindTopicsNumber_plot(c.result)
plot(c.text.topics.plot)
FindTopicsNumber_plot(d.result)
plot(d.text.topics.plot)
FindTopicsNumber_plot(f.result)
plot(f.text.topics.plot)
dev.off()


# This doesn't work
# 
# Learning new ways to fail ---------------------
# Attempt to graph the network according to their topics using k = 5

# Reset any global data frames for network function to the c subset,
# rename the user column which is used by the fucntion
# 
# c.node.topic <- c
# colnames(c.node.topic)[colnames(c.node.topic)=="user"] <- "user_key"
# 
# plotfunction(c.node.topic)
# 
# edge.table <- rt.df
# colnames(edge.table)[colnames(edge.table)=="rt.send"] <- "source"
# colnames(edge.table)[colnames(edge.table)=="rt.rec"] <- "target"
# 
# # Reset any global data frames for text analysis to the c dataframe
# textfunction(c.text)
# c.node.text.corp <- text.corp
# c.node.text.dtm.n <- text.dtm.n
# c.node.text.tdm.m <- text.corp.tdm.m
# 
# 
# c.node.dtm_lda <- LDA(c.node.text.dtm.n, k = 5, control = list(seed = 333))
# c.node.topics_beta <- tidy(c.node.dtm_lda, matrix = "beta")
# 
# pdf("18_C_subset_topics_network_attempt.pdf", width = 7, height = 7)
# par(mfrow=c(1, 1),bg="#ffffff") #ffffff white 808484 grey
# c.node.topics_beta %>%
#   group_by(term) %>%
#   top_n(1, beta) %>%
#   group_by(topic) %>%
#   top_n(50, beta) %>%
#   acast(term ~ topic, value.var = "beta", fill = 0) %>%
#   comparison.cloud(colors = brewer.pal(5, "Set1"))
# dev.off()
# 
# 
# c.node.text.topics.top <- c.node.topics_beta %>%
#   group_by(topic) %>%
#   top_n(10, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta)
# 
# #c.node.text.topics.top
# 
# 
# c.node.topics_gamma <- tidy(c.node.dtm_lda, matrix = "gamma") %>%
#   arrange(desc(gamma))
# 
# c.node.user_topic <- c.node.topics_gamma %>%
#   group_by(document) %>%
#   top_n(1, gamma)
# 
# c.node.node_table <- data.frame(name = unique(c(as.character(edge.table$source), as.character(edge.table$target)))) %>%
#   left_join(c.node.user_topic, by = c("name" = "document")) %>%
#   unique()
# 
# 
# c.node.node_table <- c.node.node_table[!duplicated(c.node.node_table$name), ]
# #library(RColorBrewer)
# pal <- brewer.pal(5, "Set1")
# c.node.node_table$color = ifelse(c.node.node_table$topic == 1, pal[1],
#                                  ifelse(c.node.node_table$topic == 2, pal[2],
#                                         ifelse(c.node.node_table$topic == 3, pal[3],
#                                                ifelse(c.node.node_table$topic == 4, pal[4], pal[5]))))
# 
# 
# c.node.graph <- graph_from_data_frame(edge.table, directed = TRUE, vertices = c.node.node_table)
# V(c.node.graph)$size <- ifelse(V(c.node.graph)$name == "ameliebaldwin", 4, 1)
# V(c.node.graph)$label <- ifelse(V(c.node.graph)$name == "ameliebaldwin", "ameliebaldwin", NA)
# 
# node.graph <- graph_from_data_frame(edge.table, directed = TRUE)
# layout <- layout_with_fr(node.graph)
# V(node.graph)$color <- ifelse(V(node.graph)$name == "ameliebaldwin", "#377F97", "#4A9888")
# V(node.graph)$size <- ifelse(V(node.graph)$name == "ameliebaldwin", 6, 1)
# V(node.graph)$label <- ifelse(V(node.graph)$name == "ameliebaldwin", "ameliebaldwin", NA)
# 
# pdf("twitter_net.pdf", width = 70, height = 80)
# plot(node.graph,
#      layout = layout,
#      vertex.label = V(node.graph)$label,
#      vertex.color = scales::alpha(V(node.graph)$color, alpha = 0.5), 
#      vertex.size = V(node.graph)$size , 
#      vertex.frame.color = "gray", 
#      vertex.label.color = "black", 
#      vertex.label.cex = 10,
#      edge.arrow.size = 1)
# dev.off()
# 
# betweenness <- igraph::betweenness(node.graph, directed = TRUE)
# #betweenness[order(betweenness, decreasing = TRUE)]
# edge_betweenness <- igraph::edge_betweenness(node.graph, directed = TRUE)
# V(node.graph)$size <- ifelse(V(node.graph)$name == "ameliebaldwin", 10, betweenness * 0.000001)
# 
# pdf("twitter_net_betweenness2.pdf", width = 70, height = 80)
# plot(node.graph,
#      layout = layout,
#      vertex.label = V(node.graph)$label,
#      vertex.color = scales::alpha(V(node.graph)$color, alpha = 0.5), 
#      vertex.size = V(node.graph)$size, 
#      vertex.frame.color = "gray", 
#      vertex.label.color = "black", 
#      vertex.label.cex = 6,
#      edge.width = edge_betweenness * 0.0000001,
#      edge.arrow.size = 1)
# dev.off()
# 
# 
# 
# #pdf("twitter_net_topics2.pdf", width = 2560, height = 1440)
# png("my_plot.png", 2560, 1440)
# plot(c.node.graph,
#      layout = layout,
#      vertex.label = V(node.graph)$label,
#      vertex.color = scales::alpha(V(c.node.graph)$color, alpha = 0.4), 
#      vertex.size = V(c.node.graph)$size , 
#      vertex.frame.color = scales::alpha(V(c.node.graph)$color, alpha = 0.4), 
#      vertex.label.color = scales::alpha("black", alpha = 1), 
#      vertex.label.cex = 8,
#      edge.color = scales::alpha("grey", alpha = 0.4),
#      edge.arrow.size = 1)
# legend("topright", legend = c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5"), pch = 19,
#        col = pal, pt.cex = 10, cex = 8, bty = "n", ncol = 1,
#        title = "Node color") 
# dev.off()
# 




# This was a nightmare -------------------
# Types is returning 0, unsure why
# types <- V(rt.g.noloop)$type
# cent.df$types <- as.data.frame(types)
# 
# deg <- degree(rt.g.noloop)
# cent.df <- as.data.frame(deg)
# 
# bet <- betweenness(rt.g.noloop)
# cent.df$bet <- as.data.frame(bet)
#
# closeness results in error as a result of disconnected graphs
# https://stackoverflow.com/questions/55876664/warning-message-when-using-closness-in-igraph
# clos <- closeness(rt.g.noloop)
# 
# eig <- eigen_centrality(rt.g.noloop)$vector
# cent.df$eig <- as.data.frame(eig)
#
# Add Clos and types back once error is determined
# cent.df <- as.data.frame(types, deg, bet, eig)
# cent_df[order(cent_df$type, decreasing = TRUE),]
#---------------
# 
# 
# #ranking users by indegree, this is only showing who the trolls RT
# rank.indegree <- g.centrality %>% select(name, troll, indegree,
#                                          indegree_weighted) %>% arrange(-indegree)
# 
# #ranking users b weigted indegree n users * n retweets (again, according to only who trolls RT)
# rank.indegree.w <- g.centrality %>% select(name, troll, indegree,
#                                            indegree_weighted) %>% arrange(-indegree_weighted)
# 
# 
# 
# # Island of misfit igraphs ---------------------------
# 
# 
# 
# ## subsetting the graph by removing non-trolls
# #selecting nodes to exclude
# exclude <- V(rt.g.noloop)[troll == "non-troll"]
# #excluding the nodes
# g.troll <- delete.vertices(rt.g.noloop, exclude)
# 
# ### vizualizing the graph
# par(bg ="grey10")
# plot.igraph(g.troll,layout= layout.fruchterman.reingold(g.troll),
#            edge.color="grey",
#            edge.curved= .2, vertex.label = NA, vertex.frame.color="#ffffff",
#            vertex.size = 2, edge.size = 0.01, edge.arrow.size = 0.01)
# 
# #decomposing the graph into components and returning the largest one
# comp <- decompose(g.troll, mode = c("weak"), max.comps = 1,
#                  min.vertices = 1)
# 
# ### plotting the graph
# par(bg ="grey10")
# plot.igraph(comp[[1]],layout= layout.fruchterman.reingold(comp[[1]]),
#            edge.color="grey",
#            edge.curved= .2, vertex.label = NA, vertex.frame.color="#ffffff",
#            vertex.size = 4, edge.size = 0.005, edge.arrow.size = 0.01)
# 
# 
#  
# Force-directed layouts try to get a nice-looking graph where edges are similar in length and cross each other as little as possible.
# They simulate the graph as a physical system. Nodes are electrically charged particles that repulse each other when they get too close.
# The edges act as springs that attract connected nodes closer together.
# As a result, nodes are evenly distributed through the chart area, and the layout is intuitive in that nodes which share more connections are closer to each other.
# The disadvantage of these algorithms is that they are rather slow and therefore less often used in graphs larger than ~1000 vertices.

## https://gephi.org/users/quick-start/


#plot(rt.g.noloop, vertex.label.cex = 0.8, vertex.label.color = "black")
#plot(g.troll, vertex.label.cex = 0.8, vertex.label.color = "black")


