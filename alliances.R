#Helena Gray

library(igraph)

#read in data from csv file

a <- read.csv("alliance_v4.1_by_dyad_yearly.csv",
              stringsAsFactors=FALSE)


#function that assigns a color attribute to type of alliance for each dyad pair, gray for no alliances
a2c <- function(x,colors=c("red","pink","green","blue")){
   allianceColor <-rep("gray",nrow(x))
	allianceColor[x$entente==1] <- colors[4]
	allianceColor[x$nonaggression==1] <- colors[3]
	allianceColor[x$neutrality==1] <- colors[2]
	allianceColor[x$defence==1] <-  colors[1]
	allianceColor
}

#function that creates a data frame including the names of the countries in a dyad, a color assigned to the type of alliance, the
#start year of the dyad, the end year of the dyad alliance, and the alliance columns with a 1 or a 0 indicating if there was an alliance and what type
#it also assigns a value of 1 for weight, which will be used later to indicate the strength or the number of years of the dyad's alliance
#it then creates graph from this data frame
getGraph <- function(){
   a <- read.csv("alliance_v4.1_by_dyad_yearly.csv",
	              stringsAsFactors=FALSE)
   data <- data.frame(from=a$state_name1,to=a$state_name2, color=a2c(a),
							 start_year=a$dyad_st_year,
							 end_year=a$dyad_end_year,
							 a[,14:17])
   data<- cbind(data,weight=rep(1,nrow(data)))
   data<-graph_from_data_frame(data,directed=FALSE)
}

#this function deletes any edges or any alliances that do not exist within the specified time interval

graph_by_year_range <- function(g,start_year=1939,end_year=1945,
                                inside=TRUE)
{
  s <- edge_attr(g,'start_year')
  e <-edge_attr(g,'end_year')
  ys2 <- start_year:end_year
  a <- (s>=start_year & e<=end_year)
  if(inside){
    ind <- which(!a)
  } else {
    a1 <- (s<=start_year) & (e>=start_year)
    a2 <- (s<=end_year) & (e>=end_year)
    ind <- which(!(a|a1|a2))
  }
  
  delete_edges(g,E(g)[ind])
}


#here's where we use the functions
#creating data frame and graph
Alliances<-getGraph()

#creating the graph with only alliances that exist with in the specified time frame
Alliances<-graph_by_year_range(Alliances, inside=FALSE)

#this part takes multiple edges representing the years a dyad has been allied and condenses into one edge with a weight
#equaling the number of years the alliance has existed. This code also takes the first alliance detected and assigns 
#the dyad's color according to it's type of alliance, so this does not account for the overall strength and nature of the alliance
#but rather the first detected type
Alliances<-simplify(Alliances,edge.attr.comb=list(weight='sum','first'))

#this part of the code deletes all isolates in the graph
Alliances<-delete.vertices(Alliances,which(degree(Alliances)<1))

#this part deletes all gray edges because they represent a lack of alliance
Alliances<-delete.edges(Alliances, which(E(Alliances)$color == "gray"))


#The edges, when graphed as weighted, are far too large to be of any service to the graph so I changed the weights
#by applying log10 to them
for (i in 1:length(E(Alliances)$weight)){
  
  E(Alliances)$weight[i]<-log10(E(Alliances)$weight[i])
}

#this was the best layout I could use for this in terms of visibility of the vertices
l <- layout.fruchterman.reingold(Alliances)


#plots out all countries that had existing or new alliances within the given time frame and the alliances
#between them according to type
dev.new()
plot(Alliances,vertex.size=2, edge.width=E(Alliances)$weight, layout=l, vertex.label.color= "red")
legend('top',legend=c("Defense","Neutrality","Non-Aggression", "Entente"), # puts text in the legend
       
       lty=c(1,1), ncol=3, # gives the legend appropriate symbols (lines)
       
       lwd=c(2.5,2.5),col=c("red","pink","green","blue"), cex=.5)

# find all the largest clique (returns a list of vector of vertiex ids)
Alliance.cliques <- largest.cliques(Alliances)

# let's just take the first of the largest cliques
# (in this case there's just one clique)
clique1 <- Alliance.cliques[[1]]

# subset the original graph by passing the clique vertices
g2 <- induced.subgraph(Alliances,Alliance.cliques[[1]])
V(g2)$name <- V(Alliances)$name[clique1]

# plot the clique
dev.new()
plot(g2)
legend('top',legend=c("Defense","Neutrality","Non-Aggression", "Entente"), # puts text in the legend
       
       lty=c(1,1),ncol=4, # gives the legend appropriate symbols (lines)
       
       lwd=c(2.5,2.5),col=c("red","pink","green","blue"), cex=.5)

#hist(degree(Alliances),probability=TRUE,main=c("Degree distribution in World War II"),breaks=((-1):50)+.5)



