#Helena Gray

#import data matrix from csv file

library(igraph)
library(sand)

#CSV files demonstrating alliances between countries 
#by country 1995-2012

#http://www.correlatesofwar.org/data-sets/formal-alliances

# stringsAsFactors=FALSE so no factors
#read in csv file 
Alliances <- read.csv("alliance_v4.1_by_dyad_yearly_edit.csv", header=TRUE, stringsAsFactors=FALSE) # read csv file 
colnames(Alliances)
#remove the 'X'
Alliances=Alliances[,-2]


# returns the the color according to type of alliance and gray if not. 
a2c<- function(x, colors=c("red","black","green","orange")){
  
  allianceColor <- "gray"

  if(x$defense==1){allianceColor <- colors[1]
  } else if(x$neutrality==1){allianceColor<-colors[2]
  } else if(x$nonaggression ==1) {allianceColor<-colors[3]
  } else if(x$entente==1){allianceColor<-colors[4]
  } 
  
  # Return colors
  allianceColor
}

#make an empty graph to fill
make_empty_graph(n = 180, directed = TRUE)

#for loop that will loop through each graph of alliances by year, spanning from 1816 to 2012 or any specified
#time interval
for(i in 1945:2012){
  
  #find which indices of the data frame are in the year of interest
  year.index<-which(Alliances[,"year"]==i)
  
  # stringsAsFactors=FALSE so no factors
  Z <- data.frame(country1= character(), country2= character(), color=character(), stringsAsFactors=FALSE)
  
  
  #iteration making lists of the indices of columns that show an alliance with the first state name 
  #and of rows that show an alliance with the second state name
  for(j in 1:length(year.index)){
    
    Z[nrow(Z)+1,] <- c(country1=Alliances[year.index[j],1], country2=Alliances[year.index[j],2], color=a2c(Alliances[year.index[j],5:8]))
  }
  #make a data frame
   Z.prime<-graph_from_data_frame(Z,directed=FALSE)
  #delete the edges that are gray, indicating no alliance, from graph 
   Z.prime<-delete.edges(Z.prime, which(E(Z.prime)$color == "gray"))
   #indicate layout
   l <- layout.fruchterman.reingold(Z.prime)
   #get largest of cliques
   #Alliance.cliques <- largest.cliques(Z.prime)
   #variable representing largest of cliques
   #clique1 <- Alliance.cliques[[1]]
   #induced subgraph to keep the colors of the alliances
   #g2 <- induced.subgraph(Z.prime,Alliance.cliques[[1]])
   #V(g2)$name <- V(Z.prime)$name[clique1]
  
   #par(mfrow=c(2,1))
   #hist(degree(Z.prime),probability=TRUE,main=c("Degree distribution in year",i),breaks=((-1):50)+.5)
   
   #this is decomposing the main graph to give the vertices a circle shape, and later the most central vertex will be a rectangle shape
   Z.alpha<-decompose.graph(Z.prime)
   
   plot(Z.prime, edge.color=E(Z.prime)$color, layout=l)
   
   title(paste("Year =",i))
   
   #this is the part that assigns the most central vertex the rectangle shape
   V(Z.alpha[[1]])$shape<- "circle"
   V(Z.alpha[[1]])[V(Z.alpha[[1]])$name[which.max(closeness(Z.alpha[[1]]))]]$shape <-"rectangle"
   
   
   legend('left',legend=c("Defense","Neutrality","Non-Aggression", "Entente"), # puts text in the legend
          
          lty=c(1,1),ncol=1, # gives the legend appropriate symbols (lines)
          
          lwd=c(2.5,2.5),col=c("red","black","green","orange"), cex=.45)
   
   
   #Takes largest cluster detected to plot them each in a separate window
   
     dev.new()
     plot(Z.alpha[[1]], edge.color = E(Z.alpha[[1]])$color, layout=l)
     
     
     title(paste("Year =",i))
     
     #this is the part that assigns the most central vertex the rectangle shape
     V(Z.alpha[[1]])$shape<- "circle"
     V(Z.alpha[[1]])[V(Z.alpha[[1]])$name[which.max(closeness(Z.alpha[[1]]))]]$shape <-"rectangle"
     
     
     legend('left',legend=c("Defense","Neutrality","Non-Aggression", "Entente"), # puts text in the legend
            
            lty=c(1,1),ncol=1, # gives the legend appropriate symbols (lines)
            
            lwd=c(2.5,2.5),col=c("red","black","green","orange"), cex=.45)
   
   
   # plot the clique
   
   #plot(g2)
   #legend('top',legend=c("Defense","Neutrality","Non-Aggression", "Entente"), # puts text in the legend
          
          #lty=c(1,1),ncol=4, # gives the legend appropriate symbols (lines)
          
          #lwd=c(2.5,2.5),col=c("red","black","green","orange"), cex=.5)
   
   
   Sys.sleep(5)
}
  



