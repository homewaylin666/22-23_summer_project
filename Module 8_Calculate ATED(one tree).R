### Module 8: Calculate ATED
library(tidyr)
library(ape)
library(picante)

# create a function to calculate ATED, the code is adopted from https://rfunctions.blogspot.com/2013/09/functions-for-phylogenetic-diversity.html
ATED <- function(trend,tree){
  p.tree<-multi2di(tree, random = TRUE) # make sure the tree is binary tree
  uninodes<-p.tree$edge[,2][-which(p.tree$edge[,2] %in% 1:length(p.tree$tip.label))] # extract nodes that are not the leafs (end).
  infonode<-cbind(p.tree$edge,p.tree$edge.length) # make the edge match their branch lengths
  nodevalues<-numeric(length(uninodes))
  for(i in 1:length(uninodes)){
    temptree<-extract.clade(p.tree,uninodes[i])
    Ie<-exp(mean(log(trend[1,which(colnames(trend) %in% temptree$tip.label)]))) #geometric mean
    nodevalues[i]<-(infonode[which(infonode[,2]==uninodes[i]),3])*(1-Ie)/(Ie)
    } # this loop is to calculate all the uni-edge's value
  nodeabval<-cbind(uninodes,nodevalues) # and match them with uni-nodes
  tipnums<-data.frame(cbind(p.tree$tip.label,1:length(p.tree$tip.label))) # match all leaf nodes with species in this tree
  ATED<-numeric(length(trend))
  for(j in 1:length(trend)){
    sp<-tipnums[,1][j]
    nodes<-mrca(p.tree)[sp,]
    splength<-infonode[which(infonode[,2]==as.numeric(as.vector(tipnums[which(tipnums[,1]==sp),2]))),3]
    Ie<-trend[which(colnames(trend)==sp)]
    splength<-splength*(1-Ie)/Ie
    ATED[j]<-(sum(nodeabval[which(nodeabval[,1] %in% nodes),2])) + splength
    }
  names(ATED)<-tipnums[,1]
  ATED<-as.data.frame(unlist(ATED))
  colnames(ATED)="ATED"
  return(ATED)
}
  
# Create a phy_data for a full data genus
# Create a tree data that the function above need (my_data and one_tree is from Module 6)
data <- separate(my_data, col = "Binomial", into = c("Genus", "Species"), sep = "_", remove = FALSE)
data <- data[data$Genus == "Panthera", c("Binomial","Sum.lambda","RL.cat")]
tree <- drop.tip(one_tree, setdiff(one_tree$tip.label, data$Binomial)) 
phy_data <- comparative.data(phy = tree, data = data, names.col = "Binomial", 
                             vcv = T, na.omit = T)

# Preview the tree of the taxa group if you want
plot.phylo(phy_data$phy)

# Create a trend data to suit the function
trend <- data.frame(lambda = character(length(data$Binomial)))
trend$lambda <- 10^phy_data$data$Sum.lambda # Turning LPI lambda into LPI
rownames(trend) <- rownames(phy_data$data)
trend <- t(trend)

# use the function
ATED_result <- ATED(trend = trend, tree = phy_data$phy)
ATED_result 
