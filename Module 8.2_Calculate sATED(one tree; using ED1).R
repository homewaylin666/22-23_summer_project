### Module 8.2: Calculate sATED

sATED <- function(trend,tree){
  p.tree<-multi2di(tree, random = TRUE) # make sure the tree is binary tree
  uninodes<-p.tree$edge[,2][-which(p.tree$edge[,2] %in% 1:length(p.tree$tip.label))] # extract nodes that are not the leafs (end).
  infonode<-cbind(p.tree$edge,p.tree$edge.length) # make the edge match their branch lengthes
  nodevalues<-numeric(length(uninodes))
  for(i in 1:length(uninodes)){
    temptree<-extract.clade(p.tree,uninodes[i])
    Se<-length(temptree$tip.label)
    nodevalues[i]<-(infonode[which(infonode[,2]==uninodes[i]),3])/Se
  } # this loop is to calculate all the uni-edge's value
  nodeabval<-cbind(uninodes,nodevalues) # and match them with uni-nodes
  tipnums<-data.frame(cbind(p.tree$tip.label,1:length(p.tree$tip.label))) # match all leaf nodes with species in this tree
  sATED<-numeric(length(trend))
  for(j in 1:length(trend)){
    sp<-tipnums[,1][j]
    nodes<-mrca(p.tree)[sp,]
    splength<-infonode[which(infonode[,2]==as.numeric(as.vector(tipnums[which(tipnums[,1]==sp),2]))),3]
    I<-trend[1,which(colnames(trend) == sp)]
    sATED[j]<-((sum(nodeabval[which(nodeabval[,1] %in% nodes),2])) + splength)*(1-I)/I
  }
  names(sATED)<-tipnums[,1]
  sATED<-as.data.frame(unlist(sATED))
  colnames(sATED)="sATED"
  return(sATED)
}

# try this function using data from Module 8.
sATED_result <- sATED(trend = trend, tree = phy_data$phy)
sATED_result
