### Module 8.3: Calculate sATED updated Ver.

sATED <- function(trend,tree){
  p.tree<-multi2di(tree, random = TRUE) # make sure the tree is binary tree
  uninodes<-p.tree$edge[,2][-which(p.tree$edge[,2] %in% 1:length(p.tree$tip.label))] # extract nodes that are not the leafs (end).
  infonode<-cbind(p.tree$edge,p.tree$edge.length) # make the edge match their branch lengthes
  nodevalues<-numeric(length(uninodes))
  nodeabval<-cbind(uninodes,nodevalues) # and match them with uni-nodes
  tipnums<-data.frame(cbind(p.tree$tip.label,1:length(p.tree$tip.label))) # match all leaf nodes with species in this tree
  sATED<-numeric(length(trend))
  for(j in 1:length(trend)){
    sp<-tipnums[,1][j]
    nodes<-mrca(p.tree)[sp,]
    splength<-infonode[which(infonode[,2]==as.numeric(as.vector(tipnums[which(tipnums[,1]==sp),2]))),3] # TBL of sp
    I<-trend[1,which(colnames(trend) == sp)]
    for(i in 1:length(uninodes)){
      temptree<-extract.clade(p.tree,uninodes[i])
      Re<-exp(sum(log(as.numeric(RLC[1,which(colnames(RLC) %in% temptree$tip.label & colnames(RLC) != sp)])))) #geometric mean without the sp
      nodevalues[i]<-(infonode[which(infonode[,2]==uninodes[i]),3])*Re
    } # this loop is to calculate all the uni-edge's value
    sATED[j]<-((sum(nodeabval[which(nodeabval[,1] %in% nodes),2])) + splength)*(1-I)/I
  }
  names(sATED)<-tipnums[,1]
  sATED<-as.data.frame(unlist(sATED))
  colnames(sATED)="sATED"
  return(sATED)
}

# create a fun. which can get risk that ED2 need.
get_risk <- function(cat) {
  if (cat == "EW") {
    return(0.97)
  } else if (cat == "CR") {
    return(0.97)
  } else if (cat == "EN") {
    return(0.485)
  } else if (cat == "VU") {
    return(0.2425)
  } else if (cat == "NT") {
    return(0.12125)
  } else if (cat == "LC") {
    return(0.60625)
  } else {
    return(NA)
  }
}

# Adding Ex. risk data 8.
RLC <- data.frame(risk = character(length(data$Binomial)))
RLC$cat <- phy_data$data$RL.cat
rownames(RLC) <- rownames(phy_data$data)
RLC$risk <- as.numeric(apply(RLC, 1, function(row) get_risk(row["cat"])))
RLC <- t(RLC)


# try this function using trend
sATED_result <- sATED(trend = trend, tree = phy_data$phy)
sATED_result
