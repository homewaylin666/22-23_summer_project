#### Module ?: Phylogenetic tree plot
#### load data #####
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(phylobase)
library(ggplot2)
library(ggtree)
library(ggtreeExtra)
library(cowplot)
library(ggnewscale)
library("RColorBrewer")   
############### the phy_data was made in Module 6.
##Let's make different orders have different colours.
##But the orders are too many, I only want 11 largest orders, let's make other order become one category.
freq_table <- table(phy_data$data$Order)
sorted_freq_table <- sort(freq_table, decreasing = TRUE)
top_orders <- names(sorted_freq_table[1:14])
phy_data$data$Order[!(phy_data$data$Order %in% top_orders)] <- "Other"
##Let's plot the central tree
od <- data.frame(sp=row.names(phy_data$data), od= phy_data$data$Order)
row.names(od) <- row.names(phy_data$data)
od_mat <- as.matrix(od)[,c(1,2)]
phy_od <- phylo4d(phy_data$phy, od_mat) 
#### Nothing changed above #####

##Alter base plot to black plot
P.tree.black <- ggtree(phy_od,layout = 'circular', size=0.2)+
  geom_tree(linewidth=0.3)
P.tree.black

#select the first row in node labels that matches the Order name
#### function ####
first_match <- function(df, col_index, keyword){
    for (i in 1:dim(df[col_index])[1]){
        if (df[i,col_index] == keyword) {
            return (df[i,])
            break
        } 
    }
}

#### find the order node ####
od_nodes <- vector()
for(i in 1:length(top_orders)){
  od <- top_orders[i]
  od_sp <- phy_data$phy$tip.label[phy_data$data$Order == od]
  od_node <- getMRCA(phy = phy_data$phy, tip = od_sp)
  od_nodes[i] <- od_node
}
od_nodes_vis <- data.frame(Order = top_orders, node = od_nodes)  

# sub-data preparation
ed <- phy_data$data$ED.median
ed_df <- data.frame(ed=ed)
rownames(ed_df) <- phy_data$phy$tip.label

lpi <- phy_data$data$Avg.lambda
lpi_df <- data.frame(lpi=lpi)
rownames(lpi_df) <- phy_data$phy$tip.label

# create color scheme #
Order_color <- c("#1E2C6C","#0B66B0","#00A9BD","#40B58E","#B3BEDB",
                 "#FBF4E5","#F2E04E","#CBA42C","#BD7918","#500000",
                 "#503300","#004000","#228B22","#47B720")

# plotting new tree #
p0 <- gheatmap(P.tree.black, ed_df, low="#99571C", high="#190000",
               offset=-32.5, width=.3, colnames=FALSE, legend_title = 'ED') 
p0.5 <- p0 + new_scale_fill()
p1 <- gheatmap(p0.5, lpi_df, low="#555B06", high="#70F03F", 
           offset=35, width=.12, colnames=FALSE, legend_title = 'Avg.lambda')
p1.5 <- p1 + new_scale_fill()
p2 <- p1.5 + geom_hilight(data=od_nodes_vis, mapping=aes(node=node, fill=Order), alpha = 0.5) +
  theme(legend.title=element_text(size=16), 
        legend.key.height=unit(.6, 'cm'), legend.key.width=unit(.5, 'cm'), 
        legend.text=element_text(size=14), # legend.background=element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth=.05),
        legend.position= "right") +
  scale_fill_manual(values = Order_color)
p2
# colorRampPalette(c("#8D4300", "white"))(7) 
# I used the code last line to carefully find the proper colours.

# save plot #
png("./ICSP_treePlot.b3.40.4.png", width=40, height=40, units="cm", res=400)
p2
dev.off()
