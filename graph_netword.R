#------------------------------------------------------------------------------
# Cocitation graph, colored per community, plus a graph per community
# node size per citation, edge size per bc_weight
#------------------------------------------------------------------------------
library(igraph)
non_zero_bcweight <- subset(bc_data_frame, BC_weight != 0)
non_zero_bcweight$rec1 <- as.character(non_zero_bcweight$rec1)
non_zero_bcweight$rec2 <- as.character(non_zero_bcweight$rec2)
source_target_df1 <- data.frame("source" = non_zero_bcweight$rec1, "target" = non_zero_bcweight$rec2)
graph_mat <- as.matrix(source_target_df1)
graph <- graph_from_edgelist(graph_mat, directed= F)
#------------------------------------------------------------------------------
# Record to citation graph for Historiogram y axis year, filter per citation,
# sample random on x divided per number of ref per yer for max width
#------------------------------------------------------------------------------
graph_mat <- matrix(c(record_to_cr$source, record_to_cr$target), ncol = 2)
graph_rec_to_cr <- graph_from_edgelist(el=na.omit(graph_mat), directed= T)
plot.igraph(graph_rec_to_cr....)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


print(g, e=TRUE, v=TRUE)

library(tcltk2)
tkplot(g)
library(rgl)
rglplot(g)


igraph(bc_data_frame)
