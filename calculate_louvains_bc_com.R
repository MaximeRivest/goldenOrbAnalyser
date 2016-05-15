#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
calculate_louvains_bc_com <- function(bc_data_frame) {
  library(igraph)
  non_zero_bcweight <- subset(bc_data_frame, BC_weight != 0)
  non_zero_bcweight$rec1 <- as.character(non_zero_bcweight$rec1)
  non_zero_bcweight$rec2 <- as.character(non_zero_bcweight$rec2)
  source_target_df1 <- data.frame("source" = non_zero_bcweight$rec1, "target" = non_zero_bcweight$rec2)
  graph <- graph_from_edgelist(as.matrix(source_target_df1), directed= F)
  community <- cluster_louvain(graph, weights = non_zero_bcweight[,3])
  return(community)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------