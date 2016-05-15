#------------------------------------------------------------------------------
# Calculate BC weight
#------------------------------------------------------------------------------
calculate_bc_weight <- function(cocitation_table) {
  w_ij <- cocitation_table$number_of_references_in_commun/sqrt(cocitation_table$number_of_references_source * cocitation_table$number_of_references_target)
  w_ij[w_ij==Inf] <- 0
  bc_data_frame <- data.frame("rec1" = cocitation_table$source, "rec2" = cocitation_table$target, "BC_weight" = w_ij)
  return(bc_data_frame)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------