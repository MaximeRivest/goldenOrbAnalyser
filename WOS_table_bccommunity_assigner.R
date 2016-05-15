#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
WOS_table_bccommunity_assigner <- function(communities_holder, global_citation_table) {
  quantity_of_community <- length(communities_holder)
  WOS_table$record_community <- rep(NA, nrow(WOS_table))
  for(communityID in 1:quantity_of_community) {
    length_of_current_community <- length(communities_holder[[communityID]])
    for(current_node in 1:length_of_current_community) {
      holder <- which(
        WOS_table$UT %in% communities_holder[[communityID]][current_node])
      WOS_table$record_community[holder] <- communityID
    }
  }
  return(WOS_table)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------