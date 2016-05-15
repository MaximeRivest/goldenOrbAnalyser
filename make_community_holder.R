#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
make_community_holder <- function(community) { 
  quantity_of_community <- max(membership(community))
  communities_holder <- list()
  for (communityID in 1:quantity_of_community) { 
    communities_holder[[communityID]] <- names(
      membership(community)[membership(community)==communityID]
    )
  }
  return(communities_holder)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------