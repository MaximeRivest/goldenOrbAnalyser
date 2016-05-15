#------------------------------------------------------------------------------
# Add a column a ; seperated countries per Record
#------------------------------------------------------------------------------
make_country_column_scopus <- function(WOS_table) {
  library(stringr)
  C1vec <- WOS_table$C1
  aff_list <- strsplit(C1vec, split="; ")
  aff_split_list <- sapply(aff_list, function(x) strsplit(x, split=", "))
  country_final_vec <- rep(NA, nrow(WOS_table))
  univec <- NULL
  for(i in 1:length(aff_split_list)){
    if(length(aff_split_list[[i]]) != 0){
      for(j in 1:length(aff_split_list[[i]])){
        k <- length(aff_split_list[[i]][[j]])
        univec <- c(univec, aff_split_list[[i]][[j]][[k]])
      }
      country_final_vec[i] <- paste(univec, collapse = ";")
      univec <- NULL
    }
  }
  WOS_table$Countries <- country_final_vec
  WOS_table$Countries <- as.character(WOS_table$Countries)
  return(WOS_table)
}
