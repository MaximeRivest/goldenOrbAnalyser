#------------------------------------------------------------------------------
# Add a column a ; seperated universities per Record
#------------------------------------------------------------------------------
make_univ_column_scopus <- function(WOS_table) {
  library(stringr)
  C1vec <- WOS_table$C1
  aff_list <- strsplit(C1vec, split="; ")
  aff_split_list <- sapply(aff_list, function(x) strsplit(x, split=", "))
  univ_final_vec <- rep(NA, nrow(WOS_table))
  univec <- NULL
  for(i in 1:length(aff_split_list)){
    if(length(aff_split_list[[i]]) != 0){
      for(j in 1:length(aff_split_list[[i]])){
        temp_list <- str_extract_all(aff_split_list[[i]][[j]], pattern = "Univ")
        for(k in 1:length(temp_list))
          if(length(temp_list[[k]]) != 0) univec <- c(univec, aff_split_list[[i]][[j]][[k]])
      }
      univ_final_vec[i] <- paste(univec, collapse = ";")
      univec <- NULL
    }
  }
  for(i in which(univ_final_vec == "")){
    if(length(aff_split_list[[i]]) != 0){
      for(j in 1:length(aff_split_list[[i]])){
        temp_list <- str_extract_all(aff_split_list[[i]][[j]], pattern = "Acad")
        for(k in 1:length(temp_list))
          if(length(temp_list[[k]]) != 0) univec <- c(univec, aff_split_list[[i]][[j]][[k]])
      }
      univ_final_vec[i] <- paste(univec, collapse = ";")
      univec <- NULL
    }
  }
  for(i in which(univ_final_vec == "")){
    if(length(aff_split_list[[i]]) != 0){
      for(j in 1:length(aff_split_list[[i]])){
        temp_list <- str_extract_all(aff_split_list[[i]][[j]], pattern = "Inst")
        for(k in 1:length(temp_list))
          if(length(temp_list[[k]]) != 0) univec <- c(univec, aff_split_list[[i]][[j]][[k]])
      }
      univ_final_vec[i] <- paste(univec, collapse = ";")
      univec <- NULL
    }
  }
  WOS_table$Universities <- univ_final_vec
  WOS_table$Universities <- as.character(WOS_table$Universities)
  return(WOS_table)
}
