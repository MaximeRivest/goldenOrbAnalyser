#------------------------------------------------------------------------------
# Add a column a ; seperated countries per Record
#------------------------------------------------------------------------------
make_clean_cr <- function(WOS_table) {
  library(stringr)
  WOS_table$old_cr <- WOS_table$CR
  crvec <- WOS_table$CR
  cr_list <- strsplit(crvec, split="; ")
  cr_split_list <- sapply(cr_list, function(x) strsplit(x, split="[,]"))
  cr_final_list <- cr_split_list
  univec <- NULL
  for(i in 1:length(cr_split_list)){
    if(length(cr_split_list[[i]]) != 0){
      for(j in 1:length(cr_split_list[[i]])){
        cr_final_list[[i]][[j]] <- ""
        if(!grepl("^[(][[:digit:]]",cr_split_list[[i]][[j]][1]) &&
           length(cr_split_list[[i]][[j]]) != 0 &&
           length(cr_split_list[[i]][[j]]) != 1 &&
           !grepl("^[-]",cr_split_list[[i]][[j]][1])&&
           !grepl("[(:1-9]",cr_split_list[[i]][[j]][1])&&
           !grepl("[(:1-9]",cr_split_list[[i]][[j]][2])) {
          cr_final_list[[i]][[j]][1] <- paste(c(cr_split_list[[i]][[j]][[1]],
                                                cr_split_list[[i]][[j]][[2]])
                                              , collapse = ",")
        }
      }
    }
  }
  for(i in 1:length(cr_split_list)){
    if(length(cr_split_list[[i]]) != 0){
      for(j in 1:length(cr_split_list[[i]])){
        if(length(cr_split_list[[i]][[j]]) != 0) {
          temp_list <- str_extract_all(cr_split_list[[i]][[j]], pattern = "[()]")
          for(k in 1:length(temp_list)){
            if(length(temp_list[[k]]) != 0){
              temp <- strsplit(cr_split_list[[i]][[j]][[k]], split="[()]")
              cr_final_list[[i]][[j]][3] <- temp[[1]][length(temp[[1]])]
              for(m in 1:length(temp[[1]])) {
                if(grepl("^[[:digit:]]{4}", temp[[1]][m])) {
                  cr_final_list[[i]][[j]][2] <- temp[[1]][m]
                }
              }
            }
          }
        }
      }
    }
  }
  for(i in 1:length(cr_final_list)){
    if(length(cr_final_list[[i]]) != 0){
      for(j in 1:length(cr_final_list[[i]])){
        cr_final_list[[i]][[j]] <- paste(cr_final_list[[i]][[j]], collapse = ",")
      }
      cr_final_list[[i]] <- paste(cr_final_list[[i]], collapse = "; ")
    }
  }
  WOS_table$CR <- cr_final_list
  WOS_table$CR <- as.character(WOS_table$CR)
  test_true <- WOS_table$CR=="list()"
  WOS_table$CR[test_true] <- NA
  return(WOS_table)
}
