indexCR <- function(WOS_table) {
  for(i in ncol(WOS_table)){
    if(is.factor(WOS_table[,i])){
      WOS_table[,i] <- as.character(WOS_table[,i])
    }
  }
  cr_list <- strsplit(WOS_table$CR, split="; ")
  cr_quant_per_record <- sapply(cr_list,function(x) length(x))
  indexed_cr_vector <- as.integer(as.factor(unlist(cr_list)))
  indexed_o_sep_cr_vec <- indexed_cr_vector
  minus_last_cr_quant_per_record <- cr_quant_per_record[1:length(cr_quant_per_record)-1]
  position_counter <- minus_last_cr_quant_per_record
  position_of_appendage <- rep(0,length(position_counter))
  
  for(n in 1:length(position_counter)) {
    position_of_appendage[n] <- sum(position_counter[1:n]) + n - 1
    indexed_o_sep_cr_vec <- append(indexed_o_sep_cr_vec, values = 0, after = position_of_appendage[n])
  }  
  unique_indexed_o_sep_cr_vec <- indexed_o_sep_cr_vec[duplicated(indexed_o_sep_cr_vec) | duplicated(indexed_o_sep_cr_vec, fromLast = T)]
  unique_indexed_1_sep_cr_vec <- as.integer(as.factor(unique_indexed_o_sep_cr_vec))
  separator_position <- which(unique_indexed_1_sep_cr_vec == 1)
  indexed_cr_list <- as.list(1:nrow(WOS_table))
  end_boundary <- separator_position - 1
  end_boundary <- end_boundary[2:length(separator_position)]
  start_boundary <- separator_position + 1
  start_boundary <- start_boundary[1:length(separator_position)-1]
  indexed_cr_list[[nrow(WOS_table)]] <- unique_indexed_1_sep_cr_vec[(separator_position[length(separator_position)] + 1):length(unique_indexed_1_sep_cr_vec)]
  indexed_cr_list[[1]] <- unique_indexed_1_sep_cr_vec[1:separator_position[1] - 1]
  
  for(m in 1:length(start_boundary)){
      indexed_cr_list[[m+1]] <- unique_indexed_1_sep_cr_vec[start_boundary[m]:end_boundary[m]]
  }
  indexed_cr_list <- sapply(indexed_cr_list, function(x) {na.omit(x)})
  return(indexed_cr_list)
}
