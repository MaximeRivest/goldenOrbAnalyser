make_cocitation_table_new <- function(indexed_cr_list, WOS_table){
  library(data.table)
  library(bigmemory)
  library(bigalgebra)
  options(bigmemory.typecast.warning=FALSE)
  for(i in ncol(WOS_table)){
    if(is.factor(WOS_table[,i])){
      WOS_table[,i] <- as.character(WOS_table[,i])
    }
  }
  max_expected_combinations <- choose(nrow(WOS_table), 2)
  number_commun_references <- big.matrix(max_expected_combinations, 1, type = "integer", init = 0, shared = FALSE)
  cr_matrix <- big.matrix(nrow(WOS_table), length(unique(unlist(indexed_cr_list)))+1, type = "integer", init = 0, shared = FALSE)
  counter <- 0
  for(x in 1:length(indexed_cr_list)) {
    counter <- counter + 1
    cr_matrix[counter,indexed_cr_list[[x]]] <- cr_matrix[counter,indexed_cr_list[[x]]] + 1
  }
  counter_combinations <- 2:nrow(cr_matrix)
  position_counter <- c((nrow(WOS_table)-1):1)
  position_counter_2 <- rep(0,length(position_counter)-1)
  position_counter_s <- rep(0,length(position_counter)-1)
  for(p in 1:length(position_counter)) {
    position_counter_2[p] <- sum(position_counter[1:p])
  }
  position_counter_s <- position_counter_2 + 1
  position_counter_s <- append(position_counter_s, 1, 0)
  position_counter_s <- position_counter_s[1:(length(position_counter_s)-1)]
  cat("\r", "                                                      ")
  for(rec in 1:length(position_counter_2)) {
    number_commun_references[position_counter_s[rec]:position_counter_2[rec],] <- cr_matrix[counter_combinations[rec]:nrow(cr_matrix),] %*% cr_matrix[rec,]
    cat("\r", rec, " of ", length(position_counter_2))
  }
  rec_vector <- WOS_table$UT
  source_target_mat <- t(combn(rec_vector, 2))
  nref <- as.matrix(number_commun_references)
  cocitation_table <- data.table("source" = source_target_mat[,1],
                                 "target" = source_target_mat[,2],
                                 "number_of_references_source" = rep(0,max_expected_combinations),
                                 "number_of_references_target" = rep(0,max_expected_combinations),
                                 "number_of_references_in_commun" = nref)
  WOS_table <- as.data.table(WOS_table)
  setkey(WOS_table,UT)
  setkey(cocitation_table,target)
  for(rec in rec_vector) {
    cocitation_table[.(rec), number_of_references_target := WOS_table[rec, NR]]
  }
  setkey(cocitation_table,source)
  for(rec in rec_vector) {
    cocitation_table[.(rec), number_of_references_source := WOS_table[rec, NR]]
  }
  cocitation_table <- cocitation_table[number_of_references_in_commun != 0]
  WOS_table <- as.data.frame(WOS_table)
  cocitation_table <- as.data.frame(cocitation_table)
  return(cocitation_table)
}




