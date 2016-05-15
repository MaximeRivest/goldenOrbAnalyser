#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
make_dictionnary <- function(indexed_cr_list, WOS_table){
  cr_list <- strsplit(WOS_table$CR, split="; ")
  nonindexed_cr_vector <- as.factor(unlist(cr_list))
  none_unique <- nonindexed_cr_vector[duplicated(nonindexed_cr_vector) | duplicated(nonindexed_cr_vector, fromLast = T)]
  indexed<- as.integer(as.factor(as.integer(as.factor(none_unique))))+1
  dictionnary <- data.frame("nonindexed" = none_unique, "indexed" =indexed)
  dictionnary <- unique(dictionnary)
  dictionnary <- rbind(list(NA,0), dictionnary)
  dictionnary <- dictionnary[order(dictionnary$indexed),]
  return(dictionnary)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
