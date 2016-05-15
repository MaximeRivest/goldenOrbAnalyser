#------------------------------------------------------------------------------
# list of community with df of most Cited references
#------------------------------------------------------------------------------
count_most_cited_authors_journals <- function(cited_reference_df) {
  library(plyr)
  X_per_X_community        <- list("author" = rep(NA, nrow(cited_reference_df)),
                                   "journal" = rep(NA, nrow(cited_reference_df))) 
  for(j in names(X_per_X_community)){
    sub_cited_reference_df <- cited_reference_df
    column_picker <- names(sub_cited_reference_df) == j
    keyword_current_bccom <- tolower(unlist(strsplit(unlist(as.vector(as.character(sub_cited_reference_df[,which(column_picker)]))), "[;][ ]")))
    keyword_current_bccom <- as.factor(keyword_current_bccom)
    freq_table_keyword <- count(keyword_current_bccom)
    ordering_command               <- with(freq_table_keyword, order(-freq))
    freq_table             <- freq_table_keyword[ordering_command, ]
    list_picker <- names(X_per_X_community) == j
    if (j == "author") {
      names(freq_table) <- c("Most Cited Author", "Frequency")
    } else if (j == "journal"){
      names(freq_table) <- c("Most Cited Journal", "Frequency")
    } 
    X_per_X_community[[which(list_picker)]] <- freq_table
  }
  return(X_per_X_community)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------