#------------------------------------------------------------------------------
# list of community with df of most said title/abstract word
#------------------------------------------------------------------------------
count_suit_of_freq_foreach_bccommunity <- function(WOS_table) {
  library(plyr)
  library(tm)
  library(qdap)
  community_quantity <- max(WOS_table$record_community,na.rm =T)
  TI_or_AB_per_X_community        <- list("TI" = as.list(1:community_quantity),
                                   "AB" = as.list(1:community_quantity)) 
  
  for(j in names(TI_or_AB_per_X_community)){
    for (i in 1:community_quantity) {   
      sub_WOS_table <- subset(WOS_table, record_community == i)
      column_picker <- names(sub_WOS_table) == j
      keyword_current_bccom <- tolower(unlist(strsplit(unlist(as.vector(as.character(sub_WOS_table[,which(column_picker)]))), "[ ]")))
      keyword_current_bccom <- removePunctuation(keyword_current_bccom, preserve_intra_word_dashes = FALSE)
      keyword_curent_bccom <- qdap::stopwords(keyword_current_bccom ,tm::stopwords("english"))
      keyword_current_bccom <- as.factor(keyword_current_bccom)
      freq_table_keyword <- count(keyword_current_bccom)
      ordering_command               <- with(freq_table_keyword, order(-freq))
      freq_table             <- freq_table_keyword[ordering_command, ]
      list_picker <- names(TI_or_AB_per_X_community) == j
      TI_or_AB_per_X_community[[which(list_picker)]][[i]] <- freq_table
    }
  }
  return(TI_or_AB_per_X_community)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

library(XML)

library(wordcloud)
library(RColorBrewer)

