#------------------------------------------------------------------------------
# Find descriminant keyword to describe each communities
#------------------------------------------------------------------------------
find_descriminant_keyword <- function(keyword_freq_df) {
  commun_word <- sapply(1:length(keyword_freq_df), function(X) keyword_freq_df[[X]][1:15,1])
  keywordvec <- as.vector(unlist(commun_word, use.names = F))
  unique_keyw <- keywordvec[!(duplicated(keywordvec) | duplicated(keywordvec, fromLast = T))]
  logical_mat <- matrix(0, dim(commun_word)[1], dim(commun_word)[2])
  for(i in 1:length(unique_keyw)){
    logical_mat <- logical_mat + matrix(as.integer(unique_keyw[i] == commun_word),
                                        dim(commun_word)[1], dim(commun_word)[2])
  }
  descri_keyword <- rep(NA, ncol(logical_mat)-1)
  for(currentcol in 1:((ncol(logical_mat)-1))){
    descri_keyword[currentcol] <- commun_word[min(which(logical_mat[,currentcol]==1)),currentcol]
  }

  keywordvec <- as.vector(unlist(commun_word, use.names = F))
  keywordvec_doub <- keywordvec[duplicated(keywordvec)]
  unique_keyw <- keywordvec_doub[!(duplicated(keywordvec_doub) | duplicated(keywordvec_doub, fromLast = T))]
  logical_mat <- matrix(0, dim(commun_word)[1], dim(commun_word)[2])
  for(i in 1:length(unique_keyw)){
    logical_mat <- logical_mat + matrix(as.integer(unique_keyw[i] == commun_word),
                                        dim(commun_word)[1], dim(commun_word)[2])
  }
  descri_keyword_doub <- rep(NA, ncol(logical_mat)-1)
  for(currentcol in 1:((ncol(logical_mat)-1))){
    descri_keyword_doub[currentcol] <- commun_word[min(which(logical_mat[,currentcol]==1)),currentcol]
  }
  descriminant_key_vector <- sapply(1:(ncol(logical_mat)-1), function(x){paste(descri_keyword[x],descri_keyword_doub[x], sep = " & ")} )
  descriminant_key_vector <- append(descriminant_key_vector, "All records")
  return(descriminant_key_vector)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
