#------------------------------------------------------------------------------
# Add a column a ; seperated universities per Record
#------------------------------------------------------------------------------
make_univ_column <- function(WOS_table) {
  library(stringr)
  C1vec <- WOS_table$C1
  univ_final_vec <- rep(NA, nrow(WOS_table))
  for (i in 1:nrow(WOS_table)) {
    if(length(str_extract_all(C1vec[i], pattern = "\\]\\s[[:alnum:]\\s]{1,}")[[1]]) != 0){
      univ1 <- str_locate_all(C1vec[i], pattern = "\\]\\s[[:alnum:]\\s]{1,}")
      univ1[[1]][,1] <- univ1[[1]][,1] + 2
      univec <- sapply(row(univ1[[1]])[,1], function(x){substr(C1vec[i], univ1[[1]][x,1], univ1[[1]][x,2])})
      univec <- unique(univec)
      univ_final_vec[i] <- paste(univec, collapse = ";")
    } else if (length(str_extract_all(C1vec[i], pattern = "^[[:alnum:]\\s]{1,}")[[1]]) != 0) {
      univ1 <- str_extract_all(C1vec[i], pattern = "^[[:alnum:]\\s]{1,}")
      univ2 <- str_locate_all(C1vec[i], pattern = ";\\s[[:alnum:]\\s]{1,}")
      if(nrow(univ2[[1]]) != 0) { 
        univ2[[1]][,1] <- univ2[[1]][,1] + 2
        univ2 <- sapply(row(univ2[[1]])[,1], function(x){substr(C1vec[i], univ2[[1]][x,1], univ2[[1]][x,2])})
      } else { univ2 <- NULL}
      univec<- unique(c(univ1, univ2))
      univ_final_vec[i] <- paste(univec, collapse = ";")
    }
  }
  WOS_table$Universities <- univ_final_vec
  WOS_table$Universities <- as.character(WOS_table$Universities)
  return(WOS_table)
}
