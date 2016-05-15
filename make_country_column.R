#------------------------------------------------------------------------------
# Add a column a ; seperated countries per Record
#------------------------------------------------------------------------------
make_country_column <- function(WOS_table) {
  library(stringr)
  C1vec <- WOS_table$C1
  country_final_vec <- rep(NA, nrow(WOS_table))
  for (i in 1:nrow(WOS_table)) {
    if(length(str_extract_all(C1vec[i], pattern = "[:alnum:]{1,};\\s\\[")[[1]]) != 0){
      univ1 <- str_locate_all(C1vec[i], pattern = "[:alnum:]{1,};\\s\\[")
      univ1[[1]][,2] <- univ1[[1]][,2] - 3
      univ2 <- str_extract(C1vec[i], pattern = "\\w*$")
      univec <- sapply(row(univ1[[1]])[,1], function(x){substr(C1vec[i], univ1[[1]][x,1], univ1[[1]][x,2])})
      univec <- unique(c(univec, univ2))
      if(!all(is.na(str_extract(univec, pattern = "[:digit:]")))) {
        country_final_vec[i] <- "USA"
      } else {
        country_final_vec[i] <- paste(univec, collapse = ";")
      }
    } else if (length(str_extract_all(C1vec[i], pattern = "\\w*$")[[1]]) != 0) {
      univ1 <- str_extract(C1vec[i], pattern = "\\w*$")
      univ2 <- str_locate_all(C1vec[i], pattern = "[:alpha:]{1,};")
      if(nrow(univ2[[1]]) != 0 & length(str_extract(C1vec[i], pattern = "\\]")) == 0 ) { 
        univ2[[1]][,2] <- univ2[[1]][,2] - 1
        univ2 <- sapply(row(univ2[[1]])[,1], function(x){substr(C1vec[i], univ2[[1]][x,1], univ2[[1]][x,2])})
      } else { univ2 <- NULL}
      univec<- unique(c(univ1, univ2))
      if(!any(is.na(str_extract(univec, pattern = "[:digit:]")))) {
        country_final_vec[i] <- "USA"
      } else {
        country_final_vec[i] <- paste(univec, collapse = ";")
      }
    }
  }
  
  WOS_table$Countries <- country_final_vec
  WOS_table$Countries <- as.character(WOS_table$Countries)
  return(WOS_table)
}
