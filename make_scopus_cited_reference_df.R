#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
make_scopus_cited_reference_df <- function(dictionnary) {
  cr_vec<- as.character(dictionnary$nonindexed)
  row_quantity <- length(cr_vec)
  splitted_cited_reference_df <- data.frame(record  = dictionnary$nonindexed,
                                            author  = rep(NA, row_quantity),
                                            year    = rep(NA, row_quantity),
                                            journal = rep(NA, row_quantity),
                                            volume  = rep(NA, row_quantity),
                                            page    = rep(NA, row_quantity),
                                            doi     = rep(NA, row_quantity),
                                            simple_doi = rep(NA, row_quantity))
  list_sep_cr <- strsplit(cr_vec, split = "[,][[:blank:]]")

  for (current_row in 1:row_quantity) {

    author  <- list_sep_cr[[current_row]][1]
    year    <- NA
    journal <- NA
    volume  <- NA
    page    <- NA
    doi     <- NA
    counter <- 2
    while (counter < (length(list_sep_cr[[current_row]])+1)) {
      if (grepl("^[[:digit:]]{4}",list_sep_cr[[current_row]][counter])) {
        year <- list_sep_cr[[current_row]][counter]
        counter <- counter+1
      } else if (!grepl("^[V][[:digit:]]",list_sep_cr[[current_row]][counter]) && !grepl("^[P][[:digit:]]",list_sep_cr[[current_row]][counter]) && !grepl("^DOI",list_sep_cr[[current_row]][counter]) && !grepl("^10.",list_sep_cr[[current_row]][counter])) {
        journal <- list_sep_cr[[current_row]][counter]
        counter <- counter+1
      } else if (grepl("^[V][[:digit:]]",list_sep_cr[[current_row]][counter])) {
        volume <- substr(list_sep_cr[[current_row]][counter],2, nchar(list_sep_cr[[current_row]][counter]))
        counter <- counter+1
      } else if (grepl("^[P][[:digit:]]",list_sep_cr[[current_row]][counter])) {
        page <- l<- substr(list_sep_cr[[current_row]][counter],2, nchar(list_sep_cr[[current_row]][counter]))
        counter <- counter+1
      } else if(grepl("^DOI",list_sep_cr[[current_row]][counter])) {
        doi <- list_sep_cr[[current_row]][counter]
        counter <- counter+1
      } else {
        break
      }
    }
    startpos <- grepRaw(x=doi, pattern="10.*")
    if ( length(startpos) !=0 ){
    simple_doi <- substr(doi, startpos,nchar(doi))
    doi <- substr(doi, startpos,nchar(doi))
    doi <- paste('<a href="https://doi.org/',doi,'">',doi,'</a>', sep = "")
    } else simple_doi <- NA
    splitted_cited_reference_df[current_row,2:8] <- list(
      author,
      year,
      journal,
      volume,
      page,
      doi,
      simple_doi)
  }
  splitted_cited_reference_df$record <- as.factor(splitted_cited_reference_df$record)
  splitted_cited_reference_df$author <- as.factor(splitted_cited_reference_df$author)
  splitted_cited_reference_df$year <- as.factor(splitted_cited_reference_df$year)
  splitted_cited_reference_df$journal <- as.factor(splitted_cited_reference_df$journal)
  splitted_cited_reference_df$volume <- as.factor(splitted_cited_reference_df$volume)
  splitted_cited_reference_df$page <- as.factor(splitted_cited_reference_df$page)
  splitted_cited_reference_df$doi <- as.factor(splitted_cited_reference_df$doi)
  splitted_cited_reference_df$simple_doi <- as.factor(splitted_cited_reference_df$simple_doi)
  return(splitted_cited_reference_df)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
