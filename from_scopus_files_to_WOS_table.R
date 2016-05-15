#------------------------------------------------------------------------------
# To work on only one WOS file. Merge and import all WOS files in the directory
#------------------------------------------------------------------------------
from_scopus_files_to_WOS_table <- function(files_directory) {
  WOS_file       <- NULL
  folder_content <- list.files(files_directory)
  files_quantity <- length(folder_content)

  for (files in 1:files_quantity) {
    full_file_path <- paste(files_directory,folder_content[files],sep = "")
    if (files == 1) {
      WOS_table <- read.delim2(full_file_path, header = T,
                               fileEncoding = "UTF-8",
                               row.names = NULL,
                               quote = '"',
                               stringsAsFactors = FALSE,
                               comment.char = "",
                               sep = ",")
    } else {
      WOS_table_temp <- read.delim2(full_file_path, header = T,
                               fileEncoding = "UTF-8",
                               row.names = NULL,
                               quote = '"',
                               stringsAsFactors = FALSE,
                               comment.char = "",
                               sep = ",")
      WOS_table <- rbind(WOS_table, WOS_table_temp)
    }
  }
  WOS_table <- WOS_table[!duplicated(WOS_table), ]
  names(WOS_table)[names(WOS_table) == "References"] <- "CR"
  names(WOS_table)[names(WOS_table) == "EID"] <- "UT"
  names(WOS_table)[names(WOS_table) == "Affiliations"] <- "C1"
  names(WOS_table)[names(WOS_table) == "Author.Keywords"] <- "DE"
  names(WOS_table)[names(WOS_table) == "Index.Keywords"] <- "ID"
  names(WOS_table)[names(WOS_table) == "Authors"] <- "AU"
  names(WOS_table)[names(WOS_table) == "Abbreviated.Source.Title"] <- "JI"
  names(WOS_table)[names(WOS_table) == "Source.tilte"] <- "SO"
  names(WOS_table)[names(WOS_table) == "Year"] <- "PY"
  #to change , for ; in authors
  WOS_table$AU <- gsub(",", ";", WOS_table$AU)
  #To calculate number of references
  cr_list <- strsplit(WOS_table$CR, split="; ")
  WOS_table$NR <- sapply(cr_list, function(x)length(x))
  return(WOS_table)
}
