#------------------------------------------------------------------------------
# To work on only one WOS file. Merge and import all WOS files in the directory
#------------------------------------------------------------------------------
from_WOS_files_to_WOS_table <- function(files_directory) {
  WOS_file       <- NULL
  folder_content <- list.files(files_directory)
  files_quantity <- length(folder_content)

  for (files in 1:files_quantity) {
    full_file_path <- paste(files_directory,folder_content[files],sep = "")
    if (files == 1) {
      WOS_table <- read.delim2(full_file_path, header = T,
                               fileEncoding = "UTF-8",
                               row.names = NULL,
                               quote = "",
                               stringsAsFactors = FALSE,
                               comment.char = "")
    } else {
      WOS_table_temp <- read.delim2(full_file_path, header = T,
                               fileEncoding = "UTF-8",
                               row.names = NULL,
                               quote = "",
                               stringsAsFactors = FALSE,
                               comment.char = "")
      WOS_table <- rbind(WOS_table, WOS_table_temp)
    }
  }
  # Fix misplaced column names
  column_names <- names(WOS_table)[2:length(names(WOS_table))]
  WOS_table <- WOS_table[, 1:(ncol(WOS_table) - 1)]
  names(WOS_table) <- column_names
  WOS_table <- WOS_table[!duplicated(WOS_table), ]
  return(WOS_table)
}
