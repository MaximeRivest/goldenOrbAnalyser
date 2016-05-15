#------------------------------------------------------------------------------
# list of community with df of most said country
#------------------------------------------------------------------------------
count_country_freq_foreach_bccommunity <- function(WOS_table) {
  library(plyr)
  community_quantity <- max(WOS_table$record_community,na.rm =T)
  country_per_X_community        <- as.list(1:(community_quantity+1))
  for (i in 1:community_quantity) {
    sub_WOS_table <- subset(WOS_table, record_community == i)
    sub_WOS_table <- subset(sub_WOS_table, sub_WOS_table$Countries != "")
    sub_WOS_table <- na.omit(sub_WOS_table$Countries)
    country_current_bccom <- toupper(unlist(strsplit(unlist(sub_WOS_table), "[;]")))
    country_current_bccom <- as.factor(country_current_bccom)
    freq_table_country <- count(country_current_bccom)
    ordering_command               <- with(freq_table_country, order(-freq))
    freq_table_country              <- freq_table_country[ordering_command, ]
    names(freq_table_country) <- c("Country", "Frequency")
    country_per_X_community[[i]] <- freq_table_country
  }
  WOS_table <- subset(WOS_table, WOS_table$Countries != "")
  WOS_table <- na.omit(WOS_table$Countries)
  country_total <- toupper(unlist(strsplit(unlist(WOS_table), "[;]")))
  country_total <- as.factor(country_total)
  freq_table_country <- count(country_total)
  ordering_command               <- with(freq_table_country, order(-freq))
  freq_table_country              <- freq_table_country[ordering_command, ]
  names(freq_table_country) <- c("Country", "Frequency")
  country_per_X_community[[community_quantity+1]] <- freq_table_country
  return(country_per_X_community)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
