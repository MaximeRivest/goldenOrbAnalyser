#------------------------------------------------------------------------------
#
# Copyright (c) 2016 Maxime Rivest
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
#------------------------------------------------------------------------------
# You will need the following package.Please make sure they are installed on
# your system. Otherwise this function will not work.
#
# install.packages("shiny")
# install.packages("plyr")
# install.packages("Hmisc")
# install.packages("data.table")
# install.packages("bigmemory")
# install.packages("devtools")
# install.packages("bigalgebra") ### this seems to work only for linux
# devtools::install_github("kaneplusplus/bigalgebra") ### this works for windows, but you need Rtools (somethimes it downloads automatically)
# install.packages("igraph")
# install.packages("DT")
# install.packages("ggplot2")
# install.packages("stringr")
# install.packages("reshape")
#
#------------------------------------------------------------------------------
source("from_WOS_files_to_WOS_table.R")
source("from_scopus_files_to_WOS_table.R")
source("make_clean_cr.R")
source("indexCR.R")
source("make_cocitation_table_v4_mat.R")
source("calculate_bc_weight.R")
source("calculate_louvains_bc_com.R")
source("make_community_holder.R")
source("WOS_table_bccommunity_assigner.R")
source("make_university_column.R")
source("make_university_column_scopus.R")
source("calculate_relative_frequency.R")
source("make_country_column.R")
source("make_country_column_scopus.R")
source("count_country_freq_foreach_bccommunity.R")
source("count_university_freq_foreach_bccommunity.R")
source("count_keyword_freq_foreach_bccommunity.R")
source("count_suit_of_freq_foreach_bccommunity.R")
source("make_dictionnary.R")
source("make_cited_reference_df.R")
source("count_most_cited_authors_journals.R")
source("commun_word.R")
source("making_visual.R")

#Scopus workflow
#WOS_table <- from_scopus_files_to_WOS_table("scopus_files/")
#WOS_table <- make_clean_cr(WOS_table)
#indexed_cr_list <- indexCR(WOS_table)
#cocitation_table <- make_cocitation_table_new(indexed_cr_list, WOS_table)

#WOS workflow
WOS_table <- from_WOS_files_to_WOS_table("WOS_files/")
indexed_cr_list <- indexCR(WOS_table)
cocitation_table <- make_cocitation_table_new(indexed_cr_list, WOS_table)

#write.table(cocitation_table, "cocitation_table")
#------------------------------------------------------------------------------
# BC community
#------------------------------------------------------------------------------
bc_data_frame <- calculate_bc_weight(cocitation_table)
community <- calculate_louvains_bc_com(bc_data_frame)
communities_holder <- make_community_holder(community)
#------------------------------------------------------------------------------
# Add column to WOS_table
#------------------------------------------------------------------------------
WOS_table <- WOS_table_bccommunity_assigner(communities_holder, WOS_table)
#WOS_table <- make_univ_column_scopus(WOS_table) #Only for scopus files
WOS_table <- make_univ_column(WOS_table)
#WOS_table <- make_country_column_scopus(WOS_table) #Only for scopus files
WOS_table <- make_country_column(WOS_table)
#------------------------------------------------------------------------------
# Make interesting tables
#------------------------------------------------------------------------------
country_freq_df <- count_country_freq_foreach_bccommunity(WOS_table)
country_freq_df <- calculate_relative_frequency(country_freq_df)
keyword_freq_df <- count_keyword_freq_foreach_bccommunity(WOS_table)
keyword_freq_df <- calculate_relative_frequency(keyword_freq_df)
university_freq_df <- count_university_freq_foreach_bccommunity(WOS_table)
university_freq_df <- calculate_relative_frequency(university_freq_df)
suit_freq_tables <- count_suit_of_freq_foreach_bccommunity(WOS_table)
for(i in 1:length(suit_freq_tables)) {
  suit_freq_tables[[i]] <- calculate_relative_frequency(suit_freq_tables[[i]])
}
#------------------------------------------------------------------------------
# Make Histograph
#------------------------------------------------------------------------------
dictionnary <- make_dictionnary(indexed_cr_list, WOS_table)
cited_reference_df <- make_cited_reference_df(dictionnary)
most_cited_df <- count_most_cited_authors_journals(cited_reference_df)
#------------------------------------------------------------------------------
# Bringing up the shiny app
#------------------------------------------------------------------------------
making_visual(WOS_table, index_cr_list,community, keyword_freq_df, suit_freq_tables,
              most_cited_df, university_freq_df, country_freq_df)
