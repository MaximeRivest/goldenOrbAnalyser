WOS_table$PY <- as.factor(WOS_table$PY)
WOS_table$J9 <- as.factor(WOS_table$J9)
WOS_table$VL <- as.factor(WOS_table$VL)
WOS_table$BP <- as.factor(WOS_table$BP)
WOS_table$DI <- as.factor(WOS_table$DI)
wosdt <- as.data.table(WOS_table)
names(cited_reference_df)[c(3,4,5,6,8)] <- c('PY','J9', 'VL', 'BP','DI')
citdt <- as.data.table(cited_reference_df)
setkeyv(wosdt, c('J9','PY', 'VL', 'BP'))
setkeyv(citdt, c('J9','PY', 'VL', 'BP'))
joindf <- citdt[wosdt]
joindf$author <- as.character(joindf$author)
setkey(joindf, author)
najoin <- joindf[na.omit(author),.(UT,record)]

names(indexed_cr_list) <- WOS_table$UT
rec_to_cr <- melt(indexed_cr_list)
names(rec_to_cr) <- c("indexed", "UT")
recdt <- as.data.table(rec_to_cr)
setkey(recdt, indexed)
dicdt <- as.data.table(dictionnary)
setkey(dicdt, indexed)
join_rec_dit <- recdt[dicdt]
names(join_rec_dit) <- c("indexed", "UT", 'CR')
names(najoin) <- c('UT', 'CR')
setkey(join_rec_dit, CR)
setkey(najoin, CR)
othdt <- najoin[join_rec_dit,allow.cartesian=TRUE]
othdf <- as.data.frame(othdt)
othdf$CR <- as.character(othdf$CR)
othdf$UT <- as.character(othdf$UT)
toreplace <- which(!is.na(othdf$UT))
othdf$CR[toreplace] <- othdf$UT[toreplace]
record_to_cr <- data.frame('source' = othdf$i.UT, 'target' = othdf$CR, stringsAsFactors= F)
