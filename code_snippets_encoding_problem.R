codepages <- setNames(iconvlist(), iconvlist())
codepages
x <- lapply(codepages, function(enc) try(read.table("savedrecs (22).txt",
                                                    fileEncoding=enc,
                                                    nrows=100, header=TRUE, sep="\t")))
unique(do.call(rbind, sapply(x, dim)))
maybe_ok <- sapply(x, function(x) isTRUE(all.equal(dim(x), c(124,1))))
codepages[maybe_ok]
x[maybe_ok]
x


writeLines(iconv(readLines("WOS_files/savedrecs (36).txt"), from = "UTF-8-BOM",
                 to = "UTF-8"),
           file("WOS_files/savedrecs (36)2.txt",
                encoding="UTF-8"))
