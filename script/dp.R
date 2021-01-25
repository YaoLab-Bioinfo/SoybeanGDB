#! /usr/bin/env Rscript
Args <- commandArgs(TRUE)
in.fl <- as.character(Args[1])
library(data.table)

a <- fread(in.fl, data.table = F)

a$DP <- gsub(".+DP=|;.+","" , a[, 8])
a$DP <- as.numeric(a$DP)
b <- a[a$DP >= 10000&a$DP <= 60000,]
b <- b[, -length(b[1,])]
out.fl <- paste0(in.fl, ".dp")

write.table(b, out.fl, row.names = F, col.names = T, quote = F, sep = "\t")