#! /usr/bin/env Rscript

Args <- commandArgs(TRUE)
file_in <- as.character(Args[1])
snp.data <- read.table(file_in, head=F, as.is=T, sep="\t")
file_out <- paste(file_in, ".snp.RData", sep="")
snp.data$V1 <- gsub("Chr", "", snp.data$V1)
snp.data$V1 <- as.numeric(snp.data$V1)
row.nm <- paste0(sprintf("%02d", snp.data$V1), sprintf("%09d", snp.data$V2))
rownames(snp.data) <- row.nm
snp.data <- snp.data[, -c(1:4)]
snp.data <- as.matrix(snp.data)
deter <- function(x){
        table_oo <- table(x)
 het_num <- sum(table_oo["H"], na.rm=T)
 N_num <- sum(table_oo["N"], na.rm=T)
        tmp <- table_oo
        tmp <- tmp[!names(tmp) %in% c("N", "H")]
 if (length(tmp)<2) {
  return(NULL)
 } else {
  tmp <- sort(tmp,decreasing=T)
                major <- names(tmp)[1]
                minor <- names(tmp)[2]
  major_num <- sum(table_oo[major],na.rm=T)
  minor_num <- sum(table_oo[minor],na.rm=T)
  if ((minor_num + het_num) < 5) {
   return(NULL)
  } else {
   return(x)
  }
 }
}
snp.data.1 <- apply(snp.data, 1, deter)
snp.data.fil <- do.call(rbind, snp.data.1)
save(snp.data.fil, file=file_out, compress="xz")
