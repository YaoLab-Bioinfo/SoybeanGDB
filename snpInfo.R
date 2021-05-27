
snpInfo <- function(chr="chr7", start=29616705, end=29629223, accession=NULL, mutType=NULL) {
  if ( exists("fetchSnp") ){
  }else{
    source("fetchSnp.R")
  }
  if (exists("chrInfo")){
  }else{
    chrInfo <- read.table("./data/chrInfo.txt", head=T, as.is=T, sep="\t")
  }
  if (exists("snp.lst")){
  }else{
    snp.lst <- read.table("./data/snp.RData.lst", head=T, as.is=T, sep="\t")
  }
  
  snp.info <- fetchSnp(chr=chr, start=start, end=end, 
                       accession = accession, mutType = mutType, filter = FALSE)
  
  chr.size <- chrInfo$size[chrInfo$chr == chr]
  start <- max(0, start)
  end <- min(end, chr.size)
  
  start <- as.numeric(start)
  end <- as.numeric(end)
  reg.gr <- IRanges::IRanges(start, end)
  snp.lst.chr <- snp.lst[snp.lst$chr==chr, ]
  snp.lst.gr <- IRanges::IRanges(start=snp.lst.chr$start, end=snp.lst.chr$end)
  snp.fls <- snp.lst.chr$file[unique(S4Vectors::queryHits(IRanges::findOverlaps(snp.lst.gr, reg.gr)))]
  snpeff.fls <- gsub("snp.RData", "snpeff.RData", snp.fls)
  snpeff.fls.lst <- lapply(snpeff.fls, function(x){
    load(x)
    return(snpeff)
  })
  snpeff <- do.call(rbind, snpeff.fls.lst)
  snpeff <- snpeff[order(as.numeric(snpeff[, 1])), ]
  
  snpeff.info <- snpeff[snpeff[,1] %in% rownames(snp.info[[1]]), , drop=FALSE]
  
  
  colnames(snpeff.info) <- c("snpID", "reference", "alternative", "effect")
  
  if (!is.null(mutType) && length(mutType)>=1 && length(mutType)!=29) {
    
    snpeff.info <- snpeff.info[snpeff.info[, "effect"] %in% mutType, , drop=FALSE]
    snpeff.info <- snpeff.info[, c("snpID", "reference", "alternative", "effect") , drop=FALSE]
  }
  
  snp.allele <- as.data.frame(snp.info[[2]], stringsAsFactors=FALSE)
  snp.allele$snpID <- rownames(snp.allele)
  rownames(snp.allele) <- NULL
  
  dat.res <- merge(snp.allele, snpeff.info, by="snpID")
  dat.res$reference <- as.character(dat.res$reference)
  dat.res$alternative <- as.character(dat.res$alternative)
  dat.res$snpID <- as.character(dat.res$snpID)
  dat.res$minor[dat.res$major == dat.res$reference] <- dat.res$alternative[dat.res$major == dat.res$reference]
  dat.res$minor[dat.res$major == dat.res$alternative] <- dat.res$reference[dat.res$major == dat.res$alternative]
  return(list(snp.info, dat.res))
}

