
anaRegw <- function(x=NULL) {
  x <- trimws(x)
  if (grepl("chr", x)) {
    myChr <- gsub(":.+", "", x)
    myPos <- as.numeric(gsub("\\s","", strsplit(gsub(".+:", "", x),"-")[[1]]))
  } else {
    myChr <- gene_info_s$chr[gene_info_s$id==x]
    myPos <- c(gene_info_s$start[gene_info_s$id==x], gene_info_s$end[gene_info_s$id==x])
  }
  
  chr.size <- c(59600650, 54790513, 49119424, 55533332, 44449196, 53606008, 47205696, 50288313, 51135784, 55391814, 
                42152160, 43682070, 46106413, 52776670, 55366112, 40198974, 44073542, 60749638, 53875614, 51026854)
  names(chr.size) <- paste0("chr", 1:20)
  if ( grepl("\\s+", x) ){
    myPos[1] <- NA
  }else{
    myPos[1] <- max(1, myPos[1])
  }
  myPos[2] <- min(myPos[2], chr.size[myChr])
  return(list(chr=myChr, start=myPos[1], end=myPos[2]))
}
