
# A function to perform phylogenetic analysis using SNP data in a specified genomic region.
# Change to the directory of MaizeSNPDB using the setwd function of R.
# Usage: type the next three lines in R Console without the leading #
# source("Global.R")
# phy.plot <- phylo(chr="chr2", start=17603220, end=17604802, accession=NULL, mutType=NULL, snpSites = NULL)
# print(phy.plot)
# Then the NJ tree would be displayed in a plotting device.
# For more info, please check the Phylogenetic menu of the MaizeSNPDB database.

phylo <- function(chr="chr9", start=37800, end=41400, accession=NULL, mutType=NULL, snpSites = NULL) {
  #library(foreach)
  if (exists("snp.lst")){
  }else{
    snp.lst <- read.table("./data/snp.RData.lst", head=T, as.is=T, sep="\t")
  }
  
  soya.tree <- read.table("./data/soya.tree.txt", head=T, as.is=T, sep="\t", row.names = 1)
  start <- as.numeric(start)
  end <- as.numeric(end)
  reg.gr <- IRanges::IRanges(start, end)
  snp.lst.chr <- snp.lst[snp.lst$chr==chr, ]
  snp.lst.gr <- IRanges::IRanges(start=snp.lst.chr$start, end=snp.lst.chr$end)
  snp.fls <- snp.lst.chr$file[unique(S4Vectors::queryHits(GenomicRanges::findOverlaps(snp.lst.gr, reg.gr)))]
  
  snp.data.lst <- lapply(snp.fls, function(x){
    load(x)
    return(snp.data.inter.Matrix)
  })
  snp.data <- do.call(rbind, snp.data.lst)
  snp.data <- snp.data[order(as.numeric(rownames(snp.data))), ]
  colnames(snp.data) <- soya.info$ID
  
  snpeff.fls <- gsub("snp.RData", "snpeff.RData", snp.fls)
  snpeff.fls.lst <- lapply(snpeff.fls, function(x){
    load(x)
    return(snpeff)
  })
  snpeff <- do.call(rbind, snpeff.fls.lst)
  snpeff <- snpeff[order(as.numeric(snpeff[, 1])), ]
  
  start <- as.numeric(paste0(sprintf("%02d", as.numeric(substr(chr, 4, 5))), sprintf("%08d", start)))
  end <- as.numeric(paste0(sprintf("%02d", as.numeric(substr(chr, 4, 5))), sprintf("%08d", end)))
  
  dat.res <- snp.data[as.numeric(rownames(snp.data))>=start & as.numeric(rownames(snp.data))<=end, , drop=FALSE]
  dat.res <- as.matrix(dat.res)
  #filter maf < 0.005
  #maf <- apply(dat.res, 1, function(x){
  #  numb <- sort(table(x), decreasing=TRUE)
  #  p1 <- sum(as.numeric(numb[names(numb) == 1]) * 2, as.numeric(numb[names(numb) == 2]))
  #  p2 <- sum(as.numeric(numb[names(numb) == 0]), as.numeric(numb[names(numb) == 1]), as.numeric(numb[names(numb) == 2]) * 2)
  #  pct <- p1/p2
  #})
  #dat.res <- dat.res[maf >= 0.005, ]
  
  accession <- sapply(accession, function(x){
    if (x %in% c("Improved cultivar", "Landrace", "G. Soja")) {
      x.dat <- readLines(paste0("./data/", x, ".soya.txt"))
      return(x.dat)
    } else {
      return(x)
    }
  })
  accession <- unique(unlist(accession))
  
  if (!is.null(accession) && length(accession)>=2) {
    dat.res <- dat.res[, colnames(dat.res) %in% accession, drop=FALSE]
  }
  
  dat.res.row.c <- apply(dat.res, 1, function(x){
    length(unique(x[!is.na(x)]))
  })
  dat.res <- dat.res[dat.res.row.c>1, , drop=FALSE]
  
  if (!is.null(mutType) && length(mutType)>=1 && length(mutType)!=29) {
    snpeff.info <- snpeff[snpeff[, 1] %in% rownames(dat.res),]
    
    snpeff.info <- snpeff.info[snpeff.info[, "eff"] %in% mutType, , drop=FALSE]
    
    dat.res <- dat.res[rownames(dat.res) %in% snpeff.info[, "id"], , drop=FALSE]
  }
  
  if (!is.null(snpSites) && length(snpSites)>=1) {
    dat.res <- dat.res[rownames(dat.res) %in% snpSites, , drop=FALSE]
  }
  
  dat.res[dat.res == 2] <- 3
  dat.res[dat.res == 1] <- 2
  dat.res[dat.res == 3] <- 1
  
  #### calculate distance matrix
  "%dis%" <- function(x,y){
    return(abs(x-y)/2 + as.numeric((x==1)&(y==1))/2)
  }
  
  dist.mat.nume <- foreach::foreach(x=1:ncol(dat.res),.combine=rbind)%dopar%{colSums(dat.res%dis%dat.res[,x],na.rm=TRUE)}
  dat.res[!is.na(dat.res)] <- 1
  dist.mat.deno <- foreach::foreach(x=1:ncol(dat.res),.combine=rbind)%dopar%{colSums(!is.na(dat.res+dat.res[,x]))}
  
  dist.mat <- dist.mat.nume/dist.mat.deno
  rownames(dist.mat) <- colnames(dist.mat)
  
  ### tree
  dist.mat <- as.dist(dist.mat)
  tre <- ape::nj(dist.mat)
  
  p <- ggtree::ggtree(tre, layout="circular", branch.length="none", size=0.01) + ggplot2::ggtitle("")
  p <- p + ggplot2::theme_void()
  p <- ggtree::gheatmap(p, soya.tree, offset = 1, width=0.1, colnames = FALSE, color=NULL) +
    ggplot2::scale_fill_manual(breaks=c("Improved cultivar", "Landrace",  
                               "G. Soja"), 
                      values=c("blue", "red", 
                               "purple"), name = "")
  figurecp <<- p
  treNwk <<- tre
  return(p)
}

