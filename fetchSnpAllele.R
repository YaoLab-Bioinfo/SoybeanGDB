

fetchSnpAllele <- function(chr="chr7", start=29616705, end=29629223, accession=NULL, mutType=NULL){
  
  if (is.null(chr)) {
    return(NULL)
  } else {
    if (exists("soya.info")){
    }else{
      soya.info <- read.table("./data/all.soya.txt", head=T, as.is=T, sep="\t", quote="")
    }
    if (exists("chrInfo")){
    }else{
      chrInfo <- read.table("./data/chrInfo.txt", head=T, as.is=T, sep="\t")
    }
    if (exists("snp.lst")){
    }else{
      snp.lst <- read.table("./data/snp.RData.lst", head=T, as.is=T, sep="\t")
    }
    
    
    chr.size <- chrInfo$size[chrInfo$chr == chr]
    start <- max(0, start)
    end <- min(end, chr.size)
    
    start <- as.numeric(start)
    end <- as.numeric(end)
    reg.gr <- IRanges::IRanges(start, end)
    snp.lst.chr <- snp.lst[snp.lst$chr==chr, ]
    snp.lst.gr <- IRanges::IRanges(start=snp.lst.chr$start, end=snp.lst.chr$end)
    snp.fls <- snp.lst.chr$file[unique(S4Vectors::queryHits(GenomicRanges::findOverlaps(snp.lst.gr, reg.gr)))]
    
    snp.fls.lst <- lapply(snp.fls, function(x){
      load(x)
      return(list(snp.data.inter.Matrix, snp.data.allele))
    })
    snp.fls.lst <- unlist(snp.fls.lst, recursive = FALSE)
    snp.data <- do.call(rbind, snp.fls.lst[seq(1, length(snp.fls.lst), by=2)])
    snp.data <- snp.data[order(as.numeric(rownames(snp.data))), ]
    colnames(snp.data) <- soya.info$ID
    snp.allele <- do.call(rbind, snp.fls.lst[seq(2, length(snp.fls.lst), by=2)])
    snp.allele <- snp.allele[order(as.numeric(rownames(snp.allele))), ]
    snp.allele[, 3] <- paste(snp.allele[, 1], snp.allele[, 2], sep="/")
    snp.allele[, 1] <- paste(snp.allele[, 1], snp.allele[, 1], sep="/")
    snp.allele[, 2] <- paste(snp.allele[, 2], snp.allele[, 2], sep="/")
    
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
    #filter maf < 0.005
    dat.res <- as.matrix(dat.res)
    maf <- apply(dat.res, 1, function(x){
      numb <- sort(table(x), decreasing=TRUE)
      p1 <- sum(as.numeric(numb[names(numb) == 1]) * 2, as.numeric(numb[names(numb) == 2]))
      p2 <- sum(as.numeric(numb[names(numb) == 0]), as.numeric(numb[names(numb) == 1]), as.numeric(numb[names(numb) == 2]) * 2)
      pct <- p1/p2
    })
    dat.res <- dat.res[maf >= 0.005, ]
    
    snp.code <- as.vector(t(snp.allele[rownames(dat.res), ]))
    allele.res <- snp.allele[rownames(dat.res), , drop=FALSE]
    
    dat.res.n <- seq_len(nrow(dat.res)) - 1
    dat.res.n <- rep(dat.res.n, each=ncol(dat.res))
    dat.res.n <- matrix(dat.res.n, ncol=ncol(dat.res), byrow = T)
    
    dat.res <- dat.res + 1 + dat.res.n * 3
    dat.res.mat <- matrix(snp.code[as.matrix(dat.res)], ncol=ncol(dat.res))
    rownames(dat.res.mat) <- rownames(dat.res)
    colnames(dat.res.mat) <- colnames(dat.res)
    
    accession <- gsub(",.+", "", accession)
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
      dat.res.mat <- dat.res.mat[, colnames(dat.res.mat) %in% accession, drop=FALSE]
    }
    
    dat.res.mat.row.c <- apply(dat.res.mat, 1, function(x){
      length(unique(x[!is.na(x)]))
    })
    
    dat.res.mat <- dat.res.mat[dat.res.mat.row.c>1, , drop=FALSE]
    allele.res <- allele.res[dat.res.mat.row.c>1, , drop=FALSE]
    
    if (!is.null(mutType) && length(mutType)>=1 && length(mutType)!=29) {
      snpeff.info <- snpeff[snpeff[, 1] %in% rownames(dat.res.mat), , drop=FALSE]
      
      snpeff.info <- snpeff.info[snpeff.info[, "eff"] %in% mutType, , drop=FALSE]
      
      dat.res.mat <- dat.res.mat[rownames(dat.res.mat) %in% snpeff.info[, "id"], , drop=FALSE]
      allele.res <- allele.res[rownames(allele.res) %in% snpeff.info[, "id"], , drop=FALSE]
    }
    
    return(list(dat.res.mat, allele.res))
  }
}

