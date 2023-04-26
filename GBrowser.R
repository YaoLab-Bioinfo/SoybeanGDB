
# A function for visualization of SNP sites in a specified genomic region.
# Change to the directory of SoyaSNPDB using the setwd function of R.
# Usage: type the next two lines in R Console without the leading #
# source("Global.R")
# snp.plot <- GBrowser(chr="chr1", start=29765419, end=29793053, accession=NULL, mutType=NULL)
# To visualization the SNP sites in static mode, type snp.plot[[1]] in R console.
# To visualization the SNP sites in interacitve mode, type snp.plot[[2]] in R console.
# For more info, please check the Browser menu of the MaizeSNPDB database.

GBrowser <- function(chr="chr1", start=29765419, end=29793053, accession=NULL, mutType=NULL) {
  
  #library(ggplot2)
  if ( exists("fetchSnp") ){
  }else{
    source("fetchSnp.R")
  }
  start <- as.numeric(start)
  end <- as.numeric(end)
  
  set.seed(123)
  
  accession <- gsub(",.+", "", accession)
  accession <- sapply(accession, function(x){
    if (x %in% c("Improved cultivar", "Landrace", "Glycine soja")) {
      x.dat <- readLines(paste0("./data/", x, ".soya.txt"))
      return(x.dat)
    } else {
      return(x)
    }
  })
  accession <- unique(unlist(accession))
  
  snp.res <- fetchSnp(chr=chr, start=start, end=end, accession=accession, filter = FALSE)
  #filter SNPs without polymorphism
  nn <- apply(snp.res[[1]], 1, function(x){
    freq <- table(x)
    return(length(freq))
  })
  
  snp.reg <- snp.res[[1]][nn != 1 ]
  snp.table <- snp.res[[1]]
  snp.table <- snp.table[rownames(snp.table) %in% rownames(snp.res[[1]])[nn != 1 ], , drop=FALSE]
  snp.data.allele <- apply(snp.table, 1, function(x){
    numbersum <- sort(table(x), decreasing=TRUE)
    if (length(table(x) == 2)){
      majornum <- as.numeric(numbersum[1] * 2)
    }else{
      majornum <- as.numeric(numbersum[1] * 2 + numbersum["H"]) 
    }
    mainornum <- as.numeric(sum(numbersum) * 2  - majornum)
    summjmi <- sum(majornum, mainornum)
    majornum <- round(majornum/summjmi * 100, 1)
    mainornum <- round(mainornum/summjmi * 100, 1)
    x <- x[!x %in% c("H", "N")]
    y <- sort(table(x), decreasing=TRUE)
    major <- names(y)[1]
    minor <- names(y)[2]
    return(c(major, minor, majornum, mainornum))
  })
  
  snp.data.allele <- t(snp.data.allele)
  colnames(snp.data.allele) <- c("major", "minor", "majornum", "mainornum")
  
  
  snpeff <- snp.res[[3]]
  
  snpeff.reg <- snpeff[snpeff[,1] %in% rownames(snp.res[[1]])[nn != 1 ], , drop=FALSE]
  snpeff.reg <- as.data.frame(snpeff.reg, stringsAsFactors = FALSE)
  snpeff.reg$chr <- as.numeric(substr(snpeff.reg$id, 1, 2))
  snpeff.reg$pos <- as.numeric(substr(snpeff.reg$id, 3, 11))
  
  snpeff.reg$tag <- ""
  snpeff.reg$tag[grepl("stop_lost&splice_region_variant", snpeff.reg$eff)] <- 29
  snpeff.reg$tag[grepl("stop_lost", snpeff.reg$eff)] <- 28
  snpeff.reg$tag[grepl("stop_gained&splice_region_variant", snpeff.reg$eff)] <- 27
  snpeff.reg$tag[grepl("stop_gained", snpeff.reg$eff)] <- 26
  snpeff.reg$tag[grepl("start_lost&splice_region_variant", snpeff.reg$eff)] <- 25
  snpeff.reg$tag[grepl("start_lost", snpeff.reg$eff)] <- 24
  snpeff.reg$tag[grepl("stop_retained_variant", snpeff.reg$eff)] <- 23
  snpeff.reg$tag[grepl("initiator_codon_variant", snpeff.reg$eff)] <- 22
  snpeff.reg$tag[grepl("initiator_codon_variant&splice_region_variant", snpeff.reg$eff)] <- 21
  snpeff.reg$tag[grepl("splice_acceptor_variant&intron_variant", snpeff.reg$eff)] <- 20
  snpeff.reg$tag[grepl("splice_donor_variant&intron_variant", snpeff.reg$eff)] <- 19
  snpeff.reg$tag[grepl("splice_region_variant&stop_retained_variant", snpeff.reg$eff)] <- 18
  snpeff.reg$tag[grepl("splice_region_variant&intron_variant", snpeff.reg$eff)] <- 17
  snpeff.reg$tag[grepl(" splice_region_variant", snpeff.reg$eff)] <- 16
  snpeff.reg$tag[grepl("missense_variant&splice_region_variant", snpeff.reg$eff)] <- 15
  snpeff.reg$tag[grepl("missense_variant", snpeff.reg$eff)] <- 14
  snpeff.reg$tag[grepl("splice_region_variant&synonymous_variant", snpeff.reg$eff)] <- 13
  snpeff.reg$tag[grepl("non_coding_transcript_exon_variant", snpeff.reg$eff)] <- 12
  snpeff.reg$tag[grepl("non_coding_transcript_variant", snpeff.reg$eff)] <- 11
  snpeff.reg$tag[grepl("splice_region_variant&non_coding_transcript_exon_variant", snpeff.reg$eff)] <- 10
  snpeff.reg$tag[grepl("intron_variant", snpeff.reg$eff)] <- 9
  snpeff.reg$tag[grepl("5_prime_UTR_premature_start_codon_gain_variant", snpeff.reg$eff)] <- 8
  snpeff.reg$tag[grepl("5_prime_UTR_variant", snpeff.reg$eff)] <- 7
  snpeff.reg$tag[grepl("3_prime_UTR_variant", snpeff.reg$eff)] <- 6
  snpeff.reg$tag[grepl("synonymous_variant", snpeff.reg$eff)] <- 5
  snpeff.reg$tag[grepl("upstream_gene_variant", snpeff.reg$eff)] <- 4
  snpeff.reg$tag[grepl("downstream_gene_variant", snpeff.reg$eff)] <- 3
  snpeff.reg$tag[grepl("intragenic_variant", snpeff.reg$eff)] <- 2
  snpeff.reg$tag[grepl("intergenic_region", snpeff.reg$eff)] <- 1
  
  snpeff.reg.1 <- snpeff.reg %>% dplyr::group_by(id, chr, pos, ref, alt) %>% dplyr::summarise(info=paste(eff, collapse="<br>"))
  snpeff.reg.2 <- snpeff.reg %>% dplyr::group_by(id, chr, pos, ref, alt) %>% dplyr::summarise(tag=max(tag))
  snpeff.reg.3 <- merge(snpeff.reg.1, snpeff.reg.2, by=c("id", "chr", "pos", "ref", "alt"))
  snpeff.reg.3$yr <- runif(nrow(snpeff.reg.3), min=-0.9, max=1.1)
  eff.tags <- c("3_prime_UTR_variant", "5_prime_UTR_premature_start_codon_gain_variant", "5_prime_UTR_variant", 
                "downstream_gene_variant","initiator_codon_variant", "initiator_codon_variant&splice_region_variant",
                "intergenic_region", "intragenic_variant", "intron_variant", "missense_variant",                                        
                "missense_variant&splice_region_variant", "non_coding_transcript_exon_variant",          
                "non_coding_transcript_variant", "splice_acceptor_variant&intron_variant", 
                "splice_donor_variant&intron_variant"," splice_region_variant",                             
                "splice_region_variant&intron_variant", "splice_region_variant&non_coding_transcript_exon_variant",
                "splice_region_variant&stop_retained_variant", "splice_region_variant&synonymous_variant",                
                "start_lost", "start_lost&splice_region_variant", "stop_gained", "stop_gained&splice_region_variant",                       
                "stop_lost","stop_lost&splice_region_variant", "stop_retained_variant",  "synonymous_variant", "upstream_gene_variant")
  eff.tags <- eff.tags[c(7,8,4,29,28,1,3,2,9,18,13,12,20,10,11,16,17,19,15,14,6,5,27,21,22,23,24,25,26)]
  names(eff.tags) <- 1:29
  snpeff.reg.3$tag <- eff.tags[snpeff.reg.3$tag]
  snpeff.reg.3$major <- snp.data.allele[,1][snpeff.reg.3$id]
  snpeff.reg.3$minor <- snp.data.allele[,2][snpeff.reg.3$id]
  snpeff.reg.3$minor[is.na(snpeff.reg.3$minor)] <- unlist(snpeff.reg.3[is.na(snpeff.reg.3$minor), ][, c(4,5)][!snpeff.reg.3[is.na(snpeff.reg.3$minor), ][, c(4,5)] %in% snpeff.reg.3[is.na(snpeff.reg.3$minor), ][, c(9, 10)]])
  snpeff.reg.3$major <- paste0(snpeff.reg.3$major, ' (', snp.data.allele[,3][snpeff.reg.3$id], '%)')
  snpeff.reg.3$minor <- paste0(snpeff.reg.3$minor, ' (', snp.data.allele[,4][snpeff.reg.3$id], '%)')
  snpeff.reg.3$info <- paste0("Effect: ", snpeff.reg.3$info)
  
  if (!is.null(mutType)) {
    snpeff.reg.3 <- snpeff.reg.3[snpeff.reg.3$tag %in% mutType, , drop=FALSE]
  }
  colnames(snpeff.reg.3)[c(3, 6, 4, 5, 9, 10)] <- c("Position", "Effect", "Reference allele", "Alternative allele","Major allele", "Minor allele")
  
  p1 <- ggplot2::ggplot(data=snpeff.reg.3) + 
    ggplot2::geom_point(ggplot2::aes(x=Position, y=yr, color=tag, text=Effect, fill=tag, ref = `Reference allele`, alt = `Alternative allele`, major = `Major allele`, minor = `Minor allele`), size=0.8, pch=25)
  
  if ( exists("gff") && gff[1,1]  == "SoyZH13_01G000001.m1" ){
  }else {
    gff <- data.table::fread("./data/zh13.gff", sep = "\t", data.table = FALSE)
  }
  gff$id <- gsub(":.+", "", gff$id)
  gff.mrna <- gff[gff$type == "mRNA", , drop=FALSE]
  gff.reg.mrna <- gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, , drop=FALSE]
  gff.reg <- gff[gff$id %in% gff.reg.mrna$id, , drop=FALSE]
  
  gff.reg$anno <- paste(gff.reg$id, gff.reg$anno, sep=" <br> ")
  
  gff.reg.mrna.ir <- IRanges::IRanges(gff.reg.mrna$start, gff.reg.mrna$end)
  gff.reg.mrna.op <- GenomicRanges::findOverlaps(gff.reg.mrna.ir, GenomicRanges::reduce(gff.reg.mrna.ir))
  gff.reg.mrna$grp <- S4Vectors::subjectHits(gff.reg.mrna.op)
  
  gff.reg.mrna.1 <- gff.reg.mrna %>% dplyr::group_by(grp) %>% dplyr::mutate(y = dplyr::row_number())
  
  gff.reg <- merge(gff.reg, gff.reg.mrna.1[, c("id", "y")], by="id")
  
  gff.reg$y <- gff.reg$y * 0.2 + 1
  
  plot.nm.lst <- lapply(unique(gff.reg$id), function(i){
    dat <- gff.reg[gff.reg$id == i, , drop=FALSE]
    i.strand <- dat$strand[1]
    
    if (i.strand == "-") {
      dat$y <- -dat$y
    }
    
    dat.nm <- dat[dat$type!="mRNA", , drop=FALSE]
    dat.nm <- dat.nm[-nrow(dat.nm), , drop=FALSE]
    
    if (nrow(dat.nm)>0) {
      dat.nm$ymin <- dat.nm$y+0.1
      dat.nm$ymax <- dat.nm$y+0.14
      dat.nm$ymin[dat.nm$type=="CDS"] <- dat.nm$ymin[dat.nm$type=="CDS"] - 0.02
      dat.nm$ymax[dat.nm$type=="CDS"] <- dat.nm$ymax[dat.nm$type=="CDS"] + 0.02
    }
    return(dat.nm)
  })
  plot.nm <- do.call(rbind, plot.nm.lst)
  if (!is.null(plot.nm) && nrow(plot.nm)>0) {
    p1 <- p1 + ggplot2::geom_rect(ggplot2::aes(xmin=start, xmax=end, ymin=ymin, ymax=ymax), 
                                  color="grey30", fill="grey30", data=plot.nm)
  }
  
  plot.mrna.lst <- lapply(unique(gff.reg$id), function(i){
    dat <- gff.reg[gff.reg$id == i, , drop=FALSE]
    i.strand <- dat$strand[1]
    
    if (i.strand == "-") {
      dat$y <- -dat$y
    }
    
    dat.mrna <- dat[dat$type=="mRNA", , drop=FALSE]
    return(dat.mrna)
  })
  plot.mrna <- do.call(rbind, plot.mrna.lst)
  if (!is.null(plot.mrna) && nrow(plot.mrna)>0) {
    p1 <- p1 + ggplot2::geom_rect(ggplot2::aes(xmin=start, xmax=end, ymin=y+0.118, ymax=y+0.122), 
                                  color="grey30", fill="grey30", data=plot.mrna)
  }
  
  plot.tail.lst <- lapply(unique(gff.reg$id), function(i){
    dat <- gff.reg[gff.reg$id == i, , drop=FALSE]
    i.strand <- dat$strand[1]
    
    if (i.strand == "-") {
      dat$y <- -dat$y
    }
    
    dat.nm <- dat[dat$type!="mRNA", , drop=FALSE]
    
    i.anno <- dat$anno[1]
    i.id <- i
    
    tail.type <- dat.nm$type[nrow(dat.nm)]
    
    dat.tail <- data.frame(xx=rep(c(dat$start[nrow(dat)], 
                                    (dat$start[nrow(dat)] + dat$end[nrow(dat)])/2, dat$end[nrow(dat)]), each=2), 
                           stringsAsFactors = FALSE)
    if (i.strand == "-") {
      dat.tail$yy <- c(0.12, 0.12, 0.1, 0.14, 0.1, 0.14) + dat$y[1]
      dat.tail <- dat.tail[c(1,3,5,6,4,2), , drop=FALSE]
      dat.tail$pare <- i.id
      dat.tail$anno <- i.anno
      if (tail.type=="CDS") {
        dat.tail$yy[2:3] <- dat.tail$yy[2:3] - 0.02
        dat.tail$yy[4:5] <- dat.tail$yy[4:5] + 0.02
      }
    } else {
      dat.tail$yy <- c(0.1, 0.14, 0.1, 0.14, 0.12, 0.12) + dat$y[1]
      dat.tail <- dat.tail[c(1,3,5,6,4,2), , drop=FALSE]
      dat.tail$pare <- i.id
      dat.tail$anno <- i.anno
      if (tail.type=="CDS") {
        dat.tail$yy[1:2] <- dat.tail$yy[1:2] - 0.02
        dat.tail$yy[5:6] <- dat.tail$yy[5:6] + 0.02
      }
    }
    
    dat.tail$id <- i.id
    
    return(dat.tail)
  })
  plot.tail <- do.call(rbind, plot.tail.lst)
  if (!is.null(plot.tail) && nrow(plot.tail)>0) {
    p1 <- p1 + ggplot2::geom_polygon(ggplot2::aes(x=xx, y=yy, group=id), color="grey30", fill="grey30", 
                                     data=plot.tail)
  }
  
  p1 <- p1 + ggplot2::scale_y_continuous("", breaks=NULL)
  p1 <- p1 + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor=ggplot2::element_blank()) + 
    ggplot2::theme(panel.background = ggplot2::element_rect(fill="white",colour="white"))
  p1 <- p1 + ggplot2::xlab("Chromosome position")
  
  p1 <- p1 + ggplot2::guides(color = ggplot2::guide_legend(title=NULL) )
  p1 <- p1 + ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                            axis.line.y = ggplot2::element_blank())
  p1 <- p1 + ggplot2::guides(fill=FALSE)
  
  p3 <- p1 + ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                            axis.line.y = ggplot2::element_blank())
  p3 <- plotly::ggplotly(p3, tooltip = c("Position", "Effect", "ref", "alt", "major", "minor"))
  
  p3 <- p3 %>% plotly::layout(
    title = "",
    xaxis = list(
      rangeselector = list(),
      
      rangeslider = list(type = "category")
    ),
    
    yaxis = list(title = "")
  )
  
  return(list(p1, p3))
}

