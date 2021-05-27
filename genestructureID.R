genestructureID <- function(chr="chr1", start=21418069, end=21434067, geneid = "SoyZH13_01G071600", gff = NULL){
  start <- as.numeric(start)
  end <- as.numeric(end)
  gff.mrna <- gff[gff$type == "mRNA", ]
  gff.reg.mrna <- gff.mrna[grep(geneid, gff.mrna$id), ]
  gff.reg <- gff[gff$id %in% gff.reg.mrna$id, ]
  
  gff.reg.mrna.ir <- IRanges::IRanges(gff.reg.mrna$start, gff.reg.mrna$end)
  gff.reg.mrna.op <- GenomicRanges::findOverlaps(gff.reg.mrna.ir, GenomicRanges::reduce(gff.reg.mrna.ir))
  
  gff.reg.mrna$grp <- S4Vectors::subjectHits(gff.reg.mrna.op)
  
  gff.reg.mrna.1 <- gff.reg.mrna %>% group_by(grp) %>% mutate(y=row_number())
  
  gff.reg <- merge(gff.reg, gff.reg.mrna.1[, c("id", "y")], by="id")
  
  gff.reg$y <- gff.reg$y * 0.2 + 1
  
  plot.mrna.lst <- lapply(unique(gff.reg$id), function(i){
    dat <- gff.reg[gff.reg$id == i, ]
    i.strand <- dat$strand[1]
    
    dat.mrna <- dat[dat$type=="mRNA", ]
    return(dat.mrna)
  })
  plot.mrna <- do.call(rbind, plot.mrna.lst)
  
  
  
  
  p1 <- ggplot2::ggplot(plot.mrna) + ggplot2::geom_rect(ggplot2::aes(xmin=start, xmax=end, ymin=y+0.118, ymax=y+0.122,
                                                                     text=anno), 
                                                        color="grey30", fill="grey30")
  
  
  plot.nm.lst <- lapply(unique(gff.reg$id), function(i){
    dat <- gff.reg[gff.reg$id == i, ]
    i.strand <- dat$strand[1]
    
    dat.nm <- dat[dat$type!="mRNA", ]
    dat.nm <- dat.nm[-nrow(dat.nm), ]
    
    if (nrow(dat.nm)>0) {
      dat.nm$ymin <- dat.nm$y+0.1
      dat.nm$ymax <- dat.nm$y+0.14
      dat.nm$ymin[dat.nm$type=="CDS"] <- dat.nm$ymin[dat.nm$type=="CDS"] - 0.02
      dat.nm$ymax[dat.nm$type=="CDS"] <- dat.nm$ymax[dat.nm$type=="CDS"] + 0.02
    }
    return(dat.nm)
  })
  plot.nm <- do.call(rbind, plot.nm.lst)
  if (nrow(plot.nm)>0) {
    p1 <- p1 + ggplot2::geom_rect(ggplot2::aes(xmin=start, xmax=end, ymin=ymin, ymax=ymax, text=anno), 
                                  color="grey30", fill="grey30", data=plot.nm)
  }
  
  
  plot.tail.lst <- lapply(unique(gff.reg$id), function(i){
    dat <- gff.reg[gff.reg$id == i, ]
    i.strand <- dat$strand[1]
    
    dat.nm <- dat[dat$type!="mRNA", ]
    
    i.anno <- dat$anno[1]
    i.id <- i
    
    tail.type <- dat.nm$type[nrow(dat.nm)]
    
    dat.tail <- data.frame(xx=rep(c(dat$start[nrow(dat)], 
                                    (dat$start[nrow(dat)] + dat$end[nrow(dat)])/2, dat$end[nrow(dat)]), each=2), 
                           stringsAsFactors = FALSE)
    if (i.strand == "-") {
      dat.tail$yy <- c(0.12, 0.12, 0.1, 0.14, 0.1, 0.14) + dat$y[1]
      dat.tail <- dat.tail[c(1,3,5,6,4,2), ]
      dat.tail$pare <- i.id
      dat.tail$anno <- i.anno
      if (tail.type=="CDS") {
        dat.tail$yy[2:3] <- dat.tail$yy[2:3] - 0.02
        dat.tail$yy[4:5] <- dat.tail$yy[4:5] + 0.02
      }
    } else {
      dat.tail$yy <- c(0.1, 0.14, 0.1, 0.14, 0.12, 0.12) + dat$y[1]
      dat.tail <- dat.tail[c(1,3,5,6,4,2), ]
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
  
  
  p1 <- p1 + ggplot2::geom_polygon(ggplot2::aes(x=xx, y=yy, group=id), color="grey30", fill="grey30", 
                                   data=plot.tail)
  
  
  p1 <- p1 + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),panel.grid.minor = ggplot2::element_blank()) + 
    ggplot2::theme(panel.background = ggplot2::element_rect(fill="white",colour="white")) + ggplot2::xlab(chr) + ggplot2::ylab("") + 
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) + ggplot2::theme(axis.text.y = ggplot2::element_blank()) + 
    ggplot2::theme(axis.text = ggplot2::element_text(size=12), axis.title=ggplot2::element_text(size=14,face="bold"))
  
  nnr <<- nrow(plot.mrna)
  grid::grid.draw(ggplot2::ggplotGrob(p1))
}