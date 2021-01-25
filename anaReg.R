
anaReg <- function(x=NULL) {
  x <- gsub("\\s+", "", x)
  if (grepl("chr", x)) {
    myChr <- gsub(":.+", "", x)
    myPos <- as.numeric(gsub("\\s","", strsplit(gsub(".+:", "", x),"-")[[1]]))
  } else {
    myChr <- gene.info$chr[gene.info$id==x]
    myPos <- c(gene.info$start[gene.info$id==x], gene.info$end[gene.info$id==x])
  }
  
  chr.size <- c(59249829, 52160593, 47695029, 53978414, 45299532, 51952677, 46590119, 49896852, 51486896, 54067360, 
                40746315, 44494750, 47100052, 53013617, 53421904, 38265740, 42350739, 60669500, 52317195, 52284028)
  names(chr.size) <- paste0("chr", 1:20)
  
  myPos[1] <- max(1, myPos[1])
  myPos[2] <- min(myPos[2], chr.size[myChr])
  
  return(list(chr=myChr, start=myPos[1], end=myPos[2]))
}
