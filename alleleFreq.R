
# A function to perform allele frequency analysis using SNP data in a specified genomic region.
# Change to the directory of ECOGEMS using the setwd function of R.
# Usage: type the next two lines in R Console without the leading #
# source("Global.R")
# allele.plot <- alleleFreq(snpSite = c("0133024709", "1403584545", "1403584761"), accGroup = c("Landrace", "G. Soja", "Improved cultivar"), pieCols = c("cornflowerblue", "forestgreen", "red"))
# Then the result plot would be displayed in a plotting device.
# For more info, please check the AlleleFreq menu of the ECOGEMS database.

alleleFreq <- function(snpSite = c("0133024709", "1403584545", "1403584761"),
                       accGroup = c("Improved cultivar", "Landrace", "G. Soja"),
                       pieCols = c("cornflowerblue", "forestgreen") ) {
  if ( exists("snpInfo") ){
  }else{
    source("snpInfo.R")
  }
  if (exists("snp.lst")){
  }else{
    snp.lst <- read.table("./data/snp.RData.lst", head=T, as.is=T, sep="\t")
  }
  myChr <- paste0("chr", as.numeric(substr(snpSite, 1, 2)))
  myPos <- as.numeric(substr(snpSite, 3, 10))
  snpSite <- snpSite[!is.na(myPos)]
  myChr <- myChr[!is.na(myPos)]
  myPos <- myPos[!is.na(myPos)]
  myChr <- myChr[myChr %in% paste0("chr", 1:20)]
  site.geno <- lapply(1:length(myChr), function(i) {
    return(snpInfo(myChr[i], myPos[i], myPos[i]))
  })
  
  acc.group <- accGroup
  
  site.allele.freq <- lapply(site.geno, function(x){
    if (nrow(x[[2]]) == 0) {
      return(NULL)
    } else {
      maj.allele <- x[[2]][1, 2]
      min.allele <- x[[2]][1, 3]
      heterozygote <- x[[2]][1, 4]
      
      acc.grp.tab <- lapply(acc.group, function(i) {
        i.acc <- readLines(paste0("./data/", i, ".soya.txt"))
        x.i.allele.freq <- table(x[[1]][[1]][, colnames(x[[1]][[1]]) %in% i.acc])
        x.i.allele.freq <- x.i.allele.freq[c(maj.allele, min.allele, heterozygote)]
        if (is.na(x.i.allele.freq[2])) {
          x.i.allele.freq[2] <- 0
          names(x.i.allele.freq)[2] <- min.allele
        }
        
        if (is.na(x.i.allele.freq[3])) {
          x.i.allele.freq[3] <- 0
          names(x.i.allele.freq)[3] <- heterozygote
        }
        
        return(x.i.allele.freq)
        
      })
      
      acc.grp.tab.df <- do.call(rbind, acc.grp.tab)
      acc.grp.tab.df[, 1] <- acc.grp.tab.df[, 1] + acc.grp.tab.df[, 3]
      acc.grp.tab.df[, 2] <- acc.grp.tab.df[, 2] + acc.grp.tab.df[, 3]
      if (length(acc.group) == 1){
      acc.grp.tab.df <- t(as.matrix(acc.grp.tab.df[, 1:2]))
      }else{
      acc.grp.tab.df <- acc.grp.tab.df[, 1:2]
      }
      rownames(acc.grp.tab.df) <- acc.group
      return(acc.grp.tab.df)
    }
  })
  snpSite <- snpSite[!sapply(site.allele.freq, is.null)]
  site.allele.freq[sapply(site.allele.freq, is.null)] <- NULL
  yrowname <- substr(snpSite, 1, 10)
  
  if(length(site.allele.freq) == 0){
    NULL
  }else{
  op <- par(mfrow=c(length(site.allele.freq), length(acc.group) + 1 ),
            oma = c(0, 0, 0, 0),
            mar = c(0, 2, 1, 0),
            mgp = c(0, 0, 0),
            xpd = NA)
  
  pie.cols <- pieCols
  
  for (i in 1:length(site.allele.freq)) {
    for (j in 1:nrow(site.allele.freq[[i]])) {
      if (i == 1) {
        if (j == 1) {
          pie(site.allele.freq[[i]][j, ], col=pie.cols, label=NA, radius=1) + 
            title(acc.group[j], line = -0.5, cex.main = 2) + title(ylab = yrowname[i], line = -0.4, cex.lab = 2)
        } else if (j == length(acc.group)) {
          pie(site.allele.freq[[i]][j, ], col=pie.cols, label=NA, radius=1) + title(acc.group[j], line = -0.5, cex.main = 2)
          plot(0, type = "n", axes = F, xlab="", ylab="")
          legend("center", legend = names(site.allele.freq[[i]][j, ]), 
                 fill = pie.cols, border = pie.cols, bty="n", cex = 2)
        } else {
          pie(site.allele.freq[[i]][j, ], col=pie.cols, label=NA, radius=1) + title(acc.group[j], line = -0.5, cex.main = 2)
        }
      } else {
        if (j == 1) {
          pie(site.allele.freq[[i]][j, ], col=pie.cols,
              label=NA, radius=1) + title(ylab = yrowname[i], line = -0.4, cex.lab = 2)
        } else if (j == length(acc.group)) {
          pie(site.allele.freq[[i]][j, ], col=pie.cols, label=NA, radius=1)
          plot(0, type = "n", axes = F, xlab="", ylab="")
          legend("center", legend = names(site.allele.freq[[i]][j, ]), 
                 fill = pie.cols, border = pie.cols, bty="n", cex = 2)
        } else {
          pie(site.allele.freq[[i]][j, ], col=pie.cols, label=NA, radius=1)
        }
      }
    }
  }
  
  par(op)
  }
}

