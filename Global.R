
options(warn=-1)

library(IRanges)
library(plotly)
library(LDheatmap)
library(chopsticks)
library(foreach)
library(ape)
library(pegas)
library(plyr)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggtree)
library(grid)
library(snpStats)
library(genetics)
library(shinycssloaders)
library(shinysky)
#library(shinycustomloader)
library(shinyWidgets)
library(shinydisconnect)
library(Biostrings)
library(GenomicRanges)
library(XML)
library(data.table)
library(shinydashboard)
library(stringr)

source("fetchSnp.R")
source("fetchSnpAllele.R")
source("ld.heatmap.R")
source("phylo.R")
source("nucDiv.R")
source("GBrowser.R")
source("anaReg.R")
source("geneStru.R")
source("snpInfo.R")
source("validReg.R")
source("box_format.R")
source("Homepage.R")

soya.info <- read.table("./data/all.soya.txt", head=T, as.is=T, sep="\t", quote="")
load("./data/zh13.gff.RData")
snp.lst <- read.table("./data/snp.RData.lst", head=T, as.is=T, sep="\t")
load("./data/gene.info.RData")
gene.info$start <- as.numeric(gene.info$start)
gene.info$end <- as.numeric(gene.info$end)

source("chooser.R")
all.soya.cho <- paste(soya.info$ID, soya.info$Species, soya.info$Category, sep=", ")
all.soya.cho <- c("Improved cultivar", "Landrace", "G. Soja", all.soya.cho)

chrInfo <- read.table("./data/chrInfo.txt", head=T, as.is=T, sep="\t")
BLASTdb.fl <- read.table("BLASTdb.txt", head=T, as.is=T, sep = "\t")

soya.tree <- read.table("./data/soya.tree.txt", 
                       head=T, as.is=T, sep="\t", row.names = 1)
mutationtypes <- c("3_prime_UTR_variant", "5_prime_UTR_premature_start_codon_gain_variant", "5_prime_UTR_variant", 
                   "downstream_gene_variant","initiator_codon_variant", "initiator_codon_variant&splice_region_variant",
                   "intergenic_region", "intragenic_variant", "intron_variant", "missense_variant",                                        
                   "missense_variant&splice_region_variant", "non_coding_transcript_exon_variant",          
                   "non_coding_transcript_variant", "splice_acceptor_variant&intron_variant", 
                   "splice_donor_variant&intron_variant"," splice_region_variant",                             
                   "splice_region_variant&intron_variant", "splice_region_variant&non_coding_transcript_exon_variant",
                   "splice_region_variant&stop_retained_variant", "splice_region_variant&synonymous_variant",                
                   "start_lost", "start_lost&splice_region_variant", "stop_gained", "stop_gained&splice_region_variant",                       
                   "stop_lost","stop_lost&splice_region_variant", "stop_retained_variant",  "synonymous_variant", "upstream_gene_variant"
)
exam1.fa <- readLines("exam1.fa")

Blast_Info_Title <- paste("qseqid: Query sequence ID;",
                          "qlen: Query sequence length;",
                          "sseqid: Subject sequence ID;",
                          "slen: Subject sequence length;",
                          "length: Alignment length;",
                          "qstart: Start of alignment in query;",
                          "qend: End of alignment in query;",
                          "sstart: Start of alignment in subject;",
                          "send: End of alignment in subject;",
                          "gaps: Number of gap openings;",
                          "pident: Percentage of identical matches;",
                          "evalue: Expect value;",
                          "bitscore: Bit score;",
                          sep = "<br>")

