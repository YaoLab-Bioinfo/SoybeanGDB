# The update file max size 200MB
# options(shiny.maxRequestSize = 200*1024^2)

shinyServer(function(input, output, session) {
  
  #Home-JBrowse_up
  observeEvent(input$Link_JBrowse_up, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>JBrowse</strong>"))
  })
  
  # Home-Browse_up
  observeEvent(input$Browse_botton_up, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Browse</strong>"))
  })
  
  #Home-Indel_up
  observeEvent(input$Link_Indel_up, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>INDELs</strong>"))
  })
  
  #Home-GeneInfoID
  observeEvent(input$Link_GeneInfoID, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Search by gene IDs</strong>"))
  })
  
  #Home-GeneInfoIT
  observeEvent(input$Link_GeneInfoIT, {
    updateNavbarPage(session, "The_page", selected =HTML("<strong style='font-size:20px'>Search by genome location</strong>"))
  })
  
  #Home-Transcription factors/regulators
  observeEvent(input$Link_Genetrf, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Transcription factors/regulators</strong>"))
  })
  
  #Home-Genome synteny
  observeEvent(input$Link_Genegsy, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Genome synteny</strong>"))
  })
  
  #Home-Structural variations
  observeEvent(input$Link_Genegsvr, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Structural variations</strong>"))
  })
  
  # Home-Browse
  observeEvent(input$Browse_botton, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Browse</strong>"))
  })
  
  #Home-Indel
  observeEvent(input$Link_Indel, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>INDELs</strong>"))
  })
  
  #Home-LDHeatmap
  observeEvent(input$Linkage_anal, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>LDheatmap</strong>"))
  })
  
  #Home-Diversity
  observeEvent(input$Link_diversity, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Diversity</strong>"))
  })
  
  #Home-SnpInfo
  observeEvent(input$Link_SnpInfo, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Search</strong>"))
  })
  
  #Home-BLAST
  observeEvent(input$Link_blast, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>BLAST</strong>"))
  })
  
  #Home-Primer
  observeEvent(input$Link_Primer, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Primer-Design</strong>"))
  })
  
  #Home-Accession
  observeEvent(input$Link_Orthologous, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Orthologous</strong>"))
  })
  
  #Home-AlleleFreq
  observeEvent(input$Link_AlleleFreq, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>AlleleFreq</strong>"))
  })
  
  #Home-GOAnnotation
  observeEvent(input$Link_GOAnnotation, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>GO Annotation</strong>"))
  })
  
  #Home-GOEnrichment
  observeEvent(input$Link_GOEnrichment, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>GO Enrichment</strong>"))
  })
  
  #Home-KEGGanootation
  observeEvent(input$Link_KEGGanootation, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>KEGG Annotation</strong>"))
  })
  
  #Home-KEGGEnrichment
  observeEvent(input$Link_KEGGEnrichment, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>KEGG Enrichment</strong>"))
  })
  
  #Home-JBrowse
  observeEvent(input$Link_JBrowse, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>JBrowse</strong>"))
  })
  
  #Home-Geneexpressionlevel
  observeEvent(input$Link_Geneexpressionlevel, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Gene expression analysis</strong>"))
  })
  
  #Home-Geneexpressioncorrelationanalysis
  observeEvent(input$Link_Geneexpressioncorrelationanalysis, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Gene co-expression analysis</strong>"))
  })
  
  #Home-Accessions
  observeEvent(input$Link_Accessions, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Accessions</strong>"))
  })
  
  
  #search gene or interval information
  #search by gene ids
  observeEvent(input$submit_GSID, {
    #library(GenomicRanges)
    #library(IRanges)
    #library(Biostrings)
    if (input$variety_BDG == "All genomes"){
      load("./data/genewithaccession.RDate")
      print(input$variety_BDG)
      
      genomenames <- genewithaccession$accession[genewithaccession$id == input$BDG_Paste1]
      gene_list <- unlist(strsplit(input$BDG_Paste1, split="\\n"))
      gene_list <- trimws(gene_list, which = c("both"), whitespace = "[ \t\r\n]")
    }else{
      genomenames <- gsub(" ", "_", input$variety_BDG) 
      gene_list <- unlist(strsplit(input$BDG_Paste, split="\\n"))
      gene_list <- trimws(gene_list, which = c("both"), whitespace = "[ \t\r\n]")
    }
    print(genomenames)
    load(paste0("./info/", genomenames, "/", genomenames, ".gene.info.RData"))
    
    #cds protein cdna信息读取
    load(paste0("./info/", genomenames, "/", genomenames, ".cds.fasta.RData"))
    cds.infoid <- cds.info
    load(paste0("./info/", genomenames, "/", genomenames, ".protein.RData"))
    proteinid <- protein
    load(paste0("./info/", genomenames, "/", genomenames, ".cdna.fasta.RData"))
    cdna.infoid <- cdna.info
    gene_list <- gene_list[gene_list %in% gene_info_s$id]

    #geneinfo
    geneinfo_id <- gene_info_s[gene_info_s$id %in% gene_list, ]
    #geneinfo
    output$geneinfoid <- DT::renderDT({
      if( length(geneinfo_id[, 1]) != 0 ){
        colnames(geneinfo_id) <- c("ID", "Chromosome", "Start", "End", "Strand")
        DT::datatable(
          geneinfo_id, 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("searchlocation", sep = "-")),
                                        list(extend = 'excel', filename =  paste("searchloaction", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      } else {
        geneinfo_id <- data.frame("V1"="Gene IDs not right!")
        colnames(geneinfo_id) <- ""
        DT::datatable(
          geneinfo_id,
          escape = FALSE, rownames= FALSE, selection="none", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("searchlocation", sep = "-")),
                                        list(extend = 'excel', filename =  paste("searchloaction", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }
    }, server = FALSE)
    
    output$geneinfoid_title <- renderText({
      if( length(geneinfo_id[, 1]) != 0 ) {
        HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the list (Click on a row to check the details of the selected gene)</b></font>')
      } else {}
    })
    
    #click gene 
    output$geneticIDstructure <- shiny::renderPlot({
      if ( is.null(input$geneinfoid_rows_selected) ) {
      } else {
        clickedid <- input$geneinfoid_rows_selected
        start <- as.numeric(data.frame(geneinfo_id[clickedid,])[1,3])
        end <- as.numeric(data.frame(geneinfo_id[clickedid,])[1,4])
        geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
        chr <- data.frame(geneinfo_id[clickedid,])[1,2]
        gff <- data.table::fread(paste0("./info/", genomenames, "/", genomenames, ".", chr, ".gene.structure.gff"), sep = "\t", data.table = FALSE)
        gff.mrna <- gff[gff$type == "mRNA", ]
        if ( nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
        } else {
          gff.reg.mrna <- gff.mrna[grep(geneid, gff.mrna$id), ]
          gff.reg <- gff[gff$id %in% gff.reg.mrna$id, ]
          
          gff.reg.mrna.ir <- IRanges::IRanges(gff.reg.mrna$start, gff.reg.mrna$end)
          gff.reg.mrna.op <- GenomicRanges::findOverlaps(gff.reg.mrna.ir, GenomicRanges::reduce(gff.reg.mrna.ir))
          gff.reg.mrna$grp <- S4Vectors::subjectHits(gff.reg.mrna.op)
          gff.reg.mrna.1 <- gff.reg.mrna %>% dplyr::group_by(grp) %>% dplyr::mutate(y = dplyr::row_number())
          gff.reg <- merge(gff.reg, gff.reg.mrna.1[, c("id", "y")], by="id")
          gff.reg$y <- gff.reg$y * 0.2 + 1
          plot.mrna.lst <- lapply(unique(gff.reg$id), function(i){
            dat <- gff.reg[gff.reg$id == i, ]
            i.strand <- dat$strand[1]
            dat.mrna <- dat[dat$type=="mRNA", ]
            return(dat.mrna)
          })
          plot.mrna <- do.call(rbind, plot.mrna.lst)
          
          if (nrow(plot.mrna) == 1) {
            p1 <- ggplot2::ggplot(plot.mrna, ggplot2::aes(x = end, y = y, label = id)) + 
              ggplot2::geom_rect(ggplot2::aes(xmin=start, xmax=end, ymin=y+0.118, ymax=y+0.122,
                                              text=anno), color="grey30", fill="grey30") + 
              ggplot2::geom_text(ggplot2::aes(label=id),hjust = -0.05, vjust = -3.0, size = 5) + ggplot2::xlim(start, end + abs(end - start)/4)
          } else {
            p1 <- ggplot2::ggplot(plot.mrna, ggplot2::aes(x = end, y = y, label = id)) + 
              ggplot2::geom_rect(ggplot2::aes(xmin=start, xmax=end, ymin=y+0.118, ymax=y+0.122,
                                              text=anno), color="grey30", fill="grey30") + 
              ggplot2::geom_text(ggplot2::aes(label=id),hjust = -0.05, vjust = -4.25, size = 5) + ggplot2::xlim(start, end + abs(end - start)/4)
          }
          
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
          
          #p1 <- p1 + ggplot2::ylim(1.18, 1.42)
          p1 <- p1 + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),panel.grid.minor = ggplot2::element_blank()) + 
            ggplot2::theme(panel.background = ggplot2::element_rect(fill="white",colour="white")) + ggplot2::xlab("") + ggplot2::ylab("") + 
            ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) + ggplot2::theme(axis.text.y = ggplot2::element_blank()) + 
            ggplot2::theme(axis.text = ggplot2::element_text(size=12), axis.title=ggplot2::element_text(size=14,face="bold"))
          
          nns <- nrow(plot.mrna)
          
          grid::grid.draw(ggplot2::ggplotGrob(p1)) 
        }
      }
    })
    
    output$geneticIDstructureif <- renderUI({
      if (is.null(input$geneinfoid_rows_selected)) {
      } else {
        clickedid <- input$geneinfoid_rows_selected
        start <- as.numeric(data.frame(geneinfo_id[clickedid,])[1,3])
        end <- as.numeric(data.frame(geneinfo_id[clickedid,])[1,4])
        geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
        chr <- data.frame(geneinfo_id[clickedid,])[1,2]
        gff <- data.table::fread(paste0("./info/", genomenames, "/", genomenames, ".", chr, ".gene.structure.gff"), sep = "\t", data.table = FALSE)
        
        gff.mrna <- gff[gff$type == "mRNA", ]
        gff.reg.mrna <- gff.mrna[grep(geneid, gff.mrna$id), ]
        if ( nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
        } else {
          nns <- nrow(gff.reg.mrna)
          hh <- paste0(nns*100, "px")
          plotOutput("geneticIDstructure", width = "100%", height = hh)
        }
      }
    })
    
    output$geneticIDstructure_title <- renderText({
      if (is.null(input$geneinfoid_rows_selected ) ) {
      } else {
        
        clickedid <- input$geneinfoid_rows_selected
        start <- as.numeric(data.frame(geneinfo_id[clickedid,])[1,3])
        end <- as.numeric(data.frame(geneinfo_id[clickedid,])[1,4])
        geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
        chr <- data.frame(geneinfo_id[clickedid,])[1,2]
        gff <- data.table::fread(paste0("./info/", genomenames, "/", genomenames, ".", chr, ".gene.structure.gff"), sep = "\t", data.table = FALSE)
        
        gff.mrna <- gff[gff$type == "mRNA", ]
        gff.reg.mrna <- gff.mrna[grep(geneid, gff.mrna$id), ]
        if ( nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
          
        } else {
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Gene structure</b></font>')
        }
      }
    })

    
    #gene sequence
    if( length(geneinfo_id[, 1]) != 0 ) {
      output$gene_id <- renderText({
        if (is.null(input$geneinfoid_rows_selected)) {
        } else {
          clickedid <- input$geneinfoid_rows_selected
          info <- data.frame(geneinfo_id[clickedid,])
          chr <- data.frame(geneinfo_id[clickedid,])[1,2]
          fastafile <- paste0("./info/", genomenames, "/", genomenames,".", chr,  ".fasta.gz")
          fasta <- Biostrings::readDNAStringSet(fastafile)
          info$seq[info$strand == "+"] <- as.character(Biostrings::subseq(fasta[info[info$strand == "+",]$chr], info[info$strand == "+",]$start, info[info$strand == "+",]$end))
          info$seq[info$strand == "-"] <- as.character(Biostrings::reverseComplement(Biostrings::subseq(fasta[info[info$strand == "-",]$chr], info[info$strand == "-",]$start, info[info$strand == "-",]$end)))
          gene <- Biostrings::DNAStringSet(info$seq)
          names(gene) <- paste0(info[1,1],  ":", as.numeric(info[1,3]), "-", as.numeric(info[1,4]), " length = ", as.numeric(info[4]) - as.numeric(info[1,3]) +1)
          tmp.d4 <- file.path(tempdir(), "d4.fa")
          Biostrings::writeXStringSet(gene, file = tmp.d4, width = 150)
          readLines(tmp.d4) 
        }
      }, sep = "\n")
      
      output$gene_title_id <- renderText({
        if (is.null(input$geneinfoid_rows_selected)) {
        } else {
          HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Gene sequence</b></font>")
        }
      })
      
      #cds sequence
      output$cds_id <- renderText({
        if (is.null(input$geneinfoid_rows_selected)) {
        } else {
          clickedid <- input$geneinfoid_rows_selected
          geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
          didcs <- cds.infoid[grep(geneid, names(cds.infoid))]
          tmp.f6 <- file.path(tempdir(), "d6.fa")
          Biostrings::writeXStringSet(didcs, file = tmp.f6, width = 150)
          readLines(tmp.f6)
        }
      }, sep = "\n")
      
      output$cds_title_id <- renderText({
        clickedid <- input$geneinfoid_rows_selected
        geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
        if(is.null(input$geneinfoid_rows_selected)) {
        } else if ( length(grep(geneid, names(cds.infoid))) != 0 ){
          HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>CDS sequence</b></font>")
        } else {}
      })
      
      #cdna sequence
      output$cdna_id <- renderText({
        if (is.null(input$geneinfoid_rows_selected)) {
        } else {
          clickedid <- input$geneinfoid_rows_selected
          geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
          ditcd <- cdna.infoid[grep(geneid, names(cdna.infoid))]
          tmp.f6 <- file.path(tempdir(), "d7.fa")
          Biostrings::writeXStringSet(ditcd, file = tmp.f6, width = 150)
          readLines(tmp.f6)
        }
      }, sep = "\n")
      
      output$cdna_title_id <- renderText({
        clickedid <- input$geneinfoid_rows_selected
        geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
        if(is.null(input$geneinfoid_rows_selected)){
        } else if ( length(grep(geneid, names(cdna.infoid))) != 0 ){
          HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>cDNA sequence</b></font>")
        } else {}
      })
      
      #protein
      output$pro_id <- shiny::renderText({
        if (is.null(input$geneinfoid_rows_selected)) {
        } else {
          clickedid <- input$geneinfoid_rows_selected
          geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
          ditp <- proteinid[grep(geneid, names(proteinid))]
          tmp.f7 <- file.path(tempdir(), "t7.fa")
          Biostrings::writeXStringSet(ditp, file = tmp.f7, width = 150)
          readLines(tmp.f7)
        }
      }, sep = "\n")
      
      output$pro_title_id <- renderText({
        clickedid <- input$geneinfoid_rows_selected
        geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
        if( is.null(input$geneinfoid_rows_selected) ) {
        } else if ( length(grep(geneid, names(proteinid))) != 0 ){
          HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>protein sequence</b></font>")
        } else {}
      })
      
      
      ##Functional annotation
      output$Functional_id <- DT::renderDT({
        DT::datatable(
          if (is.null(input$geneinfoid_rows_selected)){
          } else {
            clickedid <- input$geneinfoid_rows_selected
            geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
            chr <- data.frame(geneinfo_id[clickedid,])[1,2]
            Functionalfile <- paste0("./info/", genomenames, "/", genomenames, ".interproscan")
            gf <- data.table::fread(Functionalfile, sep = "\t", data.table = FALSE, header = F)
            gfinfo <- gf[grep(geneid, gf$V1), ]
            if( nrow(gfinfo) == 0){
              
            }else{
            
            colnames(gfinfo)[1] <- "Gene"
            colnames(gfinfo)[2] <- "Functional"
            gfinfo}
          }, extensions = "Buttons", 
          rownames = F, selection = "none",
          options = list( 
                         scrollX = TRUE, dom = 'Bfrtip', bSort = FALSE,  
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Functional", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Functional", sep = "-")),
                                        'print'),
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
                         
          )
          
        )}, server = FALSE)
      
      output$Functionaltitle_id <- renderText({
        clickedid <- input$geneinfoid_rows_selected
        geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
        clickedid <- input$geneinfoid_rows_selected
        geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
        chr <- data.frame(geneinfo_id[clickedid,])[1,2]
        Functionalfile <- paste0("./info/", genomenames, "/", genomenames, ".interproscan")
        gf <- data.table::fread(Functionalfile, sep = "\t", data.table = FALSE, header = F)
        gfinfo <- gf[grep(geneid, gf$V1), ]

        if(is.null(input$geneinfoid_rows_selected) | nrow(gfinfo) == 0) {
          
        } else {
          
          HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Functional annotation</b></font>")
        }
      })
      
      #click gff
      output$gffinfo_id <- DT::renderDT({
        DT::datatable(
          if (is.null(input$geneinfoid_rows_selected)){
          } else {
            clickedid <- input$geneinfoid_rows_selected
            geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
            chr <- data.frame(geneinfo_id[clickedid,])[1,2]
            gffile <- paste0("./info/", genomenames, "/", genomenames, ".",chr ,".gff.txt.gz")
            gf <- data.table::fread(gffile, sep = "\t", data.table = FALSE)
            gfinfo <- gf[grep(geneid, gf$Attributes), ]
            gfinfo <- gfinfo[, c(1:5, 9)]
            colnames(gfinfo)[1] <- "Chr"
            gfinfo
          }, extensions = "Buttons", 
          rownames = F, selection = "none",
          options = list(leftColumns = 8, 
                         scrollX = TRUE, dom = 'Bfrtip', bSort = FALSE,  
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("GFF", sep = "-")),
                                        list(extend = 'excel', filename =  paste("GFF", sep = "-")),
                                        'print'),
                         columnDefs = list(list(className = 'dt-left', targets = 0:4)),
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
                         
          )
        )}, server = FALSE)
      
      output$gffinfotitle_id <- renderText({
        clickedid <- input$geneinfoid_rows_selected
        geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
        if(is.null(input$geneinfoid_rows_selected)) {
        } else {
          HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Gene structure annotation</b></font>")
        }
      })
      
      output$sequence_id.txt <- downloadHandler(
        filename <- function() { paste('Region_sequence.txt') },
        content <- function(file) {
          ditd  <- Biostrings::subseq(fasta[names(fasta) == chr], as.numeric(start), as.numeric(end) )
          names(ditd) <- paste0(chr, ":", start, "-", end, " length = ", end - start +1)
          Biostrings::writeXStringSet(ditd, file, width = 150)
        }, contentType = 'text/plain'
      )
      
      #Gene sequence
      output$genesequence_BDG.txt <- downloadHandler(
        filename <- function() { paste('Gene_Sequcence.txt') },
        content <- function(file) {
          fastafile <- paste0("/data/SoybeanGDB/BLASTN/CDS/file/", genomenames, ".gene.fasta.gz")
          fasta <- Biostrings::readDNAStringSet(fastafile)
          geneid <- unlist(gene_list)
          geneseq <- fasta[names(fasta) %in% geneid ]
          Biostrings::writeXStringSet(geneseq, file, width = 150)
        }, contentType = 'text/plain'
      )
      
      output$downloadBDG01 <- renderUI({
        req(input$submit_GSID)
        column(12,
               downloadButton("genesequence_BDG.txt", "Gene Sequence", style = "width:100%;", class = "buttDown"),
               tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
        )
      })
      
      #cds sequence Download
      output$cdssequence_BDG.txt <- downloadHandler(
        filename <- function() { paste('CDS_sequence.txt') },
        content <- function(file) {
          didcs <- cds.infoid[unlist(lapply(gene_list, function(x){ grep(x, names(cds.infoid))}))]
          Biostrings::writeXStringSet(didcs, file, width = 150)
        }, contentType = 'text/plain'
      )
      
      output$downloadBDG02 <- renderUI({
        req(input$submit_GSID)
        
        column(12,
               downloadButton("cdssequence_BDG.txt", "CDS Sequence", style = "width:100%;", class = "buttDown"),
               tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
        )
      })
      
      #cdna sequence Download
      output$cdnasequence_BDG.txt <- downloadHandler(
        filename <- function() { paste('cDNA_sequence.txt') },
        content <- function(file) {
          ditcd <- cdna.infoid[unlist(lapply(gene_list, function(x){ grep(x, names(cdna.infoid))} ))]
          Biostrings::writeXStringSet(ditcd, file, width = 150)
        }, contentType = 'text/plain'
      )
      
      output$downloadBDG03 <- renderUI({
        req(input$submit_GSID)
        column(12,
               downloadButton("cdnasequence_BDG.txt", "cDNA Sequence", style = "width:100%;", class = "buttDown"),
               tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
        )
      })
      
      #proteion Download
      output$prosequence_BDG.txt <- downloadHandler(
        filename <- function() { paste('protein_sequence.txt') },
        content <- function(file) {
          ditp <- proteinid[unlist(lapply(gene_list, function(x){ grep(x, names(proteinid))} ))]
          Biostrings::writeXStringSet(ditp, file, width = 150)
        }, contentType = 'text/plain'
      )
      
      output$downloadBDG04 <- renderUI({
        req(input$submit_GSID)
        
        column(12,
               downloadButton("prosequence_BDG.txt", "protein Sequence", style = "width:100%;", class = "buttDown"),
               tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
        )
      })
      
    } else {
      output$downloadBDG01 <- renderUI({
        NULL
      })
      output$downloadBDG02 <- renderUI({
        NULL
      })
      output$downloadBDG03 <- renderUI({
        NULL
      })
      output$downloadBDG04 <- renderUI({
        NULL
      })
    }
  })
  
  ##reset
  observe({
    if (input$clearSERID >0) {
      isolate({
        updateTextAreaInput(session, "BDG_Paste", value="")
        updateTextAreaInput(session, "BDG_Paste1", value="")
        updateSelectInput(session, "variety_BDG", selected = "Zhonghuang 13")
      })
    } else {NULL}
  })
  
  #load example
  observe({
    if (input$SERExamID >0) {
      isolate({
        updateTextAreaInput(session, "BDG_Paste", value = "SoyZH13_03G069003\nSoyZH13_03G069001\nSoyZH13_03G069100\nSoyZH13_01G071100" )
        updateSelectInput(session, "variety_BDG", selected = "Zhonghuang 13")
      })
    } else {NULL}
  })
  
  #update
  SBGD <- reactive({
    req(input$variety_BDG)
    if( exists("genome.info") ){
    } else {
      gene_accession <- read.table("./data/gene_bgd.txt", sep = "\t", header = T, as.is = T)
      gene_accession[gene_accession$Accession == gsub(" ", "_", input$variety_BDG), ]
    }
  })
  
  observeEvent(SBGD(), {
    updateTextAreaInput(session, "BDG_Paste", value = gsub("\\\\n", "\\\n",SBGD()$ID))
  })
  
  #zhanwei3
  output$zhanwei3 <- renderUI({
    if (input$submit_GSID ){
      
    } else {
      plotOutput("zhanwei3", width = "100%", height = "600px")}
  })
  
  observeEvent(input$submit_GSIT, {
    #library(GenomicRanges)
    #library(IRanges)
    #library(Biostrings)
    load(paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".gene.info.RData"))
    geneinterval <- trimws(input$geneinterval, which = c("both"), whitespace = "[ \t\r\n]")
    chr <- gsub(":.+", "", geneinterval)
    start <- as.numeric(strsplit(gsub(".+:", "", geneinterval),"-")[[1]] )[1]
    end <- as.numeric( strsplit(gsub(".+:", "", geneinterval),"-")[[1]] )[2]
    if ( is.na(start) & is.na(end) ) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Please input genomic region in appropriate format!", type = "error",
        text = NULL
      )
    } else {
      if( !(chr %in% paste0("chr", 1:20)) & gsub(" ", "_", input$variety_IT) != "Glycine_dolichocarpa" )  {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Wrong chromosome ID!", type = "error",
          text = NULL
        )
        NULL
      } else {
        fasta <- Biostrings::readDNAStringSet(paste0("./info/",gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".",chr, ".fasta.gz"))
        
        #interval sequence
        if (start > Biostrings::width(fasta[chr]) | end > Biostrings::width(fasta[chr]) | end - start > 10000000 | !(end - start > 0) | is.na(end + start) ){
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Please input genomic region in appropriate format!", type = "error",
            text = NULL
          )
          NULL
        } else {
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)],]
          
          #fasta cds protein信息读取
          load(paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".cds.fasta.RData"))
          load(paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".protein.RData"))
          load(paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".cdna.fasta.RData"))
          gffile <- paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".",chr ,".gff.txt.gz")
          repeatdata <- read.table(paste0("./info/repeat/", gsub(" ", "_", input$variety_IT), ".",chr, ".repeat.gz"), sep = "\t", header = T)
          gf <- data.table::fread(gffile, sep = "\t", data.table = FALSE)
          Functionalfile <- paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".interproscan")
          functionaldata <- data.table::fread(Functionalfile, sep = "\t", data.table = FALSE, header = F)
          
          #geneinfo
          output$geneinfo <- DT::renderDT({
            DT::datatable(
              if( length(geneinfo[, 1]) != 0 ){
                sect <- "single"
                colnames(geneinfo) <- c("ID", "Chromosome", "Start", "End", "Strand")
                geneinfo
              } else {
                sect <- "none"
                geneinfo <- data.frame("V1"="No genes found in the input region!")
                colnames(geneinfo) <- ""
                geneinfo
              }, escape = FALSE, rownames= FALSE, selection=sect, 
              options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                             buttons = list('pageLength', 'copy', 
                                            list(extend = 'csv',   filename =  paste("searchlocation", sep = "-")),
                                            list(extend = 'excel', filename =  paste("searchloaction", sep = "-")),
                                            'print'), 
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                               "}")
              )
            )
          }, server = FALSE)
          
          output$geneinfo_title <- renderText({
            if( length(geneinfo[, 1]) != 0 ){
              HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the query genomic region (Click on a row to check the details of the selected gene)</b></font>')
            } else {}
          })
          
          gff <- data.table::fread(paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".", chr, ".gene.structure.gff"), sep = "\t", data.table = FALSE)
          
          #repeatmasker
          ggrepeat <- repeatdata[repeatdata$Start <= end & repeatdata$End >= start, ]
          output$repeatmasker <- DT::renderDT({
            DT::datatable(
              if(nrow(ggrepeat) == 0) {
              } else {
                colnames(ggrepeat)[1] <- "Chr"
                ggrepeat
              }, extensions = c("Buttons"), 
              rownames = F, selection = "none",
              options = list(leftColumns = 8, 
                             scrollX = TRUE, dom = 'Bfrtip', bSort = FALSE,  
                             buttons = list('pageLength', 'copy', 
                                            list(extend = 'csv',   filename =  paste("repeatmasker", geneinterval, sep = "-")),
                                            list(extend = 'excel', filename =  paste("repeatmasker", geneinterval, sep = "-")),
                                            'print'),
                             columnDefs = list(list(className = 'dt-left', targets = 0:4)),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                               "}")
              )
            )
          }, server = FALSE)
          
          output$repeatmasker_title_it <- renderText({
            if( nrow(ggrepeat) == 0 ) {
              
            } else {
              HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Transposable elements</b></font>')
            }
          })
          
          #click gene 
          output$geneticITstructure <- shiny::renderPlot({
            if ( is.null(input$geneinfo_rows_selected) ) {
            } else {
              clicked <- input$geneinfo_rows_selected
              start <- as.numeric(data.frame(geneinfo[clicked,])[1,3])
              end <- as.numeric(data.frame(geneinfo[clicked,])[1,4])
              geneid <- data.frame(geneinfo[clicked,])[1,1]
              gff.mrna <- gff[gff$type == "mRNA", ]
              if ( nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
              } else {
                gff.reg.mrna <- gff.mrna[grep(geneid, gff.mrna$id), ]
                gff.reg <- gff[gff$id %in% gff.reg.mrna$id, ]
                
                gff.reg.mrna.ir <- IRanges::IRanges(gff.reg.mrna$start, gff.reg.mrna$end)
                gff.reg.mrna.op <- GenomicRanges::findOverlaps(gff.reg.mrna.ir, GenomicRanges::reduce(gff.reg.mrna.ir))
                gff.reg.mrna$grp <- S4Vectors::subjectHits(gff.reg.mrna.op)
                
                gff.reg.mrna.1 <- gff.reg.mrna %>% dplyr::group_by(grp) %>% dplyr::mutate(y = dplyr::row_number())
                
                gff.reg <- merge(gff.reg, gff.reg.mrna.1[, c("id", "y")], by="id")
                
                gff.reg$y <- gff.reg$y * 0.2 + 1
                
                plot.mrna.lst <- lapply(unique(gff.reg$id), function(i){
                  dat <- gff.reg[gff.reg$id == i, ]
                  i.strand <- dat$strand[1]
                  
                  dat.mrna <- dat[dat$type=="mRNA", ]
                  return(dat.mrna)
                })
                plot.mrna <- do.call(rbind, plot.mrna.lst)
                
                if (nrow(plot.mrna) == 1) {
                  p1 <- ggplot2::ggplot(plot.mrna, ggplot2::aes(x = end, y = y, label = id)) + 
                    ggplot2::geom_rect(ggplot2::aes(xmin=start, xmax=end, ymin=y+0.118, ymax=y+0.122,
                                                    text=anno), color="grey30", fill="grey30") + 
                    ggplot2::geom_text(ggplot2::aes(label=id),hjust = -0.05, vjust = -3.0, size = 5) + ggplot2::xlim(start, end + abs(end - start)/4)
                } else {
                  p1 <- ggplot2::ggplot(plot.mrna, ggplot2::aes(x = end, y = y, label = id)) + 
                    ggplot2::geom_rect(ggplot2::aes(xmin=start, xmax=end, ymin=y+0.118, ymax=y+0.122,
                                                    text=anno), color="grey30", fill="grey30") + 
                    ggplot2::geom_text(ggplot2::aes(label=id),hjust = -0.05, vjust = -4.25, size = 5) + ggplot2::xlim(start, end + abs(end - start)/4)
                }
                
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
                
                #p1 <- p1 + ggplot2::ylim(1.18, 1.42)
                p1 <- p1 + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),panel.grid.minor = ggplot2::element_blank()) + 
                  ggplot2::theme(panel.background = ggplot2::element_rect(fill="white",colour="white")) + ggplot2::xlab("") + ggplot2::ylab("") + 
                  ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) + ggplot2::theme(axis.text.y = ggplot2::element_blank()) + 
                  ggplot2::theme(axis.text = ggplot2::element_text(size=12), axis.title=ggplot2::element_text(size=14,face="bold"))
                
                nnr <- nrow(plot.mrna)
                
                grid::grid.draw(ggplot2::ggplotGrob(p1)) 
              }
            }
          })
          
          output$geneticITstructureif <- renderUI({
            if (is.null(input$geneinfo_rows_selected)) {
            } else {
              clicked <- input$geneinfo_rows_selected
              start <- as.numeric(data.frame(geneinfo[clicked,])[1,3])
              end <- as.numeric(data.frame(geneinfo[clicked,])[1,4])
              geneid <- data.frame(geneinfo[clicked,])[1,1]
              gff.mrna <- gff[gff$type == "mRNA", ]
              gff.reg.mrna <- gff.mrna[grep(geneid, gff.mrna$id), ]
              nnr <- nrow(gff.reg.mrna)
              if ( nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
                
              } else {
                hh <- paste0(nnr*100, "px")
                plotOutput("geneticITstructure", width = "100%", height = hh)
              }
            }
          })
          
          output$geneticITstructure_title <- renderText({
            if (is.null(input$geneinfo_rows_selected ) ) {
              
            } else {
              clicked <- input$geneinfo_rows_selected
              start <- as.numeric(data.frame(geneinfo[clicked,])[1,3])
              end <- as.numeric(data.frame(geneinfo[clicked,])[1,4])
              geneid <- data.frame(geneinfo[clicked,])[1,1]
              gff.mrna <- gff[gff$type == "mRNA", ]
              gff.reg.mrna <- gff.mrna[grep(geneid, gff.mrna$id), ]
              nnr <- nrow(gff.reg.mrna)
              if ( nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
                
              } else {
                HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Gene structure</b></font>')
              }
            }
          })
          
          #gene sequence
          if( length(geneinfo[, 1]) != 0 ) {
            output$gene_it <- renderText({
              if (is.null(input$geneinfo_rows_selected)) {
              } else {
                clicked <- input$geneinfo_rows_selected
                info <- data.frame(geneinfo[clicked,])
                info$seq[info$strand == "+"] <- as.character(Biostrings::subseq(fasta[info[info$strand == "+",]$chr], info[info$strand == "+",]$start, info[info$strand == "+",]$end))
                info$seq[info$strand == "-"] <- as.character(Biostrings::reverseComplement(Biostrings::subseq(fasta[info[info$strand == "-",]$chr], info[info$strand == "-",]$start, info[info$strand == "-",]$end)))
                gene <- Biostrings::DNAStringSet(info$seq)
                names(gene) <- paste0(info[1,1],  ":", as.numeric(info[1,3]), "-", as.numeric(info[1,4]), " length = ", as.numeric(info[4]) - as.numeric(info[1,3]) +1)
                tmp.f4 <- file.path(tempdir(), "d4.fa")
                Biostrings::writeXStringSet(gene, file = tmp.f4, width = 150)
                readLines(tmp.f4) 
              }
            }, sep = "\n")
            
            output$gene_title_it <- renderText({
              if (is.null(input$geneinfo_rows_selected)) {
                
              } else {
                HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Gene sequence</b></font>")
              }
            })
            
            #cds sequence
            output$cds_it <- renderText({
              if (is.null(input$geneinfo_rows_selected)) {
              } else {
                clicked <- input$geneinfo_rows_selected
                geneid <- data.frame(geneinfo[clicked,])[1,1]
                
                if (length(grepl(geneid, names(cds.info))) == 0 ){
                } else { 
                  ditcs <- cds.info[grep(geneid, names(cds.info))]
                  tmp.f6 <- file.path(tempdir(), "t6.fa")
                  Biostrings::writeXStringSet(ditcs, file = tmp.f6, width = 150)
                  readLines(tmp.f6)
                }
              }
            }, sep = "\n")
            
            output$cds_title_it <- renderText({
              clicked <- input$geneinfo_rows_selected
              geneid <- data.frame(geneinfo[clicked,])[1,1]
              if(is.null(input$geneinfo_rows_selected)) {
              } else if ( length(grep(geneid, names(cds.info))) != 0 ){
                HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>CDS sequence</b></font>")
              } else {}
            })
            
            #cdna sequence
            output$cdna_it <- renderText({
              if (is.null(input$geneinfo_rows_selected)) {
              } else {
                clicked <- input$geneinfo_rows_selected
                geneid <- data.frame(geneinfo[clicked,])[1,1]
                ditcd <- cdna.info[grep(geneid, names(cdna.info))]
                tmp.f6 <- file.path(tempdir(), "t6.fa")
                Biostrings::writeXStringSet(ditcd, file = tmp.f6, width = 150)
                readLines(tmp.f6)
              }
            }, sep = "\n")
            
            output$cdna_title_it <- renderText({
              clicked <- input$geneinfo_rows_selected
              geneid <- data.frame(geneinfo[clicked,])[1,1]
              if(is.null(input$geneinfo_rows_selected)){
              } else if ( length(grep(geneid, names(cdna.info))) != 0 ){
                HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>cDNA sequence</b></font>")
              } else {}
            })
            
            #proteion
            output$pro_it <- shiny::renderText({
              if (is.null(input$geneinfo_rows_selected)) {
              } else {
                clicked <- input$geneinfo_rows_selected
                geneid <- data.frame(geneinfo[clicked,])[1,1]
                ditp <- protein[grep(geneid, names(protein))]
                tmp.f7 <- file.path(tempdir(), "t7.fa")
                Biostrings::writeXStringSet(ditp, file = tmp.f7, width = 150)
                readLines(tmp.f7)
              }
            }, sep = "\n")
            
            output$pro_title_it <- renderText({
              clicked <- input$geneinfo_rows_selected
              geneid <- data.frame(geneinfo[clicked,])[1,1]
              if( is.null(input$geneinfo_rows_selected) ) {
              } else if ( length(grep(geneid, names(protein))) != 0 ){
                HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Protein sequence</b></font>")
              } else {}
            })
            
            #click gff
            output$gffinfo_it <- DT::renderDT({
              DT::datatable(
                if (is.null(input$geneinfo_rows_selected)){
                } else {
                  clicked <- input$geneinfo_rows_selected
                  geneid <- data.frame(geneinfo[clicked,])[1,1]
                  gfinfo <- gf[grep(geneid, gf$Attributes), ]
                  gfinfo <- gfinfo[, c(1:5, 9)]
                  colnames(gfinfo)[1] <- "Chr"
                  gfinfo
                }, extensions = "Buttons", 
                rownames = F, selection = "none",
                options = list(leftColumns = 8, 
                               scrollX = TRUE, dom = 'Bfrtip', bSort = FALSE,  
                               buttons = list('pageLength', 'copy', 
                                              list(extend = 'csv',   filename =  paste("GFF", sep = "-")),
                                              list(extend = 'excel', filename =  paste("GFF", sep = "-")),
                                              'print'),
                               columnDefs = list(list(className = 'dt-left', targets = 0:4)),
                               initComplete = DT::JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                                 "}")
                               
                )
              )}, server = FALSE)
            
            output$gffinfotitle_it <- renderText({
              clicked <- input$geneinfo_rows_selected
              geneid <- data.frame(geneinfo[clicked,])[1,1]
              if (is.null(input$geneinfo_rows_selected)) {
              } else if (length(grep(geneid, gf$Attributes)) != 0 ){
                HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Gene structure annotation</b></font>")
              } else {}
            })
            
            output$Functional_it_id <- DT::renderDT({
              DT::datatable(
                if (is.null(input$geneinfo_rows_selected)){
                } else {
                  clicked <- input$geneinfo_rows_selected
                  geneid <- data.frame(geneinfo[clicked,])[1,1]
                  print(geneid)
                  funcinfo <- functionaldata[grep(geneid, functionaldata$V1), ]
                  if( nrow(funcinfo) == 0){
                  }else{
                    colnames(funcinfo)[1] <- "Gene"
                    colnames(funcinfo)[2] <- "Functional"
                    funcinfo
                  }
                }, extensions = "Buttons",
                rownames = F, selection = "none",
                options = list(
                  scrollX = TRUE, dom = 'Bfrtip', bSort = FALSE,
                  buttons = list('pageLength', 'copy',
                                 list(extend = 'csv',   filename =  paste("Functional_gene", sep = "-")),
                                 list(extend = 'excel', filename =  paste("Functional_gene", sep = "-")),
                                 'print'),
                  initComplete = DT::JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                    "}")
                  
                )
                
              )}, server = FALSE)
            
            output$Functional_it_title <- renderText({
              if (is.null(input$geneinfo_rows_selected)){
              } else {
                clicked <- input$geneinfo_rows_selected
                geneid <- data.frame(geneinfo[clicked,])[1,1]
                funcinfo <- functionaldata[grep(geneid, functionaldata$V1), ]
                if( nrow(funcinfo) == 0){
                }else{
                  HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Functional annotation</b></font>")
                }
              }
            })
            
            
            output$sequence_IT.txt <- downloadHandler(
              filename <- function() { paste('Region_sequence.txt') },
              content <- function(file) {
                ditf  <- Biostrings::subseq(fasta[names(fasta) == chr], as.numeric(start), as.numeric(end) )
                names(ditf) <- paste0(chr, ":", start, "-", end, " length = ", end - start +1)
                Biostrings::writeXStringSet(ditf, file, width = 150)
              }, contentType = 'text/plain'
            )
            
            output$downloadgenesit01 <- renderUI({
              req(input$submit_GSIT)
              column(12,
                     downloadButton("sequence_IT.txt", "Query region", style = "width:100%;", class = "buttDown"),
                     tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
              )
            })
            
            output$cdssequence_IT.txt <- downloadHandler(
              filename <- function() { paste('CDS_sequence.txt') },
              content <- function(file) {
                clicked <- input$geneinfo_rows_selected
                geneid <- data.frame(geneinfo[clicked,])[1,1]
                ditcs <- cds.info[grep(geneid, names(cds.info))]
                Biostrings::writeXStringSet(ditcs, file, width = 150)
              }, contentType = 'text/plain'
            )
            
            output$downloadgenesit02 <- renderUI({
              req(input$submit_GSIT)
              if(is.null(input$geneinfo_rows_selected)){
                
              } else {
                column(12,
                       downloadButton("cdssequence_IT.txt", "CDS Sequence", style = "width:100%;", class = "buttDown"),
                       tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
                )
              }
            })
            
            output$cdnasequence_IT.txt <- downloadHandler(
              filename <- function() { paste('cDNA_sequence.txt') },
              content <- function(file) {
                clicked <- input$geneinfo_rows_selected
                geneid <- data.frame(geneinfo[clicked,])[1,1]
                ditcd <- cdna.info[grep(geneid, names(cdna.info))]
                Biostrings::writeXStringSet(ditcd, file, width = 150)
              }, contentType = 'text/plain'
            )
            
            output$downloadgenesit03 <- renderUI({
              req(input$submit_GSIT)
              if(is.null(input$geneinfo_rows_selected)){
                
              } else {
                column(12,
                       downloadButton("cdnasequence_IT.txt", "cDNA Sequence", style = "width:100%;", class = "buttDown"),
                       tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
                )
              }
            })
            
            output$prosequence_IT.txt <- downloadHandler(
              filename <- function() { paste('Protein_sequence.txt') },
              content <- function(file) {
                clicked <- input$geneinfo_rows_selected
                geneid <- data.frame(geneinfo[clicked,])[1,1]
                ditp <- protein[grep(geneid, names(protein))]
                Biostrings::writeXStringSet(ditp, file, width = 150)
              }, contentType = 'text/plain'
            )
            
            output$downloadgenesit04 <- renderUI({
              req(input$submit_GSIT)
              if(is.null(input$geneinfo_rows_selected)){
                
              } else {
                column(12,
                       downloadButton("prosequence_IT.txt", "Protein Sequence", style = "width:100%;", class = "buttDown"),
                       tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
                )
              }
            })
          } else {
            output$sequence_IT.txt <- downloadHandler(
              filename <- function() { paste('Region_sequence.txt') },
              content <- function(file) {
                ditf  <- Biostrings::subseq(fasta[names(fasta) == chr], as.numeric(start), as.numeric(end) )
                names(ditf) <- paste0(chr, ":", start, "-", end, " length = ", end - start +1)
                Biostrings::writeXStringSet(ditf, file, width = 150)
              }, contentType = 'text/plain'
            )
            
            output$downloadgenesit01 <- renderUI({
              req(input$submit_GSIT)
              column(12,
                     downloadButton("sequence_IT.txt", "Query region", style = "width:100%;", class = "buttDown"),
                     tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
              )
            })
            
            output$cdssequence_IT.txt <- downloadHandler(
              filename <- function() { paste('CDS_sequence.txt') },
              content <- function(file) {
                writeLines("NO gene in this location", file)
              }, contentType = 'text/plain'
            )
            
            output$downloadgenesit02 <- renderUI({
              req(input$submit_GSIT)
              column(12,
                     downloadButton("cdssequence_IT.txt", "CDS Sequence", style = "width:100%;", class = "buttDown"),
                     tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
              )
            })
            
            output$cdnasequence_IT.txt <- downloadHandler(
              filename <- function() { paste('cDNA_sequence.txt') },
              content <- function(file) {
                writeLines("NO gene in this location", file)
              }, contentType = 'text/plain'
            )
            
            output$downloadgenesit03 <- renderUI({
              req(input$submit_GSIT)
              column(12,
                     downloadButton("cdnasequence_IT.txt", "cDNA Sequence", style = "width:100%;", class = "buttDown"),
                     tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
              )
            })
            
            output$prosequence_IT.txt <- downloadHandler(
              filename <- function() { paste('Protein_sequence.txt') },
              content <- function(file) {
                writeLines("NO gene in this location", file)
              }, contentType = 'text/plain'
            )
            
            output$downloadgenesit04 <- renderUI({
              req(input$submit_GSIT)
              column(12,
                     downloadButton("prosequence_IT.txt", "Protein Sequence", style = "width:100%;", class = "buttDown"),
                     tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
              )
            })
            
            
          }
        }
      }
    }
  })
  
  observe({
    if (input$clearSERIT>0) {
      isolate({
        updateTextInput(session, "geneinterval", value = "")
      })
    } else {NULL}
  })
  
  observe({
    if (input$SERExamIT >0) {
      isolate({
        updateSelectInput(session, "variety_IT", selected = "Zhonghuang 13")
        updateTextInput(session, "geneinterval", value = "chr1:20260371-20686979")
      })
    } else {NULL}
  })
  
  #IT update
  SIT <- reactive({
    req(input$variety_IT)
    if( exists("genome.info") ){
    } else {
      gene_accession <- read.table("./data/gene_location.txt", sep = "\t", header = T, as.is = T)
      gene_accession[gene_accession$Accession == gsub(" ", "_", input$variety_IT), ]
    }
  })
  observeEvent(SIT(), {
    updateTextInput(session, "geneinterval", value = unique(SIT()$LOC))
  })
  
  #zhanwei2
  output$zhanwei2 <- renderUI({
    if (input$submit_GSIT ) {
      
    } else {
      plotOutput("zhanwei2", width = "100%", height = "600px")}
  })
  
  #Genomic distribution of genes
  observeEvent(input$GL_sbumit, {
    load(paste0("./info/", gsub(" ", "_", input$variety_GL), "/", gsub(" ", "_", input$variety_GL), ".gene.info.RData"))
    
    gene_list <- unlist(strsplit(input$GL_Paste, split="\\n"))
    gene_list <- trimws(gene_list, which = c("both"), whitespace = "[ \t\r\n]")
    gene_list <- gene_list[gene_list %in% gene_info_s$id]
    #geneinfo
    geneinfo <- gene_info_s[gene_info_s$id %in% gene_list, ]
    
    #gene sequence
    if( length(gene_list) != 0 ) {
      if(gsub(" ", "_", input$variety_GL) == "Glycine_dolichocarpa"){
        
        chrsize <- read.table(paste0("./info/", gsub(" ", "_", input$variety_GL), "/", "chrsize.txt"), sep = "\t", header = T)
        chrsize$chr <- factor(chrsize$chr, levels = paste0("chr", 1:40) )
        
        output$genelocplotif <- renderUI({
          if (input$genelocwidth >= 400 & input$genelocheight >= 400 ){
            plotOutput("GLplot", width = paste0(input$genelocwidth, "px"), height = paste0(input$genelocheight, "px"))
          } else {
            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Error input!", type = "error",
              text = "Width or height must be greater than 400px!"
            )
            NULL
          }
        })
        
        output$GLplot <- renderPlot({
          op <- par(family = "serif")
          circlize::circos.par("track.height" = 0.1, track.margin=c(0,0))
          circlize::circos.genomicInitialize(chrsize, labels.cex = 1.5, axis.labels.cex = 1, major.by = 50000000)
          circlize::circos.genomicTrackPlotRegion(ylim = c(0, 1),bg.col = rep(c("#2DA1D0FF", "#6F51FFFF"), 20), bg.border = NA, track.height = 0.1)	
          circlize::circos.genomicLabels(geneinfo[, c(2, 3, 4, 1)], labels.column = 4, side = "inside", cex = 1)
          circlize::circos.clear()
          par(op)
        })
        
      }else{
        
        chrsize <- read.table(paste0("./info/", gsub(" ", "_", input$variety_GL), "/", "chrsize.txt"), sep = "\t", header = T)
        chrsize$chr <- factor(chrsize$chr, levels = paste0("chr", 1:20) )
        
        output$genelocplotif <- renderUI({
          if (input$genelocwidth >= 400 & input$genelocheight >= 400 ){
            plotOutput("GLplot", width = paste0(input$genelocwidth, "px"), height = paste0(input$genelocheight, "px"))
          } else {
            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Error input!", type = "error",
              text = "Width or height must be greater than 400px!"
            )
            NULL
          }
        })
        
        output$GLplot <- renderPlot({
          op <- par(family = "serif")
          circlize::circos.par("track.height" = 0.1, track.margin=c(0,0))
          circlize::circos.genomicInitialize(chrsize, labels.cex = 1.5, axis.labels.cex = 1, major.by = 50000000)
          circlize::circos.genomicTrackPlotRegion(ylim = c(0, 1),bg.col = rep(c("#2DA1D0FF", "#6F51FFFF"), 10), bg.border = NA, track.height = 0.1)	
          circlize::circos.genomicLabels(geneinfo[, c(2, 3, 4, 1)], labels.column = 4, side = "inside", cex = 1)
          circlize::circos.clear()
          par(op)
        })
        
      }
    } else {      
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input at least one Gene ID!"
      )
    }
  })
  
  output$downloadgeneloc1 <- renderUI({
    req(input$GL_sbumit)
    column(12,
           downloadButton("downloadSL.pdf", "PDF-file", style = "width:100%;", class = "buttDown"),
           tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
    )
  })
  
  output$downloadSL.pdf <- downloadHandler(
    filename <- function() { paste('Geneloc.pdf') },
    content <- function(file) {
      load(paste0("./info/", gsub(" ", "_", input$variety_GL), "/", gsub(" ", "_", input$variety_GL), ".gene.info.RData"))
      gene_list <- unlist(strsplit(input$GL_Paste, split="\\n"))
      gene_list <- trimws(gene_list, which = c("both"), whitespace = "[ \t\r\n]")
      gene_list <- gene_list[gene_list %in% gene_info_s$id]
      #geneinfo
      geneinfo <- gene_info_s[gene_info_s$id %in% gene_list, ]
      chrsize <- read.table(paste0("./info/", gsub(" ", "_", input$variety_GL), "/", "chrsize.txt"), sep = "\t", header = T)
      if(gsub(" ", "_", input$variety_GL) == "Glycine_dolichocarpa"){
        chrsize$chr <- factor(chrsize$chr, levels = paste0("chr", 1:40) )
      }else{
        chrsize$chr <- factor(chrsize$chr, levels = paste0("chr", 1:20) ) 
      }
      
      
      pdf(file, width = 900/72, height = 900/72)
      op <- par(family = "serif")
      circlize::circos.par("track.height" = 0.1, track.margin=c(0,0))
      circlize::circos.genomicInitialize(chrsize, labels.cex = 1.5, axis.labels.cex = 1, major.by = 50000000)
      if(gsub(" ", "_", input$variety_GL) == "Glycine_dolichocarpa"){
      circlize::circos.genomicTrackPlotRegion(ylim = c(0, 1),bg.col = rep(c("#2DA1D0FF", "#6F51FFFF"), 20), bg.border = NA, track.height = 0.1)	
      }else{
      circlize::circos.genomicTrackPlotRegion(ylim = c(0, 1),bg.col = rep(c("#2DA1D0FF", "#6F51FFFF"), 10), bg.border = NA, track.height = 0.1)	
      }
      circlize::circos.genomicLabels(geneinfo[, c(2, 3, 4, 1)], labels.column = 4, side = "inside", cex = 1)
      circlize::circos.clear()
      par(op)
      dev.off()
    }, contentType = 'application/pdf')
  
  
  output$downloadgeneloc2 <- renderUI({
    req(input$GL_sbumit)
    column(12,
           downloadButton("downloadGL.svg", "SVG-file", style = "width:100%;", class = "buttDown"),
           tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
    )
  })
  
  output$downloadGL.svg <- downloadHandler(
    filename <- function() { paste('Geneloc.svg') },
    content <- function(file) {
      load(paste0("./info/", gsub(" ", "_", input$variety_GL), "/", gsub(" ", "_", input$variety_GL), ".gene.info.RData"))
      gene_list <- unlist(strsplit(input$GL_Paste, split="\\n"))
      gene_list <- trimws(gene_list, which = c("both"), whitespace = "[ \t\r\n]")
      gene_list <- gene_list[gene_list %in% gene_info_s$id]
      #geneinfo
      geneinfo <- gene_info_s[gene_info_s$id %in% gene_list, ]
      chrsize <- read.table(paste0("./info/", gsub(" ", "_", input$variety_GL), "/", "chrsize.txt"), sep = "\t", header = T)
      if(gsub(" ", "_", input$variety_GL) == "Glycine_dolichocarpa"){
        chrsize$chr <- factor(chrsize$chr, levels = paste0("chr", 1:40) )
      }else{
        chrsize$chr <- factor(chrsize$chr, levels = paste0("chr", 1:20) ) 
      }
      svg(file, width = 900/72, height = 900/72)
      op <- par(family = "serif")
      circlize::circos.par("track.height" = 0.1, track.margin=c(0,0))
      circlize::circos.genomicInitialize(chrsize, labels.cex = 1.5, axis.labels.cex = 1, major.by = 50000000)
      if(gsub(" ", "_", input$variety_GL) == "Glycine_dolichocarpa"){
        circlize::circos.genomicTrackPlotRegion(ylim = c(0, 1),bg.col = rep(c("#2DA1D0FF", "#6F51FFFF"), 20), bg.border = NA, track.height = 0.1)	
      }else{
        circlize::circos.genomicTrackPlotRegion(ylim = c(0, 1),bg.col = rep(c("#2DA1D0FF", "#6F51FFFF"), 10), bg.border = NA, track.height = 0.1)	
      }
      circlize::circos.genomicLabels(geneinfo[, c(2, 3, 4, 1)], labels.column = 4, side = "inside", cex = 1)
      circlize::circos.clear()
      par(op)
      dev.off()
    }, contentType = 'image/svg')
  
  ##reset
  observe({
    if (input$cleargl >0) {
      isolate({
        updateTextAreaInput(session, "GL_Paste", value="")
      })
    } else {NULL}
  })
  
  #load example
  observe({
    if (input$glExam >0) {
      isolate({
        updateTextAreaInput(session, "GL_Paste", value = "SoyZH13_20G124201\nSoyZH13_14G131103\nSoyZH13_10G049500\nSoyZH13_01G054202\nSoyZH13_13G043301\nSoyZH13_10G214500\nSoyZH13_05G061800\nSoyZH13_11G118400\nSoyZH13_02G038800\nSoyZH13_16G155300\nSoyZH13_13G319400\nSoyZH13_19G153500\nSoyZH13_20G135100\nSoyZH13_18G142622\nSoyZH13_12G186900\nSoyZH13_11G123500\nSoyZH13_18G024500\nSoyZH13_20G078300\nSoyZH13_02G231900\nSoyZH13_03G179600" )
        updateSelectInput(session, "variety_GL", selected = "Zhonghuang 13")
      })
    } else {NULL}
  })
  
  #update
  SBGL <- reactive({
    req(input$variety_GL)
    if( exists("genome.info") ){
    } else {
      gene_accession <- read.table("./data/geneloc.txt", sep = "\t", header = T, as.is = T)
      gene_accession[gene_accession$Accession == gsub(" ", "_", input$variety_GL), ]
    }
  })
  observeEvent(SBGL(), {
    updateTextAreaInput(session, "GL_Paste", value = gsub("\\\\n", "\\\n",SBGL()$ID))
  })
  
  
  #Transcription factors/regulators
  observe({
    #library(GenomicRanges)
    #library(IRanges)
    #library(Biostrings)
    load(paste0("./info/", gsub(" ", "_", input$variety_trf), "/", gsub(" ", "_", input$variety_trf), ".gene.info.RData"))
    
    #tfinfo
    tfinfo <- read.table( paste0("./info/", gsub(" ", "_", input$variety_trf), "/tf_classification.txt"), sep = "\t", header = F )
    
    output$trfaccessions <- renderUI({
      tags$div(align = 'left',
               class = 'multicol', style = "width: 100%",
               checkboxGroupInput(
                 inputId = "Id001",
                 label = NULL, 
                 choices = paste0(data.frame(table(tfinfo$V2))[,1], "(", data.frame(table(tfinfo$V2))[,2], ")"),
                 selected = paste0(data.frame(table(tfinfo$V2))[1,1], "(", data.frame(table(tfinfo$V2))[1,2], ")"),
                 inline = FALSE,
               )
      )
    })
    
    output$trfaccessions_title <- renderText({
      HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Annotated transcription factors/regulators</b></font>")
    })
    
    output$trftitle_BDG <- renderText({
      HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Click on a row to check the details of the selected gene</b></font>")
    }) 
    output$trf_BDG <- DT::renderDT({
      tfinfo$V3 <- gsub("TR", "Transcriptional Regulator", tfinfo$V3)
      tfinfo$V3 <- gsub("TF", "Transcription Factor", tfinfo$V3)
      colnames(tfinfo) <- c("ID", "Subfamily", "TR/TF", "Family")
      DT::datatable(
        tfinfo[tfinfo[,2] %in% gsub("\\(.+", "", input$Id001), ], 
        escape = FALSE, rownames= FALSE, selection="single", 
        options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                       buttons = list('pageLength', 'copy', 
                                      list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                      list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                      'print'), 
                       initComplete = DT::JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                         "}")
        )
      )
    }, server = FALSE)
    
    #fasta cds protein信息读取
    load(paste0("./info/", gsub(" ", "_", input$variety_trf), "/", gsub(" ", "_", input$variety_trf), ".cds.fasta.RData"))
    load(paste0("./info/", gsub(" ", "_", input$variety_trf), "/", gsub(" ", "_", input$variety_trf), ".protein.RData"))
    load(paste0("./info/", gsub(" ", "_", input$variety_trf), "/", gsub(" ", "_", input$variety_trf), ".cdna.fasta.RData"))
    
    #gene sequence
    output$gene_trf <- renderText({
      if (is.null(input$trf_BDG_rows_selected)) {
      } else {
        tt <- tfinfo[tfinfo[,2] %in% gsub("\\(.+", "", input$Id001), ]
        clicked <- input$trf_BDG_rows_selected
        geneid <- tt[clicked, 1]
        info <- data.frame(gene_info_s[gene_info_s$id == geneid,])
        fasta <- Biostrings::readDNAStringSet(paste0("./info/", gsub(" ", "_", input$variety_trf), "/", gsub(" ", "_", input$variety_trf),".", info[1,2],".fasta.gz"))
        info$seq[info$strand == "+"] <- as.character(Biostrings::subseq(fasta[info[info$strand == "+",]$chr], info[info$strand == "+",]$start, info[info$strand == "+",]$end))
        info$seq[info$strand == "-"] <- as.character(Biostrings::reverseComplement(Biostrings::subseq(fasta[info[info$strand == "-",]$chr], info[info$strand == "-",]$start, info[info$strand == "-",]$end)))
        gene <- Biostrings::DNAStringSet(info$seq)
        names(gene) <- paste0(info[1,1],  ":", as.numeric(info[1,3]), "-", as.numeric(info[1,4]), " length = ", as.numeric(info[4]) - as.numeric(info[1,3]) +1)
        tmp.f4 <- file.path(tempdir(), "t4.fa")
        Biostrings::writeXStringSet(gene, file = tmp.f4, width = 150)
        readLines(tmp.f4) 
      }
    }, sep = "\n")
    
    output$gene_title_trf <- renderText({
      if (is.null(input$trf_BDG_rows_selected)) {
        
      } else {
        HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Gene sequence</b></font>")
      }
    })
    
    #description
    output$description_title_trf <- renderText({
      tt <- tfinfo[tfinfo[,2] %in% gsub("\\(.+", "", input$Id001), ]
      clicked <- input$trf_BDG_rows_selected
      geneid <- tt[clicked, 1]
      if(is.null(input$trf_BDG_rows_selected)) {
      } else if ( length(grep(geneid, names(cds.info))) != 0 ){
        
        HTML(paste0('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Description of ', tt[clicked, 2],' transcription factor</b></font>'))
      } else {}
    })
    
    output$description_trf <- renderText({
      if (is.null(input$trf_BDG_rows_selected)) {
      } else {
        tt <- tfinfo[tfinfo[,2] %in% gsub("\\(.+", "", input$Id001), ]
        clicked <- input$trf_BDG_rows_selected
        TFANDTR <- tt[clicked, 2]
        if ( tt[clicked, 3] == "TF"){
          descriptiontf <- data.table::fread("./info/description_of_transcript_factors.txt", sep = "\t", header = T, data.table = F)
          descr <- descriptiontf[descriptiontf$TF == TFANDTR, ]
        } else {
          descriptiontf <- data.table::fread("./info/description_of_transcriptional_regulator.txt", sep = "\t", header = T, data.table = F)
          descr <- descriptiontf[descriptiontf$TR == TFANDTR, ]
        }
        paste(descr[1,2], descr[1,3], descr[1,4], sep = "\n") 
      }
    })
    
    #cds
    output$cds_title_trf <- renderText({
      tt <- tfinfo[tfinfo[,2] %in% gsub("\\(.+", "", input$Id001), ]
      clicked <- input$trf_BDG_rows_selected
      geneid <- tt[clicked, 1]
      if (is.null(input$trf_BDG_rows_selected)) {
      } else if ( length(grep(geneid, names(cds.info))) != 0 ){
        HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>CDS sequence</b></font>")
      } else {}
    })
    
    output$cds_trf <- renderText({
      if (is.null(input$trf_BDG_rows_selected)) {
      } else {
        tt <- tfinfo[tfinfo[,2] %in% gsub("\\(.+", "", input$Id001), ]
        clicked <- input$trf_BDG_rows_selected
        geneid <- tt[clicked, 1]
        ditcs <- cds.info[grep(geneid, names(cds.info))]
        tmp.f6 <- file.path(tempdir(), "t6.fa")
        Biostrings::writeXStringSet(ditcs, file = tmp.f6, width = 150)
        readLines(tmp.f6)
      }
    }, sep = "\n")
    
    #cdna sequence
    output$cdna_trf <- renderText({
      if (is.null(input$trf_BDG_rows_selected)) {
      } else {
        tt <- tfinfo[tfinfo[,2] %in% gsub("\\(.+", "", input$Id001), ]
        clicked <- input$trf_BDG_rows_selected
        geneid <- tt[clicked, 1]
        ditcd <- cdna.info[grep(geneid, names(cdna.info))]
        tmp.f6 <- file.path(tempdir(), "t6.fa")
        Biostrings::writeXStringSet(ditcd, file = tmp.f6, width = 150)
        readLines(tmp.f6)
      }
    }, sep = "\n")
    
    output$cdna_title_trf <- renderText({
      tt <- tfinfo[tfinfo[,2] %in% gsub("\\(.+", "", input$Id001), ]
      clicked <- input$trf_BDG_rows_selected
      geneid <- tt[clicked, 1]
      if(is.null(input$trf_BDG_rows_selected)){
      } else if ( length(grep(geneid, names(cdna.info))) != 0 ){
        HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>cDNA sequence</b></font>")
      } else {}
    })
    
    #proteion
    output$pro_trf <- shiny::renderText({
      if (is.null(input$trf_BDG_rows_selected)) {
      } else {
        tt <- tfinfo[tfinfo[,2] %in% gsub("\\(.+", "", input$Id001), ]
        clicked <- input$trf_BDG_rows_selected
        geneid <- tt[clicked, 1]
        ditp <- protein[grep(geneid, names(protein))]
        tmp.f7 <- file.path(tempdir(), "t7.fa")
        Biostrings::writeXStringSet(ditp, file = tmp.f7, width = 150)
        readLines(tmp.f7)
      }
    }, sep = "\n")
    
    output$pro_title_trf <- renderText({
      tt <- tfinfo[tfinfo[,2] %in% gsub("\\(.+", "", input$Id001), ]
      clicked <- input$trf_BDG_rows_selected
      geneid <- tt[clicked, 1]
      if( is.null(input$trf_BDG_rows_selected) ) {
      } else if ( length(grep(geneid, names(protein))) != 0 ){
        HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Protein sequence</b></font>")
      } else {}
    })
    
    #click gff
    output$gffinfo_trf <- DT::renderDT({
      DT::datatable(
        if (is.null(input$trf_BDG_rows_selected)){
        } else {
          tt <- tfinfo[tfinfo[,2] %in% gsub("\\(.+", "", input$Id001), ]
          clicked <- input$trf_BDG_rows_selected
          geneid <- tt[clicked, 1]
          gffile <- paste0("./info/", gsub(" ", "_", input$variety_trf), "/", gsub(" ", "_", input$variety_trf), ".",gene_info_s[gene_info_s$id == geneid, 2] ,".gff.txt.gz")
          gf <- data.table::fread(gffile, sep = "\t", data.table = FALSE)
          gfinfo <- gf[grep(geneid, gf$Attributes), ]
          gfinfo <- gfinfo[, c(1:5, 9)]
          colnames(gfinfo)[1] <- "Chr"
          gfinfo
        }, extensions = "Buttons", 
        rownames = F, selection = "none",
        options = list(leftColumns = 8, 
                       scrollX = TRUE, dom = 'Bfrtip', bSort = FALSE,  
                       buttons = list('pageLength', 'copy', 
                                      list(extend = 'csv',   filename =  paste("GFF", sep = "-")),
                                      list(extend = 'excel', filename =  paste("GFF", sep = "-")),
                                      'print'),
                       columnDefs = list(list(className = 'dt-left', targets = 0:4)),
                       initComplete = DT::JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                         "}")
                       
        )
      )}, server = FALSE)
    
    output$gffinfotitle_trf <- renderText({
      if(is.null(input$trf_BDG_rows_selected)) {
      } else {
        HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Gene structure annotation</b></font>")
      }
    })
  })
  
  #Genome synteny
  observeEvent(input$submit_gsy, {
    genomename <- input$variety_gsy1
    if (input$variety_gsy1 == "Zhonghuang 13") {
      comparisongenome <- input$variety_gsy21
    } else if (input$variety_gsy1 == "Williams 82") {
      comparisongenome <- input$variety_gsy22
    }
    
    chromosome <- input$variety_gsy3
    filename <- gsub(" ", "_", paste0("./info/geneSyntenic/", genomename, "and", comparisongenome, ".allout.SYN.gz"))
    
    gsytable <- read.table(filename, sep = "\t", as.is = T)
    gsytable <- gsytable[gsytable[, 4] == gsub(" ", "_", paste0(comparisongenome, "_", chromosome) ), ]
    gsytable <- gsytable[, c(1:6)]
    gsytable$V1 <- gsub("_chr", ", chr", gsytable$V1)
    gsytable$V1 <- gsub("_", " ", gsytable$V1)
    gsytable$V4 <- gsub("_chr", ", chr", gsytable$V4)
    gsytable$V4 <- gsub("_", " ", gsytable$V4)
    colnames(gsytable) <- c("Reference chromosome", "Reference start", "Reference end", "Query chromosome", "Query start", "Query end")
    if (nrow(gsytable) == 0) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "No Genome synteny!"
      )
      output$gsy_BDG <- DT::renderDT({
        DT::datatable(
          gsytable, 
          extensions = c("Buttons"), 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         columnDefs=list(list(targets="_all", class="dt-center", 
                                              render = DT::JS(
                                                "function(data, type, row, meta) {",
                                                "return type === 'display' && data.length > 30 ?",
                                                "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                "}"))),
                         scrollX = TRUE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Syntenic_Blocks", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Syntenic_Blocks", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
    } else {
      output$gsy_BDG <- DT::renderDT({
        DT::datatable(
          gsytable, 
          extensions = c("Buttons"), 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         columnDefs=list(list(targets="_all", class="dt-center", 
                                              render = DT::JS(
                                                "function(data, type, row, meta) {",
                                                "return type === 'display' && data.length > 30 ?",
                                                "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                "}"))),
                         scrollX = TRUE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Syntenic_Blocks", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Syntenic_Blocks", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$gsy_BDG_title <- renderText({
        if( nrow(gsytable) != 0 ){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Syntenic Blocks</b></font>')
        } else {}
      })
      
      output$genomedata1_title <- renderText({
        if( !is.null(input$gsy_BDG_rows_selected)){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the reference genome</b></font>')
        } else {}
      })
      
      output$genomedata1 <- DT::renderDT({
        if (is.null(input$gsy_BDG_rows_selected)) {
        } else {
          load( paste0("./info/", gsub(" ", "_", genomename), "/", gsub(" ", "_", genomename), ".gene.info.RData") )
          clicked <- input$gsy_BDG_rows_selected
          chr <- chromosome
          start <- as.numeric(gsytable[clicked, 2] )
          end <- as.numeric( gsytable[clicked, 3] )
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)], ]
          DT::datatable(
            geneinfo, 
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                          list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE)
      
      output$genomedata2_title <- renderText({
        if( !is.null(input$gsy_BDG_rows_selected) ){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the query genome</b></font>')
        } else {}
      })
      
      output$genomedata2 <- DT::renderDT({
        if (is.null(input$gsy_BDG_rows_selected)) {
        } else {
          load( paste0("./info/", gsub(" ", "_", comparisongenome), "/", gsub(" ", "_", comparisongenome), ".gene.info.RData") )
          clicked <- input$gsy_BDG_rows_selected
          chr <- chromosome
          start <- as.numeric(gsytable[clicked, 5] )
          end <- as.numeric( gsytable[clicked, 6] )
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)], ]
          DT::datatable(
            geneinfo, 
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                          list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE) 
    }
  })
  
  output$zhanwei5 <- renderUI({
    if (input$submit_gsy ){
      
    } else {
      plotOutput("zhanwei5", width = "100%", height = "600px")}
  })
  
  #gene structure variations
  observeEvent( input$submit_gsv, {
    genomename <- input$variety_gsv1
    if (input$variety_gsv1 == "Zhonghuang 13") {
      comparisongenome <- input$variety_gsv21
    } else if (input$variety_gsv1 == "Williams 82") {
      comparisongenome <- input$variety_gsv22
    }
    
    chromosome <- input$variety_gsv3
    #DUP
    DELfilename <- gsub(" ", "_", paste0("./info/genestructurevariation/DUP/", genomename, "and", comparisongenome, ".allout.DUP.gz"))
    
    duptable <- read.table(DELfilename, sep = "\t", as.is = T)
    duptable <- duptable[duptable[, 4] == gsub(" ", "_", paste0(comparisongenome, "_", chromosome) ), ]
    duptable <- duptable[, c(1:6)]
    duptable$V1 <- gsub("_chr", ", chr", duptable$V1)
    duptable$V1 <- gsub("_", " ", duptable$V1)
    duptable$V4 <- gsub("_chr", ", chr", duptable$V4)
    duptable$V4 <- gsub("_", " ", duptable$V4)
    colnames(duptable) <- c("Reference chromosome", "Reference start", "Reference end", "Query chromosome", "Query start", "Query end")
    if (nrow(duptable) == 0){
      output$dupresult <- DT::renderDT({
        DT::datatable(
          duptable, 
          extensions = c("Buttons"), 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         columnDefs=list(list(targets="_all", class="dt-center")), scrollX = TRUE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Syntenic_Blocks", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Syntenic_Blocks", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
    } else {
      output$dupresult <- DT::renderDT({
        DT::datatable(
          duptable, 
          extensions = c("Buttons"), 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         columnDefs=list(list(targets="_all", class="dt-center")), scrollX = TRUE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Syntenic_Blocks", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Syntenic_Blocks", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$dupresult_title <- renderText({
        if( nrow(duptable) != 0 ){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Syntenic Blocks</b></font>')
        } else {}
      })
      
      output$dupdata1_title <- renderText({
        if( !is.null(input$dupresult_rows_selected)){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the reference genome</b></font>')
        } else {}
      })
      
      output$dupdata1 <- DT::renderDT({
        if (is.null(input$dupresult_rows_selected)) {
        } else {
          load( paste0("./info/", gsub(" ", "_", genomename), "/", gsub(" ", "_", genomename), ".gene.info.RData") )
          clicked <- input$dupresult_rows_selected
          chr <- chromosome
          start <- as.numeric(duptable[clicked, 2] )
          end <- as.numeric( duptable[clicked, 3] )
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)], ]
          DT::datatable(
            geneinfo, 
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                          list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE)
      
      output$dupdata2_title <- renderText({
        if( !is.null(input$dupresult_rows_selected) ){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the query genome</b></font>')
        } else {}
      })
      
      output$dupdata2 <- DT::renderDT({
        if (is.null(input$dupresult_rows_selected)) {
        } else {
          load( paste0("./info/", gsub(" ", "_", comparisongenome), "/", gsub(" ", "_", comparisongenome), ".gene.info.RData") )
          clicked <- input$dupresult_rows_selected
          chr <- chromosome
          start <- as.numeric(duptable[clicked, 5] )
          end <- as.numeric( duptable[clicked, 6] )
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)], ]
          DT::datatable(
            geneinfo, 
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                          list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE) 
    }
    
    #INV FILE
    INVfilename <- gsub(" ", "_", paste0("./info/genestructurevariation/INV/", genomename, "and", comparisongenome, ".allout.INV.gz"))
    
    invtable <- read.table(INVfilename, sep = "\t", as.is = T)
    invtable <- invtable[invtable[, 4] == gsub(" ", "_", paste0(comparisongenome, "_", chromosome) ), ]
    invtable <- invtable[, c(1:6)]
    invtable$V1 <- gsub("_chr", ", chr", invtable$V1)
    invtable$V1 <- gsub("_", " ", invtable$V1)
    invtable$V4 <- gsub("_chr", ", chr", invtable$V4)
    invtable$V4 <- gsub("_", " ", invtable$V4)
    colnames(invtable) <- c("Reference chromosome", "Reference start", "Reference end", "Query chromosome", "Query start", "Query end")
    if (nrow(invtable) == 0){
      output$invresult <- DT::renderDT({
        DT::datatable(
          invtable, 
          extensions = c("Buttons"), 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         columnDefs=list(list(targets="_all", class="dt-center")), scrollX = TRUE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Syntenic_Blocks", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Syntenic_Blocks", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
    } else {
      output$invresult <- DT::renderDT({
        DT::datatable(
          invtable, 
          extensions = c("Buttons"), 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         columnDefs=list(list(targets="_all", class="dt-center")), scrollX = TRUE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Syntenic_Blocks", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Syntenic_Blocks", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$invresult_title <- renderText({
        if( nrow(invtable) != 0 ){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Syntenic Blocks</b></font>')
        } else {}
      })
      
      output$invdata1_title <- renderText({
        if( !is.null(input$invresult_rows_selected)){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the reference genome</b></font>')
        } else {}
      })
      
      output$invdata1 <- DT::renderDT({
        if (is.null(input$invresult_rows_selected)) {
        } else {
          load( paste0("./info/", gsub(" ", "_", genomename), "/", gsub(" ", "_", genomename), ".gene.info.RData") )
          clicked <- input$invresult_rows_selected
          chr <- chromosome
          start <- as.numeric(invtable[clicked, 2] )
          end <- as.numeric( invtable[clicked, 3] )
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)], ]
          DT::datatable(
            geneinfo, 
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                          list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE)
      
      output$invdata2_title <- renderText({
        if( !is.null(input$invresult_rows_selected) ){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the query genome</b></font>')
        } else {}
      })
      
      output$invdata2 <- DT::renderDT({
        if (is.null(input$invresult_rows_selected)) {
        } else {
          load( paste0("./info/", gsub(" ", "_", comparisongenome), "/", gsub(" ", "_", comparisongenome), ".gene.info.RData") )
          clicked <- input$invresult_rows_selected
          chr <- chromosome
          start <- as.numeric(invtable[clicked, 5] )
          end <- as.numeric( invtable[clicked, 6] )
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)], ]
          DT::datatable(
            geneinfo, 
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                          list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE) 
    }
    
    #TDM FILE
    TDMfilename <- gsub(" ", "_", paste0("./info/genestructurevariation/TDM/", genomename, "and", comparisongenome, ".allout.TDM.gz"))
    
    tdmtable <- read.table(TDMfilename, sep = "\t", as.is = T)
    tdmtable <- tdmtable[tdmtable[, 4] == gsub(" ", "_", paste0(comparisongenome, "_", chromosome) ), ]
    tdmtable <- tdmtable[, c(1:6)]
    tdmtable$V1 <- gsub("_chr", ", chr", tdmtable$V1)
    tdmtable$V1 <- gsub("_", " ", tdmtable$V1)
    tdmtable$V4 <- gsub("_chr", ", chr", tdmtable$V4)
    tdmtable$V4 <- gsub("_", " ", tdmtable$V4)
    colnames(tdmtable) <- c("Reference chromosome", "Reference start", "Reference end", "Query chromosome", "Query start", "Query end")
    if (nrow(tdmtable) == 0){
      output$tdmresult <- DT::renderDT({
        DT::datatable(
          tdmtable, 
          extensions = c("Buttons"), 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         columnDefs=list(list(targets="_all", class="dt-center")), scrollX = TRUE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Syntenic_Blocks", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Syntenic_Blocks", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
    } else {
      output$tdmresult <- DT::renderDT({
        DT::datatable(
          tdmtable, 
          extensions = c("Buttons"), 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         columnDefs=list(list(targets="_all", class="dt-center")), scrollX = TRUE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Syntenic_Blocks", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Syntenic_Blocks", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$tdmresult_title <- renderText({
        if( nrow(tdmtable) != 0 ){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Syntenic Blocks</b></font>')
        } else {}
      })
      
      output$tdmdata1_title <- renderText({
        if( !is.null(input$tdmresult_rows_selected)){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the reference genome</b></font>')
        } else {}
      })
      
      output$tdmdata1 <- DT::renderDT({
        if (is.null(input$tdmresult_rows_selected)) {
        } else {
          load( paste0("./info/", gsub(" ", "_", genomename), "/", gsub(" ", "_", genomename), ".gene.info.RData") )
          clicked <- input$tdmresult_rows_selected
          chr <- chromosome
          start <- as.numeric(tdmtable[clicked, 2] )
          end <- as.numeric( tdmtable[clicked, 3] )
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)], ]
          DT::datatable(
            geneinfo, 
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                          list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE)
      
      output$tdmdata2_title <- renderText({
        if( !is.null(input$tdmresult_rows_selected) ){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the query genome</b></font>')
        } else {}
      })
      
      output$tdmdata2 <- DT::renderDT({
        if (is.null(input$tdmresult_rows_selected)) {
        } else {
          load( paste0("./info/", gsub(" ", "_", comparisongenome), "/", gsub(" ", "_", comparisongenome), ".gene.info.RData") )
          clicked <- input$tdmresult_rows_selected
          chr <- chromosome
          start <- as.numeric(tdmtable[clicked, 5] )
          end <- as.numeric( tdmtable[clicked, 6] )
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)], ]
          DT::datatable(
            geneinfo, 
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                          list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE) 
    }
    
    #TRANS FILE
    TRANSfilename <- gsub(" ", "_", paste0("./info/genestructurevariation/TRANS/", genomename, "and", comparisongenome, ".allout.TRANS.gz"))
    
    transtable <- read.table(TRANSfilename, sep = "\t", as.is = T)
    transtable <- transtable[transtable[, 4] == gsub(" ", "_", paste0(comparisongenome, "_", chromosome) ), ]
    transtable <- transtable[, c(1:6)]
    transtable$V1 <- gsub("_chr", ", chr", transtable$V1)
    transtable$V1 <- gsub("_", " ", transtable$V1)
    transtable$V4 <- gsub("_chr", ", chr", transtable$V4)
    transtable$V4 <- gsub("_", " ", transtable$V4)
    colnames(transtable) <- c("Reference chromosome", "Reference start", "Reference end", "Query chromosome", "Query start", "Query end")
    if (nrow(transtable) == 0){
      output$transresult <- DT::renderDT({
        DT::datatable(
          transtable, 
          extensions = c("Buttons"), 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         columnDefs=list(list(targets="_all", class="dt-center")), scrollX = TRUE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Syntenic_Blocks", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Syntenic_Blocks", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
    } else {
      output$transresult <- DT::renderDT({
        DT::datatable(
          transtable, 
          extensions = c("Buttons"), 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         columnDefs=list(list(targets="_all", class="dt-center")), scrollX = TRUE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Syntenic_Blocks", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Syntenic_Blocks", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$transresult_title <- renderText({
        if( nrow(transtable) != 0 ){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Syntenic Blocks</b></font>')
        } else {}
      })
      
      output$transdata1_title <- renderText({
        if( !is.null(input$transresult_rows_selected)){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the reference genome</b></font>')
        } else {}
      })
      
      output$transdata1 <- DT::renderDT({
        if (is.null(input$transresult_rows_selected)) {
        } else {
          load( paste0("./info/", gsub(" ", "_", genomename), "/", gsub(" ", "_", genomename), ".gene.info.RData") )
          clicked <- input$transresult_rows_selected
          chr <- chromosome
          start <- as.numeric(transtable[clicked, 2] )
          end <- as.numeric( transtable[clicked, 3] )
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)], ]
          DT::datatable(
            geneinfo, 
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                          list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE)
      
      output$transdata2_title <- renderText({
        if( !is.null(input$transresult_rows_selected) ){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the query genome</b></font>')
        } else {}
      })
      
      output$transdata2 <- DT::renderDT({
        if (is.null(input$transresult_rows_selected)) {
        } else {
          load( paste0("./info/", gsub(" ", "_", comparisongenome), "/", gsub(" ", "_", comparisongenome), ".gene.info.RData") )
          clicked <- input$transresult_rows_selected
          chr <- chromosome
          start <- as.numeric(transtable[clicked, 5] )
          end <- as.numeric( transtable[clicked, 6] )
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)], ]
          DT::datatable(
            geneinfo, 
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                          list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE) 
    }
    
    #DEL FILE
    DELfilename <- gsub(" ", "_", paste0("./info/genestructurevariation/DEL/", genomename, "and", comparisongenome, ".allout.DEL.gz"))
    
    deltable <- read.table(DELfilename, sep = "\t", as.is = T)
    deltable <- deltable[deltable[, 6] == gsub(" ", "_", paste0(comparisongenome, "_", chromosome) ), ]
    deltable <- deltable[, c(1:8)]
    deltable$V1 <- gsub("_chr", ", chr", deltable$V1)
    deltable$V1 <- gsub("_", " ", deltable$V1)
    deltable$V6 <- gsub("_chr", ", chr", deltable$V6)
    deltable$V6 <- gsub("_", " ", deltable$V6)
    colnames(deltable) <- c("Reference chromosome", "Reference start", "Reference end", "Reference", "Alternative", "Query chromosome", "Query start", "Query end")
    if (nrow(deltable) == 0){
      output$delresult <- DT::renderDT({
        DT::datatable(
          deltable, 
          extensions = c("Buttons"), 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         columnDefs=list(list(targets="_all", class="dt-center", 
                                              render = DT::JS(
                                                "function(data, type, row, meta) {",
                                                "return type === 'display' && data.length > 30 ?",
                                                "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                "}"))),
                         scrollX = TRUE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Syntenic_Blocks", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Syntenic_Blocks", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
                         
          )
        )
      }, server = FALSE)
      
    } else {
      output$delresult <- DT::renderDT({
        DT::datatable(
          deltable, 
          extensions = c("Buttons"), 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         columnDefs=list(list(targets="_all", class="dt-center", 
                                              render = DT::JS(
                                                "function(data, type, row, meta) {",
                                                "return type === 'display' && data.length > 30 ?",
                                                "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                "}"))),
                         scrollX = TRUE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Syntenic_Blocks", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Syntenic_Blocks", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
                         
          )
        )
      }, server = FALSE)
      
      output$deldata1_title <- renderText({
        if( !is.null(input$delresult_rows_selected)){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the reference genome</b></font>')
        } else {}
      })
      
      output$deldata1 <- DT::renderDT({
        if (is.null(input$delresult_rows_selected)) {
        } else {
          load( paste0("./info/", gsub(" ", "_", genomename), "/", gsub(" ", "_", genomename), ".gene.info.RData") )
          clicked <- input$delresult_rows_selected
          chr <- chromosome
          start <- as.numeric(deltable[clicked, 2] )
          end <- as.numeric( deltable[clicked, 3] )
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)], ]
          DT::datatable(
            geneinfo, 
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                          list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE)
      
      output$deldata2_title <- renderText({
        if( !is.null(input$delresult_rows_selected) ){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the query genome</b></font>')
        } else {}
      })
      
      output$deldata2 <- DT::renderDT({
        if (is.null(input$delresult_rows_selected)) {
        } else {
          load( paste0("./info/", gsub(" ", "_", comparisongenome), "/", gsub(" ", "_", comparisongenome), ".gene.info.RData") )
          clicked <- input$delresult_rows_selected
          chr <- chromosome
          start <- as.numeric(deltable[clicked, 7] )
          end <- as.numeric( deltable[clicked, 8] )
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)], ]
          DT::datatable(
            geneinfo, 
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                          list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE) 
    }
    
    #INS FILE
    INSfilename <- gsub(" ", "_", paste0("./info/genestructurevariation/INS/", genomename, "and", comparisongenome, ".allout.INS.gz"))
    
    instable <- read.table(INSfilename, sep = "\t", as.is = T)
    instable <- instable[instable[, 6] == gsub(" ", "_", paste0(comparisongenome, "_", chromosome) ), ]
    instable <- instable[, c(1:8)]
    instable$V1 <- gsub("_chr", ", chr", instable$V1)
    instable$V1 <- gsub("_", " ", instable$V1)
    instable$V6 <- gsub("_chr", ", chr", instable$V6)
    instable$V6 <- gsub("_", " ", instable$V6)
    colnames(instable) <- c("Reference chromosome", "Reference start", "Reference end",  "Reference", "Alternative", "Query chromosome", "Query start", "Query end")
    if (nrow(instable) == 0){
      output$insresult <- DT::renderDT({
        DT::datatable(
          instable, 
          extensions = c("Buttons"), 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         columnDefs=list(list(targets="_all", class="dt-center", 
                                              render = DT::JS(
                                                "function(data, type, row, meta) {",
                                                "return type === 'display' && data.length > 30 ?",
                                                "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                "}"))),
                         scrollX = TRUE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Syntenic_Blocks", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Syntenic_Blocks", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
    } else {
      output$insresult <- DT::renderDT({
        DT::datatable(
          instable, 
          extensions = c("Buttons"), 
          escape = FALSE, rownames= FALSE, selection="single", 
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         columnDefs=list(list(targets="_all", class="dt-center", 
                                              render = DT::JS(
                                                "function(data, type, row, meta) {",
                                                "return type === 'display' && data.length > 30 ?",
                                                "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                "}"))),
                         scrollX = TRUE,
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Syntenic_Blocks", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Syntenic_Blocks", sep = "-")),
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$insdata1_title <- renderText({
        if( !is.null(input$insresult_rows_selected)){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the reference genome</b></font>')
        } else {}
      })
      
      output$insdata1 <- DT::renderDT({
        if (is.null(input$insresult_rows_selected)) {
        } else {
          load( paste0("./info/", gsub(" ", "_", genomename), "/", gsub(" ", "_", genomename), ".gene.info.RData") )
          clicked <- input$insresult_rows_selected
          chr <- chromosome
          start <- as.numeric(instable[clicked, 2] )
          end <- as.numeric( instable[clicked, 3] )
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)], ]
          DT::datatable(
            geneinfo, 
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                          list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE)
      
      output$insdata2_title <- renderText({
        if( !is.null(input$insresult_rows_selected) ){
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the query genome</b></font>')
        } else {}
      })
      
      output$insdata2 <- DT::renderDT({
        if (is.null(input$insresult_rows_selected)) {
        } else {
          load( paste0("./info/", gsub(" ", "_", comparisongenome), "/", gsub(" ", "_", comparisongenome), ".gene.info.RData") )
          clicked <- input$insresult_rows_selected
          chr <- chromosome
          start <- as.numeric(instable[clicked, 7] )
          end <- as.numeric( instable[clicked, 8] )
          gene <- GenomicRanges::GRanges(seqnames = gene_info_s$chr, IRanges::IRanges(start = gene_info_s$start, end = gene_info_s$end))
          thefind <- GenomicRanges::GRanges(seqnames = chr, IRanges::IRanges(start = start, end = end))
          find.f1 <- GenomicRanges::findOverlaps(gene, thefind)
          geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)], ]
          DT::datatable(
            geneinfo, 
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("transcription_factors", sep = "-")),
                                          list(extend = 'excel', filename =  paste("transcription_factors", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE) 
    }
  })
  
  output$genestructure_tab_show <- renderUI({
    req(input$submit_gsv)
    tabsetPanel(id = "genestructure_tab",
                tabPanel("Duplication",
                         fixedRow(
                           DT::dataTableOutput("dupresult"),
                           column(12,htmlOutput("dupdata1_title")),
                           column(12,DT::dataTableOutput("dupdata1")),
                           tags$head(tags$style("#trfaccessions_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
                           )),
                           column(12,htmlOutput("dupdata2_title")),
                           column(12,DT::dataTableOutput("dupdata2")))
                         
                ),
                
                tabPanel("Inversion",
                         fixedRow(
                           DT::dataTableOutput("invresult"),
                           column(12,htmlOutput("invdata1_title")),
                           column(12,DT::dataTableOutput("invdata1")),
                           tags$head(tags$style("#trfaccessions_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
                           )),
                           column(12,htmlOutput("invdata2_title")),
                           column(12,DT::dataTableOutput("invdata2")))
                ),
                
                tabPanel("Tandem repeat",
                         fixedRow(
                           DT::dataTableOutput("tdmresult"),
                           column(12,htmlOutput("tdmdata1_title")),
                           column(12,DT::dataTableOutput("tdmdata1")),
                           tags$head(tags$style("#trfaccessions_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
                           )),
                           column(12,htmlOutput("tdmdata2_title")),
                           column(12,DT::dataTableOutput("tdmdata2")))
                ),
                
                tabPanel("Translocation",
                         fixedRow(
                           DT::dataTableOutput("transresult"),
                           column(12,htmlOutput("transdata1_title")),
                           column(12,DT::dataTableOutput("transdata1")),
                           tags$head(tags$style("#trfaccessions_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
                           )),
                           column(12,htmlOutput("transdata2_title")),
                           column(12,DT::dataTableOutput("transdata2")))
                ),
                
                tabPanel("Deletion",
                         fixedRow(
                           DT::dataTableOutput("delresult"),
                           column(12,htmlOutput("deldata1_title")),
                           column(12,DT::dataTableOutput("deldata1")),
                           tags$head(tags$style("#trfaccessions_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
                           )),
                           column(12,htmlOutput("deldata2_title")),
                           column(12,DT::dataTableOutput("deldata2")))
                ),
                
                tabPanel("Insertion",
                         fixedRow(
                           DT::dataTableOutput("insresult"),
                           column(12,htmlOutput("insdata1_title")),
                           column(12,DT::dataTableOutput("insdata1")),
                           tags$head(tags$style("#trfaccessions_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
                           )),
                           column(12,htmlOutput("insdata2_title")),
                           column(12,DT::dataTableOutput("insdata2")))
                ),
                br()
    )
   })
  
  
  # GBrowser
  observe({
    if (input$submit_browse>0) {
      #library(ggplot2)
      
      if ( exists("GBrowser") ){
      } else {
        source("GBrowser.R")
      }
      
      if ( exists("anaReg") ){
      } else {
        source("anaReg.R")
      }
      
      isolate({
        myPos <- anaReg(input$regB)
        if ( exists("snpInfo") ){
        } else {
          source("snpInfo.R")
        }
        
        if ( exists("validReg") ){
        } else {
          source("validReg.R")
        }
        
        if (validReg(myPos)) {
          if (!is.null(myPos)) {
            snp.info <- snpInfo(chr=myPos$chr, start=myPos$start - input$GBUP, end=myPos$end + input$GBDOWN, 
                                accession = gsub(",.+", "", input$mychooserB), mutType = input$GB_mut_group)
          } else {
            snp.info <- NULL
          }
          
          if (is.null(snp.info) || nrow(snp.info[[1]][[1]]) < 1) {
            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Error input!", type = "error",
              text = "No SNPs are detected in the specified genomic region or the specified genomic region is too large!"
            )
          } else {
            GBplot <<- NULL
            output$gbrowser <- plotly::renderPlotly({
              GBplot <<- GBrowser(chr=myPos$chr, start=myPos$start - input$GBUP, 
                                  end=myPos$end + input$GBDOWN,
                                  accession = gsub(",.+", "", input$mychooserB),
                                  mutType = input$GB_mut_group)
              GBplot[[2]]
            })
            
            ## Download PDF file of GBrowser
            output$downloadGB.pdf <- downloadHandler(
              filename <- function() { paste('GBrowser.pdf') },
              content <- function(file) {
                pdf(file, width = 900/72, height = 300/72)
                grid::grid.draw(GBplot[[1]])
                dev.off()
              }, contentType = 'application/pdf')
            
            output$downloadBro03 <- renderUI({
              req(input$submit_browse)
              column(12,
                     downloadButton("downloadGB.pdf",  style = "width:100%;", "PDF-file", class = "buttDown"),
                     tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
              )
            })
            
            # Download genotypes of seleceted SNPs
            output$downloadsnp.txt <- downloadHandler(
              filename = function() { "snp.geno.txt" },
              content = function(file) {
                accession <- gsub(",.+", "", input$mychooserB)
                accession <- sapply(accession, function(x){
                  if (x %in% c("Improved cultivar", "Landrace", "Glycine soja")) {
                    x.dat <- readLines(paste0("./data/", x, ".soya.txt"))
                    return(x.dat)
                  } else {
                    return(x)
                  }
                })
                accession <- unique(unlist(accession))
                write.table(snp.info[[1]][[1]][ ,colnames(snp.info[[1]][[1]]) %in% accession], file, sep="\t", quote=F)
              })
            
            output$downloadBro01 <- renderUI({
              req(input$submit_browse)
              column(12,
                     downloadButton("downloadsnp.txt", style = "width:100%;", "Genotype data", class = "buttDown"),
                     tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
              )
            })
            
            # Download information of SNPs
            output$downloadBro02 <- renderUI({
              req(input$submit_browse)
              column(12,
                     downloadButton("downloadsnpInfo.txt", style = "width:100%;", "SNPs information", class = "buttDown"),
                     tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
              )
            })
            
            output$downloadsnpInfo.txt <- downloadHandler(
              filename = function() { "snp.info.txt" },
              content = function(file) {
                mutType = input$GB_mut_group
                allmutType <- snp.info[[2]]$effect
                allmutType <- gsub(".+-", "", allmutType)
                write.table(snp.info[[2]][allmutType %in% mutType, ], file, sep="\t", quote=F, row.names=F)
              })
          }
        } else {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please input genomic region or gene model in appropriate format!"
          )
        }
      })
    } else {
      NULL
    }
  })
  
  observeEvent(input$browseall1, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "mychooserB",
      selected = all.soya.cho
    )
  })
  
  observeEvent(input$browsenone1, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "mychooserB",
      selected = character(0)
    )
  })
  
  observeEvent(input$browseall2, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "GB_mut_group",
      selected = mutationtypes
    )
  })
  
  observeEvent(input$browsenone2, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "GB_mut_group",
      selected = character(0)
    )
  })
  
  observe({
    if (input$clearGB>0) {
      isolate({
        updateTextInput(session, "regB", value="")
      })
    } else {NULL}
  })
  
  observe({
    if (input$GBExam >0) {
      isolate({
        updateTextInput(session, "regB", value="chr1:29765419-29793053")
      })
    } else {NULL}
  })
  
  # Search for seleceted SNPs
  observeEvent(input$submit_SSNP, {
    if ( exists("anaReg") ){
    } else {
      source("anaReg.R")
    }
    
    if ( exists("snpInfo") ){
    } else {
      source("snpInfo.R")
    }
    
    if ( exists("validReg") ){
    } else {
      source("validReg.R")
    }
    
    if (exists("snp.lst")){
    } else {
      snp.lst <- read.table("./data/snp.RData.lst", head=T, as.is=T, sep="\t")
    }
    
    myPos <- anaReg(input$regBB)
    
    if (validReg(myPos)) {
      if (!is.null(myPos)) {
        snp.info.down <<- snpInfo(chr=myPos$chr, start=myPos$start, end=myPos$end, 
                                  accession = gsub(",.+", "", input$mychooserD), mutType = NULL)
        ddt <- data.frame(snp.info.down[[1]][[1]], stringsAsFactors = F)
        accession <- sapply(gsub(",.+", "", input$mychooserD), function(x){
          if (x %in% c("Improved cultivar", "Landrace", "Glycine soja")) {
            x.dat <- readLines(paste0("./data/", x, ".soya.txt"))
            return(x.dat)
          } else {
            return(x)
          }
        })
        accession <- unlist(accession)
        ddt$snpID <- rownames(ddt)
        ddt <- ddt[, c(accession, "snpID")]
        
        reg.gr <- IRanges::IRanges(myPos$start, myPos$end)
        snp.lst.chr <- snp.lst[snp.lst$chr==myPos$chr, ]
        snp.lst.gr <- IRanges::IRanges(start=snp.lst.chr$start, end=snp.lst.chr$end)
        snp.fls <- snp.lst.chr$file[unique(S4Vectors::queryHits(IRanges::findOverlaps(snp.lst.gr, reg.gr)))]
        snpeff.fls <- gsub("snp.RData", "snpeff_gene.RData", snp.fls)
        snpeff.fls <- gsub("data", "data/gene_snp", snpeff.fls)
        
        snpeff.fls.lst <- lapply(snpeff.fls, function(x){
          load(x)
          return(snpeff_gene)
        })
        snpgene_eff <- do.call(rbind, snpeff.fls.lst)
        
        if ( length(input$mychooserD) == 0 ) {
          serchSNP <- snp.info.down[[2]][, c(1:3,5:7)]
          serchSNP$gene <- snpgene_eff$gene[snpgene_eff$id %in% serchSNP$snpID]
        } else {
          snp.info.down[[2]]$gene <- snpgene_eff$gene[snpgene_eff$id %in% snp.info.down[[2]]$snpID]
          serchSNP <- merge(snp.info.down[[2]][, c(1:3,5:8)], ddt, by = "snpID")
        }
        serchSNP[is.na(serchSNP)] <- "N"
        colnames(serchSNP)[1:7] <- c("SNPID", "Major", "Minor", "Reference", "Alternative", "Effect", "Gene")
        if ( nrow(serchSNP) == 0) {
          output$mytable2 <- DT::renderDT({
            nuldata <- data.frame("No SNP in this location!")
            colnames(nuldata) <- ""
            DT::datatable(
              nuldata,
              extensions = "Buttons", escape = FALSE,
              rownames = F, selection = "none", 
              options = list(bSort = FALSE, pageLength = 20,  dom = 'Bfrtip',
                             buttons = list('pageLength', 'copy', 
                                            list(extend = 'csv',   filename =  paste("SNPs", sep = "-")),
                                            list(extend = 'excel', filename =  paste("SNPs", sep = "-"))
                             ),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                               "}")
              )
            )
          })
        } else {
          output$mytable2 <- DT::renderDT({
            IDNAME <- soya.info$Names
            names(IDNAME) <- soya.info$ID
            IDNAMEsd <- IDNAME[colnames(serchSNP)[-c(1:7)]]
            colnames(serchSNP)[-c(1:7)] <- paste0(colnames(serchSNP)[-c(1:7)], " (", IDNAMEsd, ")")
            DT::datatable(
              serchSNP
              # ,selection = 'none', rownames = '', filter = 'none',
              # extensions = c('Buttons','FixedColumns'),
              # options = list(
              #   dom = 'Bfrtip',
              #   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
              #   paging = TRUE, searching = TRUE, info = FALSE,
              #   sort = TRUE, scrollX = TRUE, fixedColumns = list(leftColumns = 2)
              # )
              ,selection = 'none', rownames = FALSE, escape = FALSE,
              extensions = c("Buttons"), #"FixedColumns"
              options = list(
                buttons = list('pageLength',
                               list(extend = 'csv',   filename =  paste("snp", sep = "-")),
                               list(extend = 'excel', filename =  paste("snp", sep = "-")),
                               'copy'),dom = 'Bfrtip',
                pageLength = 15, columnDefs=list(list(targets="_all", class="dt-center")),#fixedHeader = TRUE,
                bSort = FALSE, scrollX = TRUE, # fixedColumns = list(leftColumns = 6),
                initComplete = DT::JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                  "}")
              )
            )
          }, server = FALSE)
        }
        
        output$bulkdownloadsnp.txt <- downloadHandler(
          filename = function() { "down.snp.geno.txt" },
          content = function(file) {
            write.table(snp.info.down[[1]][[1]], file, sep="\t", quote=F)
          })
        
        output$downloadSD02 <- renderUI({
          req(input$submit_SSNP)
          column(12,
                 downloadButton("bulkdownloadsnp.txt", style = "width:100%;", "Genotype data", class = "buttDown"),
                 tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
          )
        })
        
        # Bulk download information of SNPs
        output$bulkdownloadsnpInfo.txt <- downloadHandler(
          filename = function() { "down.snp.info.txt" },
          content = function(file) {
            write.table(snp.info.down[[2]], file, sep="\t", quote=F, row.names=F)
          })
        
        output$downloadSD01 <- renderUI({
          req(input$submit_SSNP)
          column(12,
                 downloadButton("bulkdownloadsnpInfo.txt", style = "width:100%;", "SNPs information", class = "buttDown"),
                 tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
          )
        })
        
        # Bulk download gene annotation
        output$bulkdownloadgene.txt <- downloadHandler(
          filename = function() { "down.gene.info.txt" },
          content = function(file) {
            gff <- data.table::fread("./data/zh13.gff", sep = "\t", data.table = FALSE)
            gene.info <- gff[gff$chr==myPos$chr & gff$start>=myPos$start & gff$end<=myPos$end, ]
            write.table(gene.info[,1:6], file, sep="\t", quote=F, row.names=F)
          })
        
        output$downloadSD03 <- renderUI({
          req(input$submit_SSNP)
          column(12,
                 downloadButton("bulkdownloadgene.txt", style = "width:100%;", "Gene annotation", class = "buttDown"),
                 tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
          )
        })
      } else {
        NULL
      }
    } else {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input genomic region or gene model in appropriate format!"
      )
      NULL
    }
  })
  
  observeEvent(input$snpsearchall1, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "mychooserD",
      selected = all.soya.cho
    )
  })
  
  observeEvent(input$snpsearchnone1, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "mychooserD",
      selected = character(0)
    )
  })
  
  observe({
    if (input$clearDOW>0) {
      isolate({
        updateTextInput(session, "regBB", value="")
        shinyWidgets::updateMultiInput(
          session = session,
          inputId = "mychooserD",
          selected = character(0)
        )
      })
    } else {NULL}
  })
  
  observe({
    if (input$DOWExam >0) {
      isolate({
        updateTextInput(session, "regBB", value="chr7:29560705-29573051")
        shinyWidgets::updateMultiInput(
          session = session,
          inputId = "mychooserD",
          selected = "Glycine soja"
        )
      })
    } else {NULL}
  })
  
  # LDheatmap
  observe({
    if (input$submitLD>0) {
      if ( exists("anaReg") ){
      } else {
        source("anaReg.R")
      }
      if ( exists("ld.heatmap") ){
      } else {
        source("ld.heatmap.R")
      }
      
      if ( exists("fetchSnp") ){
      } else {
        source("fetchSnp.R")
      }
      
      isolate({
        ld.height <<- input$ldHeight
        ld.width <<- input$ldWidth
        myPos <- anaReg(input$regL)
        
        if ( exists("validReg") ){
        } else {
          source("validReg.R")
        }
        if (validReg(myPos)) {
          if (!is.null(myPos)) {
            snp.reg <- fetchSnp(chr=myPos$chr, start=myPos$start - input$ldUp, 
                                end=myPos$end + input$ldDown, accession = gsub(",.+", "", input$mychooserLD),
                                mutType = input$ld_mut_group, filter = TRUE)[[1]]
          } else {
            snp.reg <- NULL
          }
          
          if (is.null(snp.reg) || nrow(snp.reg) < 5) {
            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Error input!", type = "error",
              text = "Too few SNPs are detected in the specified genomic region or the specified genomic region is too large!"
            )
          } else {
            snp.pos <- as.numeric(unlist(strsplit(input$ldpos, split=",")))
            
            if ( input$ldSize ){
              output$ldheatmap <- shiny::renderPlot({
                if (input$flip == "0") {
                  ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=c(FALSE, TRUE)[as.numeric(input$showText)+1],
                             snp.pos=snp.pos, gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                             col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                             mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD))
                } else if (input$flip == "1") {
                  if (input$LDshowGene) {
                    ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=FALSE,
                               snp.pos=snp.pos, ld.y=input$ldY/100, ld.w=input$ldW/100, gene=TRUE, 
                               col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                               mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD))
                  } else {
                    ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=FALSE,
                               gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                               col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                               mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD))
                  }
                }
              }, height = ld.height, width = ld.width)
              
            } else {
              output$ldheatmap <- shiny::renderPlot({
                if (input$flip == "0") {
                  ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=c(FALSE, TRUE)[as.numeric(input$showText)+1],
                             snp.pos=snp.pos, gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                             col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                             mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD))
                } else if (input$flip == "1") {
                  if (input$LDshowGene) {
                    ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=FALSE,
                               snp.pos=snp.pos, ld.y=input$ldY/100, ld.w=input$ldW/100, gene=TRUE, 
                               col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                               mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD))
                  } else {
                    ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=FALSE,
                               gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                               col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                               mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD))
                  }
                }
              })
            }
          }
        } else {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please input genomic region or gene model in appropriate format!" 
          )
        }
      })
    } else {
      NULL
    }
  })
  
  observeEvent(input$ldall1, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "mychooserLD",
      selected = all.soya.cho
    )
  })
  
  observeEvent(input$ldnone1, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "mychooserLD",
      selected = character(0)
    )
  })
  
  observeEvent(input$ldall2, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "ld_mut_group",
      selected = mutationtypes
    )
  })
  
  observeEvent(input$ldnone2, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "ld_mut_group",
      selected = character(0)
    )
  })
  
  observe({
    if (input$clearLD>0) {
      isolate({
        updateTextInput(session, "regL", value="")
      })
    } else {NULL}
  })
  
  observe({
    if (input$LDExam >0) {
      isolate({
        updateTextInput(session, "regL", value="SoyZH13_09G103313")
      })
    } else {NULL}
  })
  
  
  ## Download PDF file of LDheatmap
  output$downloadLD.pdf <- downloadHandler(
    filename <- function() { paste('LDheatmap.pdf') },
    content <- function(file) {
      withProgress(message='Calculation in progress...',value = 0, detail = 'This may take a while...', {
        myPos <- anaReg(input$regL)
        
        snp.reg <- fetchSnp(chr=myPos$chr, start=myPos$start - input$ldUp, 
                            end=myPos$end + input$ldDown, accession = gsub(",.+", "", input$mychooserLD),
                            mutType = input$ld_mut_group, filter = TRUE)[[1]]
        if (nrow(snp.reg) < 5) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Too few SNPs in specified genomic region!"
          )
        } else {
          pdf(file, width = input$ldWidth/72, height = input$ldHeight/72, onefile = FALSE)
          
          snp.pos <- as.numeric(unlist(strsplit(input$ldpos, split=",")))
          
          if (input$flip == "0") {
            ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=c(FALSE, TRUE)[as.numeric(input$showText)+1],
                       snp.pos=snp.pos, gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                       col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                       mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD))
          } else if (input$flip == "1") {
            if (input$LDshowGene) {
              ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=FALSE,
                         snp.pos=snp.pos, ld.y=input$ldY/100, ld.w=input$ldW/100, gene=TRUE, 
                         col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                         mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD))
            } else {
              ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=FALSE,
                         gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                         col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                         mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD))
            }
          }
          dev.off()
        }
      })
    }, contentType = 'application/pdf')
  
  output$downloadLD01 <- renderUI({
    req(input$submitLD)
    column(12,
           downloadButton("downloadLD.pdf", style = "width:100%;", "PDF-file", class = "buttDown"),
           tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
    )
  })
  
  ## Download SVG file of LDheatmap
  output$downloadLD.svg <- downloadHandler(
    filename <- function() { paste('LDheatmap.svg') },
    content <- function(file) {
      withProgress(message='Calculation in progress...',value = 0, detail = 'This may take a while...', {
        myPos <- anaReg(input$regL)
        snp.reg <- fetchSnp(chr=myPos$chr, start=myPos$start - input$ldUp, 
                            end=myPos$end + input$ldDown, accession = gsub(",.+", "", input$mychooserLD),
                            mutType = input$ld_mut_group, filter = TRUE)[[1]]
        if (nrow(snp.reg) < 5) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Too few SNPs in specified genomic region!"
          )
        } else {
          svg(file, width = input$ldWidth/72, height = input$ldHeight/72)
          
          snp.pos <- as.numeric(unlist(strsplit(input$ldpos, split=",")))
          
          if (input$flip == "0") {
            ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=c(FALSE, TRUE)[as.numeric(input$showText)+1],
                       snp.pos=snp.pos, gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                       col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                       mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD))
          } else if (input$flip == "1") {
            if (input$LDshowGene) {
              ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=FALSE,
                         snp.pos=snp.pos, ld.y=input$ldY/100, ld.w=input$ldW/100, gene=TRUE, 
                         col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                         mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD))
            } else {
              ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=FALSE,
                         gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                         col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                         mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD))
            }
          }
          dev.off()
        }
      })
    }, contentType = 'image/svg')
  
  output$downloadLD02 <- renderUI({
    req(input$submitLD)
    column(12,
           downloadButton("downloadLD.svg", style = "width:100%;", "SVG-file", class = "buttDown"),
           tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
    )
  })
  
  
  # Diversity
  observe({
    if (input$submit4>0) {
      if ( exists("anaReg") ){
      } else {
        source("anaReg.R")
      }
      
      if ( exists("nucDiv") ){
      } else {
        source("nucDiv.R")
      }
      
      if ( exists("fetchSnp") ){
      } else {
        source("fetchSnp.R")
      }
      
      if ( exists("validReg") ){
      } else {
        source("validReg.R")
      }
      
      isolate({
        div.height <<- input$divHeight
        div.width <<- input$divWidth
        
        myPos <- anaReg(input$regD)
        if (!is.null(input$div_acc_group)){
          if (validReg(myPos)) {
            div.up <- input$divUp
            div.down <- input$divDown
            div.group <- input$div_acc_group
            div.step <- input$snpnumD
            div.numerator <- input$nuc_numerator
            div.denominator <- input$nuc_denominator
            div.mut.group <- input$div_mut_group
            
            if (!is.null(myPos)) {
              snp.reg <- fetchSnp(chr=myPos$chr, start=myPos$start - div.up, end=myPos$end + div.down,
                                  mutType=input$div_mut_group, filter = TRUE)[[1]]
            } else {
              snp.reg <- NULL
            }
            
            if (is.null(snp.reg) || nrow(snp.reg) < 10) {
              shinyWidgets::sendSweetAlert(
                session = session,
                title = "Error input!", type = "error",
                text = "Too few SNPs are detected in the specified genomic region or the specified genomic region is too large!"
              )
            } else {
              nuc.div.plot <<- NULL
              
              if (input$divSize ){
                output$diversity <- shiny::renderPlot({
                  par(family = "serif")
                  nuc.div.plot <<- nucDiv(chr = myPos$chr, nuc.start = myPos$start - div.up, nuc.end = myPos$end + div.down, 
                                          groups = div.group, step = div.step,
                                          numerator = div.numerator, denominator = div.denominator, 
                                          mutType = div.mut.group)
                  grid::grid.draw(gridExtra::grid.arrange(nuc.div.plot[[1]], nuc.div.plot[[2]], ncol=1, heights=c(2.3, 1)))
                }, height = div.height, width = div.width)
              } else {
                output$diversity <- shiny::renderPlot({
                  par(family = "serif")
                  nuc.div.plot <<- nucDiv(chr = myPos$chr, nuc.start = myPos$start - div.up, nuc.end = myPos$end + div.down, 
                                          groups = div.group, step = div.step,
                                          numerator = div.numerator, denominator = div.denominator, 
                                          mutType = div.mut.group)
                  grid::grid.draw(gridExtra::grid.arrange(nuc.div.plot[[1]], nuc.div.plot[[2]], ncol=1, heights=c(2.3, 1)))
                })
              }
              
              ## Download PDF file of Diversity
              output$downloadDiv01 <- renderUI({
                req(input$submit4, nuc.div.plot)
                column(12,
                       downloadButton("downloadDiv.pdf", "PDF-file", style = "width:100%;", class = "buttDown"),
                       tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
                )
              })
              
              output$downloadDiv.pdf <- downloadHandler(
                filename <- function() { paste('diversity.pdf') },
                content <- function(file) {
                  pdf(file, width = input$divWidth/72, height = input$divHeight/72)
                  grid::grid.draw(gridExtra::grid.arrange(nuc.div.plot[[1]], nuc.div.plot[[2]], ncol=1, heights=c(2.3, 1)))
                  
                  dev.off()
                }, contentType = 'application/pdf')
              
              ## Download SVG file of Diversity
              output$downloadDiv02 <- renderUI({
                req(input$submit4, nuc.div.plot)
                column(12,
                       downloadButton("downloadDiv.svg", "SVG-file", style = "width:100%;", class = "buttDown"),
                       tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
                )
              })
              
              output$downloadDiv.svg <- downloadHandler(
                filename <- function() { paste('diversity.svg') },
                content <- function(file) {
                  svg(file, width = input$divWidth/72, height = input$divHeight/72)
                  grid::grid.draw(gridExtra::grid.arrange(nuc.div.plot[[1]], nuc.div.plot[[2]], ncol=1, heights=c(2.3, 1)))
                  
                  dev.off()
                }, contentType = 'image/svg')
              
              ## Download TXT file of diversity
              output$downloadDiv03 <- renderUI({
                req(input$submit4)
                column(12,
                       downloadButton("downloadDiv.txt", "TXT-file", style = "width:100%;", class = "buttDown"),
                       tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
                )
              })
              
              output$downloadDiv.txt <- downloadHandler(
                filename <- function() { paste('diversity.txt') },
                content <- function(file) {
                  write.table(diVTxt, file, sep="\t", quote=F, row.names = F)
                }, contentType = 'text/plain')
            }
          } else {
            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Error input!", type = "error",
              text = "Please input genomic region or gene model in appropriate format!"
            )
          }
        } else {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please choose at least one denominator ecotype!"
          )
        }
      })
    } else {
      NULL
    }
  })
  
  observe({
    if (input$clearDIV>0) {
      isolate({
        updateTextInput(session, "regD", value="")
      })
    } else {NULL}
  })
  
  observe({
    if (input$DIVExam >0) {
      isolate({
        updateTextInput(session, "regD", value="SoyZH13_12G067900")
      })
    } else {NULL}
  })
  
  observeEvent(input$diversityldall2, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "div_mut_group",
      selected = mutationtypes
    )
  })
  
  observeEvent(input$diversitynone2, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "div_mut_group",
      selected = character(0)
    )
  })
  
  # allele frequency
  observe({
    if (input$submitaf1>0) {
      if ( exists("alleleFreq") ){
      } else {
        source("alleleFreq.R")
      }
      
      isolate({
        in.snpid <- unlist(strsplit(input$af_snp_site, split="\\n"))
        in.snpid <- trimws(in.snpid, which = c("both"), whitespace = "[ \t\r\n]")
        in.snpid <- in.snpid[in.snpid!=""]
        in.snpid <- in.snpid[nchar(in.snpid) == 10]
        in.snpid <- in.snpid[grep("[0-9]", in.snpid)]
        
        af.group <- input$af_acc_group
        
        in.af.col <- strsplit(input$jscolora, ", ")[[1]]
        
        af.height <<- input$afHeight
        af.width <<- input$afWidth
        
        myChr <- paste0("chr", as.numeric(substr(in.snpid, 1, 2)))
        myPos <- as.numeric(substr(in.snpid, 3, 10))
        if ( length(myPos) == 0 || is.na(myPos) ) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please input correct SNP site!"
          )
          NULL
        } else if ( is.null(af.group) ){
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please select at least one Ecotype!"
          )
          NULL
        } else {
          output$alleleFreq <- shiny::renderPlot({
            ll <- alleleFreq(
              snpSite = in.snpid,
              accGroup = af.group,
              pieCols = in.af.col
            )
            if (is.null(ll)){
              shinyWidgets::sendSweetAlert(
                session = session,
                title = "Error input!", type = "error",
                text = "SNP site you entered is missing or filtered!"
              )
              NULL
            } else {
              ll
            }
          }, height = af.height, width = af.width)
        }
      })
    } else {
      NULL
    }
  })
  
  observe({
    if (input$clearAf>0) {
      isolate({
        updateTextAreaInput(session, "af_snp_site", value="")
        updateSelectInput(session, "jscolora", selected = "steelblue, yellow2")
      })
    } else {NULL}
  })
  
  observe({
    if (input$AfExam >0) {
      isolate({
        updateTextAreaInput(session, "af_snp_site", value="0133024709\n1403584545\n1403584761")
        updateSelectInput(session, "jscolora", selected = "steelblue, yellow2")
      })
    } else {NULL}
  })
  
  ## Download PDF file of allele frequency
  output$downloadAfq01 <- renderUI({
    req(input$submitaf1)
    column(12,
           downloadButton("downloadAlleleFreq.pdf", "PDF-file", style = "width:100%;", class = "buttDown"),
           tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
    )
  })
  
  output$downloadAlleleFreq.pdf <- downloadHandler(
    filename <- function() { paste('alleleFreq.pdf') },
    content <- function(file) {
      pdf(file, width = input$afWidth/72, height = input$afHeight/72)
      
      in.snpid <- unlist(strsplit(input$af_snp_site, split="\\n"))
      in.snpid <- gsub("^\\s+", "", in.snpid)
      in.snpid <- gsub("\\s+$", "", in.snpid)
      in.snpid <- in.snpid[in.snpid!=""]
      
      af.group <- input$af_acc_group
      in.af.col <- strsplit(input$jscolora, ", ")[[1]]
      alleleFreq(
        snpSite = in.snpid,
        accGroup = af.group,
        pieCols = in.af.col
      )
      
      dev.off()
    }, contentType = 'application/pdf')
  
  ## Download SVG file of allele frequency
  output$downloadAfq02 <- renderUI({
    req(input$submitaf1)
    column(12,
           downloadButton("downloadAlleleFreq.svg", "SVG-file", style = "width:100%;", class = "buttDown"),
           tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
    )
  })
  
  output$downloadAlleleFreq.svg <- downloadHandler(
    filename <- function() { paste('alleleFreq.svg') },
    content <- function(file) {
      svg(file, width = input$afWidth/72, height = input$afHeight/72)
      
      in.snpid <- unlist(strsplit(input$af_snp_site, split="\\n"))
      in.snpid <- gsub("^\\s+", "", in.snpid)
      in.snpid <- gsub("\\s+$", "", in.snpid)
      in.snpid <- in.snpid[in.snpid!=""]
      
      af.group <- input$af_acc_group
      in.af.col <- strsplit(input$jscolora, ", ")[[1]]
      alleleFreq(
        snpSite = in.snpid,
        accGroup = af.group,
        pieCols = in.af.col
      )
      
      dev.off()
    }, contentType = 'image/svg')
  
  output$downloadAfq03 <- renderUI({
    req(input$submitaf1)
    column(12,
           downloadButton("downloadAlleleFreq.txt", "TXT-file", style = "width:100%;", class = "buttDown"),
           tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
    )
  })
  
  output$downloadAlleleFreq.txt <- downloadHandler(
    filename = function() { "Allele_frequency.txt" },
    content = function(file) {
      write.table(freqsallwrite, file, sep="\t", quote=F, row.names = F, col.names = T)
    })
  
  
  #INDEL
  observeEvent(input$submiti, {
    if ( exists("anaReg") ){
    } else {
      source("anaReg.R")
    }
    
    if ( exists("validReg") ){
    } else {
      source("validReg.R")
    }
    
    myPos <- anaReg(input$regi)
    if(validReg(myPos)) {
      if (!is.null(myPos)) {
        chr <- as.character(myPos$chr)
        start <- as.numeric(myPos$start)
        end <- as.numeric(myPos$end)
        tmp.fg <- file.path(tempdir(), "tg.txt")
        tabix <- paste0("tabix ./indel/", chr, ".delete.gz ", chr, ":", start, "-", end, " > ", tmp.fg)
        system(tabix)
        if ( file.info(tmp.fg)$size == 0){
          indel <- data.frame("V1"="No INDELs found!")
          colnames(indel) <- ""
          gffindel <- c("V1"="No INDELs found!")
          indels <- indel
        } else {
          gffindel <- data.table::fread(tmp.fg, sep = "\t", data.table = F)
          load(paste0("./info/indeleff/", chr, ".indel.eff.RData"))
          
          names(gffindel) <- c("Chromosome", "Position", "Reference", "Alternative" , paste0("s", 1:2898))
          
          indeleff <- b1[b1$V2 %in% gffindel$Position, ]
          indeleff <- unique(indeleff)
          gk <- merge(gffindel, indeleff, by.x = "Position", by.y = "V2")
          gffindel <- gk[, c(2,1,3,4,2903:2904,5:2902)]
          names(gffindel) <- c("Chromosome", "Position", "Reference", "Alternative", "Effect", "Gene" , paste0("s", 1:2898, " (", soya.info$Names, ")"))
          
          # IDNAME <- soya.info$Names
          # names(IDNAME) <- soya.info$ID
          # IDNAMEsd <- IDNAME[colnames(serchSNP)[-c(1:7)]]
          # colnames(serchSNP)[-c(1:7)] <- paste0(colnames(serchSNP)[-c(1:7)], " (", IDNAMEsd, ")")
          
          accession = input$mychooseri
          accession <- sapply(accession, function(x){
            if (x %in% c("Improved cultivar", "Landrace", "Glycine soja")) {
              x.dat <- readLines(paste0("./data/", x, ".soya.txt"))
              return(x.dat)
            } else {
              return(x)
            }
          })
          
          choosei <- unique(gsub(",.+", "", unlist(accession)))
          indel <- gffindel[, c(1:6, as.numeric(gsub("s", "", choosei)) + 6)]
          colnames(indel)[1] <- "Chr"
          indels <- indel[,c(1:6, order(as.numeric(gsub("s", "", choosei)), decreasing = F) + 6)]
        }
        output$indeltable <- DT::renderDT({
          DT::datatable(
            indels
            ,selection = 'none', rownames = FALSE, escape = FALSE,
            extensions = c("Buttons"),#"FixedColumns",
            options = list(
              buttons = list('pageLength', 'copy',
                             list(extend = 'csv',   filename =  paste("INDEL", sep = "-")),
                             list(extend = 'excel', filename =  paste("INDEL", sep = "-"))
              ),dom = 'Bfrtip',
              pageLength = 15, columnDefs=list(list(targets="_all", class="dt-center")), 
              bSort = FALSE, scrollX = TRUE, #fixedColumns = list(leftColumns = 2),
              initComplete = DT::JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                "}")
            )
          )
        }, server = FALSE)        
        output$bulkdownloadindelInfo.txt <- downloadHandler(
          filename <- function() { paste('The_indelinfo.txt') },
          content <- function(file) {
            write.table(indels[, c(1:6)], file, col.names = T, row.names = F, quote = F, sep = "\t")
          }, contentType = 'text/plain'
        )
        
        output$downloadindel01 <- renderUI({
          req(input$submiti)
          column(12,
                 downloadButton("bulkdownloadindelInfo.txt", style = "width:100%;", "Indels information", class = "buttDown"),
                 tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
          )
        })
        
        output$bulkdownloadindelALLInfo.txt <- downloadHandler(
          filename <- function() { paste('All_indelinfo.txt') },
          content <- function(file) {
            write.table(indels, file, col.names = T, row.names = F, quote = F, sep = "\t")
          }, contentType = 'text/plain'
        )
        
        output$downloadindel02 <- renderUI({
          req(input$submiti)
          column(12,
                 downloadButton("bulkdownloadindelALLInfo.txt", style = "width:100%;", "Genotype data", class = "buttDown"),
                 tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
          )
        })
      } else { NULL }
    } else {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input genomic region or gene model in appropriate format!"
      )
    }
  })
  
  observe({
    if (input$clearINDEL>0) {
      isolate({
        updateTextInput(session, "regi", value="")
        shinyWidgets::updateMultiInput(
          session = session,
          inputId = "mychooseri",
          selected = character(0)
        )
      })
    } else {NULL}
  })
  
  observe({
    if (input$INDELExam >0) {
      isolate({
        updateTextInput(session, "regi", value="SoyZH13_01G186100")
        shinyWidgets::updateMultiInput(
          session = session,
          inputId = "mychooseri",
            selected = all.soya.cho
        )
      })
    } else {NULL}
  })

  observeEvent(input$indelall1, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "mychooseri",
      selected = all.soya.cho
    )
  })
  
  observeEvent(input$indelnone1, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "mychooseri",
      selected = character(0)
    )
  })
  
  #gene expression
  observeEvent(input$submit_GSgst, {
    if (!exists("geneexpression") & input$expression_genome == "Expression data of Zhonghuang 13") {
      geneexpression <- data.table::fread("./data/Gene_expression.txt", sep = "\t", data.table = F, check.names = F)
      dknm <- c("Gene ID", "Samples", "Expression value (FPKM)")
      exprnm <- "Expression value (FPKM)"
    } else if (!exists("geneexpression") & input$expression_genome == "Expression data of A81-356022") {
      geneexpression <- data.table::fread("./data/Gene_expression_williams82.txt", sep = "\t", data.table = F, check.names = F)
      dknm <- c("Gene ID", "Samples", "Expression value (RPKM)")
      exprnm <- "Expression value (RPKM)"
    } else if (!exists("geneexpression") & input$expression_genome == "Expression data of W05") {
      geneexpression <- data.table::fread("./data/Gene_expression_w05.txt", sep = "\t", data.table = F, check.names = F)
      dknm <- c("Transcription ID", "Samples", "Expression value (FPKM)")
      exprnm <- "Expression value (FPKM)"
    } else if (!exists("geneexpression") & input$expression_genome == "Transcriptomes data of 102 soybean accessions"){
      geneexpression <- data.table::fread("./data/Gene_expression_102soybean.txt", sep = "\t", data.table = F, check.names = F)
      dknm <- c("Transcription ID", "Samples", "Expression value (FPKM)")
      exprnm <- "Expression value (FPKM)"
    } else {
      NULL
    }
    geneid <- unlist(strsplit(input$gtsgenePaste, split="\\n"))
    geneid <- trimws(geneid, which = c("both"), whitespace = "[ \t\r\n]")
    exforgene <- geneexpression[geneexpression$Gene_ID %in% geneid, ]
    tgene <- geneid[geneid %in% geneexpression$Gene_ID]
    tgene <- tgene[length(tgene):1]
    exforgene$Gene_ID <- factor(exforgene$Gene_ID, levels = tgene)
    
    if( nrow(exforgene) >= 1 & (length(input$Gexp_mut_groupA) != 0 | length(input$Gexp_mut_groupB) != 0 | length(input$Gexp_mut_groupD) != 0) ){
      dk <- data.table::melt(exforgene)
      colnames(dk) <- dknm
      if ( input$expression_genome == "Expression data of Zhonghuang 13"){
        dd <- dk[dk$Samples %in% input$Gexp_mut_groupA, ]
        dmdnm <- c("Gene", "Samples", "Value")
      } else if (input$expression_genome == "Expression data of A81-356022") {
        dd <- dk[dk$Samples %in% input$Gexp_mut_groupB, ]
        dmdnm <- c("Gene", "Samples", "Value")
      } else if (input$expression_genome == "Expression data of W05") {
        dd <- dk[dk$Samples %in% input$Gexp_mut_groupC, ]
        dmdnm <- c("Gene", "Samples", "Value")
      } else if (input$expression_genome == "Transcriptomes data of 102 soybean accessions") {
        dd <- dk[dk$Samples %in% input$Gexp_mut_groupD, ]
        dmdnm <- c("Gene", "Accessions", "Value")
      }
      colnames(dd) <- dmdnm
      output$gstoutresult <- DT::renderDT({
        DT::datatable(
          dd, escape = FALSE, rownames= FALSE, selection="none", extensions = c("Buttons"),
          options = list(pageLength = 15, autoWidth = FALSE, bSort = FALSE, scrollX = TRUE,
                         buttons = list('copy', 
                                        list(extend = 'csv',   filename =  paste("Gene_expression", geneid, sep = "-")),
                                        list(extend = 'excel', filename =  paste("Gene_expression", geneid, sep = "-")),
                                        'print'), dom = 'Bfrtip', columnDefs = list(list(className = 'dt-center', targets = "_all")),
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$gstbarplot <- shiny::renderPlot({
        dmd <- dd
        colnames(dmd) <- dmdnm
        fontsize <- input$gstfontsize
        labfontsize <- fontsize
        #labfontsize <- input$gstlabsize
        if (dmdnm[2] == "Samples"){
          p1 <- ggplot2::ggplot(dmd, ggplot2::aes(Samples, Gene, fill= Value )) + ggplot2::geom_raster() +
            ggplot2::scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF")) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = .9, size = fontsize), axis.text.y = ggplot2::element_text(size = fontsize)) +
            ggplot2::xlab("Samples") + ggplot2::ylab("Gene ID") +
            ggplot2::theme(axis.text=ggplot2::element_text(size=fontsize), axis.title=ggplot2::element_text(size=labfontsize,face="bold")) + 
            ggplot2::theme(legend.text = ggplot2::element_text(size=labfontsize), legend.title = ggplot2::element_text(size=labfontsize))+
            ggplot2::labs(fill=exprnm)
        }else{
          p1 <- ggplot2::ggplot(dmd, ggplot2::aes(Accessions, Gene, fill= Value )) + ggplot2::geom_raster() +
            ggplot2::scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF")) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = .9, size = fontsize), axis.text.y = ggplot2::element_text(size = fontsize)) +
            ggplot2::xlab("Accessions") + ggplot2::ylab("Gene ID") +
            ggplot2::theme(axis.text=ggplot2::element_text(size=fontsize), axis.title=ggplot2::element_text(size=labfontsize,face="bold")) + 
            ggplot2::theme(legend.text = ggplot2::element_text(size=labfontsize), legend.title = ggplot2::element_text(size=labfontsize))+
            ggplot2::labs(fill=exprnm)
        }
        p1
      })
      
      output$gstbarplotif <- renderUI({
        if ( is.na(input$gstwidth) | is.na(input$gstheight) | input$gstheight*input$gstwidth == 0 ){
          NULL
        } else if ( nrow(exforgene) <= 50 & nrow(exforgene) >= 8){
          hegihtexforgene <- nrow(exforgene) * 30
          hegihtexforgene <- paste0(hegihtexforgene, "px")
          widthexforgene <- "100%"
        } else if ( nrow(exforgene) < 8){
          hegihtexforgene <- paste0("200", "px")
          widthexforgene <- "100%"
        } else if( input$gstwidth == 1300 & input$gstheight == 1000 ){
          hegihtexforgene <- 1000
          widthexforgene <- "100%"
        } else {
          widthexforgene = paste0(input$gstwidth, "px")
          hegihtexforgene = paste0(input$gstheight, "px")
        }
        plotOutput("gstbarplot", width = widthexforgene, height = hegihtexforgene )
      })
      
      output$gstout_title <- renderText({
        HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Expression level</b></font>')
      })
    } else {
      if (length( input$Gexp_mut_groupA ) == 0 | length( input$Gexp_mut_groupB ) == 0 |length( input$Gexp_mut_groupC ) == 0 | length( input$Gexp_mut_groupD ) == 0){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error input!", type = "error",
          text = "Please choose at least one tissue/stage/accession!"
        )
      } else{
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error input!", type = "error",
          text = "Input gene list not found or no expression data found for the input gene list!"
        )
      }
      NULL
    }
  })
  
  output$expressionname1 <- DT::renderDT({
    DT::datatable(
      expression_name_description
      , escape = FALSE, rownames= FALSE, selection="single", 
      options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                     buttons = list('pageLength', 'copy', 
                                    list(extend = 'csv',   filename =  paste("searchlocation", sep = "-")),
                                    list(extend = 'excel', filename =  paste("searchloaction", sep = "-")),
                                    'print'), 
                     initComplete = DT::JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                       "}")
      )
    )
  }, server = FALSE)
  
  output$expressionname2 <- DT::renderDT({
    DT::datatable(
      expression_name_description2
      , escape = FALSE, rownames= FALSE, selection="single", 
      options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                     buttons = list('pageLength', 'copy', 
                                    list(extend = 'csv',   filename =  paste("searchlocation", sep = "-")),
                                    list(extend = 'excel', filename =  paste("searchloaction", sep = "-")),
                                    'print'), 
                     initComplete = DT::JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                       "}")
      )
    )
  }, server = FALSE)
  
  output$expressionname3 <- DT::renderDT({
    DT::datatable(
      expression_name_description3
      , escape = FALSE, rownames= FALSE, selection="single", 
      options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                     buttons = list('pageLength', 'copy', 
                                    list(extend = 'csv',   filename =  paste("searchlocation", sep = "-")),
                                    list(extend = 'excel', filename =  paste("searchloaction", sep = "-")),
                                    'print'), 
                     initComplete = DT::JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                       "}")
      )
    )
  }, server = FALSE)
  
  
  #load example
  observe({
    if (input$gstExamID >0 & input$expression_genome == "Expression data of Zhonghuang 13") {
      isolate({
        updateTextAreaInput(session, "gtsgenePaste", value = "SoyZH13_06G264500\nSoyZH13_13G194400\nSoyZH13_13G210100\nSoyZH13_04G102300\nSoyZH13_19G209300\nSoyZH13_06G036700\nSoyZH13_10G274300\nSoyZH13_19G158800\nSoyZH13_07G167500\nSoyZH13_19G103600\nSoyZH13_15G137100\nSoyZH13_07G047000\nSoyZH13_08G078200\nSoyZH13_18G266500\nSoyZH13_18G011800\nSoyZH13_01G013900\nSoyZH13_13G100700\nSoyZH13_13G008800\nSoyZH13_08G309800\nSoyZH13_02G172500\nSoyZH13_16G103700\nSoyZH13_08G044300\nSoyZH13_20G054700\nSoyZH13_08G315800\nSoyZH13_04G031600\nSoyZH13_02G037300\nSoyZH13_02G230600\nSoyZH13_02G245000\nSoyZH13_07G195800\nSoyZH13_17G009700\nSoyZH13_19G213900\nSoyZH13_19G160100\nSoyZH13_16G017400\nSoyZH13_16G127300\nSoyZH13_20G009700\nSoyZH13_06G114600\nSoyZH13_07G225200\nSoyZH13_20G172900\nSoyZH13_14G028900\nSoyZH13_13G020601\nSoyZH13_02G243100\nSoyZH13_14G193601\nSoyZH13_03G083400\nSoyZH13_15G126800\nSoyZH13_14G193602\nSoyZH13_17G165700\nSoyZH13_13G141300\nSoyZH13_02G090600\nSoyZH13_04G048900\nSoyZH13_05G155700\nSoyZH13_12G035700\nSoyZH13_09G212400\nSoyZH13_10G044200\nSoyZH13_13G186400\nSoyZH13_02G061800\nSoyZH13_10G040800\nSoyZH13_13G316000\nSoyZH13_15G081900\nSoyZH13_11G064900\nSoyZH13_07G185103\nSoyZH13_11G159000\nSoyZH13_03G026900\nSoyZH13_09G033300\nSoyZH13_08G207100\nSoyZH13_02G143100")
    })
    } else if (input$gstExamID >0 & input$expression_genome == "Expression data of A81-356022") {
      isolate({
        updateTextAreaInput(session, "gtsgenePaste", value = "Glyma.13G035600\nGlyma.02G178400\nGlyma.14G164900\nGlyma.13G246900\nGlyma.05G160700\nGlyma.06G264900\nGlyma.03G119800\nGlyma.03G127000\nGlyma.04G103700\nGlyma.04G173900\nGlyma.16G021800\nGlyma.02G135000\nGlyma.08G316400\nGlyma.10G161900\nGlyma.17G139400\nGlyma.16G134400\nGlyma.13G142800\nGlyma.09G218900\nGlyma.13G109800\nGlyma.14G201200\nGlyma.19G041800\nGlyma.15G106300\nGlyma.10G090700\nGlyma.02G000800\nGlyma.10G045300\nGlyma.08G117700\nGlyma.03G112800\nGlyma.12G104800\nGlyma.13G323300\nGlyma.06G159600\nGlyma.18G063900\nGlyma.13G168800\nGlyma.20G204300\nGlyma.13G198300\nGlyma.01G141400\nGlyma.15G244100\nGlyma.02G046500\nGlyma.08G189000\nGlyma.02G166400\nGlyma.02G057500\nGlyma.08G281200\nGlyma.11G081700\nGlyma.12G100600\nGlyma.13G321200\nGlyma.15G184300\nGlyma.17G165300\nGlyma.08G009400\nGlyma.13G071400\nGlyma.12G171600\nGlyma.20G201500\nGlyma.07G200500\nGlyma.10G119900\nGlyma.02G270800\nGlyma.08G120800\nGlyma.13G202600\nGlyma.20G184600\nGlyma.20G118200\nGlyma.02G191900\nGlyma.01G079800\nGlyma.02G176200\nGlyma.03G142000\nGlyma.03G165900\nGlyma.03G225900\nGlyma.04G070600\nGlyma.04G118900\nGlyma.06G071000\nGlyma.06G119200\nGlyma.08G243400\nGlyma.10G142400\nGlyma.16G170600\nGlyma.u006400\nGlyma.18G034000\n")
    })
    }else if (input$gstExamID >0 & input$expression_genome == "Expression data of W05") {
      isolate({
        updateTextAreaInput(session, "gtsgenePaste", value = "Glysoja.12G033168\nGlysoja.11G032887\nGlysoja.11G029100\nGlysoja.19G052276\nGlysoja.10G028256\nGlysoja.09G023049\nGlysoja.14G039260\nGlysoja.11G031520\nGlysoja.15G040333\nGlysoja.11G030835\nGlysoja.07G017077\nGlysoja.07G017176\nGlysoja.11G030022\nGlysoja.14G037254\nGlysoja.16G042725\nGlysoja.16G043516\nGlysoja.19G052342\nGlysoja.20G054238\nGlysoja.08G021027\nGlysoja.02G003041\nGlysoja.06G015175\nGlysoja.02G004667\nGlysoja.15G040126\nGlysoja.17G045104\nGlysoja.09G025212\nGlysoja.18G047191\nGlysoja.07G018706")
      })
    }else if (input$gstExamID >0 & input$expression_genome == "Transcriptomes data of 102 soybean accessions") {
      isolate({
        updateTextAreaInput(session, "gtsgenePaste", value = "Glyma.09G073800\nGlyma.14G134300\nGlyma.13G178400\nGlyma.14G165500\nGlyma.12G181100\nGlyma.03G065900\nGlyma.14G019000\nGlyma.06G249000\nGlyma.03G064700\nGlyma.14G121900\nGlyma.10G297200\nGlyma.12G124000\nGlyma.07G106500\nGlyma.02G131100\nGlyma.14G114900\nGlyma.14G100100\nGlyma.11G099600\nGlyma.11G153600\nGlyma.13G086800\nGlyma.05G220800\nGlyma.14G148300\nGlyma.14G196000\nGlyma.08G278400\nGlyma.02G252400\nGlyma.08G339700\nGlyma.14G103300\nGlyma.04G112400\nGlyma.13G208300\nGlyma.18G272500\nGlyma.11G214300\nGlyma.06G213200\nGlyma.06G038100\nGlyma.04G154800\nGlyma.01G145500\nGlyma.04G053400\nGlyma.09G052300\nGlyma.13G162700\nGlyma.07G218000\nGlyma.04G126500\nGlyma.10G277500\nGlyma.09G122400\nGlyma.11G007700\nGlyma.14G105300\nGlyma.15G146900\nGlyma.02G024600\nGlyma.05G063300\nGlyma.04G159400\nGlyma.10G229600\nGlyma.02G213000\nGlyma.04G199700")
      })
    }else {NULL}
  })
  
  #reset
  observe({
    if (input$cleargst >0) {
      isolate({
        updateTextInput(session, "gtsgenePaste", value = "")
      })
    } else {NULL}
  })
  
  observeEvent(input$accessiongexall, {
    if(input$expression_genome == "Expression data of Zhonghuang 13"){
      shinyWidgets::updateMultiInput(
        session = session,
        inputId = "Gexp_mut_groupA",
        selected = c("cotyledon-1", "root", "stem", "leafbud-1", "leafbud-2", "leaf-1", "cotyledon-2", "stem-2", "leafbud-3", "leaf-2", "flo-1", "shoot_meristem", "flo-2", "flo-3", "flo-4", "flo-5", "pod&seed-1", "pod&seed-2", "pod&seed-3", "pod-1", "pod-2", "pod-3", "seed-1", "seed-2", "seed-3", "seed-4", "seed-5")
      )
    }else if (input$expression_genome == "Expression data of A81-356022"){
      shinyWidgets::updateMultiInput(
        session = session,
        inputId = "Gexp_mut_groupB",
        selected = c("young_leaf", "flower", "one.cm.pod", "pod.shell.10DAF", "pod.shell.14DAF", "seed.10DAF", "seed.14DAF", "seed.21DAF", "seed.25DAF", "seed.28DAF", "seed.35DAF", "seed.42DAF", "root", "nodule")
      )
    }else if (input$expression_genome == "Expression data of W05"){
      shinyWidgets::updateMultiInput(
        session = session,
        inputId = "Gexp_mut_groupC",
        selected = c("Trifoliate", "Leaves", "Roots")
      )
    }else if (input$expression_genome == "Transcriptomes data of 102 soybean accessions"){
      shinyWidgets::updateMultiInput(
        session = session,
        inputId = "Gexp_mut_groupD",
        selected = c("Anderson", "Young", "Williams82", "Lawrence", "A.K.(Harrow)", "Capital", "Dunfield", "Illini", "Kanro", "Korean", "Lincoln", "Mandarin", "Mukden", "Richland", "Haberlandt", "Ogden", "Ralsoy", "Roanoke", "S-100", "Tokyo", "Amcor", "Beeson", "Century", "Blackhawk", "Bonus", "Pella", "Calland", "Chippewa", "Clark", "Corsoy", "Cumberland", "Oakland", "Merit", "Douglas", "Ford", "Harcor", "Harosoy", "Shelby", "Hawkeye", "Kent", "Perry", "Wayne", "Williams", "Woodworth", "Zane", "Hill", "Jackson", "Centennial", "Hood", "Tracy", "Brim", "Dare", "Pickett", "Ransom", "Davis", "Gasoy17", "Holladay", "NC-Roy", "5601T", "NC-Raleigh", "PI88.788", "TN05-3027", "4J105-3-4", "5M20-2-5-2", "CL0J095-4-6", "CL0J173-6-8", "HS6-3976", "Prohio", "LD00-3309", "LD01-5907", "LD02-4485", "LD02-9050", "Magellan", "Maverick", "S06-13640", "NE3001", "Skylla", "U03-100612", "LG03-2979", "LG03-3191", "LG04-4717", "LG05-4292", "LG05-4317", "LG05-4464", "LG05-4832", "LG90-2550", "LG92-1255", "LG94-1128", "LG94-1906", "LG97-7012", "LG98-1605", "LG00-3377", "LG04-6000", "PI398.881", "PI427.136", "PI437.169B", "PI507.681B", "PI518.751", "PI561.370", "PI404.188A", "PI574.486", "IA3023")
      )
    }
    

    
  })
  
  observeEvent(input$accessiongexnone, {
    if(input$expression_genome == "Expression data of Zhonghuang 13"){
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "Gexp_mut_groupA",
      selected = character(0)
    )
    }else if (input$expression_genome == "Expression data of A81-356022"){
      shinyWidgets::updateMultiInput(
        session = session,
        inputId = "Gexp_mut_groupB",
        selected = character(0)
      )
    }else if (input$expression_genome == "Expression data of W05"){
      shinyWidgets::updateMultiInput(
        session = session,
        inputId = "Gexp_mut_groupC",
        selected = character(0)
      )
    }else if (input$expression_genome == "Transcriptomes data of 102 soybean accessions"){
      shinyWidgets::updateMultiInput(
        session = session,
        inputId = "Gexp_mut_groupD",
        selected = character(0)
      )
    }
    
  })
  
  
  #Gene correlation
  observeEvent(input$submit_GSgcor, {
    if (!exists("geneexpressioncor") & input$cor_expression_genome == "Expression data of Zhonghuang 13") {
      geneexpressioncor <- data.table::fread("./data/Gene_expression.txt", sep = "\t", data.table = F)
      geneexpressioncor <- geneexpressioncor[-55312,]
    } else if (!exists("geneexpressioncor") & input$cor_expression_genome == "Expression data of A81-356022") {
      geneexpressioncor <- data.table::fread("./data/Gene_expression_williams82.txt", sep = "\t", data.table = F)
    } else if (!exists("geneexpressioncor") & input$cor_expression_genome == "Expression data of W05") {
      geneexpressioncor <- data.table::fread("./data/Gene_expression_w05.txt", sep = "\t", data.table = F)
    } else if (!exists("geneexpressioncor") & input$cor_expression_genome == "Transcriptomes data of 102 soybean accessions") {
      geneexpressioncor <- data.table::fread("./data/Gene_expression_102soybean.txt", sep = "\t", data.table = F)
    } else {
      NULL
    }
    geneid <- unlist(strsplit(input$genecorPaste, split="\\n"))
    geneid <- trimws(geneid, which = c("both"), whitespace = "[ \t\r\n]")
    head(geneid)
    exforgene <- geneexpressioncor[geneexpressioncor$Gene_ID %in% geneid, ]
    if( nrow(exforgene) > 1 & nrow(exforgene) < 50){
      rownames(exforgene) <- exforgene$Gene_ID
      exforgene <- as.matrix(exforgene[,-1])
      exforgene <- exforgene[which(rowSums(exforgene) > 0), ]
      dk <- t(exforgene)
      ds <- cor(dk)
      dm <- data.table::melt(ds)
      dm <- dm[, c(2,1,3)]
      dm <- dm[order(abs(dm[,3]), decreasing = TRUE), ]
      dm <- dm[abs(dm[,3]) >= 0.3, ]
      colnames(dm) <- c("Gene1", "Gene2", "Correlation Coefficient")
      dm$Gene1 <- as.character(dm$Gene1)
      dm$Gene2 <- as.character(dm$Gene2)
      dm <- dm[dm[,3] != 1, ]
      output$genecorplot <- shiny::renderPlot({
        corrplot::corrplot(ds)
      })
      
      output$gcordata <- DT::renderDT({
        DT::datatable(
          dm, 
          escape = FALSE, rownames= FALSE, selection="none", extensions  = c("Buttons"),
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, scrollX = TRUE, 
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Gene_correlation", sep = "-")),
                                        list(extend = 'excel', filename =  paste("Gene_correlation", sep = "-")),
                                        'print'), dom = 'Bfrtip',
                         columnDefs = list(list(className = 'dt-center', targets = 0:2)),
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$genecorplotif <- renderUI({
        shinyjqui::jqui_resizable(plotOutput("genecorplot", width = paste0(input$genecorwidth, "px"), height = paste0(input$genecorheight, "px")) )
        
      })
      
      output$gcorout_title <- renderText({
        HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Correlation coefficient of gene expression levels</b></font>')
      })
    } else {
      if ( length(geneid) == 1) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error input!", type = "error",
          text = "Input at least two Gene ID"
        )
        NULL
      } else if(length(geneid) > 50){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error input!", type = "error",
          text = "Too many genes!"
        )
        NULL
      }else {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error input!", type = "error",
          text = "Input gene IDs not found or no expression data found for the input gene list!"
        )
        NULL
      }
    }
  })
  
  #load example
  observe({
    if (input$gcorExamID >0 & input$cor_expression_genome == "Expression data of Zhonghuang 13") {
      isolate({
        updateTextAreaInput(session, "genecorPaste", value = "SoyZH13_11G196000\nSoyZH13_08G253500\nSoyZH13_11G016200\nSoyZH13_05G230900\nSoyZH13_18G091401\nSoyZH13_18G133500\nSoyZH13_07G133800\nSoyZH13_07G215300\nSoyZH13_18G212600\nSoyZH13_06G190300\nSoyZH13_03G065800\nSoyZH13_04G034200\nSoyZH13_13G267500\nSoyZH13_10G274400\nSoyZH13_02G255800\nSoyZH13_16G057000\nSoyZH13_19G208300\nSoyZH13_12G018300\nSoyZH13_05G117701")
      })
    } else if (input$gcorExamID >0 & input$cor_expression_genome == "Expression data of A81-356022") {
      isolate({
        updateTextAreaInput(session, "genecorPaste", value = "Glyma.13G035600\nGlyma.02G178400\nGlyma.14G164900\nGlyma.13G246900\nGlyma.05G160700\nGlyma.06G264900\nGlyma.03G119800\nGlyma.03G127000\nGlyma.04G103700\nGlyma.04G173900\nGlyma.16G021800\nGlyma.02G135000\nGlyma.08G316400\nGlyma.10G161900\nGlyma.17G139400\nGlyma.16G134400\nGlyma.13G142800\nGlyma.09G218900\nGlyma.13G109800\nGlyma.14G201200\nGlyma.19G041800\nGlyma.15G106300")
      })
    }else {NULL}
    
  })
  
  #reset
  observe({
    if (input$cleargcor >0) {
      isolate({
        updateTextInput(session, "genecorPaste", value = "")
      })
    } else {NULL}
  })
  
  
  #blast
  blast.result <- eventReactive(input$submitBLAST, {
    #library(XML)
    blast.in.seq <- ""
    if (input$In_blast == "paste") {
      blast.in.seq <- input$BlastSeqPaste
      blast.in.seq <- gsub("^\\s+", "", blast.in.seq)
      blast.in.seq <- gsub("\\s+$", "", blast.in.seq)
    } else if (input$In_blast == "upload") {
      blast.in.seq <- readLines(input$BlastSeqUpload$datapath)
    }
    
    if (input$In_blast == "paste" && blast.in.seq == "") {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "No input sequence received!", type = "error",
        text = NULL
      )
      NULL 
     } else if(input$In_blast == "upload" && file.info(input$BlastSeqUpload$datapath) == 0){
       shinyWidgets::sendSweetAlert(
         session = session,
         title = "The uploaded file is empty.", type = "error",
         text = NULL
       )
       NULL
      }else{
      blast.in.file <- gsub("\\s+", "-", Sys.time())
      blast.in.file <- gsub(":", "-", blast.in.file)
      blast.in.file <- paste0(blast.in.file, ".fasta")
      blast.in.file <- file.path(tempdir(), blast.in.file)
      writeLines(blast.in.seq, con = blast.in.file)
      
      blast.db <- input$BLASTdb
      blast.db <- gsub(" ", "_", blast.db)
      blast.db <- paste0("www/BLASTN/", blast.db)
      blast.db.fl <- paste0(blast.db, ".nhr")
      
      #if (!file.exists(blast.db.fl)) {
      if (FALSE) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "BLAST database not found!", type = "error",
          text = NULL
        )
        NULL
      } else {
        if (input$program_database == "Genome Sequence"){
          blastmethod <- input$programdna
          blastshort <- input$Filtershortsequence1
        } else if (input$program_database == "Protein Sequence"){
          blastmethod <- input$programpro
          blastshort <- input$Filtershortsequence2
        } else if (input$program_database == "Gene Sequence"){
          blastmethod <- input$programgene
          blast.db <- input$BLASTdb
          blast.db <- gsub(" ", "_", blast.db)
          blast.db <- paste0("www/BLASTN/CDS/", blast.db, ".gene")
          blastshort <- input$Filtershortsequence3
        } else {
          blastmethod <- input$programcds
          blast.db <- input$BLASTdb
          blast.db <- gsub(" ", "_", blast.db)
          blast.db <- paste0("www/BLASTN/CDS/", blast.db, ".cds")
          blastshort <- input$Filtershortsequence4
        }
        #blastp/tblastn/tblastx/blastx : seg  blastn : dust
        if (blastmethod == "blastn"){
          chosefiltermethod <- "-dust no"
        }else{
          chosefiltermethod <- "-seg no"
        }
        blast.out.file <- paste0(blast.in.file, ".blast.out")
        
        if(input$Filtercomplexity){
          blast.cmds <- paste0(blastmethod," -query ", blast.in.file," -db ", '"', paste(blast.db, sep=" ", collapse = " "), '"', " -evalue ",
                               input$BLASTev, " -outfmt 5", " -out ", blast.out.file)
        }else{
          blast.cmds <- paste0(blastmethod," -query ", blast.in.file," -db ", '"', paste(blast.db, sep=" ", collapse = " "), '" ', chosefiltermethod,  " -evalue ",
                               input$BLASTev, " -outfmt 5", " -out ", blast.out.file)
        
        }
        if (blastshort & blastmethod == "blastp") {
          blast.cmds <- paste0(blast.cmds, " -task blastp-short")
        }else if (blastshort & blastmethod != "blastp"){
          blast.cmds <- paste0(blast.cmds, " -task blastn-short")
        }else{
          NULL
        }
        system(blast.cmds, ignore.stdout = TRUE, ignore.stderr = TRUE)
        
        if ( file.size(blast.out.file) > 0 ) {
          XML::xmlParse(blast.out.file)
        } else {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "No BLAST hits found!", type = "info",
            text = NULL
          )
          NULL
        }
      }
    }   
  })
  
  #makes the datatable 
  blastedResults <- reactive({
    if ( is.null( blast.result() ) ){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "No BLAST hits found!", type = "info",
        text = NULL
      )
    } else {
      xmltop = XML::xmlRoot(blast.result())
      
      #the first chunk is for multi-fastas
      x <- which(sapply(blast.result()["//Iteration//Iteration_hits//Hit//Hsp//Hsp_num"], XML::xmlValue) == "1")
      y <- sapply(blast.result()["//Iteration//Iteration_hits//Hit//Hit_def"], XML::xmlValue)
      x1 <- c(x[-1], length(sapply(blast.result()["//Iteration//Iteration_hits//Hit//Hsp//Hsp_num"], XML::xmlValue)) + 1)
      z <- sapply(blast.result()["//Iteration//Iteration_hits//Hit//Hit_def"], XML::xmlValue)
      d <- sapply(blast.result()["//Iteration//Iteration_hits//Hit//Hit_len"], XML::xmlValue)
      
      results <- XML::xpathApply(blast.result(), '//Iteration', function(row){
        qseqid <- XML::getNodeSet(row, 'Iteration_query-def') %>% sapply(., XML::xmlValue)
        qlen <- XML::getNodeSet(row, 'Iteration_query-len') %>% sapply(., XML::xmlValue)
        qstart <- XML::getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_query-from')  %>% sapply(., XML::xmlValue)
        qend <- XML::getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_query-to')  %>% sapply(., XML::xmlValue)
        sstart <- XML::getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_hit-from')  %>% sapply(., XML::xmlValue)
        send <- XML::getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_hit-to')  %>% sapply(., XML::xmlValue)
        bitscore <- XML::getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_bit-score')  %>% sapply(., XML::xmlValue)
        evalue <- XML::getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_evalue')  %>% sapply(., XML::xmlValue)
        gaps <- XML::getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_gaps')  %>% sapply(., XML::xmlValue)
        length <- XML::getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_align-len')  %>% sapply(., XML::xmlValue)
        identity <- XML::getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_identity')  %>% sapply(., XML::xmlValue)
        pident <- round(as.integer(identity) / as.integer(length) * 100, 2)
        cbind(qseqid, qlen, qstart, qend, sstart, send, bitscore, evalue, gaps, pident, length)
      })
      
      #this ensures that NAs get added for no hits
      results <- plyr::rbind.fill( lapply(results, function(y) {as.data.frame((y), stringsAsFactors=FALSE)} ))
      results <- results[!is.na(results$qstart), ]
      
      if (ncol(results) != 11) {
        results <- NULL
      } else {
        results$sseqid <- rep(z, x1-x)
        results$sslen <- rep(d, x1-x)
        
        results <- results[, c("qseqid", "qlen", "sseqid", "sslen", "qstart", "qend", "sstart", 
                               "send", "bitscore", "evalue", "gaps", "pident", "length")]
        results$sseqid <- gsub("_chr", ", chr", results$sseqid)
        if (input$program_database == "Genome Sequence"){
        results$sseqid <- gsub("_", " ", results$sseqid)
        }else{
          NULL
        }
        
      }
      results
    }
  })
  
  output$BLASTresult <- DT::renderDT({
    DT::datatable(
      if (is.null(blastedResults()) || is.null(blast.result())) {
        blastedResults <- data.frame("V1"="No BLAST hits found!")
        colnames(blastedResults) <- ""
        blastedResults
      } else {
        blastedResults()
      }, extensions = "Buttons", escape = FALSE, rownames= FALSE, selection="single", filter = 'top', 
      options = list(scrollX = TRUE, dom = 'Brtip', bSort = FALSE,
                     buttons = list('pageLength', 'copy', 
                                    list(extend = 'csv',   filename =  paste("BLAST-result", sep = "-")),
                                    list(extend = 'excel', filename =  paste("BLAST-result", sep = "-")),
                                    'print'),
                     initComplete = DT::JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                       "}")
      ))
  }, server = FALSE)
  
  # Update Tab Panel
  observe({
    if (input$submitBLAST >0) {
      isolate({
        if (!is.null(blast.result())) {
          updateTabsetPanel(session, 'BLAST_tab', selected = 'Output')
        } else {
          NULL
        }
      })
    } else {NULL}
  })
  
  output$blastalignmentif <- renderUI({
    if (is.null(input$BLASTresult_rows_selected) || is.null(blastedResults())){
      NULL
    }else{
      shinycssloaders::withSpinner(verbatimTextOutput("alignment", placeholder = FALSE))
    }
    
  })
  
  output$alignment <- renderText({
    if(is.null(input$BLASTresult_rows_selected) || is.null(blastedResults()) ){
      NULL
    } else {
      xmltop = XML::xmlRoot(blast.result())
      
      clicked = input$BLASTresult_rows_selected
      #loop over the xml to get the alignments
      align <- XML::xpathApply(blast.result(), '//Iteration',function(row){
        top <- XML::getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_qseq') %>% sapply(., XML::xmlValue)
        mid <- XML::getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_midline') %>% sapply(., XML::xmlValue)
        bottom <- XML::getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_hseq') %>% sapply(., XML::xmlValue)
        rbind(top,mid,bottom)
      })
      
      #split the alignments every 100 carachters to get a "wrapped look"
      alignx <- do.call("cbind", align)
      splits <- strsplit(gsub("(.{100})", "\\1,", alignx[1:3,clicked]),",")
      
      #paste them together with returns '\n' on the breaks
      split_out <- lapply(1:length(splits[[1]]),function(i){
        rbind(paste0("Q:",splits[[1]][i],"\n"),paste0("M:",splits[[2]][i],"\n"),paste0("H-",splits[[3]][i],"\n"), "\n")
      })
      split_out[[1]][1] <- paste0(" ", split_out[[1]][1])
      unlist(split_out)
    }
  })
  
  #this chunk gets the alignemnt information from a clicked row
  output$clicked <- renderTable({
    if(is.null(input$BLASTresult_rows_selected) || is.null(blastedResults())){
      NULL
    } else {
      clicked = input$BLASTresult_rows_selected
      tableout<- data.frame(blastedResults()[clicked, ])
      tableout <- t(tableout)
      names(tableout) <- c("")
      rownames(tableout) <- c("query ID","query length", "subject ID", "subject length", "query start", "query end", 
                              "subject start", "subject end", "bit-score", "e-value", "gaps", "percentage of identical matches", 
                              "alignment length")
      colnames(tableout) <- NULL
      data.frame(tableout)
    }
  }, rownames =T, colnames =F)
  
  output$Alignment <- renderText({
    if (is.null(input$BLASTresult_rows_selected) || is.null(blastedResults()) ) {
      
    } else {
      HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>The detailed alignment of the selected BLAST hit</b></font>')
    }
  })
  
  ## Download BLAST example input
  output$BLAST_Input.txt <- downloadHandler(
    filename <- function() { paste('BLAST_example_input.txt') },
    content <- function(file) {
      if ( exists("exam1.fa") ){
        
      } else {
        exam1.fa <- readLines("exam1.fa")
      }
      writeLines(exam1.fa, con=file)
    }, contentType = 'text/plain'
  )
  
  ##reset
  observe({
    if (input$clear3 >0) {
      isolate({
        updateSelectInput(session, "In_blast", selected = "paste")
        updateTextAreaInput(session, "BlastSeqPaste", value="")
        shinyWidgets::updateMultiInput(session, "BLASTdb", selected = "")
        updateSelectInput(session, "program_database", selected = "Genome Sequence")
        updateSelectInput(session, "programdna", selected = "blastn")
        updateCheckboxInput(session, "Filtercomplexity", label = "Filter low complexity region", FALSE)
        updateCheckboxInput(session, "Filtershortsequence1", "Short sequence", FALSE)
        updateCheckboxInput(session, "Filtershortsequence2", "Short sequence", FALSE)
        updateCheckboxInput(session, "Filtershortsequence3", "Short sequence", FALSE)
        updateCheckboxInput(session, "Filtershortsequence4", "Short sequence", FALSE)
      })
    } else {NULL}
  })
  
  #load example
  observe({
    if (input$blastExam >0) {
      isolate({
        updateSelectInput(session, "In_blast", selected = "paste")
        if ( exists("exam1.fa") ){
        } else {
          exam1.fa <- readLines("exam1.fa")
        }
        updateTextAreaInput(session, "BlastSeqPaste", value = paste(exam1.fa, collapse = "\n"))
        shinyWidgets::updateMultiInput(session, "BLASTdb", selected = c("Zhonghuang 13"))
        updateSelectInput(session, "program_database", selected = "Genome Sequence")
        updateSelectInput(session, "programdna", selected = "blastn")
        updateCheckboxInput(session, "Filtercomplexity", "Filter low complexity region", FALSE)
        updateCheckboxInput(session, "Filtershortsequence1", "Short sequence", FALSE)
        updateCheckboxInput(session, "Filtershortsequence2", "Short sequence", FALSE)
        updateCheckboxInput(session, "Filtershortsequence3", "Short sequence", FALSE)
        updateCheckboxInput(session, "Filtershortsequence4", "Short sequence", FALSE)
      })
    } else {NULL}
  })
  
  
  #primer3
  observeEvent(input$submitprimer, {
    if ( exists("anaReg") ){
    } else {
      source("anaReg.R")
    }
    
    if ( exists("validReg") ){
    } else {
      source("validReg.R")
    }
    
    if ( exists("gene.info") ){
    } else {
      load("./data/gene.info.RData")
    }
    
    myPos <- anaReg(input$primergene)
    #library(GenomicRanges)
    if(validReg(myPos)) {
      if (!is.null(myPos)) {
        chr <- myPos$chr
        start <- myPos$start
        end <- myPos$end
        fasta <- Biostrings::readDNAStringSet(paste0("./info/Zhonghuang_13/Zhonghuang_13." ,chr, ".fasta.gz"))
        seqprimer <- Biostrings::subseq(fasta, start, end)
        SEQUENCE_ID <- "example"
        SEQUENCE_TEMPLATE <- paste0("SEQUENCE_TEMPLATE=", seqprimer)
        PRIMER_OPT_SIZE <- paste0("PRIMER_OPT_SIZE=", input$PRIMER_OPT_SIZE)
        PRIMER_MAX_SIZE <- paste0("PRIMER_MAX_SIZE=", input$PRIMER_SIZE[2])
        PRIMER_MIN_SIZE <- paste0("PRIMER_MIN_SIZE=", input$PRIMER_SIZE[1])
        
        PRIMER_OPT_TM <- paste0("PRIMER_OPT_TM=", input$PRIMER_OPT_TM)
        PRIMER_MAX_TM <- paste0("PRIMER_MAX_TM=", input$PRIMER_MAX_TM)
        PRIMER_MIN_TM <- paste0("PRIMER_MIN_TM=", input$PRIMER_MIN_TM)
        
        PRIMER_OPT_GC_PERCENT <- paste0("PRIMER_OPT_GC_PERCENT=", input$PRIMER_OPT_GC_PERCENT)
        PRIMER_MAX_GC <- paste0("PRIMER_MAX_GC=", input$PRIMER_GC[2])
        PRIMER_MIN_GC <- paste0("PRIMER_MIN_GC=", input$PRIMER_GC[1])
        
        PRIMER_MAX_NS_ACCEPTED <- paste0("PRIMER_MAX_NS_ACCEPTED=", input$PRIMER_MAX_NS_ACCEPTED)
        PRIMER_MAX_POLY_X <- paste0("PRIMER_MAX_POLY_X=", input$PRIMER_MAX_POLY_X)
        PRIMER_INTERNAL_MAX_POLY_X <- paste0("PRIMER_INTERNAL_MAX_POLY_X=", input$PRIMER_INTERNAL_MAX_POLY_X)
        PRIMER_MAX_SELF_ANY <-  paste0("PRIMER_MAX_SELF_ANY=", input$PRIMER_MAX_SELF_ANY)
        PRIMER_MAX_SELF_ANY_TH <- paste0("PRIMER_MAX_SELF_ANY_TH=", input$PRIMER_MAX_SELF_ANY_TH)
        
        PRIMER_MAX_SELF_END<- paste0("PRIMER_MAX_SELF_END=", input$PRIMER_MAX_SELF_END)
        PRIMER_MAX_SELF_END_TH <- paste0("PRIMER_MAX_SELF_END_TH=", input$PRIMER_MAX_SELF_END_TH)
        
        PRIMER_PRODUCT_SIZE_RANGE <- paste0("PRIMER_PRODUCT_SIZE_RANGE=", gsub(",", " ", input$PRIMER_PRODUCT_SIZE_RANGE))
        
        indelpos <- data.table::fread(paste0("./info/Position/", chr, ".indel.position"), sep = "\t", header = T, data.table = F)
        snppos <- data.table::fread(paste0("./info/Position/", chr, ".snp.position"), sep = "\t", header = T, data.table = F)
        
        snpnu <- snppos[snppos$POS <= end & snppos$POS >= start, ]
        indelnu <- indelpos[indelpos$POS <= end & indelpos$POS >= start, ]
        allnu <- rbind(snpnu, indelnu)
        allnu$POS <- allnu$POS - start
        SEQUENCE_TARGET <- paste0("SEQUENCE_TARGET=", stringr::str_c(allnu$POS, ",", allnu$length, collapse = " "))
        if (length(allnu$POS) > 0){
          primerorder <- paste(SEQUENCE_TEMPLATE, PRIMER_OPT_SIZE, PRIMER_MAX_SIZE, PRIMER_MIN_SIZE,
                               PRIMER_OPT_TM, PRIMER_MAX_TM, PRIMER_MIN_TM,
                               PRIMER_OPT_GC_PERCENT, PRIMER_MAX_GC, PRIMER_MIN_GC,
                               PRIMER_MAX_NS_ACCEPTED, PRIMER_MAX_POLY_X, PRIMER_INTERNAL_MAX_POLY_X,
                               PRIMER_MAX_SELF_ANY, PRIMER_MAX_SELF_ANY_TH, PRIMER_MAX_SELF_END, PRIMER_MAX_SELF_END_TH, 
                               SEQUENCE_TARGET, PRIMER_PRODUCT_SIZE_RANGE,"=", sep = "\n")
        } else {
          primerorder <- paste(SEQUENCE_TEMPLATE, PRIMER_OPT_SIZE, PRIMER_MAX_SIZE, PRIMER_MIN_SIZE,
                               PRIMER_OPT_TM, PRIMER_MAX_TM, PRIMER_MIN_TM,
                               PRIMER_OPT_GC_PERCENT, PRIMER_MAX_GC, PRIMER_MIN_GC,
                               PRIMER_MAX_NS_ACCEPTED, PRIMER_MAX_POLY_X, PRIMER_INTERNAL_MAX_POLY_X,
                               PRIMER_MAX_SELF_ANY,  PRIMER_MAX_SELF_ANY_TH, PRIMER_MAX_SELF_END, PRIMER_MAX_SELF_END_TH,
                               PRIMER_PRODUCT_SIZE_RANGE, "=", sep = "\n")
        }
        
        tmp.order <- file.path(tempdir(), "primer3.order")
        writeLines(primerorder, tmp.order)
        tmp.output <- file.path(tempdir(), "primer3.output")
        tmp.output1 <- file.path(tempdir(), "primer3.output1")
        
        writeLines(primerorder, tmp.order, sep = "\n")
        system(paste0("primer3_core -format_output ", tmp.order, " > ", tmp.output))
        system(paste0("primer3_core ", tmp.order, " > ", tmp.output1))
        primer3output <- readLines(tmp.output)
        primer3output1 <- readLines(tmp.output1)
        
        primertable <- primer3output[sort(c(grep("LEFT PRIMER", primer3output), grep("RIGHT PRIMER", primer3output)))]
        primertable[-c(1:2)] <- sub("^...", "", primertable[-c(1:2)])
        
        Penalty <- primer3output1[sort(c(grep("PENALTY", primer3output1)))][grep("PRIMER_PAIR", primer3output1[sort(c(grep("PENALTY", primer3output1)))])]
        Penalty <- gsub(".+=", "", Penalty)
        if ( length(primertable > 0) ) {
          output$primertable <- DT::renderDT({
            primertable <- read.table(text = primertable)
            primertable$Oligos <- paste(primertable$V1 , primertable$V2)
            primertable <- primertable[, c(11,3:10)]
            primertable$Penalty <- unlist( lapply( Penalty, function(x){rep(x, 2)} ) )
            colnames(primertable) <- c("Oligos", "Start position", "Length", "Tm", "GC percent", "Self any", "Self end", "Hairpin", "Sequence", "Penalty")
            primertable$Oligos <- gsub("LEFT PRIMER", "Forward primer", primertable$Oligos)
            primertable$Oligos <- gsub("RIGHT PRIMER", "Reverse primer", primertable$Oligos)
            DT::datatable(
              primertable
              , extensions = "Buttons", rownames = F, selection = "none",
              options = list(leftColumns = 8, scrollX = TRUE, dom = 'Bfrtip', bSort = FALSE,
                             buttons = list('pageLength', 'copy', 
                                            list(extend = 'csv',   filename =  paste("PRIMER", input$primergene, sep = "-")),
                                            list(extend = 'excel', filename =  paste("PRIMER", input$primergene, sep = "-")),
                                            'print'), 
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                               "}")
              )
            )}, server = FALSE)
          
          output$primerseq <- renderText({
            startcut <- grep("INCLUDED REGION SIZE:", primer3output) + 1
            endcut <- grep("ADDITIONAL OLIGOS", primer3output) - 1
            t <- primer3output[startcut:endcut][!primer3output[startcut:endcut] == ""]
            
            if (length(allnu$POS) > 0){
              num1 <- length(grep(",", strsplit(t[grep("TARGETS", t)], "")[[1]])) - 1
              prnm <- paste0("The sequence is in ", chr, ", from ", start, " to ", end, ". Totally ", num1, " SNPs/INDELs found.")
              t[grep("TARGETS", t)] <- prnm
              t[grep("PRODUCT SIZE", t)] <- "Primer design results in the sequence:"
            } else {
              num1 <- "0"
              prnm <- paste0("The sequence is in ", chr, ", from ", start, " to ", end, ". Totally ", num1, " SNPs/INDELs found.")
              t[grep("PRODUCT SIZE", t)] <- "Primer design results in the sequence:"
              t <- c(t[1], prnm, t[2:length(t)])
            }
            t <- gsub("left primer", "forwar primer", t)
            t <- gsub("right primer", "reverse primer", t)
            t
          }, sep = "\n")
          
          output$primerview <- renderText({
            if( length(primertable > 0) ){
              HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Primer design view for the best primer pair highlighted in lightblue in the table above</b></font>')
            } else {
            }
          })
        } else {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = primer3output[[1]]
          )
        }
      } else {
        NULL 
      }
    } else {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input a genomic region in appropriate format!"
      )
    }
  })
  
  ##reset
  observe({
    if (input$clearprimer >0) {
      isolate({
        updateTextInput(session, "primergene", value = "")
      })
    } else {NULL}
  })
  
  #load example
  observe({
    if (input$primerExam >0) {
      updateTextInput(session, "primergene", value = "SoyZH13_02G148200")
    } else {NULL}
  })
  
  
  #JBrowse
  observe({
    HTML.tab <- read.table("./data/jbrowse.link", sep = "\n")
    HTML.tab <- data.frame(matrix(HTML.tab$V1, ncol = 3))
    colnames(HTML.tab) <- rep("",3 )
    
    # HTML.tab[13,3] <- ""
    # HTML.tab[14,3] <- ""
    
    output$JBrowsetable <- DT::renderDataTable(HTML.tab,  
                                               options = list(pageLength = 15, lengthChange = FALSE, Search = FALSE, info = FALSE, dom = 't', 
                                                              searchHighlight = FALSE, autoWidth = FALSE, bSort = FALSE, sDom  = '<"top">lrt<"bottom">ip' ),
                                               escape = FALSE, rownames = FALSE, selection = "none"
    )
  })
  
  
  # Accession
  output$mytable1 = DT::renderDT({
    accession <- input$mychooserA
    accession <- gsub(",.+", "", accession)
    accession <- sapply(accession, function(x){
      if (x %in% c("Improved cultivar", "Landrace", "Glycine soja")) {
        x.dat <- readLines(paste0("./data/", x, ".soya.txt") )
        return(x.dat)
      } else {
        return(x)
      }
    })
    accession <- unique(unlist(accession))
    DT::datatable(
      soya.info[soya.info$ID %in% accession, ], extensions = "Buttons", escape = FALSE, rownames = F, selection = "none", 
      options = list(bSort = FALSE, pageLength = 15,  dom = 'Bfrtip', 
                     buttons = list('pageLength', 'copy', 
                                    list(extend = 'csv',   filename =  paste("Accession", sep = "-")),
                                    list(extend = 'excel', filename =  paste("Accession", sep = "-"))
                     ),
                     initComplete = DT::JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                       "}")
      )
    )}, server = FALSE)
  
  observeEvent(input$accessionall1, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "mychooserA",
      selected = all.soya.cho
    )
  })
  
  observeEvent(input$accessionnone1, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "mychooserA",
      selected = character(0)
    )
  })
  
  output$mytable12 = DT::renderDT({
    accessionB <- input$mychooserAB
    accessionB <- gsub(",.+", "", accessionB)
    accessionB <- unique(unlist(accessionB))
    soya481 <- data.table::fread("./data/soybean481info.txt", sep = "\t", header = T, data.table = F)
    rownames(soya481) <- soya481$`Sequence Identifier`
    soya481_info <- soya481[accessionB, ]
    DT::datatable(
      soya481_info, extensions = "Buttons", escape = FALSE, rownames = F, selection = "none", 
      options = list(bSort = FALSE, pageLength = 15,  dom = 'Bfrtip', 
                     buttons = list('pageLength', 'copy', 
                                    list(extend = 'csv',   filename =  paste("Accession", sep = "-")),
                                    list(extend = 'excel', filename =  paste("Accession", sep = "-"))
                     ),
                     initComplete = DT::JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                       "}")
      )
    )}, server = FALSE)
  
  observeEvent(input$accessionall2, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "mychooserAB",
      selected = soynm
    )
  })
  
  observeEvent(input$accessionnone2, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "mychooserAB",
      selected = character(0)
    )
  })
  
  #Orthologous
  observeEvent(input$ORT_sbumit, {
    if ( exists("ort") ) {
    } else {
      ort <- data.table::fread("./info/sort_ortgroups.tsv", sep = "\t", header = T, data.table = F)
    }
    
    geneid <- input$ORT_ID
    geneid <- unlist(strsplit(geneid, "\n"))
    geneid <- trimws(geneid, which = c("both"), whitespace = "[ \t\r\n]")
    
    # geneidtrue <- gsub(".+\\.", "", gsub(".+_", "", geneid))
    # accession <- gsub("_.+", "", gsub("\\..+", "", geneid))
    # if ( geneid == "") {
    #   ogid <- data.frame() 
    # } else if ( paste0(strsplit(geneid, "")[[1]][1:3], collapse = "") %in% c("Gcy", "Gtt", "Gfa", "Gst", "Gsy", "Gto") ){
    #   ids <- paste0(strsplit(geneid, "")[[1]][1:3], collapse = "")
    #   ogid <- ort[grep(geneid, ort[ ,grepl(ids, ort[1,]) ] ), ]
    # } else{
    #   ogid <- ort[grep(geneid, ort[ ,which(accession == gsub("\\..+|_.+", "", ort[1,])) ] ), ]
    # }
    orthpplydata <- lapply(geneid, function(x){
      # geneidtrue <- gsub(".+\\.", "", gsub(".+_", "", x))
      accession <- gsub("_.+", "", gsub("\\..+", "", x))
      loc <- which(x == geneid)
      GCCID <- unlist(lapply(1:40, function(x){paste0(c("Gcy", "Gtt", "Gfa", "Gst", "Gsy", "Gto"), x, "g")}))
      if ( x == "") {
      } else if ( paste0(strsplit(x, "")[[1]][1:3], collapse = "") %in% c("Gcy", "Gtt", "Gfa", "Gst", "Gsy", "Gto") & nchar(unlist(strsplit(x, "g")[[1]][2])) == 6 ){
        ids <- paste0(strsplit(x, "")[[1]][1:3], collapse = "")
        ogid <- ort[grep(x, ort[ ,grepl(ids, ort[1,]) ] ), ]
        ogid[2, ] <- paste0("Group", loc)
        ogid <- t(ogid)
        return(ogid)
        
        
      } else if (paste0(strsplit(x, "")[[1]][1:8], collapse = "") == "GlymaLee" & nchar(x) == 18){
        ogid <- ort[grep(x, ort[ ,which(accession == gsub("\\..+|_.+", "", ort[1,])) ] ), ]
        ogid[2, ] <- paste0("Group", loc)
        ogid <- t(ogid)
        return(ogid)
      } else if (paste0(strsplit(x, "")[[1]][1:13], collapse = "") == "GlysoPI483463" & nchar(x) == 23){
        ogid <- ort[grep(x, ort[ ,which(accession == gsub("\\..+|_.+", "", ort[1,])) ] ), ]
        ogid[2, ] <- paste0("Group", loc)
        ogid <- t(ogid)
        return(ogid)
      } else if (paste0(strsplit(x, "")[[1]][1:3], collapse = "") == "Soy" & paste0(strsplit(x, "")[[1]][1:5], collapse = "") != "SoyZH" & nchar(x) == 16){
        ogid <- ort[grep(x, ort[ ,which(accession == gsub("\\..+|_.+", "", ort[1,])) ] ), ]
        ogid[2, ] <- paste0("Group", loc)
        ogid <- t(ogid)
        return(ogid)
      } else if (paste0(strsplit(x, "")[[1]][1:5], collapse = "") == "Glyma" & nchar(x) == 15){
        ogid <- ort[grep(x, ort[ ,which(accession == gsub("\\..+|_.+", "", ort[1,])) ] ), ]
        ogid[2, ] <- paste0("Group", loc)
        ogid <- t(ogid)
        return(ogid)
      } else if (paste0(strsplit(x, "")[[1]][1:5], collapse = "") == "SoyZH" & nchar(x) == 17) {
        ogid <- ort[grep(x, ort[ ,which(accession == gsub("\\..+|_.+", "", ort[1,])) ] ), ]
        ogid[2, ] <- paste0("Group", loc)
        ogid <- t(ogid)
        return(ogid)
      }
    })
    
    orthpplydatasum <- do.call(rbind, orthpplydata)
    orthpplydatasum <- data.frame(rownames(orthpplydatasum), orthpplydatasum)
    if(nrow(orthpplydatasum) == 0){
      nrowsuppl <- 0
    } else {
      nrowsuppl <- nrow(orthpplydatasum[!is.na(orthpplydatasum[,2]), ])
    }
    
    if (  nrow(orthpplydatasum) != 0 & nrowsuppl != 0  ){
      
      drawogid <- orthpplydatasum
      
      
      colnames(orthpplydatasum) <- c("Genome", "Orthologous genes", "Ortholog groups")
      orthpplydatasum <- orthpplydatasum[orthpplydatasum[ ,2] != "", ]
      orthpplydatasum <- orthpplydatasum[!is.na(orthpplydatasum[ ,2]), ]
      orthpplydatasum$Genome <- gsub("_", " ", orthpplydatasum$Genome)
      ##热图
      colnames(drawogid) <- c("Genome", "Genes", "Groups")
      drawogid$Genes[is.na(drawogid$Genes)] <- ""
      drawogid$Genes <- as.character(drawogid$Genes)
      orthnum <- sapply(drawogid$Genes, function(x){
        num <- length(unlist(strsplit(x, ",")))
      })
      drawogid$Orthologous <- orthnum
      drawogidnm <- geneid
      names(drawogidnm) <- paste0("Group", 1:length(drawogidnm))
      drawogid[,3] <- as.character(drawogid[,3])
      drawogid[,3] <- drawogidnm[drawogid[,3]]
      drawogid <- drawogid[,c(1,3,4)]
      rownames(drawogid) <- NULL
      gogidorder <- c("Glycine tomentella D3", "Glycine syndetika", "Glycine stenophita", "Glycine falcata", "Glycine dolichocarpa", 
                      "Glycine cyrtoloba", "Hwangkeum", "Zhonghuang 13", "Tianlong1", "Yu Dou No.22", "Xu Dou No.1", "Williams 82", 
                      "Wan Dou No.28", "Tie Feng No.18", "Qi Huang No.34", "PI 548362", "Ju Xuan No.23", "Jin Dou No.23", "Ji Dou No.17",
                      "Ke Shan No.1", "Hei He No.43", "Han Dou No.5", "Dong Nong No.50", "Amsoy", "Lee", "Zi Hua No.4", "Zhutwinning2", 
                      "Zhang Chun Man Cang Jin", "Tong Shan Tian E Dan", "Tie Jia Si Li Huang", 
                      "Shi Sheng Chang Ye", "PI 398296", "Feng Di Huang", "58-161", "W05", "PI 483463", "PI 578357", "PI 562565", "PI 549046")
      drawogid$Genome <- gsub("_", " ", drawogid$Genome)
      gogidorder <- gogidorder[gogidorder %in% unique(drawogid$Genome)]
      drawogid$Genome <- factor(drawogid$Genome, levels = gogidorder)
      #delete character(0)
      output$ortID <- DT::renderDT({
        DT::datatable(
          orthpplydatasum, 
          escape = FALSE, rownames= FALSE, selection="none", extensions  = c("Buttons"),
          options = list(pageLength = 15, autoWidth = FALSE, bSort = FALSE, scrollX = TRUE, 
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Orthologous",  sep = "-")),
                                        list(extend = 'excel', filename =  paste("Orthologous",  sep = "-")), 
                                        'print'), dom = 'Bfrtip',
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
          #extensions = c("Buttons"), selection = "none",
          # options = list(pageLength = 40, lengthChange = FALSE, Search = FALSE, info = FALSE, dom = 'Bt', selection = "none",
          #                buttons = list('copy', 
          #                               list(extend = 'csv',   filename =  paste("Orthologous", geneid, sep = "-")),
          #                               list(extend = 'excel', filename =  paste("Orthologous", geneid, sep = "-")), 
          #                               'print'),
          #                searchHighlight = FALSE, autoWidth = FALSE, bSort = FALSE, 
          #                sDom  = '<"top">lrt<"bottom">ip',
          #                initComplete = DT::JS(
          #                  "function(settings, json) {",
          #                  "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
          #                  "}")
          # ), escape = FALSE, rownames = FALSE
        )}, server = FALSE)
      drawogid$Orthologous <- as.numeric(drawogid$Orthologous)
      if ( input$shownum ){
        orthplot <- ggplot2::ggplot(drawogid, ggplot2::aes(Groups, Genome)) +
          ggplot2::geom_tile(ggplot2::aes(fill = factor(Orthologous)),colour = "white") +
          ggplot2::geom_text(ggplot2::aes(label =  Orthologous) )+
          #ggplot2::geom_tile(ggplot2::aes(fill = Orthologous),colour = "white") +
          #ggplot2::scale_fill_gradientn(colours=c("#CCCCFF","#9999FF","#6666FF","#3333FF","#FFCCCC","#FF9999","#FF6666","#FF3333"))+
          #ggplot2::geom_raster() + 
          ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, angle = 90, hjust = 1)) +
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12)) +
          ggplot2::xlab("") + ggplot2::ylab("") +
          ggplot2::labs(fill="# Orthologs")
        
      }else{
        orthplot <- ggplot2::ggplot(drawogid, ggplot2::aes(Groups, Genome)) +
          
          ggplot2::geom_tile(ggplot2::aes(fill = factor(Orthologous)),colour = "white") +
          #ggplot2::geom_text(ggplot2::aes(label =  Orthologous) )+
          #ggplot2::geom_tile(ggplot2::aes(fill = Orthologous),colour = "white") +
          #ggplot2::scale_fill_gradientn(colours=c("#CCCCFF","#9999FF","#6666FF","#3333FF","#FFCCCC","#FF9999","#FF6666","#FF3333"))+
          #ggplot2::geom_raster() + 
          ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, angle = 90, hjust = 1)) +
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12)) +
          ggplot2::xlab("") + ggplot2::ylab("") +
          ggplot2::labs(fill="Orthologous")
        
      }

      output$orthplotif <- renderUI({
        if (length(geneid) > 5){
        plotOutput("orthplot", width = "100%", height = paste0("800px"))
          }else{
          orthwidth <- paste0((length(geneid)*100 + 200), "px")
          plotOutput("orthplot", width = orthwidth, height = paste0("800px"))
        }
        
        })
      output$orthplot <- shiny::renderPlot({
        orthplot
      })
      
    } else {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input correct Gene ID!"
      )
      NULL
    }
  })
  
  ##reset
  observe({
    if (input$clearort >0) {
      isolate({
        updateTextInput(session, "ORT_ID", value="")
      })
    } else {NULL}
  })
  
  #load example
  # observe({
  #   if (input$ortExam >0) {
  #     isolate({
  #       updateTextInput(session, "ORT_ID", value="SoyZH13_01G225600")
  #     })
  #   } else {NULL}
  # })
  #load example
  observe({
    if (input$ortExam >0) {
      isolate({
        updateTextAreaInput(session, "ORT_ID", value = "SoyZH13_20G124201\nSoyZH13_14G131103\nSoyZH13_10G049500\nSoyZH13_01G054202\nSoyZH13_13G043301\nSoyZH13_10G214500\nSoyZH13_05G061800\nSoyZH13_11G118400\nSoyZH13_02G038800\nSoyZH13_16G155300\nSoyZH13_13G319400\nSoyZH13_19G153500\nSoyZH13_20G135100\nSoyZH13_18G142622\nSoyZH13_12G186900\nSoyZH13_11G123500\nSoyZH13_18G024500\nSoyZH13_20G078300\nSoyZH13_02G231900\nSoyZH13_03G179600" )
      })
    } else {NULL}
  })
  
  # 
  #zhanwei4
  output$zhanwei4 <- renderUI({
    if (input$ORT_sbumit ){
      
    } else {
      plotOutput("zhanwei4", width = "100%", height = "600px")}
  })
  
  #GO Annotation
  observeEvent(input$GOre_sbumit, {
    if(exists("goid.tb")) {
    } else {
      goid.tb <- read.table("./data/eggNOG/go.tb", sep = "\t", header = T, as.is = T, quote = "", fill = T)
    }
    
    if (input$In_GOre == "paste2") {
      geneid <- input$GOGENErePaste
      geneid <- unlist(strsplit(geneid, "\n"))
      geneid <- trimws(geneid, which = c("both"), whitespace = "[ \t\r\n]")
    } else if (input$In_GOre == "upload2") {
      geneid <- readLines(input$GOGENEreUpload$datapath)
    }
    geneid <- unique(geneid)
    
    path_GOID <- paste0("./data/eggNOG/", gsub(" ", "_", input$GO_variety_ID))
    #KOannotation <- data.table::fread(paste0(path_GOID, "/KOannotation.tsv"), sep = "\t", quote = "", data.table = F)
    GOannotation <- data.table::fread(paste0(path_GOID, "/GOannotation.tsv"), sep = "\t", quote = "", data.table = F)
    GOannotation[GOannotation == ""] <- NA
    GOannotation <- na.omit(GOannotation)
    load(paste0("./info/", gsub(" ", "_", input$GO_variety_ID), "/", gsub(" ", "_", input$GO_variety_ID), ".gene.info.RData"))
    
    geneid <- geneid[geneid %in% gene_info_s$id]
    geneid <- geneid[geneid %in% GOannotation$gene]
    
    if (length(geneid) != 0 ){
      out.GOID <- lapply(geneid, function(x){
        chrGOID <- gene_info_s[gene_info_s$id == x, 2]
        startGOID <- gene_info_s[gene_info_s$id == x, 3]
        endGOID <- gene_info_s[gene_info_s$id == x, 4]
        GOID <- GOannotation[GOannotation$gene == x, ]
        GOID$GO_Name <- goid.tb$Description[unlist(lapply(GOID$GO, function(i){which( i == goid.tb$GO)}))]
        GOID$Chr <- chrGOID
        GOID$Start <- startGOID
        GOID$End <- endGOID
        return(GOID)
      })
      
      out.GOID <- do.call(rbind, out.GOID)
      out.GOID <- out.GOID[,c(5:7, 1, 2, 4,3)]
      colnames(out.GOID) <- c("Chr", "Start", "End", "Locus", "GO ID", "GO Name", "Gene Ontology")
      
      output$GOidout <- DT::renderDT({
        DT::datatable(
          out.GOID, 
          escape = FALSE, rownames= FALSE, selection="none", extensions = "Buttons",
          options = list(pageLength = 15, autoWidth = FALSE, bSort = FALSE, dom = 'Bfrtip',
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("GO_Annotation", sep = "-")),
                                        list(extend = 'excel', filename =  paste("GO_Annotation", sep = "-")), 
                                        'print'), 
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$GOidout_title <- renderText({
        HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>GO Annotation</b></font>')
      })
      
      output$GOre1barplotif <- renderUI({
        if ( is.na(input$goanwidth) | is.na(input$goanheight) | input$goanheight*input$goanwidth == 0 ){
          NULL
        } else {
          plotOutput("GOidbarplot", width = paste0(input$goanwidth, "px"), height = paste0(input$goanheight, "px"))
        }
      })
      
      output$GOidbarplot <- shiny::renderPlot({
        goant <- GOannotation[GOannotation$gene %in% geneid, ]
        bargo <- sort(table(goant$GO), decreasing = T)
        bargod <- data.frame(bargo, stringsAsFactors = F)
        bargod$Description <- as.character(lapply(1:nrow(bargod), function(x){goid.tb[goid.tb$GO == bargod$Var1[x], 2]}))
        bargod$Level <- as.character(lapply(1:nrow(bargod), function(x){goid.tb[goid.tb$GO == bargod$Var1[x], 3]}))
        bargod$color <- "red"
        bargod$color[bargod$Levels == "Biological Process"] <- "skyblue"
        bargod$color[bargod$Levels == "Cellular Component"] <- "green"
        if(input$GOnrow >= nrow(bargod)){
          drawdata <- bargod
        }else{
          drawdata <- bargod[1:input$GOnrow, ]
        }

        p1 <- ggplot2::ggplot(drawdata, ggplot2::aes(x = Description, y = Freq, fill = Level)) + 
          ggplot2::geom_bar(stat="identity", width=0.9) + ggplot2::facet_grid(~Level, scale="free", space='free') + 
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1 , size = 15, face="bold"), 
                         axis.text.y = ggplot2::element_text(size = 12, face="bold"),
                         axis.title.y = ggplot2::element_text(size = 20), axis.title.x = ggplot2::element_text(size = 20)) + 
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                         panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), 
                         strip.text.x = ggplot2::element_text(size = 15, colour = "black", face = "bold")) + 
          ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(drawdata$Freq)*1.1)) + 
          ggplot2::scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=40)) + ggplot2::xlab("") + 
          ggplot2::ylab("Number of genes") + ggplot2::geom_text(ggplot2::aes(label=Freq), position = ggplot2::position_dodge(width=0.9), vjust=-0.25, size = 300/(as.numeric(input$GOnrow)+30)) +
          ggplot2::guides(fill=FALSE) + ggplot2::theme(text =  ggplot2::element_text(family = "serif"))
        p1
      })
    } else {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input correct gene IDs of the selected genome."
      )
      NULL
    }
  })
  
  output$GOre_Input.txt <- downloadHandler(
    filename <- function() { paste('GO_Annotation_example_input.txt') },
    content <- function(file) {
      if ( exists("genelist") ){
      } else {
        genelist <- readLines("./data/eggNOG/genelist.txt")
      }
      writeLines(genelist, con=file)
    }, contentType = 'text/plain'
  )
  
  #load example
  observe({
    if (input$GOreExam >0) {
      isolate({
        updateSelectInput(session, "In_GOre", selected = "paste")
        if ( exists("genelist") ){
        } else {
          genelist <- readLines("./data/eggNOG/genelist.txt")
        }
        updateSelectInput(session, "GO_variety_ID", selected = "Zhonghuang 13")
        updateSelectInput(session, "In_GOre", selected = "paste2")
        updateSliderInput(session, "GOnrow", value = 30)
        updateTextAreaInput(session, "GOGENErePaste", value = paste(genelist, collapse = "\n"))
      })
    } else {NULL}
  })
  
  #reset
  observe({
    if (input$clearGOre >0) {
      isolate({
        updateSelectInput(session, "GO_variety_ID", selected = "Zhonghuang 13")
        updateSelectInput(session, "In_GOre", selected = "paste2")
        updateTextAreaInput(session, "GOGENErePaste", value = "")
        updateSliderInput(session, "GOnrow", value = 30)
      })
    } else {NULL}
  })
  
  #GO annotation update
  GOA <- reactive({
    req(input$GO_variety_ID)
    c("")
  })
  
  observeEvent(GOA(), {
    if ( input$GO_variety_ID == "Zhonghuang 13"){
      
    } else {
      updateTextInput(session, "GOGENErePaste", value = "")
    }
  })
  
  #GO enrichment
  observeEvent(input$GOge_sbumit, {
    if(exists("goid.tb")){
      
    } else {
      goid.tb <- read.table("./data/eggNOG/go.tb", sep = "\t", header = T, as.is = T, quote = "", fill = T)
    }
    
    if (input$In_GOge == "paste3") {
      geneid <- input$GOGENEgePaste
      geneid <- unlist(strsplit(geneid, "\n"))
      geneid <- trimws(geneid, which = c("both"), whitespace = "[ \t\r\n]")
    } else if (input$In_GOge == "upload3") {
      geneid <- readLines(input$GOGENEgeUpload$datapath)
    }
    geneid <- unique(geneid)
    
    path_GOID <- paste0("./data/eggNOG/", gsub(" ", "_", input$GOge_variety_ID))
    GOannotation <- data.table::fread(paste0(path_GOID, "/GOannotation.tsv"), sep = "\t", quote = "", data.table = F)
    GOannotation[GOannotation == ""] <- NA
    GOannotation <- na.omit(GOannotation)
    load(paste0("./info/", gsub(" ", "_", input$GOge_variety_ID), "/", gsub(" ", "_", input$GOge_variety_ID), ".gene.info.RData"))
    geneid <- geneid[geneid %in% gene_info_s$id]
    
    if (length(geneid) != 0 ) {
      GOannotation = split(GOannotation, with(GOannotation, level))
      GOEmf <- clusterProfiler::enricher(geneid, TERM2GENE=GOannotation[['Molecular Function']][c(2,1)], TERM2NAME=goid.tb[1:2], pvalueCutoff = input$GOp, qvalueCutoff = input$GOq)
      GOEbp <- clusterProfiler::enricher(geneid, TERM2GENE=GOannotation[['Biological Process']][c(2,1)], TERM2NAME=goid.tb[1:2], pvalueCutoff = input$GOp, qvalueCutoff = input$GOq)
      GOEcc <- clusterProfiler::enricher(geneid, TERM2GENE=GOannotation[['Cellular Component']][c(2,1)], TERM2NAME=goid.tb[1:2], pvalueCutoff = input$GOp, qvalueCutoff = input$GOq)
      
      output$GOgeout1 <- DT::renderDT({
        DT::datatable(
          GOEmf@result[,1:8],
          escape = FALSE, rownames= FALSE, selection="none", extensions = c("Buttons"),
          options = list(pageLength = 5, autoWidth = FALSE, bSort = FALSE, scrollX = TRUE, #fixedColumns = list(leftColumns = 2),
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("GO_Molecular_Function",  sep = "-")),
                                        list(extend = 'excel', filename =  paste("GO_Molecular_Function",  sep = "-")), 
                                        'print'), dom = 'Bfrtip',
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$GOgeout2 <- DT::renderDT({
        DT::datatable(
          GOEbp@result[,1:8],
          escape = FALSE, rownames= FALSE, selection="none", extensions = c("Buttons"),
          options = list(pageLength = 5, autoWidth = FALSE, bSort = FALSE, scrollX = TRUE, #fixedColumns = list(leftColumns = 2),
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("GO_Biological_Process",  sep = "-")),
                                        list(extend = 'excel', filename =  paste("GO_Biological_Process",  sep = "-")), 
                                        'print'), dom = 'Bfrtip',
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$GOgeout3 <- DT::renderDT({
        DT::datatable(
          GOEcc@result[,1:8],
          escape = FALSE, rownames= FALSE, selection="none", extensions  = c("Buttons"),
          options = list(pageLength = 5, autoWidth = FALSE, bSort = FALSE, scrollX = TRUE, #fixedColumns = list(leftColumns = 2),
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("GO_Cellular_Component", sep = "-")),
                                        list(extend = 'excel', filename =  paste("GO_Cellular_Component", sep = "-")), 
                                        'print'), dom = 'Bfrtip',
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$GOgeout1_title <- renderText({
        HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Molecular Function</b></font>')
      })
      output$GOgeout2_title <- renderText({
        HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Biological Process</b></font>')
      })
      
      output$GOgeout3_title <- renderText({
        HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Cellular Component</b></font>')
      })
      
      if ( nrow(GOEmf@result[GOEmf@result$p.adjust <= input$GOp & GOEmf@result$qvalue <= input$GOq, ]) != 0 ){
        output$GOge1barplotif <- renderUI({
          if ( is.na(input$gowidth1) | is.na(input$goheight1) | input$gowidth1*input$goheight1 == 0 ){
            NULL
          } else {
            plotOutput("GOge1barplot", width = paste0(input$gowidth1, "px"), height = paste0(input$goheight1, "px"))
          }
        })
      } else {
        output$GOge1barplotif <- renderUI({
          NULL
        })
      }
      
      if ( nrow(GOEbp@result[GOEbp@result$p.adjust <= input$GOp & GOEbp@result$qvalue <= input$GOq, ]) != 0){
        output$GOge2barplotif <- renderUI({
          if ( is.na(input$gowidth2) | is.na(input$goheight2) | input$gowidth2*input$goheight2 == 0 ){
            NULL
          } else {
            plotOutput("GOge2barplot", width = paste0(input$gowidth2, "px"), height = paste0(input$goheight2, "px"))
          }
        }) 
      } else {
        output$GOge2barplotif <- renderUI({
          NULL
        })
      }
      
      if ( nrow(GOEcc@result[GOEcc@result$p.adjust <= input$GOp & GOEcc@result$qvalue <= input$GOq, ]) != 0 ) {
        output$GOge3barplotif <- renderUI({
          if ( is.na(input$gowidth3) | is.na(input$goheight3) | input$goheight3*input$gowidth3 == 0 ){
            NULL
          } else {
            plotOutput("GOge3barplot", width = paste0(input$gowidth3, "px"), height = paste0(input$goheight3, "px"))
          }  
        })
      } else {
        output$GOge3barplotif <- renderUI({
          NULL
        })
      }
      
      if ( nrow(GOEmf@result[GOEmf@result$p.adjust <= input$GOp & GOEmf@result$qvalue <= input$GOq, ]) != 0){
        output$GOge1barplot <- shiny::renderPlot({
          p1 <- barplot(GOEmf, showCategory = input$GOgenrow, order = T) + ggplot2::scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=40)) + ggplot2::ylab("Number of genes")
          p2 <- enrichplot::dotplot(GOEmf, showCategory=input$GOgenrow, order = "x") + ggplot2::scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=40)) + ggplot2::xlab("Gene Ratio")
          cowplot::plot_grid(p1, p2, ncol=2)
        }) 
      } else {NULL}
      
      if ( nrow(GOEbp@result[GOEbp@result$p.adjust <= input$GOp & GOEbp@result$qvalue <= input$GOq, ]) != 0 ){
        output$GOge2barplot <- shiny::renderPlot({
          p1 <- barplot(GOEbp, showCategory = input$GOgenrow, order = T) + ggplot2::scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=40)) + ggplot2::ylab("Number of genes")
          p2 <- enrichplot::dotplot(GOEbp, showCategory=input$GOgenrow, order = "x") + ggplot2::scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=40)) + ggplot2::xlab("Gene Ratio")
          cowplot::plot_grid(p1, p2, ncol=2)
        }) 
      } else {NULL}
      
      if ( nrow(GOEcc@result[GOEcc@result$p.adjust <= input$GOp & GOEcc@result$qvalue <= input$GOq, ]) != 0 ){
        output$GOge3barplot <- shiny::renderPlot({
          p1 <- barplot(GOEcc, showCategory = input$GOgenrow, order = T) + ggplot2::scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=40)) + ggplot2::ylab("Number of genes")
          p2 <- enrichplot::dotplot(GOEcc, showCategory=input$GOgenrow, order = "x") + ggplot2::scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=40)) + ggplot2::xlab("Gene Ratio")
          cowplot::plot_grid(p1, p2, ncol=2)
        })
      } else {NULL}
      
    } else {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input correct gene IDs of the selected genome."
      )
      NULL
    }
  })
  
  output$GOge_Input.txt <- downloadHandler(
    filename <- function() { paste('GO_Enrichment_example_input.txt') },
    content <- function(file) {
      if ( exists("genelist") ){
        
      } else {
        genelist <- readLines("./data/eggNOG/gogenelist.txt")
      }
      writeLines(genelist, con=file)
    }, contentType = 'text/plain'
  )
  
  #load example
  observe({
    if (input$GOgeExam >0) {
      isolate({
        updateSelectInput(session, "In_GOge", selected = "paste")
        if ( exists("genelist") ){
        } else {
          genelist <- readLines("./data/eggNOG/gogenelist.txt")
        }
        updateSelectInput(session, "GOge_variety_ID", selected = "Zhonghuang 13")
        updateSelectInput(session, "In_GOge", selected = "paste3")
        updateSliderInput(session, "GOgenrow", value = 30)
        updateTextAreaInput(session, "GOGENEgePaste", value = paste(genelist, collapse = "\n"))
        updateSliderInput(session, "GOp", value = 0.05)
        updateSliderInput(session, "GOq", value = 0.05)
      })
    } else {NULL}
  })
  
  #reset
  observe({
    if (input$clearGOge >0) {
      isolate({
        updateSelectInput(session, "GOge_variety_ID", selected = "Zhonghuang 13")
        updateSelectInput(session, "In_GOge", selected = "paste3")
        updateTextAreaInput(session, "GOGENEgePaste", value = "")
        updateSliderInput(session, "GOgenrow", value = 30)
        updateSliderInput(session, "GOp", value = 0.05)
        updateSliderInput(session, "GOq", value = 0.05)
      })
    } else {NULL}
  })
  
  #GO Enrichment update
  GOE <- reactive({
    req(input$GOge_variety_ID)
    c("")
  })
  
  observeEvent(GOE(), {
    if ( input$GOge_variety_ID == "Zhonghuang 13"){
      
    } else {
      updateTextInput(session, "GOGENEgePaste", value = "")
    }
  })
  
  #KEGG Annotation
  observeEvent(input$KEGGre_sbumit, {
    if (input$In_KEre == "paste2") {
      geneid <- input$KEGG_GENErePaste
      geneid <- unlist(strsplit(geneid, "\n"))
      geneid <- trimws(geneid, which = c("both"), whitespace = "[ \t\r\n]")
    } else if (input$In_KEre == "upload2") {
      geneid <- readLines(input$KEGG_GENEreUpload$datapath)
    }
    geneid <- unique(geneid)
    
    path_KOID <- paste0("./data/eggNOG/", gsub(" ", "_", input$KE_variety_ID))
    KOannotation <- data.table::fread(paste0(path_KOID, "/KOannotation.tsv"), sep = "\t", quote = "", data.table = F)
    KOannotation[KOannotation == ""] <- NA
    KOannotation <- na.omit(KOannotation)
    
    load(paste0("./info/", gsub(" ", "_", input$KE_variety_ID), "/", gsub(" ", "_", input$KE_variety_ID), ".gene.info.RData"))
    
    geneid <- geneid[geneid %in% gene_info_s$id]
    geneid <- geneid[geneid %in% KOannotation$gene]
    
    if (length(geneid) != 0 ) {
      out.KOID <- lapply(geneid, function(x){
        chrKOID <- gene_info_s[gene_info_s$id == x, 2]
        startKOID <- gene_info_s[gene_info_s$id == x, 3]
        endKOID <- gene_info_s[gene_info_s$id == x, 4]
        KOID <- KOannotation[KOannotation$gene == x, ]
        KOID$Chr <- chrKOID
        KOID$Start <- startKOID
        KOID$End <- endKOID
        return(KOID)
      })
      
      out.KOID <- do.call(rbind, out.KOID)
      out.KOID <- out.KOID[,c(5:7, 1:4)]
      colnames(out.KOID) <- c("Chr", "Start", "End", "Locus", "KO", "Pathway ID", "Pathway Name")
      
      output$KOidout <- DT::renderDT({
        DT::datatable(
          out.KOID, 
          escape = FALSE, rownames= FALSE, selection="none", extensions  = c("Buttons"),
          options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE, scrollX = TRUE, 
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("KEGG_Annotation",  sep = "-")),
                                        list(extend = 'excel', filename =  paste("KEGG_Annotation",  sep = "-")), 
                                        'print'), dom = 'Bfrtip',
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$KOidout_title <- renderText({
        HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>KEGG Annotation</b></font>')
      })
      
      output$KOidbarplotif <- renderUI({
        if ( is.na(input$kegganwidth) | is.na(input$kegganheight) | input$kegganheight*input$kegganheight == 0){
          NULL
        } else {
          plotOutput("KOidbarplot", height = paste0(input$kegganheight, "px"), width = paste0(input$kegganwidth, "px"))
        }
      })
      
      output$KOidbarplot <- shiny::renderPlot({
        koant <- KOannotation[KOannotation$gene %in% geneid, ]
        barko <- sort(table(koant$KO), decreasing = T)
        barkod <- data.frame(barko, stringsAsFactors = F)
        if( nrow(barkod) < input$KOnrow & nrow(barkod) < 2){
          drawdata <- barkod
          colnames(drawdata) <- "Freq"
          drawdata$Var1 <- rownames(drawdata)
        } else if (nrow(barkod) < input$KOnrow & nrow(barkod) > 1){
          drawdata <- barkod
        } else {
          drawdata <- barkod[1:input$KOnrow, ]
        }
        
        p1 <- ggplot2::ggplot(drawdata, ggplot2::aes(x = Var1, y = Freq)) + 
          ggplot2::geom_bar(stat="identity", width=0.9, fill='steelblue') + 
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1 , size = 15, face="bold"), 
                         axis.text.y = ggplot2::element_text(size = 12, face="bold"),
                         axis.title.y = ggplot2::element_text(size = 20), axis.title.x = ggplot2::element_text(size = 20)) + 
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                         panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), 
                         strip.text.x = ggplot2::element_text(size = 15, colour = "black", face = "bold")) + 
          ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(drawdata$Freq)*1.1) ) + 
          ggplot2::xlab("KEGG Pathway") + ggplot2::ylab("Number of genes") + 
          ggplot2::geom_text(ggplot2::aes(label=Freq), position = ggplot2::position_dodge(width=0.9), vjust=-0.25, size = 300/(as.numeric(input$KOnrow)+30)) + 
          ggplot2::theme(text =  ggplot2::element_text(family = "serif"))
        p1
      })
    } else {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input correct gene IDs of the selected genome."
      )
      NULL
    }
  })
  
  output$KOre_Input.txt <- downloadHandler(
    filename <- function() { paste('KEGG_Enrichment_example_input.txt') },
    content <- function(file) {
      if ( exists("genelist") ){
        
      } else {
        genelist <- readLines("./data/eggNOG/kegggenelist.txt")
      }
      writeLines(genelist, con=file)
    }, contentType = 'text/plain'
  )
  
  #load example
  observe({
    if (input$KOreExam >0) {
      isolate({
        updateSelectInput(session, "In_KOre", selected = "paste")
        if ( exists("genelist") ) {
        } else {
          genelist <- readLines("./data/eggNOG/kegggenelist.txt")
        }
        updateSelectInput(session, "KE_variety_ID", selected = "Zhonghuang 13")
        updateSelectInput(session, "In_KEre", selected = "paste2")
        updateSliderInput(session, "KOnrow", value = 30)
        updateTextAreaInput(session, "KEGG_GENErePaste", value = paste(genelist, collapse = "\n"))
      })
    } else {NULL}
  })
  
  #reset
  observe({
    if (input$clearKOre >0) {
      isolate({
        updateSelectInput(session, "KE_variety_ID", selected = "Zhonghuang 13")
        updateSelectInput(session, "In_KEre", selected = "paste2")
        updateTextAreaInput(session, "KEGG_GENErePaste", value = "")
        updateSliderInput(session, "KOnrow", value = 30)
      })
    } else {NULL}
  })
  
  #KEGG Annotation update
  SKEGGA <- reactive({
    req(input$KE_variety_ID)
    c("")
  })
  
  observeEvent(SKEGGA(), {
    if ( input$KE_variety_ID == "Zhonghuang 13"){
      
    } else {
      updateTextInput(session, "KEGG_GENErePaste", value = "")
    }
  })
  
  #KEGG Enrichment
  observeEvent(input$KEGG_sbumit, {
    if (input$In_KE == "paste4") {
      geneid <- input$KEGG_GENErePaste1
      geneid <- unlist(strsplit(geneid, "\n"))
      geneid <- trimws(geneid, which = c("both"), whitespace = "[ \t\r\n]")
    } else if (input$In_KE == "upload4") {
      geneid <- readLines(input$KEGG_GENEreUpload1$datapath)
    }
    geneid <- unique(geneid)
    
    path_KOID <- paste0("./data/eggNOG/", gsub(" ", "_", input$KEGG_variety_ID))
    #KOannotation <- data.table::fread(paste0(path_KOID, "/KOannotation.tsv"), sep = "\t", quote = "", data.table = F)
    KOannotation <- data.table::fread(paste0(path_KOID, "/KOannotation.tsv"), sep = "\t", quote = "", data.table = F)
    KOannotation[KOannotation == ""] <- NA
    KOannotation <- na.omit(KOannotation)
    
    load(paste0("./info/", gsub(" ", "_", input$KEGG_variety_ID), "/", gsub(" ", "_", input$KEGG_variety_ID), ".gene.info.RData"))
    
    geneid <- geneid[geneid %in% gene_info_s$id]
    geneid <- geneid[geneid %in% KOannotation$gene]
    if (length(geneid) != 0 ){
      KOE <- clusterProfiler::enricher(geneid, TERM2GENE=KOannotation[c(3,1)], TERM2NAME=KOannotation[c(3,4)],  pvalueCutoff = input$KEp, qvalueCutoff = input$KEq)
      
      output$KEidout <- DT::renderDT({
        DT::datatable(
          KOE@result[,1:8], 
          escape = FALSE, rownames= FALSE, selection="none", extensions = c("Buttons"),
          options = list(pageLength = 5, autoWidth = FALSE, bSort = FALSE, scrollX = TRUE, #fixedColumns = list(leftColumns = 2),
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("KEGG_Enrichment", sep = "-")),
                                        list(extend = 'excel', filename =  paste("KEGG_Enrichment", sep = "-")),
                                        'print'), dom = 'Bfrtip',
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$KEidout_title <- renderText({
        HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>KEGG Enrichment</b></font>')
      })
      
      if ( nrow(KOE@result[KOE@result$p.adjust <= input$KEp & KOE@result$qvalue <= input$KEq, ]) != 0 ){
        output$KEidbarplot <- shiny::renderPlot({
          p1 <- barplot(KOE, showCategory = input$KEnrow, order = T) + ggplot2::scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=40))
          p2 <- enrichplot::dotplot(KOE, showCategory=input$KEnrow, order = "x") + ggplot2::scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=40)) + ggplot2::xlab("")
          cowplot::plot_grid(p1, p2, ncol=2)
        })
        
        output$KEidbarplotif <- renderUI({
          if ( nrow(KOE@result[KOE@result$p.adjust <= input$KEp & KOE@result$qvalue <= input$KEq, ]) != 0 ){
            if ( is.na(input$keggwidth) | is.na(input$keggheight) | input$keggwidth*input$keggheight == 0 ){
              NULL
            } else {
              plotOutput("KEidbarplot", width = paste0(input$keggwidth, "px"), height = paste0(input$keggheight, "px"))
            }
          } else {
            NULL        
          }
        })
      } else {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error input!", type = "error",
          text = "No enrichement results for the selected P-value or Q-value threshold."
        )
        output$KEidbarplot <- shiny::renderPlot({
          NULL
        })
        output$KEidbarplotif <- renderUI({
          plotOutput("KEidbarplot", width = "100%")
        })
      }
    } else {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input correct gene IDs of the selected genome."
      )
      NULL
    }
  })
  
  output$KO_Input.txt <- downloadHandler(
    filename <- function() { paste('KEGG_Enrichment_example_input.txt') },
    content <- function(file) {
      if ( exists("genelist") ){
        
      } else {
        genelist <- readLines("./data/eggNOG/kegggenelist.txt")
      }
      writeLines(genelist, con=file)
    }, contentType = 'text/plain'
  )
  
  #load example
  observe({
    if (input$KOExam >0) {
      isolate({
        updateSelectInput(session, "In_KO", selected = "paste")
        if ( exists("genelist") ){
        } else {
          genelist <- readLines("./data/eggNOG/kegggenelist.txt")
        }
        updateSelectInput(session, "KEGG_variety_ID", selected = "Zhonghuang 13")
        updateSelectInput(session, "In_KE", selected = "paste4")
        updateSliderInput(session, "KEnrow", value = 30)
        updateSliderInput(session, "KEp", value = 0.05)
        updateSliderInput(session, "KEq", value = 0.05)
        updateTextAreaInput(session, "KEGG_GENErePaste1", value = paste(genelist, collapse = "\n"))
      })
    } else {NULL}
  })
  
  #reset
  observe({
    if (input$clearKO >0) {
      isolate({
        updateSelectInput(session, "KEGG_variety_ID", selected = "Zhonghuang 13")
        updateSelectInput(session, "In_KE", selected = "paste4")
        updateTextAreaInput(session, "KEGG_GENErePaste1", value = "")
        updateSliderInput(session, "KEp", value = 0.05)
        updateSliderInput(session, "KEq", value = 0.05)
        updateSliderInput(session, "KEnrow", value = 30)
      })
    } else {NULL}
  })
  
  #KEGG Enrichment update
  SKEGGE <- reactive({
    req(input$KEGG_variety_ID)
    c("")
  })
  observeEvent(SKEGGE(), {
    if ( input$KEGG_variety_ID == "Zhonghuang 13"){
      
    } else {
      updateTextInput(session, "KEGG_GENErePaste1", value = "")
    }
  })
  
  #Download genome data
  observe({
    SoybeanGDB_download1 <- read.table("SoybeanGDB_download.txt", head=T, as.is=T, sep = "\t", check.names = F)
    SoybeanGDB_download1 <- SoybeanGDB_download1[,-c(9:12)]
    output$Download_table_out1 = DT::renderDataTable(
      SoybeanGDB_download1,
      options = list(lengthMenu = c(20, 30, 50), pageLength = 15,
                     searching = TRUE, bSort=FALSE,autoWidth = FALSE,
                     dom = 'Bfrtip',scrollX = TRUE,
                     columnDefs=list(list(targets="_all"))
      ), escape = FALSE, selection="none", rownames= FALSE, extensions = "Buttons"
    )
    
  })
  observe({
    SoybeanGDB_download2 <- read.table("SoybeanGDB_download.txt", head=T, as.is=T, sep = "\t", check.names = F)
    SoybeanGDB_download2 <- SoybeanGDB_download2[,-c(4:8,11:12)]
    output$Download_table_out2 = DT::renderDataTable(
      SoybeanGDB_download2,
      options = list(lengthMenu = c(20, 30, 50), pageLength = 15,
                     searching = TRUE, bSort=FALSE,autoWidth = FALSE,
                     dom = 'Bfrtip',scrollX = TRUE,
                     columnDefs=list(list(targets="_all"))
      ), escape = FALSE, selection="none", rownames= FALSE, extensions = "Buttons"
    )
    
  })
  
  observe({
    SoybeanGDB_download3 <- read.table("SoybeanGDB_download.txt", head=T, as.is=T, sep = "\t", check.names = F)
    SoybeanGDB_download3 <- SoybeanGDB_download3[,-c(4:10)]
    output$Download_table_out3 = DT::renderDataTable(
      SoybeanGDB_download3,
      options = list(lengthMenu = c(20, 30, 50), pageLength = 15,
                     searching = TRUE, bSort=FALSE,autoWidth = FALSE,
                     dom = 'Bfrtip',scrollX = TRUE,
                     columnDefs=list(list(targets="_all"))
      ), escape = FALSE, selection="none", rownames= FALSE, extensions = "Buttons"
    )
    
  })
  
  
  #ID converter
  
  observeEvent(input$submit_GSidcov, {
    #library(GenomicRanges)
    #library(IRanges)
    #library(Biostrings)
    refgenomenames <- gsub(" ","_", input$variety_idcov)
    genomenames <- gsub(" ","_", input$variety_idcov_1)
    
    if ( exists("ort") ) {
    } else {
      ort <- data.table::fread("./info/sort_ortgroups.tsv", sep = "\t", header = T, data.table = F)
    }
    
    geneid_cv <- input$IDCOV_idcov
    
    orthpplydata <- lapply(geneid_cv, function(x){
      accession <- gsub("_.+", "", gsub("\\..+", "", x))
      loc <- which(x == geneid_cv)
      GCCID <- unlist(lapply(1:40, function(x){paste0(c("Gcy", "Gtt", "Gfa", "Gst", "Gsy", "Gto"), x, "g")}))
      if ( x == "") {
      } else if ( paste0(strsplit(x, "")[[1]][1:3], collapse = "") %in% c("Gcy", "Gtt", "Gfa", "Gst", "Gsy", "Gto") & nchar(unlist(strsplit(x, "g")[[1]][2])) == 6 ){
        ids <- paste0(strsplit(x, "")[[1]][1:3], collapse = "")
        ogid <- ort[grep(x, ort[ ,grepl(ids, ort[1,]) ] ), ]
        ogid[2, ] <- paste0("Group", loc)
        ogid <- t(ogid)
        return(ogid)
      } else if (paste0(strsplit(x, "")[[1]][1:8], collapse = "") == "GlymaLee" & nchar(x) == 18){
        ogid <- ort[grep(x, ort[ ,which(accession == gsub("\\..+|_.+", "", ort[1,])) ] ), ]
        ogid[2, ] <- paste0("Group", loc)
        ogid <- t(ogid)
        return(ogid)
      } else if (paste0(strsplit(x, "")[[1]][1:13], collapse = "") == "GlysoPI483463" & nchar(x) == 23){
        ogid <- ort[grep(x, ort[ ,which(accession == gsub("\\..+|_.+", "", ort[1,])) ] ), ]
        ogid[2, ] <- paste0("Group", loc)
        ogid <- t(ogid)
        return(ogid)
      } else if (paste0(strsplit(x, "")[[1]][1:3], collapse = "") == "Soy" & paste0(strsplit(x, "")[[1]][1:5], collapse = "") != "SoyZH" & nchar(x) == 16){
        ogid <- ort[grep(x, ort[ ,which(accession == gsub("\\..+|_.+", "", ort[1,])) ] ), ]
        ogid[2, ] <- paste0("Group", loc)
        ogid <- t(ogid)
        return(ogid)
      } else if (paste0(strsplit(x, "")[[1]][1:5], collapse = "") == "Glyma" & nchar(x) == 15){
        ogid <- ort[grep(x, ort[ ,which(accession == gsub("\\..+|_.+", "", ort[1,])) ] ), ]
        ogid[2, ] <- paste0("Group", loc)
        ogid <- t(ogid)
        return(ogid)
      } else if (paste0(strsplit(x, "")[[1]][1:5], collapse = "") == "SoyZH" & nchar(x) == 17) {
        ogid <- ort[grep(x, ort[ ,which(accession == gsub("\\..+|_.+", "", ort[1,])) ] ), ]
        ogid[2, ] <- paste0("Group", loc)
        ogid <- t(ogid)
        return(ogid)
      }
    })
    orthpplydatasum <- do.call(rbind, orthpplydata)
    orthpplydatasum <- data.frame(rownames(orthpplydatasum), orthpplydatasum)
    orthpplydatasum <- orthpplydatasum[genomenames, ]
    drawogid <- orthpplydatasum
    colnames(orthpplydatasum) <- c("Genome", "Orthologous genes", "Ortholog groups")
    orthpplydatasum <- orthpplydatasum[orthpplydatasum[ ,2] != "", ]
    orthpplydatasum <- orthpplydatasum[!is.na(orthpplydatasum[ ,2]), ]
    load(paste0("./info/", genomenames, "/", genomenames, ".gene.info.RData"))
    orthpplydatasum$Genome <- gsub("_", " ", orthpplydatasum$Genome)
    gene_list <- unlist(strsplit(orthpplydatasum$`Orthologous genes`, split=","))
    gene_list <- trimws(gene_list, which = c("both"), whitespace = "[ \t\r\n]")
    geneinfo_id <- gene_info_s[gene_info_s$id %in% gene_list, ]
    
    if(nrow(orthpplydatasum) == 0){
      nrowsuppl <- 0
    } else {
      nrowsuppl <- nrow(orthpplydatasum[!is.na(orthpplydatasum[,2]), ])
    }
    if (  nrow(orthpplydatasum) != 0 & nrowsuppl != 0 & refgenomenames != genomenames){
      #cds protein cdna信息读取
      load(paste0("./info/", genomenames, "/", genomenames, ".cds.fasta.RData"))
      cds.infoid <- cds.info
      load(paste0("./info/", genomenames, "/", genomenames, ".protein.RData"))
      proteinid <- protein
      load(paste0("./info/", genomenames, "/", genomenames, ".cdna.fasta.RData"))
      cdna.infoid <- cdna.info
      gene_list <- gene_list[gene_list %in% gene_info_s$id]

      #geneinfo
      output$geneinfoidCOV <- DT::renderDT({
        if( length(geneinfo_id[, 1]) != 0 ){
          colnames(geneinfo_id) <- c("ID", "Chromosome", "Start", "End", "Strand")
          DT::datatable(
            geneinfo_id, 
            escape = FALSE, rownames= FALSE, selection="single", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("searchlocation", sep = "-")),
                                          list(extend = 'excel', filename =  paste("searchloaction", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        } else {
          geneinfo_id <- data.frame("V1"="No data available in table")
          colnames(geneinfo_id) <- ""
          DT::datatable(
            geneinfo_id,
            escape = FALSE, rownames= FALSE, selection="none", 
            options = list(pageLength = 10, autoWidth = FALSE, bSort = FALSE,
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("searchlocation", sep = "-")),
                                          list(extend = 'excel', filename =  paste("searchloaction", sep = "-")),
                                          'print'), 
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
            )
          )
        }
      }, server = FALSE)
      
      output$geneinfoidCOV_title <- renderText({
        if( length(geneinfo_id[, 1]) != 0 ) {
          HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Genes in the list (Click on a row to check the details of the selected gene)</b></font>')
        } else {}
      })
      
      #click gene 
      output$geneticIDstructureCOV <- shiny::renderPlot({
        if ( is.null(input$geneinfoidCOV_rows_selected) ) {
        } else {
          clickedid <- input$geneinfoidCOV_rows_selected
          start <- as.numeric(data.frame(geneinfo_id[clickedid,])[1,3])
          end <- as.numeric(data.frame(geneinfo_id[clickedid,])[1,4])
          geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
          chr <- data.frame(geneinfo_id[clickedid,])[1,2]
          gff <- data.table::fread(paste0("./info/", genomenames, "/", genomenames, ".", chr, ".gene.structure.gff"), sep = "\t", data.table = FALSE)
          gff.mrna <- gff[gff$type == "mRNA", ]
          if ( nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
          } else {
            gff.reg.mrna <- gff.mrna[grep(geneid, gff.mrna$id), ]
            gff.reg <- gff[gff$id %in% gff.reg.mrna$id, ]
            
            gff.reg.mrna.ir <- IRanges::IRanges(gff.reg.mrna$start, gff.reg.mrna$end)
            gff.reg.mrna.op <- GenomicRanges::findOverlaps(gff.reg.mrna.ir, GenomicRanges::reduce(gff.reg.mrna.ir))
            gff.reg.mrna$grp <- S4Vectors::subjectHits(gff.reg.mrna.op)
            gff.reg.mrna.1 <- gff.reg.mrna %>% dplyr::group_by(grp) %>% dplyr::mutate(y = dplyr::row_number())
            gff.reg <- merge(gff.reg, gff.reg.mrna.1[, c("id", "y")], by="id")
            gff.reg$y <- gff.reg$y * 0.2 + 1
            plot.mrna.lst <- lapply(unique(gff.reg$id), function(i){
              dat <- gff.reg[gff.reg$id == i, ]
              i.strand <- dat$strand[1]
              dat.mrna <- dat[dat$type=="mRNA", ]
              return(dat.mrna)
            })
            plot.mrna <- do.call(rbind, plot.mrna.lst)
            
            if (nrow(plot.mrna) == 1) {
              p1 <- ggplot2::ggplot(plot.mrna, ggplot2::aes(x = end, y = y, label = id)) + 
                ggplot2::geom_rect(ggplot2::aes(xmin=start, xmax=end, ymin=y+0.118, ymax=y+0.122,
                                                text=anno), color="grey30", fill="grey30") + 
                ggplot2::geom_text(ggplot2::aes(label=id),hjust = -0.05, vjust = -3.0, size = 5) + ggplot2::xlim(start, end + abs(end - start)/4)
            } else {
              p1 <- ggplot2::ggplot(plot.mrna, ggplot2::aes(x = end, y = y, label = id)) + 
                ggplot2::geom_rect(ggplot2::aes(xmin=start, xmax=end, ymin=y+0.118, ymax=y+0.122,
                                                text=anno), color="grey30", fill="grey30") + 
                ggplot2::geom_text(ggplot2::aes(label=id),hjust = -0.05, vjust = -4.25, size = 5) + ggplot2::xlim(start, end + abs(end - start)/4)
            }
            
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
            
            #p1 <- p1 + ggplot2::ylim(1.18, 1.42)
            p1 <- p1 + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),panel.grid.minor = ggplot2::element_blank()) + 
              ggplot2::theme(panel.background = ggplot2::element_rect(fill="white",colour="white")) + ggplot2::xlab("") + ggplot2::ylab("") + 
              ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) + ggplot2::theme(axis.text.y = ggplot2::element_blank()) + 
              ggplot2::theme(axis.text = ggplot2::element_text(size=12), axis.title=ggplot2::element_text(size=14,face="bold"))
            
            nns <- nrow(plot.mrna)
            
            grid::grid.draw(ggplot2::ggplotGrob(p1)) 
          }
        }
      })
      
      output$geneticIDstructureCOVif <- renderUI({
        if (is.null(input$geneinfoidCOV_rows_selected)) {
        } else {
          clickedid <- input$geneinfoidCOV_rows_selected
          start <- as.numeric(data.frame(geneinfo_id[clickedid,])[1,3])
          end <- as.numeric(data.frame(geneinfo_id[clickedid,])[1,4])
          geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
          chr <- data.frame(geneinfo_id[clickedid,])[1,2]
          gff <- data.table::fread(paste0("./info/", genomenames, "/", genomenames, ".", chr, ".gene.structure.gff"), sep = "\t", data.table = FALSE)
          
          gff.mrna <- gff[gff$type == "mRNA", ]
          gff.reg.mrna <- gff.mrna[grep(geneid, gff.mrna$id), ]
          if ( nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
          } else {
            nns <- nrow(gff.reg.mrna)
            hh <- paste0(nns*100, "px")
            plotOutput("geneticIDstructureCOV", width = "100%", height = hh)
          }
        }
      })
      
      output$geneticIDstructureCOV_title <- renderText({
        if (is.null(input$geneinfoidCOV_rows_selected ) ) {
        } else {
          
          clickedid <- input$geneinfoidCOV_rows_selected
          start <- as.numeric(data.frame(geneinfo_id[clickedid,])[1,3])
          end <- as.numeric(data.frame(geneinfo_id[clickedid,])[1,4])
          geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
          chr <- data.frame(geneinfo_id[clickedid,])[1,2]
          gff <- data.table::fread(paste0("./info/", genomenames, "/", genomenames, ".", chr, ".gene.structure.gff"), sep = "\t", data.table = FALSE)
          
          gff.mrna <- gff[gff$type == "mRNA", ]
          gff.reg.mrna <- gff.mrna[grep(geneid, gff.mrna$id), ]
          if ( nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
            
          } else {
            HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Gene structure</b></font>')
          }
        }
      })
      
      
      #gene sequence
      if( length(geneinfo_id[, 1]) != 0 ) {
        output$gene_idCOV <- renderText({
          if (is.null(input$geneinfoidCOV_rows_selected)) {
          } else {
            clickedid <- input$geneinfoidCOV_rows_selected
            info <- data.frame(geneinfo_id[clickedid,])
            chr <- data.frame(geneinfo_id[clickedid,])[1,2]
            fastafile <- paste0("./info/", genomenames, "/", genomenames,".", chr,  ".fasta.gz")
            fasta <- Biostrings::readDNAStringSet(fastafile)
            info$seq[info$strand == "+"] <- as.character(Biostrings::subseq(fasta[info[info$strand == "+",]$chr], info[info$strand == "+",]$start, info[info$strand == "+",]$end))
            info$seq[info$strand == "-"] <- as.character(Biostrings::reverseComplement(Biostrings::subseq(fasta[info[info$strand == "-",]$chr], info[info$strand == "-",]$start, info[info$strand == "-",]$end)))
            gene <- Biostrings::DNAStringSet(info$seq)
            names(gene) <- paste0(info[1,1],  ":", as.numeric(info[1,3]), "-", as.numeric(info[1,4]), " length = ", as.numeric(info[4]) - as.numeric(info[1,3]) +1)
            tmp.d4 <- file.path(tempdir(), "d4.fa")
            Biostrings::writeXStringSet(gene, file = tmp.d4, width = 150)
            readLines(tmp.d4) 
          }
        }, sep = "\n")
        
        output$gene_titleCOV_id <- renderText({
          if (is.null(input$geneinfoidCOV_rows_selected)) {
          } else {
            HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Gene sequence</b></font>")
          }
        })
        
        #cds sequence
        output$cds_idCOV <- renderText({
          if (is.null(input$geneinfoidCOV_rows_selected)) {
          } else {
            clickedid <- input$geneinfoidCOV_rows_selected
            geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
            didcs <- cds.infoid[grep(geneid, names(cds.infoid))]
            tmp.f6 <- file.path(tempdir(), "d6.fa")
            Biostrings::writeXStringSet(didcs, file = tmp.f6, width = 150)
            readLines(tmp.f6)
          }
        }, sep = "\n")
        
        output$cds_titleCOV_id <- renderText({
          clickedid <- input$geneinfoidCOV_rows_selected
          geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
          if(is.null(input$geneinfoidCOV_rows_selected)) {
          } else if ( length(grep(geneid, names(cds.infoid))) != 0 ){
            HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>CDS sequence</b></font>")
          } else {}
        })
        
        #cdna sequence
        output$cdna_idCOV <- renderText({
          if (is.null(input$geneinfoidCOV_rows_selected)) {
          } else {
            clickedid <- input$geneinfoidCOV_rows_selected
            geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
            ditcd <- cdna.infoid[grep(geneid, names(cdna.infoid))]
            tmp.f6 <- file.path(tempdir(), "d7.fa")
            Biostrings::writeXStringSet(ditcd, file = tmp.f6, width = 150)
            readLines(tmp.f6)
          }
        }, sep = "\n")
        
        output$cdna_titleCOV_id <- renderText({
          clickedid <- input$geneinfoidCOV_rows_selected
          geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
          if(is.null(input$geneinfoidCOV_rows_selected)){
          } else if ( length(grep(geneid, names(cdna.infoid))) != 0 ){
            HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>cDNA sequence</b></font>")
          } else {}
        })
        
        #protein
        output$pro_idCOV <- shiny::renderText({
          if (is.null(input$geneinfoidCOV_rows_selected)) {
          } else {
            clickedid <- input$geneinfoidCOV_rows_selected
            geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
            ditp <- proteinid[grep(geneid, names(proteinid))]
            tmp.f7 <- file.path(tempdir(), "t7.fa")
            Biostrings::writeXStringSet(ditp, file = tmp.f7, width = 150)
            readLines(tmp.f7)
          }
        }, sep = "\n")
        
        output$pro_titleCOV_id <- renderText({
          clickedid <- input$geneinfoidCOV_rows_selected
          geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
          if( is.null(input$geneinfoidCOV_rows_selected) ) {
          } else if ( length(grep(geneid, names(proteinid))) != 0 ){
            HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>protein sequence</b></font>")
          } else {}
        })
        
        
        ##Functional annotation
        output$Functional_idCOV <- DT::renderDT({
          DT::datatable(
            if (is.null(input$geneinfoidCOV_rows_selected)){
            } else {
              clickedid <- input$geneinfoidCOV_rows_selected
              geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
              chr <- data.frame(geneinfo_id[clickedid,])[1,2]
              Functionalfile <- paste0("./info/", genomenames, "/", genomenames, ".interproscan")
              gf <- data.table::fread(Functionalfile, sep = "\t", data.table = FALSE, header = F)
              gfinfo <- gf[grep(geneid, gf$V1), ]
              if( nrow(gfinfo) == 0){
                
              }else{
                
                colnames(gfinfo)[1] <- "Gene"
                colnames(gfinfo)[2] <- "Functional"
                gfinfo}
            }, extensions = "Buttons", 
            rownames = F, selection = "none",
            options = list( 
              scrollX = TRUE, dom = 'Bfrtip', bSort = FALSE,  
              buttons = list('pageLength', 'copy', 
                             list(extend = 'csv',   filename =  paste("Functional", sep = "-")),
                             list(extend = 'excel', filename =  paste("Functional", sep = "-")),
                             'print'),
              initComplete = DT::JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                "}")
              
            )
            
          )}, server = FALSE)
        
        output$Functionaltitle_idCOV <- renderText({
          clickedid <- input$geneinfoidCOV_rows_selected
          geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
          clickedid <- input$geneinfoidCOV_rows_selected
          geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
          chr <- data.frame(geneinfo_id[clickedid,])[1,2]
          Functionalfile <- paste0("./info/", genomenames, "/", genomenames, ".interproscan")
          gf <- data.table::fread(Functionalfile, sep = "\t", data.table = FALSE, header = F)
          gfinfo <- gf[grep(geneid, gf$V1), ]
          
          if(is.null(input$geneinfoidCOV_rows_selected) | nrow(gfinfo) == 0) {
            
          } else {
            
            HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Functional annotation</b></font>")
          }
        })
        
        #click gff
        output$gffinfo_idCOV <- DT::renderDT({
          DT::datatable(
            if (is.null(input$geneinfoidCOV_rows_selected)){
            } else {
              clickedid <- input$geneinfoidCOV_rows_selected
              geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
              chr <- data.frame(geneinfo_id[clickedid,])[1,2]
              gffile <- paste0("./info/", genomenames, "/", genomenames, ".",chr ,".gff.txt.gz")
              gf <- data.table::fread(gffile, sep = "\t", data.table = FALSE)
              gfinfo <- gf[grep(geneid, gf$Attributes), ]
              gfinfo <- gfinfo[, c(1:5, 9)]
              colnames(gfinfo)[1] <- "Chr"
              gfinfo
            }, extensions = "Buttons", 
            rownames = F, selection = "none",
            options = list(leftColumns = 8, 
                           scrollX = TRUE, dom = 'Bfrtip', bSort = FALSE,  
                           buttons = list('pageLength', 'copy', 
                                          list(extend = 'csv',   filename =  paste("GFF", sep = "-")),
                                          list(extend = 'excel', filename =  paste("GFF", sep = "-")),
                                          'print'),
                           columnDefs = list(list(className = 'dt-left', targets = 0:4)),
                           initComplete = DT::JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                             "}")
                           
            )
          )}, server = FALSE)
        
        output$gffinfotitle_idCOV <- renderText({
          clickedid <- input$geneinfoidCOV_rows_selected
          geneid <- data.frame(geneinfo_id[clickedid,])[1,1]
          if(is.null(input$geneinfoidCOV_rows_selected)) {
          } else {
            HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Gene structure annotation</b></font>")
          }
        })
        
        output$sequence_id.txt <- downloadHandler(
          filename <- function() { paste('Region_sequence.txt') },
          content <- function(file) {
            ditd  <- Biostrings::subseq(fasta[names(fasta) == chr], as.numeric(start), as.numeric(end) )
            names(ditd) <- paste0(chr, ":", start, "-", end, " length = ", end - start +1)
            Biostrings::writeXStringSet(ditd, file, width = 150)
          }, contentType = 'text/plain'
        )
        
        #Gene sequence
        output$genesequence_IDCOV.txt <- downloadHandler(
          filename <- function() { paste('Gene_Sequcence.txt') },
          content <- function(file) {
            fastafile <- paste0("/data/SoybeanGDB/BLASTN/CDS/file/", genomenames, ".gene.fasta.gz")
            fasta <- Biostrings::readDNAStringSet(fastafile)
            geneid <- unlist(gene_list)
            geneseq <- fasta[names(fasta) %in% geneid ]
            Biostrings::writeXStringSet(geneseq, file, width = 150)
          }, contentType = 'text/plain'
        )
        
        output$downloadIDCOV01 <- renderUI({
          req(input$submit_GSidcov)
          column(12,
                 downloadButton("genesequence_IDCOV.txt", "Gene Sequence", style = "width:100%;", class = "buttDown"),
                 tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
          )
        })
        
        #cds sequence Download
        output$cdssequence_IDCOV.txt <- downloadHandler(
          filename <- function() { paste('CDS_sequence.txt') },
          content <- function(file) {
            didcs <- cds.infoid[unlist(lapply(gene_list, function(x){ grep(x, names(cds.infoid))}))]
            Biostrings::writeXStringSet(didcs, file, width = 150)
          }, contentType = 'text/plain'
        )
        
        output$downloadIDCOV02 <- renderUI({
          req(input$submit_GSidcov)
          
          column(12,
                 downloadButton("cdssequence_IDCOV.txt", "CDS Sequence", style = "width:100%;", class = "buttDown"),
                 tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
          )
        })
        
        #cdna sequence Download
        output$cdnasequence_IDCOV.txt <- downloadHandler(
          filename <- function() { paste('cDNA_sequence.txt') },
          content <- function(file) {
            ditcd <- cdna.infoid[unlist(lapply(gene_list, function(x){ grep(x, names(cdna.infoid))} ))]
            Biostrings::writeXStringSet(ditcd, file, width = 150)
          }, contentType = 'text/plain'
        )
        
        output$downloadIDCOV03 <- renderUI({
          req(input$submit_GSidcov)
          column(12,
                 downloadButton("cdnasequence_IDCOV.txt", "cDNA Sequence", style = "width:100%;", class = "buttDown"),
                 tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
          )
        })
        
        #proteion Download
        output$prosequence_IDCOV.txt <- downloadHandler(
          filename <- function() { paste('protein_sequence.txt') },
          content <- function(file) {
            ditp <- proteinid[unlist(lapply(gene_list, function(x){ grep(x, names(proteinid))} ))]
            Biostrings::writeXStringSet(ditp, file, width = 150)
          }, contentType = 'text/plain'
        )
        
        output$downloadIDCOV04 <- renderUI({
          req(input$submit_GSidcov)
          
          column(12,
                 downloadButton("prosequence_IDCOV.txt", "protein Sequence", style = "width:100%;", class = "buttDown"),
                 tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
          )
        })
        
      } else {
        output$downloadIDCOV01 <- renderUI({
          NULL
        })
        output$downloadIDCOV02 <- renderUI({
          NULL
        })
        output$downloadIDCOV03 <- renderUI({
          NULL
        })
        output$downloadIDCOV04 <- renderUI({
          NULL
        })
      }
    } else if (nrow(geneinfo_id) == 0 & refgenomenames != genomenames){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "No homologous genes found"
      )
      NULL
    } else if (refgenomenames == genomenames){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Cannot select the same genome!"
      )
      
    }else{
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input correct Gene ID!"
      )
      
    }
  })
  
  
  
  ###SNP for 481
  observeEvent(input$submiti481, {
    if ( exists("anaReg") ){
    } else {
      source("anaReg.R")
    }
    myPos <- anaReg(input$regi481)
    if (!is.null(myPos)) {
      chr <- as.character(myPos$chr)
      start <- as.numeric(myPos$start)
      end <- as.numeric(myPos$end)
      tmp.fg <- file.path(tempdir(), "tg.txt")
      tabix <- paste0("tabix ./info/Williams82SNP/Wm82.", chr, ".snp.gz ", chr, ":", start, "-", end, " > ", tmp.fg)
      system(tabix)
      if ( file.info(tmp.fg)$size == 0){
        snp <- data.frame("V1"="No SNPs found!")
        colnames(snp) <- ""
        gffSNP <- c("V1"="No SNPs found!")
        SNPs <- snp
      } else {
        
        gffSNP <- data.table::fread(tmp.fg, sep = "\t", data.table = F)
        names(gffSNP) <- c("Chromosome", "Position", "Reference", "Alternative" , soynm)
        gffSNP <- gffSNP[, -486]
        accession = input$mychooseri481
        
        choosei <- c("Chromosome", "Position", "Reference", "Alternative", unique(gsub(",.+", "", unlist(accession))))
        snp <- gffSNP[, c(choosei)]
        
        SNPs <- snp
      }
      output$snptable481 <- DT::renderDT({
        DT::datatable(
          SNPs
          ,selection = 'none', rownames = FALSE, escape = FALSE,
          extensions = c("Buttons"),#"FixedColumns",
          options = list(
            buttons = list('pageLength', 'copy',
                           list(extend = 'csv',   filename =  paste("SNP", sep = "-")),
                           list(extend = 'excel', filename =  paste("SNP", sep = "-"))
            ),dom = 'Bfrtip',
            pageLength = 15, columnDefs=list(list(targets="_all", class="dt-center")), 
            bSort = FALSE, scrollX = TRUE, #fixedColumns = list(leftColumns = 2),
            initComplete = DT::JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
              "}")
          )
        )
      }, server = FALSE)        
      # output$bulkdownloadSNPInfo1.txt <- downloadHandler(
      #   filename <- function() { paste('The_SNPinfo.txt') },
      #   content <- function(file) {
      #     write.table(SNPs[, c(1:6)], file, col.names = T, row.names = F, quote = F, sep = "\t")
      #   }, contentType = 'text/plain'
      # )
      # 
      # output$downloadSNP01 <- renderUI({
      #   req(input$submiti481)
      #   column(12,
      #          downloadButton("bulkdownloadSNPInfo1.txt", style = "width:100%;", "SNPs information", class = "buttDown"),
      #          tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
      #   )
      # })
      
      output$bulkdownloadSNPALLInfo1.txt <- downloadHandler(
        filename <- function() { paste('All_SNPinfo.txt') },
        content <- function(file) {
          write.table(SNPs, file, col.names = T, row.names = F, quote = F, sep = "\t")
        }, contentType = 'text/plain'
      )
      
      output$downloadSNP02 <- renderUI({
        req(input$submiti481)
        column(12,
               downloadButton("bulkdownloadSNPALLInfo1.txt", style = "width:100%;", "Genotype data", class = "buttDown"),
               tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
        )
      })
    } else { 
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input genomic region or gene model in appropriate format!"
      )
    }
    
  })
  
  observe({
    if (input$clearSNP481>0) {
      isolate({
        updateTextInput(session, "regi481", value="")
        shinyWidgets::updateMultiInput(
          session = session,
          inputId = "mychooseri481",
          selected = character(0)
        )
      })
    } else {NULL}
  })
  
  observe({
    if (input$SNPExam481 >0) {
      isolate({
        updateTextInput(session, "regi481", value="chr1:29506705-29659223")
        shinyWidgets::updateMultiInput(
          session = session,
          inputId = "mychooseri481",
          selected = soynm
        )
      })
    } else {NULL}
  })
  
  observeEvent(input$snpall481, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "mychooseri481",
      selected = soynm
    )
  })
  
  observeEvent(input$snpnone481, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "mychooseri481",
      selected = character(0)
    )
  })
  
  observe({
    if (input$clearSNP481>0) {
      isolate({
        updateTextInput(session, "regi481", value="")
        shinyWidgets::updateMultiInput(
          session = session,
          inputId = "mychooseri481",
          selected = character(0)
        )
      })
    } else {NULL}
  })
  
  observe({
    if (input$SNPExam481 >0) {
      isolate({
        updateTextInput(session, "regi481", value="chr7:29560705-29573051")
        shinyWidgets::updateMultiInput(
          session = session,
          inputId = "mychooseri481",
          selected = soynm[grepl("HN", soynm)]
        )
      })
    } else {NULL}
  })
  
  
  # #Download fasta
  # observe({
  #   SoybeanGDB_fasta <- read.table("SoybeanGDB_fasta.txt", head=T, as.is=T, sep = "\t")
  #   output$fasta_table_out = DT::renderDataTable(
  #     SoybeanGDB_fasta,
  #     options = list(lengthMenu = c(20, 30, 50), pageLength = 10,
  #                    searching = TRUE, autoWidth = TRUE, bSort=FALSE,
  #                    dom = 'Bfrtip',scrollX = TRUE,
  #                    columnDefs=list(list(targets="_all"))
  #     ), escape = FALSE, selection="none", rownames= FALSE, extensions = "Buttons"
  #   )
  #   
  # })
  # #Download Transposable elements
  # observe({
  #   SoybeanGDB_TE <- read.table("SoybeanGDB_TE.txt", head=T, as.is=T, sep = "\t")
  #   output$TE_table_out = DT::renderDataTable(
  #     SoybeanGDB_TE,
  #     options = list(lengthMenu = c(20, 30, 50), pageLength = 10,
  #                    searching = TRUE, autoWidth = TRUE, bSort=FALSE,
  #                    dom = 'Bfrtip', scrollX = TRUE,
  #                    columnDefs=list(list(targets="_all"))
  #     ), escape = FALSE, selection="none", rownames= FALSE, extensions = "Buttons"
  #   )
  #   
  # })
  # 
  # #Download gff
  # observe({
  #   SoybeanGDB_gff <- read.table("SoybeanGDB_gff.txt", head=T, as.is=T, sep = "\t")
  #   output$gff_table_out = DT::renderDataTable(
  #     SoybeanGDB_gff,
  #     options = list(lengthMenu = c(20, 30, 50), pageLength = 10,
  #                    searching = TRUE, autoWidth = TRUE, bSort=FALSE,
  #                    dom = 'Bfrtip', scrollX = TRUE,
  #                    columnDefs=list(list(targets="_all"))
  #     ), escape = FALSE, selection="none", rownames= FALSE, extensions = "Buttons"
  #   )
  #   
  # })
  # 
  # 
  # 
  # observe({
  #   SoybeanGDB_blastdb_Genome <- read.table("SoybeanGDB_blastdb_Genome.txt", head=T, as.is=T, sep = "\t")
  #   output$genomeTable = DT::renderDataTable(
  #     
  #     SoybeanGDB_blastdb_Genome,
  #     options = list(lengthMenu = c(20, 30, 50), pageLength = 10,
  #                    searching = TRUE, autoWidth = TRUE, bSort=FALSE,
  #                    dom = 'Bfrtip',
  #                    columnDefs=list(list(targets="_all"))
  #     ), escape = FALSE, selection="none", rownames= FALSE, extensions = "Buttons"
  #   )
  # 
  # })
  # 
  # observe({
  #   SoybeanGDB_blastdb_cds <- read.table("SoybeanGDB_blastdb_CDS.txt", head=T, as.is=T, sep = "\t")
  #   output$cdsTable = DT::renderDataTable(
  #     
  #     SoybeanGDB_blastdb_cds,
  #     options = list(lengthMenu = c(20, 30, 50), pageLength = 15,
  #                    searching = TRUE, autoWidth = TRUE, bSort=FALSE,
  #                    dom = 'Bfrtip',
  #                    columnDefs=list(list(targets="_all"))
  #     ), escape = FALSE, selection="none", rownames= FALSE, extensions = "Buttons"
  #   )
  #   
  # })
  # 
  # observe({
  #   SoybeanGDB_blastdb_Gene <- read.table("SoybeanGDB_blastdb_Gene.txt", head=T, as.is=T, sep = "\t")
  #   output$GeneTable = DT::renderDataTable(
  #     
  #     SoybeanGDB_blastdb_Gene,
  #     options = list(lengthMenu = c(20, 30, 50), pageLength = 15,
  #                    searching = TRUE, autoWidth = TRUE, bSort=FALSE,
  #                    dom = 'Bfrtip',
  #                    columnDefs=list(list(targets="_all"))
  #     ), escape = FALSE, selection="none", rownames= FALSE, extensions = "Buttons"
  #   )
  #   
  # })
  # 
  # observe({
  #   SoybeanGDB_blastdb_Protein <- read.table("SoybeanGDB_blastdb_Protein.txt", head=T, as.is=T, sep = "\t")
  #   output$ProteinTable = DT::renderDataTable(
  #     
  #     SoybeanGDB_blastdb_Protein,
  #     options = list(lengthMenu = c(20, 30, 50), pageLength = 15,
  #                    searching = TRUE, autoWidth = TRUE, bSort=FALSE,
  #                    dom = 'Bfrtip',
  #                    columnDefs=list(list(targets="_all"))
  #     ), escape = FALSE, selection="none", rownames= FALSE, extensions = "Buttons"
  #   )
  #   
  # })
  
  
  #Download29Soybean
  observe({
    Download.tab <- read.table("./data/31download.link", sep = "\t", as.is = T, header = T)
    
    output$Downloadtable <- DT::renderDataTable(Download.tab,  
                        options = list(pageLength = 40, lengthChange = FALSE, Search = FALSE, info = FALSE, dom = 't', 
                                       searchHighlight = FALSE, autoWidth = FALSE, bSort = FALSE, sDom  = '<"top">lrt<"bottom">ip' ),
                        escape = FALSE, rownames = FALSE, selection = "none"
    )
  })
  
})

