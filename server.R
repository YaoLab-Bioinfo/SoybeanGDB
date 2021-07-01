
# options(shiny.maxRequestSize = 200*1024^2)

shinyServer(function(input, output, session) {

  # Home-browser
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
  
  #Home-GeneInfoID
  observeEvent(input$Link_GeneInfoID, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Search by gene ID</strong>"))
  })
  
  #Home-GeneInfoIT
  observeEvent(input$Link_GeneInfoIT, {
    updateNavbarPage(session, "The_page", selected =HTML("<strong style='font-size:20px'>Search by genome location</strong>"))
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
  
  
  #search gene or interval information
  #Search by gene ID
  observeEvent(input$submit_GSID, {
    #library(GenomicRanges)
    load(paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".gene.info.RData"))
    #判断用户输入数据 chr start end
    geneid <- input$searchgeneid
    geneid <- trimws(geneid)
    start <- gene_info_s$start[gene_info_s$id == geneid]
    end <- gene_info_s$end[gene_info_s$id == geneid]
    chr <- gene_info_s$chr[gene_info_s$id == geneid]
    
    if( identical(chr, character(0)) ){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Input gene model not found!", type = "error",
        text = NULL
      )
      NULL
    } else {
      fasta <- Biostrings::readDNAStringSet(paste0("./info/",gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".",chr, ".fasta.gz"))
      #判断长度
      if (start > Biostrings::width(fasta[chr]) | end > Biostrings::width(fasta[chr]) | end - start > 10000000 | !(end - start > 0) | is.na(end + start) ){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Please input gene model name in appropriate format!", type = "error",
          text = NULL
        )
        NULL 
      } else {
        #fasta cds protein信息读取
        load(paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".cds.fasta.RData"))
        load(paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".protein.RData"))
        load(paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".cdna.fasta.RData"))
        
        #输入为GENEID
        if( geneid %in% gene_info_s$id ) {
          gfinfo <- gene_info_s[gene_info_s$id == geneid, ]
          
          gfinfo$seq[gfinfo$strand == "+"] <- as.character(Biostrings::subseq(fasta[gfinfo[gfinfo$strand == "+",]$chr], gfinfo[gfinfo$strand == "+",]$start, gfinfo[gfinfo$strand == "+",]$end))
          gfinfo$seq[gfinfo$strand == "-"] <- as.character(Biostrings::reverseComplement(Biostrings::subseq(fasta[gfinfo[gfinfo$strand == "-",]$chr], gfinfo[gfinfo$strand == "-",]$start, gfinfo[gfinfo$strand == "-",]$end)))
          
          df <- Biostrings::DNAStringSet(gfinfo$seq)
          names(df) <- paste0(geneid, " ",gfinfo$chr, ":", start, "-", end, " length = ", end - start +1)
          tmp.f1 <- file.path(tempdir(), "t1.fa")
          
          output$seq <- renderText({
            Biostrings::writeXStringSet(df, file = tmp.f1, width = 150)
            readLines(tmp.f1)
          }, sep = "\n")
          
          output$seq_title <- renderText({
            if( is.null(df) ){
              NULL
            } else {
              HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Gene sequence</b></font>')
            }
          })
          
          gff <- data.table::fread(paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".", chr, ".gene.structure.gff"), sep = "\t", data.table = FALSE)
          #genetic structureinput$submit_GSID
          start <- as.numeric(gfinfo[1, 3])
          end <- as.numeric(gfinfo[1, 4])
          gff.mrna <- gff[gff$type == "mRNA", ]
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
          nnr <- nrow(plot.mrna)
          
          output$geneticIDstructure <- shiny::renderPlot({
            if ( nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
            } else {
              if (nnr == 1){
                p1 <- ggplot2::ggplot(plot.mrna, ggplot2::aes(x = end, y = y, label = id)) +
                  ggplot2::geom_rect(ggplot2::aes(xmin=start, xmax=end, ymin=y+0.118, ymax=y+0.122, text=anno), 
                                     color="grey30", fill="grey30")+ ggplot2::geom_text(ggplot2::aes(label=id),hjust = -0.05, vjust = -3.0, size = 5) + 
                  ggplot2::xlim(start, end + abs(end - start)/4)
              } else {
                p1 <- ggplot2::ggplot(plot.mrna, ggplot2::aes(x = end, y = y, label = id)) +
                  ggplot2::geom_rect(ggplot2::aes(xmin=start, xmax=end, ymin=y+0.118, ymax=y+0.122, text=anno), 
                                     color="grey30", fill="grey30")+ ggplot2::geom_text(ggplot2::aes(label=id),hjust = -0.05, vjust = -4.25, size = 5) + 
                  ggplot2::xlim(start, end + abs(end - start)/4) 
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
              
              grid::grid.draw(ggplot2::ggplotGrob(p1))
            }
          })
          
          output$geneticIDstructureif <- renderUI({
            if ( nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
            } else {
              hh <- paste0(nnr*100, "px")
              plotOutput("geneticIDstructure", width = "100%", height = hh)
            }
          })
          
          output$geneticIDstructure_title <- renderText({
            if ( nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
            } else {
              HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Gene structure</b></font>')
            }
          })
          
          #protein
          dp <- protein[grep(geneid, names(protein))]
          output$pro <- shiny::renderText({
            tmp.f2 <- file.path(tempdir(), "t2.fa")
            Biostrings::writeXStringSet(dp, file = tmp.f2, width = 150)
            readLines(tmp.f2)
          }, sep = "\n")
          
          output$pro_title <- renderText({
            if( is.null(dp) | nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
              NULL
            } else {
              HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Protein sequence</b></font>')
              
            }
          })
          
          #cds
          dc <- cds.info[grep(geneid, names(cds.info))]
          output$cds <- renderText({
            tmp.f3 <- file.path(tempdir(), "t3.fa")
            Biostrings::writeXStringSet(dc, file = tmp.f3, width = 150)
            readLines(tmp.f3)
          }, sep = "\n")
          
          output$cds_title <- renderText({
            if(is.null(dc) | nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
              NULL
            } else {
              HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>CDS sequence</b></font>')
            }            
          })
          
          #cdna
          dcdna <- cdna.info[grep(geneid, names(cdna.info))]
          output$cdna_title <- renderText({
            if(is.null(dcdna) | nrow(gff.mrna[gff.mrna$chr==chr & gff.mrna$start>=start & gff.mrna$end<=end, ]) == 0 ){
              NULL
            } else {
              HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>cDNA sequence</b></font>')
            }
          })
          
          output$cdna <- renderText({
            tmp.f4 <- file.path(tempdir(), "t4.fa")
            Biostrings::writeXStringSet(dcdna, file = tmp.f4, width = 150)
            readLines(tmp.f4)
          }, sep = "\n")
          
          gffile <- paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".",chr ,".gff.txt.gz")
          gf <- read.table(gffile, header = T, sep = "\t", as.is = T)
          
          output$gffinfo <- DT::renderDT({
            gfinfo <- gf[grep(geneid, gf$Attributes), ]
            DT::datatable(
              gfinfo
              , extensions = "Buttons",
              rownames = F, selection = "none",
              options = list(leftColumns = 8, 
                             scrollX = TRUE, dom = 'Bfrtip', bSort = FALSE,  
                             buttons = list('pageLength', 'copy', 
                                            list(extend = 'csv',   filename =  paste("searchgeneID", geneid, sep = "-")),
                                            list(extend = 'excel', filename =  paste("searchgeneID", geneid, sep = "-")),
                                            'print'),
                             columnDefs = list(list(className = 'dt-left', targets = 0:4)),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                               "}")
              )
            )
          }, server = FALSE)
          
          output$gffinfotitle <- renderText({
            if(is.null(gf) | nrow(gf) == 0){
              NULL
            } else {
              HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Gene structure annotation</b></font>")
            }
          })
        } else {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Input gene ID not found!", type = "error",
            text = NULL
          )
          NULL 
        }
      }
    }
    if ( !identical(chr, character(0)) ){
    output$genesequence_ID.txt <- downloadHandler(
      filename <- function() { paste('Gene_sequence.txt') },
      content <- function(file) {
        Biostrings::writeXStringSet(df, file, width = 150)
      }, contentType = 'text/plain'
    )
    
    output$downloadgenesid01 <- renderUI({
      req(input$submit_GSID)
      column(12,
             downloadButton("genesequence_ID.txt", "Gene Sequence", style = "width:100%;", class = "buttDown"),
             tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
      )
    })
    
    output$cdssequence_ID.txt <- downloadHandler(
      filename <- function() { paste('CDS_sequence.txt') },
      content <- function(file) {
        Biostrings::writeXStringSet(dc, file, width = 150)
      }, contentType = 'text/plain'
    )
    
    output$downloadgenesid02 <- renderUI({
      req(input$submit_GSID)
      column(12,
             downloadButton("cdssequence_ID.txt", "CDS Sequence", style = "width:100%;", class = "buttDown"),
             tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
      )
    })
    
    output$cdnasequence_ID.txt <- downloadHandler(
      filename <- function() { paste('cDNA_sequence.txt') },
      content <- function(file) {
        Biostrings::writeXStringSet(dcdna, file, width = 150)
      }, contentType = 'text/plain'
    )
    
    output$downloadgenesid03 <- renderUI({
      req(input$submit_GSID)
      column(12,
             downloadButton("cdnasequence_ID.txt", "cDNA Sequence", style = "width:100%;", class = "buttDown"),
             tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
      )
    })
    
    output$prosequence_ID.txt <- downloadHandler(
      filename <- function() { paste('Protein_sequence.txt') },
      content <- function(file) {
        Biostrings::writeXStringSet(dp, file, width = 150)
      }, contentType = 'text/plain'
    )
  
  
  output$downloadgenesid04 <- renderUI({
    req(input$submit_GSID)
    column(12,
           downloadButton("prosequence_ID.txt", "Protein Sequence", style = "width:100%;", class = "buttDown"),
           tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
    )
  })
  } else {
    NULL
  }
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
  
  observe({
    if (input$clearSERID>0) {
      isolate({
        updateTextInput(session, "searchgeneid", value = "")
      })
    } else {NULL}
  })
  
  observe({
    if (input$SERExamID >0) {
      isolate({
        updateSelectInput(session, "variety_ID", selected = "Zhonghuang 13")
        updateTextInput(session, "searchgeneid", value = "SoyZH13_02G148200")
      })
    } else {NULL}
  })
  
  #zhanwei1
  output$zhanwei1 <- renderUI({
    if ( input$submit_GSID ) {
    } else {
      plotOutput("zhanwei1", width = "100%", height = "600px")}
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
      if( !(chr %in% paste0("chr", 1:20)) ){
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
          
          #geneinfo
          output$geneinfo <- DT::renderDT({
            DT::datatable(
              if( length(geneinfo[, 1]) != 0 ){
                colnames(geneinfo) <- c("ID", "Chromosome", "Start", "End", "Strand")
                geneinfo
              }else{
                geneinfo <- data.frame("V1"="No genes found in the input region!")
                colnames(geneinfo) <- ""
                geneinfo
              }, escape = FALSE, rownames= FALSE, selection="single", 
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
            } else {
            }
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
              }else{
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
          if( length(geneinfo[, 1]) != 0 ){
            output$gene_it <- renderText({
              if (is.null(input$geneinfo_rows_selected)) {
              } else {
                clicked <- input$geneinfo_rows_selected
                info <- data.frame(geneinfo[clicked,])
                info$seq[info$strand == "+"] <- as.character(Biostrings::subseq(fasta[info[info$strand == "+",]$chr], info[info$strand == "+",]$start, info[info$strand == "+",]$end))
                info$seq[info$strand == "-"] <- as.character(Biostrings::reverseComplement(Biostrings::subseq(fasta[info[info$strand == "-",]$chr], info[info$strand == "-",]$start, info[info$strand == "-",]$end)))
                gene <- Biostrings::DNAStringSet(info$seq)
                names(gene) <- paste0(info[1,1],  ":", as.numeric(info[1,3]), "-", as.numeric(info[1,4]), " length = ", as.numeric(info[4]) - as.numeric(info[1,3]) +1)
                tmp.f4 <- file.path(tempdir(), "t4.fa")
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
                if (grepl(geneid, names(cds.info))){
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
              } else {
              }
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
              if(is.null(input$geneinfo_rows_selected)) {
              } else if (length(grep(geneid, gf$Attributes)) != 0 ){
                HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Gene structure annotation</b></font>")
              } else {}
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
  
  #ID&IT update
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
  
  SID <- reactive({
    req(input$variety_ID)
    if( exists("genome.info") ) {
    } else {
      gene_accession <- read.table("./data/gene_accession.txt", sep = "\t", header = T, as.is = T)
      gene_accession[gene_accession$Accession == gsub(" ", "_", input$variety_ID), ]
    }
  })
  observeEvent(SID(), {
    updateTextInput(session, "searchgeneid", value = unique(SID()$ID))
  })
  
  #zhanwei2
  output$zhanwei2 <- renderUI({
    if (input$submit_GSIT ) {
      
    } else {
      plotOutput("zhanwei2", width = "100%", height = "600px")}
  })
  
  #Bulk download gene annotation
  observeEvent(input$submit_BDG, {
    genomenames <- gsub(" ", "_", input$variety_BDG)
    load(paste0("./info/", gsub(" ", "_", input$variety_BDG), "/", gsub(" ", "_", input$variety_BDG), ".gene.info.RData"))
    
    #cds protein cdna gff信息读取
    load(paste0("./info/", gsub(" ", "_", input$variety_BDG), "/", gsub(" ", "_", input$variety_BDG), ".cds.fasta.RData"))
    load(paste0("./info/", gsub(" ", "_", input$variety_BDG), "/", gsub(" ", "_", input$variety_BDG), ".protein.RData"))
    load(paste0("./info/", gsub(" ", "_", input$variety_BDG), "/", gsub(" ", "_", input$variety_BDG), ".cdna.fasta.RData"))
    
    gene_list <- unlist(strsplit(input$BDG_Paste, split="\\n"))
    gene_list <- trimws(gene_list, which = c("both"), whitespace = "[ \t\r\n]")
    gene_list <- gene_list[gene_list %in% gene_info_s$id]
    #geneinfo
    geneinfo <- gene_info_s[gene_info_s$id %in% gene_list, ]
    
    #gene sequence
    if( length(gene_list) != 0 ) {
      chrlist <- unique(unlist(geneinfo$chr))
      gffgene <- lapply(chrlist, function(x){
        chr <- x
        gffile <- paste0("./info/", gsub(" ", "_", input$variety_BDG), "/", gsub(" ", "_", input$variety_BDG), ".", chr, ".gff.txt.gz")
        gf <- data.table::fread(gffile, sep = "\t", data.table = FALSE)
        genechrx <- geneinfo[geneinfo$chr == chr, ]
        geneid <- unlist(genechrx$id)
        gfinfo <- lapply(geneid, function(x){
          d <- gf[grep(x, gf$Attributes), ]
          return(d)
        })
        gfinfo <- do.call(rbind, gfinfo)
        return(gfinfo)
      })
      gffgene <- do.call(rbind, gffgene)
      
      output$gffinfotitle_BDG <- renderText({
        HTML("<i class='fa fa-circle' aria-hidden='true'></i> <font size='5' color='red'><b>Gene annotation</b></font>")
      })
      
      output$gffinfo_BDG <- DT::renderDT({
        DT::datatable(
          gffgene, extensions = "Buttons",
          rownames = F, selection = "none",
          options = list(leftColumns = 8, 
                         scrollX = TRUE, dom = 'Bfrtip', bSort = FALSE,  
                         buttons = list('pageLength', 'copy', 
                                        list(extend = 'csv',   filename =  paste("Gene_annotation",  sep = "-")),
                                        list(extend = 'excel', filename =  paste("Gene_annotation",  sep = "-")),
                                        'print'),
                         columnDefs = list(list(className = 'dt-left', targets = 0:4)),
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
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
        req(input$submit_BDG)
        
        column(12,
               downloadButton("genesequence_BDG.txt", "Gene Sequence", style = "width:100%;", class = "buttDown"),
               tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
        )
      })
      
      #cds sequence Download
      output$cdssequence_BDG.txt <- downloadHandler(
        filename <- function() { paste('CDS_sequence.txt') },
        content <- function(file) {
          ditcs <- cds.info[unlist(lapply(gene_list, function(x){ grep(x, names(cds.info))}))]
          Biostrings::writeXStringSet(ditcs, file, width = 150)
        }, contentType = 'text/plain'
      )
      
      output$downloadBDG02 <- renderUI({
        req(input$submit_BDG)
        
        column(12,
               downloadButton("cdssequence_BDG.txt", "CDS Sequence", style = "width:100%;", class = "buttDown"),
               tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
        )
      })
      
      #cdna sequence Download
      output$cdnasequence_BDG.txt <- downloadHandler(
        filename <- function() { paste('cDNA_sequence.txt') },
        content <- function(file) {
          ditcd <- cdna.info[unlist(lapply(gene_list, function(x){ grep(x, names(cdna.info))} ))]
          Biostrings::writeXStringSet(ditcd, file, width = 150)
        }, contentType = 'text/plain'
      )
      
      output$downloadBDG03 <- renderUI({
        req(input$submit_BDG)
        
        column(12,
               downloadButton("cdnasequence_BDG.txt", "cDNA Sequence", style = "width:100%;", class = "buttDown"),
               tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
        )
      })
      
      #proteion Download
      output$prosequence_BDG.txt <- downloadHandler(
        filename <- function() { paste('Protein_sequence.txt') },
        content <- function(file) {
          ditp <- protein[unlist(lapply(gene_list, function(x){ grep(x, names(protein))} ))]
          Biostrings::writeXStringSet(ditp, file, width = 150)
        }, contentType = 'text/plain'
      )
      
      output$downloadBDG04 <- renderUI({
        req(input$submit_BDG)
        
        column(12,
               downloadButton("prosequence_BDG.txt", "Protein Sequence", style = "width:100%;", class = "buttDown"),
               tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
        )
      })
    } else {      
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input at least one Gene ID!"
      )
      
      output$gffinfo_BDG <- DT::renderDT({
        DT::datatable(
          NULL, extensions = "Buttons",
          rownames = F, selection = "none",
          options = list(leftColumns = 8, 
                         scrollX = TRUE, dom = 'Bfrtip', bSort = FALSE,  
                         buttons = list('pageLength', 'copy', 'csv', 'excel', 'print'),
                         columnDefs = list(list(className = 'dt-left', targets = 0:4)),
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
                         
          )
        )
      }, server = FALSE)
      
      output$gffinfotitle_BDG <- renderText({
        NULL
      })
      
      output$downloadBDG01 <- renderUI({
        req(input$submit_BDG)
        column(12)
      })
      
      output$cdssequence_BDG.txt <- downloadHandler(
        filename <- function() { paste('CDS_sequence.txt') },
        content <- function(file) {
          writeLines("No sequence found for the input gene list.", file)
        }, contentType = 'text/plain'
      )
      
      output$downloadBDG02 <- renderUI({
        req(input$submit_BDG)
        column(12)
      })
      
      output$cdnasequence_BDG.txt <- downloadHandler(
        filename <- function() { paste('cDNA_sequence.txt') },
        content <- function(file) {
          writeLines("No sequence found for the input gene list.", file)
        }, contentType = 'text/plain'
      )
      
      output$downloadBDG03 <- renderUI({
        req(input$submit_BDG)
        column(12)
      })
      
      output$prosequence_BDG.txt <- downloadHandler(
        filename <- function() { paste('Protein_sequence.txt') },
        content <- function(file) {
          writeLines("No sequence found for the input gene list.", file)
        }, contentType = 'text/plain'
      )
      
      output$downloadBDG04 <- renderUI({
        req(input$submit_BDG)
        column(12)
      })
    }
  })
  
  ##reset
  observe({
    if (input$clearBDG >0) {
      isolate({
        updateTextAreaInput(session, "BDG_Paste", value="")
      })
    } else {NULL}
  })
  
  #load example
  observe({
    if (input$BDGExam >0) {
      isolate({
        updateTextAreaInput(session, "BDG_Paste", value = "SoyZH13_03G069002\nSoyZH13_03G069001\nSoyZH13_03G069100\nSoyZH13_01G071100" )
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
    if (input$submit_BDG ){
      
    } else {
      plotOutput("zhanwei3", width = "100%", height = "600px")}
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
                  if (x %in% c("Improved cultivar", "Landrace", "G. Soja")) {
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
                
                write.table(snp.info[[2]][snp.info[[2]]$effect %in% mutType, ], file, sep="\t", quote=F, row.names=F)
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
          if (x %in% c("Improved cultivar", "Landrace", "G. Soja")) {
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
          serchSNP <- merge(snp.info.down[[2]][, c(1:3,5:8)], ddt, by = "snpID")}
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
            DT::datatable(
              serchSNP
              ,selection = 'none', rownames = FALSE, escape = FALSE,
              extensions = c("FixedColumns","Buttons"),
              options = list(
                buttons = list('pageLength',
                               list(extend = 'csv',   filename =  paste("snp", sep = "-")),
                               list(extend = 'excel', filename =  paste("snp", sep = "-")),
                               'copy'),dom = 'Bfrtip',
                pageLength = 15, columnDefs=list(list(targets="_all", class="dt-center")), 
                bSort = FALSE, scrollX = TRUE, fixedColumns = list(leftColumns = 6),
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
            if ( exists("gff") && gff[1,1]  == "SoyZH13_01G000001.m1" ){
              
            } else {
               gff <- data.table::fread("./data/zh13.gff", sep = "\t", data.table = FALSE)
            }
            gene.info <- gff[gff$chr==myPos$chr & gff$Start>=myPos$start & gff$End<=myPos$end, ]
            write.table(gene.info, file, sep="\t", quote=F, row.names=F)
            
            gffile <- paste0("./info/Zhonghuang_13/Zhonghuang_13", ".",myPos$chr ,".gff.txt.gz")
            gf <- read.table(gffile, header = T, sep = "\t", as.is = T)
            gfinfo <- gf[grep(geneid, gf$Attributes), ]
            gfinfo
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
          selected = "G. Soja"
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
                  nuc.div.plot <<- nucDiv(chr = myPos$chr, nuc.start = myPos$start - div.up, nuc.end = myPos$end + div.down, 
                                          groups = div.group, step = div.step,
                                          numerator = div.numerator, denominator = div.denominator, 
                                          mutType = div.mut.group)
                  grid::grid.draw(gridExtra::grid.arrange(nuc.div.plot[[1]], nuc.div.plot[[2]], ncol=1, heights=c(2.3, 1)))
                }, height = div.height, width = div.width)
              } else {
                output$diversity <- shiny::renderPlot({
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
          names(gffindel) <- c("Chromosome", "Position", "Reference", "Alternative", "Effect", "Gene" , paste0("s", 1:2898))
          
          accession = input$mychooseri
          accession <- sapply(accession, function(x){
            if (x %in% c("Improved cultivar", "Landrace", "G. Soja")) {
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
            extensions = c("FixedColumns","Buttons"),
            options = list(
              buttons = list('pageLength', 'copy',
                             list(extend = 'csv',   filename =  paste("INDEL", sep = "-")),
                             list(extend = 'excel', filename =  paste("INDEL", sep = "-"))
              ),dom = 'Bfrtip',
              pageLength = 15, columnDefs=list(list(targets="_all", class="dt-center")), 
              bSort = FALSE, scrollX = TRUE, fixedColumns = list(leftColumns = 2),
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
          selected = c("G. Soja")
        )
      })
    } else {NULL}
  })

  
  #gene expression
  observeEvent(input$submit_GSgst, {
    if (!exists("geneexpression")) {
      geneexpression <- data.table::fread("./data/Gene_expression.txt", sep = "\t", data.table = F, check.names = F)
    } else {
      NULL
    }
    
    geneid <- input$gtsgeneid
    geneid <- trimws(geneid, which = c("both"), whitespace = "[ \t\r\n]")
    
    exforgene <- geneexpression[geneexpression$Gene_ID == geneid, ]
    
    if( nrow(exforgene) == 1 & length(input$Gexp_mut_group) != 0 ){
      dk <- data.frame(t(exforgene), stringsAsFactors = F)
      dk$Samples <- rownames(dk)
      colnames(dk) <- c("Expression", "Samples")
      dk <- dk[-1, ]
      dk$Expression <- as.numeric(dk$Expression)
      dk <- dk[dk$Samples %in% input$Gexp_mut_group, ]
      ds <- exforgene[, sort(colnames(exforgene))]
      dd <- data.frame(cbind(exforgene[,1], ds[, colnames(ds) %in% input$Gexp_mut_group]), stringsAsFactors = F, check.names = F)
      colnames(dd)[1] <- "Gene ID"
      
      if( length(input$Gexp_mut_group) == 1) {
        colnames(dd)[2] <- input$Gexp_mut_group
      } else {}
      
      output$gstoutresult <- DT::renderDT({
        DT::datatable(
          dd, escape = FALSE, rownames= FALSE, selection="none", extensions = c("Buttons","FixedColumns"),
          options = list(pageLength = 5, autoWidth = FALSE, bSort = FALSE, scrollX = TRUE, fixedColumns = list(leftColumns = 1),
                         buttons = list('copy', 
                                        list(extend = 'csv',   filename =  paste("Gene_expression", geneid, sep = "-")),
                                        list(extend = 'excel', filename =  paste("Gene_expression", geneid, sep = "-")),
                                        'print'), dom = 'Bfrtip',
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          )
        )
      }, server = FALSE)
      
      output$gstbarplot <- shiny::renderPlot({
        p1 <- ggplot2::ggplot(dk, ggplot2::aes(x = Samples, y = Expression)) + 
          ggplot2::geom_bar(stat="identity", width=0.9, fill='deepskyblue') + 
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, vjust = 1, hjust=1 , size = 15, face="bold"), 
                         axis.text.y = ggplot2::element_text(size = 12, face="bold"),
                         axis.title.y = ggplot2::element_text(size = 20), axis.title.x = ggplot2::element_text(size = 20),
                         plot.title = ggplot2::element_text(color="red", size=15, face="bold", hjust = 0.5)) + 
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                         panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), 
                         strip.text.x = ggplot2::element_text(size = 15, colour = "black", face = "bold")) + 
          ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(dk$Expression)*1.1) ) + 
          ggplot2::xlab("") + ggplot2::ylab("Expression level (FPKM)") + ggplot2::ggtitle(geneid) + 
          ggplot2::geom_text(ggplot2::aes(label=Expression), position = ggplot2::position_dodge(width=0.9),  size = 4, angle = 90, hjust = -0.1)
        p1
      })
      
      output$gstbarplotif <- renderUI({
        if ( is.na(input$gstwidth) | is.na(input$gstheight) | input$gstheight*input$gstwidth == 0 ){
          NULL
        } else {
          plotOutput("gstbarplot", width = paste0(input$gstwidth, "px"), height = paste0(input$gstheight, "px"))
        }
      })
      
      output$gstout_title <- renderText({
        HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Expression level</b></font>')
      })
    } else {
      if (length( input$Gexp_mut_group ) == 0){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error input!", type = "error",
          text = "Please choose at least one tissue/stage!"
        )
      } else {
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
  
  #load example
  observe({
    if (input$gstExamID >0) {
      isolate({
        updateTextInput(session, "gtsgeneid", value = "SoyZH13_05G201900")
        shinyWidgets::updateMultiInput(
          session = session,
          inputId = "Gexp_mut_group",
          selected = c("cotyledon-1", "root", "stem", "leafbud-1", "leafbud-2", "leaf-1", "cotyledon-2", "stem-2", "leafbud-3", "leaf-2", "flo-1", "shoot_meristem", "flo-2", "flo-3", "flo-4", "flo-5", "pod&seed-1", "pod&seed-2", "pod&seed-3", "pod-1", "pod-2", "pod-3", "seed-1", "seed-2", "seed-3", "seed-4", "seed-5")
        )
      })
    } else {NULL}
  })
  
  #reset
  observe({
    if (input$cleargst >0) {
      isolate({
        updateTextInput(session, "gtsgeneid", value = "")
        shinyWidgets::updateMultiInput(
          session = session,
          inputId = "Gexp_mut_group",
          selected = c("")
        )
      })
    } else {NULL}
  })
  
  observeEvent(input$accessiongexall, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "Gexp_mut_group",
      selected = c("cotyledon-1", "root", "stem", "leafbud-1", "leafbud-2", "leaf-1", "cotyledon-2", "stem-2", "leafbud-3", "leaf-2", "flo-1", "shoot_meristem", "flo-2", "flo-3", "flo-4", "flo-5", "pod&seed-1", "pod&seed-2", "pod&seed-3", "pod-1", "pod-2", "pod-3", "seed-1", "seed-2", "seed-3", "seed-4", "seed-5")
    )
  })
  
  observeEvent(input$accessiongexnone, {
    shinyWidgets::updateMultiInput(
      session = session,
      inputId = "Gexp_mut_group",
      selected = character(0)
    )
  })
  
  
  
  #Gene correlation
  observeEvent(input$submit_GSgcor, {
    if (!exists("geneexpressioncor") ){
      geneexpressioncor <- data.table::fread("./data/Gene_expression.txt", sep = "\t", data.table = F)
      geneexpressioncor <- geneexpressioncor[-55312,]
    } else { NULL }
    
    geneid <- unlist(strsplit(input$genecorPaste, split="\\n"))
    geneid <- trimws(geneid, which = c("both"), whitespace = "[ \t\r\n]")
    exforgene <- geneexpressioncor[geneexpressioncor$Gene_ID %in% geneid, ]
    if( nrow(exforgene) > 1){
      rownames(exforgene) <- exforgene$Gene_ID
      exforgene <- as.matrix(exforgene[,-1])
      exforgene <- exforgene[which(rowSums(exforgene) > 0), ]
      dk <- t(exforgene)
      ds <- cor(dk)
      dm <- data.table::melt(ds)
      dm <- dm[, c(2,1,3)]
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
      }else{
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
    if (input$gcorExamID >0) {
      isolate({
        updateTextAreaInput(session, "genecorPaste", value = "SoyZH13_11G196000\nSoyZH13_08G253500\nSoyZH13_11G016200\nSoyZH13_05G230900\nSoyZH13_18G091401\nSoyZH13_18G133500\nSoyZH13_07G133800\nSoyZH13_07G215300\nSoyZH13_18G212600\nSoyZH13_06G190300\nSoyZH13_03G065800\nSoyZH13_04G034200\nSoyZH13_13G267500\nSoyZH13_10G274400\nSoyZH13_02G255800\nSoyZH13_16G057000\nSoyZH13_19G208300\nSoyZH13_12G018300\nSoyZH13_05G117701")
      })
    } else {NULL}
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
    
    if (blast.in.seq == "") {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "No input sequence received!", type = "error",
        text = NULL
      )
      NULL 
    } else {
      blast.in.file <- gsub("\\s+", "-", Sys.time())
      blast.in.file <- gsub(":", "-", blast.in.file)
      blast.in.file <- paste0(blast.in.file, ".fasta")
      blast.in.file <- file.path(tempdir(), blast.in.file)
      writeLines(blast.in.seq, con = blast.in.file)
      
      blast.db <- input$BLASTdb
      blast.db <- gsub(" ", "_", blast.db)
      blast.db <- paste0("www/BLASTN/", blast.db)
      blast.db.fl <- paste0(blast.db, ".nhr")
      
      if (!file.exists(blast.db.fl)) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "BLAST database not found!", type = "error",
          text = NULL
        )
        NULL
      } else {
        
        if (input$program_database == "Genome Sequence"){
          blastmethod <- input$programdna
        } else if (input$program_database == "Protein Sequence"){
          blastmethod <- input$programpro
        } else if (input$program_database == "Gene Sequence"){
          blastmethod <- input$programgene
          blast.db <- input$BLASTdb
          blast.db <- gsub(" ", "_", blast.db)
          blast.db <- paste0("www/BLASTN/CDS/", blast.db, ".gene")
        } else{
          blastmethod <- input$programcds
          blast.db <- input$BLASTdb
          blast.db <- gsub(" ", "_", blast.db)
          blast.db <- paste0("www/BLASTN/CDS/", blast.db, ".cds")
        }
        
        blast.out.file <- paste0(blast.in.file, ".blast.out")
        blast.cmds <- paste0(blastmethod," -query ", blast.in.file," -db ", '"', paste(blast.db, sep=" ", collapse = " "), '"', " -evalue ",
                             input$BLASTev, " -outfmt 5", " -out ", blast.out.file)
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
              HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Primer design view</b></font>')
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
  
  
  #JBrowseR
  observe({
    HTML.tab <- read.table("./data/jbrowse.link", sep = "\n")
    HTML.tab <- data.frame(matrix(HTML.tab$V1, ncol = 3))
    colnames(HTML.tab) <- rep("",3 )
    
    HTML.tab[10,3] <- ""
    output$JBrowsetable <- DT::renderDataTable(HTML.tab,  
                                               options = list(pageLength = 10, lengthChange = FALSE, Search = FALSE, info = FALSE, dom = 't', 
                                                              searchHighlight = FALSE, autoWidth = FALSE, bSort = FALSE, sDom  = '<"top">lrt<"bottom">ip' ),
                                               escape = FALSE, rownames = FALSE, selection = "none"
    )
  })


  # Accession
  output$mytable1 = DT::renderDT({
    accession <- input$mychooserA
    accession <- gsub(",.+", "", accession)
    accession <- sapply(accession, function(x){
      if (x %in% c("Improved cultivar", "Landrace", "G. Soja")) {
        x.dat <- readLines(paste0("./data/", x, ".soya.txt") )
        return(x.dat)
      } else {
        return(x)
      }
    })
    
    accession <- unique(unlist(accession))
    DT::datatable(
      soya.info[soya.info$ID %in% accession, ]
      , extensions = "Buttons", escape = FALSE, rownames = F, selection = "none", 
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
  
  
  #Orthologous
  observeEvent(input$ORT_sbumit, {
    if ( exists("ort") ) {
    } else {
      ort <- data.table::fread("./info/sort_ortgroups.tsv", sep = "\t", header = T, data.table = F)
    }
    
    geneid <- input$ORT_ID
    geneid <- trimws(geneid, which = c("both"), whitespace = "[ \t\r\n]")
    geneidtrue <- gsub(".+\\.", "", gsub(".+_", "", geneid))
    accession <- gsub("_.+", "", gsub("\\..+", "", geneid))
    if ( geneid == "") {
      ogid <- data.frame() 
    } else {
      ogid <- ort[grep(geneid, ort[ ,grep(accession, ort[1,])] ), ]}
    
    if ( nchar(geneidtrue) == 9 & nrow(ogid) != 0){
      ogid <- data.frame(names(ogid), t(ogid))
      ogid <- ogid[ogid[ ,2] != "", ]
      
      colnames(ogid) <- c("Genome", "Orthologous genes")
      #delete character(0)
      output$ortID <- DT::renderDT({
        DT::datatable(
          ogid
          , extensions = c("Buttons"), selection = "none",
          options = list(pageLength = 30, lengthChange = FALSE, Search = FALSE, info = FALSE, dom = 'Bt', selection = "none",
                         buttons = list('copy', 
                                        list(extend = 'csv',   filename =  paste("Orthologous", geneid, sep = "-")),
                                        list(extend = 'excel', filename =  paste("Orthologous", geneid, sep = "-")), 
                                        'print'),
                         searchHighlight = FALSE, autoWidth = FALSE, bSort = FALSE, 
                         sDom  = '<"top">lrt<"bottom">ip',
                         initComplete = DT::JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#676464', 'color': '#fff'});",
                           "}")
          ), escape = FALSE, rownames = FALSE
        )}, server = FALSE)
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
  observe({
    if (input$ortExam >0) {
      isolate({
        updateTextInput(session, "ORT_ID", value="SoyZH13_01G225600")
      })
    } else {NULL}
  })
  
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
        drawdata <- bargod[1:input$GOnrow, ]
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
          ggplot2::guides(fill=FALSE)
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
      }else {
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
        }else {
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
      
    }else{
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
          escape = FALSE, rownames= FALSE, selection="none", extensions = c("Buttons","FixedColumns"),
          options = list(pageLength = 5, autoWidth = FALSE, bSort = FALSE, scrollX = TRUE, fixedColumns = list(leftColumns = 2),
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
          escape = FALSE, rownames= FALSE, selection="none", extensions = c("Buttons","FixedColumns"),
          options = list(pageLength = 5, autoWidth = FALSE, bSort = FALSE, scrollX = TRUE, fixedColumns = list(leftColumns = 2),
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
          escape = FALSE, rownames= FALSE, selection="none", extensions  = c("Buttons","FixedColumns"),
          options = list(pageLength = 5, autoWidth = FALSE, bSort = FALSE, scrollX = TRUE, fixedColumns = list(leftColumns = 2),
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
          }else {
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
          p1 <- barplot(GOEmf, showCategory = input$GOgenrow, order = T) + ggplot2::scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=40)) + ggplot2::ylab("Number of genes")
          p2 <- enrichplot::dotplot(GOEmf, showCategory=input$GOgenrow, order = "x") + ggplot2::scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=40)) + ggplot2::xlab("Gene Ratio")
          cowplot::plot_grid(p1, p2, ncol=2)
        }) 
      } else {NULL}
      
      if ( nrow(GOEbp@result[GOEbp@result$p.adjust <= input$GOp & GOEbp@result$qvalue <= input$GOq, ]) != 0 ){
        output$GOge2barplot <- shiny::renderPlot({
          p1 <- barplot(GOEbp, showCategory = input$GOgenrow, order = T) + ggplot2::scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=40)) + ggplot2::ylab("Number of genes")
          p2 <- enrichplot::dotplot(GOEbp, showCategory=input$GOgenrow, order = "x") + ggplot2::scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=40)) + ggplot2::xlab("Gene Ratio")
          cowplot::plot_grid(p1, p2, ncol=2)
        }) 
      } else {NULL}
      
      if ( nrow(GOEcc@result[GOEcc@result$p.adjust <= input$GOp & GOEcc@result$qvalue <= input$GOq, ]) != 0 ){
        output$GOge3barplot <- shiny::renderPlot({
          p1 <- barplot(GOEcc, showCategory = input$GOgenrow, order = T) + ggplot2::scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=40)) + ggplot2::ylab("Number of genes")
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
      
    }else{
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
    
    if (length(geneid) != 0 ){
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
          ggplot2::geom_text(ggplot2::aes(label=Freq), position = ggplot2::position_dodge(width=0.9), vjust=-0.25, size = 300/(as.numeric(input$KOnrow)+30))
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
        
      }else {
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
        if ( exists("genelist") ){
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
      
    }else{
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
          escape = FALSE, rownames= FALSE, selection="none", extensions = c("Buttons","FixedColumns"),
          options = list(pageLength = 5, autoWidth = FALSE, bSort = FALSE, scrollX = TRUE, fixedColumns = list(leftColumns = 2),
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
          p1 <- barplot(KOE, showCategory = input$KEnrow, order = T) + ggplot2::scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=40))
          p2 <- enrichplot::dotplot(KOE, showCategory=input$KEnrow, order = "x") + ggplot2::scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=40)) + ggplot2::xlab("")
          cowplot::plot_grid(p1, p2, ncol=2)
        })
        
        output$KEidbarplotif <- renderUI({
          if ( nrow(KOE@result[KOE@result$p.adjust <= input$KEp & KOE@result$qvalue <= input$KEq, ]) != 0 ){
            if ( is.na(input$keggwidth) | is.na(input$keggheight) | input$keggwidth*input$keggheight == 0 ){
              NULL
            }else{
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
        
      }else {
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
        }else {
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
  
  #Download29Soybean
  observe({
    Download.tab <- read.table("./data/29download.link", sep = "\t", as.is = T, header = T)
    
    output$Downloadtable <- DT::renderDataTable(Download.tab,  
                                                options = list(pageLength = 30, lengthChange = FALSE, Search = FALSE, info = FALSE, dom = 't', 
                                                               searchHighlight = FALSE, autoWidth = FALSE, bSort = FALSE, sDom  = '<"top">lrt<"bottom">ip' ),
                                                escape = FALSE, rownames = FALSE, selection = "none"
    )
  })
  
})

