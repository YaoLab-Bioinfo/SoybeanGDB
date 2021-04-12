
library(shiny)

ui <- fluidPage(
  navbarPage(
    title = HTML("<strong style='font-size:18px'>Genomes</strong>"), icon = icon("search"),
    tabPanel(
      
      title = HTML("<strong style='font-size:18px'>Search by gene ID</strong>"), icon = icon("search"),
      sidebarPanel(
        fixedRow(
          column(12,
                 tags$div(HTML('<i class="fa fa-star" aria-hidden="true"></i> <font size="4" color="red"><b>Gene information</b></font>'),
                          bsButton("qsgsid", label="", icon=icon("question"), style="info", size="small")),
                 bsPopover("qsgsid", "Input gene to query base sequences and other information", trigger = "focus")
          )
        ),
        
        selectInput("variety_ID", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>select variety</b></font>'),
                                             bsButton("qsgs0", label="", icon=icon("question"), style="info", size="small")), 
                    list(`G.soja` = list("PI 562565", "PI 549046", "PI 578357"),
                         `Improved cultivar` = list("Zhonghuang 13", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                    "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                    "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1"),
                         `Landrace` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                           "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye")) ,
                    selected = "Zhonghuang 13"
        ),
        bsPopover("qsgs0", "Select a database that you would like to blast",
                  trigger = "focus"),
        
        textInput("geneid", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Gene ID</b></font>'),
                                       bsButton("qsgs1", label="", icon=icon("question"), style="info", size="small")),
                  value = "SoyZH13_02G148200"),
        
        bsPopover("qsgs1", "A genomic region can be determined by chromosome positions or gene locus. For example, SoyZH13_02G148200",
                  trigger = "focus"),
        
        actionButton("submit_GSID", strong("submit!",
                                           bsButton("qsgs2", label="", icon=icon("question"), style="info", size="small")
        ), styleclass = "success"),
        
        actionButton("clearSERID", strong("Reset"), styleclass = "warning"),
        actionButton("SERExamID", strong("Load example"), styleclass = "info"),
        conditionalPanel(condition="input.submit_GSID != '0'", busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
        bsPopover("qsgs2", "Whenever the genomic region or any option is updated, please click Go!",
                  trigger = "focus")
      ),
      mainPanel(
        fluidRow(
          column(3, downloadButton("sequence_ID.txt", "Download Gene Sequence", style = "width:100%;", class = "buttDown")),
          column(3, downloadButton("cdssequence_ID.txt", "Download CDS Sequence", style = "width:100%;", class = "buttDown")),
          column(3, downloadButton("cdnasequence_ID.txt", "Download cDNA Sequence", style = "width:100%;", class = "buttDown")),
          column(3, downloadButton("prosequence_ID.txt", "Download Protein Sequence", style = "width:100%;", class = "buttDown"))
        ),
        
        textOutput("seq_title"),
        verbatimTextOutput("seq"),
        tags$head(tags$style("#seq_title{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                       font-family:Times New Roman;
                                      }"
        ),
        tags$style("#seq{
                     max-width = 100%;
                     white-space: pre-wrap;
                     max-height: 300px;
          }")
        ),
        textOutput("cds_title"),
        verbatimTextOutput("cds"),
        tags$head(tags$style("#cds_title{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                       font-family:Times New Roman;
                                      }"
        ),
        tags$style("#cds{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
          }")
        ),
        textOutput("cdna_title"),
        verbatimTextOutput("cdna"),
        tags$head(tags$style("#cdna_title{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                       font-family:Times New Roman;
                                      }"
        ),
        tags$style("#cdna{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
          }")
        ),
        textOutput("pro_title"),
        shiny::verbatimTextOutput("pro"),
        tags$head(tags$style("#pro_title{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                       font-family:Times New Roman;
                                      }"
        ),
        tags$style("#pro{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
                     
          }")
        ),
        
        textOutput("gffinfotitle"),
        DT::dataTableOutput("gffinfo"),
        tags$head(tags$style("#gffinfotitle{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                       font-family:Times New Roman;
                                      }"
        ),
        tags$style("#gffinfo{
                     width = 100%;
                     max-height: 300px;
          }")
        )
      )
      
    ),
    
    tabPanel(
      
      title = HTML("<strong style='font-size:18px'>Search interval</strong>"), icon = icon("search"),
      
      sidebarPanel(
        
        fixedRow(
          column(12,
                 tags$div(HTML('<i class="fa fa-star" aria-hidden="true"></i> <font size="4" color="red"><b>Interval information</b></font>'),
                          bsButton("qsgsit", label="", icon=icon("question"), style="info", size="small")),
                 bsPopover("qsgsit", "Input interval to query base sequences and other information", trigger = "focus")
          )
        ),
        
        selectInput("variety_IT", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>select variety</b></font>'),
                                             bsButton("qsit0", label="", icon=icon("question"), style="info", size="small")), 
                    list(`G.soja` = list("PI 562565", "PI 549046", "PI 578357"),
                         `Improved cultivar` = list("Zhonghuang 13", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                    "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                    "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1"),
                         `Landrace` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                           "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye")) ,
                    selected = "Zhonghuang 13"
        ),
        bsPopover("qsit0", "Select a database that you would like to blast",
                  trigger = "focus"),
        
        textInput("geneinterval", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Interval</b></font>'),
                                             bsButton("qsit1", label="", icon=icon("question"), style="info", size="small")),value = "chr2:20260371-22686979"),
        
        
        bsPopover("qsit1", "A genomic region can be determined by chromosome positions or gene locus. For example, chr2:20260371-22686979",
                  trigger = "focus"),
        
        actionButton("submit_GSIT", strong("submit!",
                                           bsButton("qsit2", label="", icon=icon("question"), style="info", size="small")
        ), styleclass = "success"),
        
        actionButton("clearSERIT", strong("Reset"), styleclass = "warning"),
        actionButton("SERExamIT", strong("Load example"), styleclass = "info"),
        conditionalPanel(condition="input.submit_GSIT != '0'", busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
        bsPopover("qsit2", "Whenever the genomic region or any option is updated, please click Go!",
                  trigger = "focus")
        
      ),
      mainPanel(
        
        fluidRow(
          column(4, downloadButton("sequence_IT.txt", "Download Sequence", style = "width:100%;", class = "buttDown")),
          column(4, downloadButton("genesequence_IT.txt", "Download Gene Sequence", style = "width:100%;", class = "buttDown")),
          column(4, downloadButton("genesinfo_IT.txt", "Download Gene Information", style = "width:100%;", class = "buttDown"))
        ),
        
        textOutput("seq_title_it"),
        verbatimTextOutput("seq_it"),
        tags$head(tags$style("#seq_title_it{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                       font-family:Times New Roman;
                                      }"
        ),
        tags$style("#seq_it{
                     max-width = 100%;
                     white-space: pre-wrap;
                     max-height: 300px;
          }")
        ),
        DT::dataTableOutput("geneinfo"),
        
        textOutput("gene_title_it"),
        verbatimTextOutput("gene_it"),
        tags$head(tags$style("#gene_title_it{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                      }"
        ),
        tags$style("#gene_it{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
          }")
        ),
        
        textOutput("cds_title_it"),
        verbatimTextOutput("cds_it"),
        tags$head(tags$style("#cds_title_it{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                       font-family:Times New Roman;
                                      }"
        ),
        tags$style("#cds_it{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
          }")
        ),
        textOutput("cdna_title_it"),
        verbatimTextOutput("cdna_it"),
        tags$head(tags$style("#cdna_title_it{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                       font-family:Times New Roman;
                                      }"
        ),
        tags$style("#cdna_it{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
          }")
        ),
        textOutput("pro_title_it"),
        shiny::verbatimTextOutput("pro_it"),
        tags$head(tags$style("#pro_title_it{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                       font-family:Times New Roman;
                                      }"
        ),
        tags$style("#pro_it{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
                     
          }")
        ),
        
        textOutput("gffinfotitle_it"),
        DT::dataTableOutput("gffinfo_it"),
        tags$head(tags$style("#gffinfotitle_it{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                       font-family:Times New Roman;
                                      }"
        ),
        tags$style("#gffinfo_it{
                     width = 100%;
                     max-height: 300px;
          }")
        )
        
      )
      
    )
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$submit_GSID,{
    
    load(paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".gene.info.RData"))
    #判断用户输入数据 chr start end
    geneid <- input$geneid
    start <- gene_info_s$start[gene_info_s$id == input$geneid]
    end <- gene_info_s$end[gene_info_s$id == input$geneid]
    chr <- gene_info_s$chr[gene_info_s$id == input$geneid]
    
    
    if(identical(chr, character(0))){
      sendSweetAlert(
        session = session,
        title = "Yor input ID is wrong!", type = "error",
        text = NULL
      )
      NULL 
      
    }else{
      fasta <- readDNAStringSet(paste0("./info/",gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".",chr, ".fasta.gz"))
      #判断长度
      if (start > width(fasta[chr]) | end > width(fasta[chr]) | end - start > 10000000 | !(end - start > 0) | is.na(end + start) ){
        sendSweetAlert(
          session = session,
          title = "The length of the chromosome is not right!", type = "error",
          text = NULL
        )
        NULL 
      }else{
        #fasta cds protein信息读取
        load(paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".cds.fasta.RData"))
        load(paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".protein.RData"))
        load(paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".cdna.fasta.RData"))
        
        #输入为GENEID
        if( geneid %in% gene_info_s$id ){
          
          gfinfo <- gene_info_s[gene_info_s$id == geneid, ]
          
          gfinfo$seq[gfinfo$strand == "+"] <- as.character(subseq(fasta[gfinfo[gfinfo$strand == "+",]$chr], gfinfo[gfinfo$strand == "+",]$start, gfinfo[gfinfo$strand == "+",]$end))
          gfinfo$seq[gfinfo$strand == "-"] <- as.character(reverseComplement(subseq(fasta[gfinfo[gfinfo$strand == "-",]$chr], gfinfo[gfinfo$strand == "-",]$start, gfinfo[gfinfo$strand == "-",]$end)))
          
          df <- DNAStringSet(gfinfo$seq)
          
          names(df) <- paste0(geneid, " ",gfinfo$chr, ":", start, "-", end, " length = ", end - start +1)
          
          tmp.f1 <- file.path(tempdir(), "t1.fa")
          
          output$seq <- renderText({
            writeXStringSet(df, file = tmp.f1, width = 150)
            readLines(tmp.f1)
          }, sep = "\n")
          output$seq_title <- renderText({
            if( is.null(df) ){
              NULL
            }else{
              "Gene sequence"
            }
          })
          
          #protein
          dp <- protein[grep(geneid, names(protein))]
          output$pro <- shiny::renderText({
            tmp.f2 <- file.path(tempdir(), "t2.fa")
            writeXStringSet(dp, file = tmp.f2, width = 150)
            readLines(tmp.f2)
          }, sep = "\n")
          
          output$pro_title <- renderText({
            if( is.null(dp) ){
              NULL
            }else{
              "Protein information"
            }
          })
          
          #cds
          dc <- cds.info[grep(geneid, names(cds.info))]
          
          output$cds <- renderText({
            tmp.f3 <- file.path(tempdir(), "t3.fa")
            writeXStringSet(dc, file = tmp.f3, width = 150)
            readLines(tmp.f3)
          }, sep = "\n")
          
          output$cds_title <- renderText({
            if(is.null(dc)){
              NULL
            }else{
              "CDS information"
            }            
          })
          
          #cdna
          dcdna <- cdna.info[grep(geneid, names(cdna.info))]
          output$cdna_title <- renderText({
            if(is.null(dcdna)){
              NULL
            }else{
              "cDNA information"
            }
          })
          
          output$cdna <- renderText({
            
            tmp.f4 <- file.path(tempdir(), "t4.fa")
            writeXStringSet(dcdna, file = tmp.f4, width = 150)
            readLines(tmp.f4)
          }, sep = "\n")
          
          output$cds_title <- renderText({
            "The cds sequence of gene:"
          })
          
          #geneinfo
          
          output$geneinfo = DT::renderDataTable({
            NULL
          })
          
          gffile <- paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".",chr ,".gff.txt.gz")
          gf <- read.table(gffile, header = T, sep = "\t", as.is = T)
          
          output$gffinfo <- DT::renderDataTable({
            gfinfo <- gf[grep(geneid, gf$Attributes), ]
            gfinfo
          }, extensions = "Buttons", rownames = F, options = list(leftColumns = 8, scrollX = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'print'))
          )
          
          output$gffinfotitle <- renderText({
            if(is.null(gf)){
              NULL
            }else{
              "The gene's gff information"
            }
          })
        }else{
          sendSweetAlert(
            session = session,
            title = "Your ID is wrong", type = "error",
            text = NULL
          )
          NULL 
        }
        
        
      }
    }  
    
    output$sequence.txt <- downloadHandler(
      filename <- function() { paste('The_sequence.txt') },
      content <- function(file) {
        writeXStringSet(df, file, width = 150)
      }, contentType = 'text/plain'
    )
    
    output$genesequence.txt <- downloadHandler(
      filename <- function() { paste('The_gene_sequence.txt') },
      content <- function(file) {
        writeXStringSet(dt, file, width = 150)
      }, contentType = 'text/plain'
    )
    
    output$genesinfo.txt <- downloadHandler(
      filename <- function() { paste('The_gene_infomation.txt') },
      content <- function(file) {
        write.table(gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)],], file, sep = "\t", col.names = T, row.names = F, quote = F)
      }, contentType = 'text/plain'
    )
    
    
    
  }) 
  
  observe({
    if (input$clearSERID>0) {
      isolate({
        updateTextInput(session, "geneid", value = "")
      })
    } else {NULL}
  })
  
  observe({
    if (input$SERExamID >0) {
      isolate({
        updateSelectInput(session, "variety_ID", selected = "Zhonghuang 13")
        updateTextInput(session, "geneid", value = "SoyZH13_02G148200")
        
        
      })
    } else {NULL}
  })
  
  
  observeEvent(input$submit_GSIT,{
    
    load(paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".gene.info.RData"))
    chr <- gsub(":.+", "", input$geneinterval)
    start <- as.numeric(gsub("\\s","", strsplit(gsub(".+:", "", input$geneinterval),"-")[[1]]))[1]
    end <- as.numeric(gsub("\\s","", strsplit(gsub(".+:", "", input$geneinterval),"-")[[1]]))[2]
    
    fasta <- readDNAStringSet(paste0("./info/",gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".",chr, ".fasta.gz"))
    
    gene <- GRanges(seqnames = gene_info_s$chr, IRanges(start = gene_info_s$start, end = gene_info_s$end))
    thefind <- GRanges(seqnames = chr, IRanges(start = start, end = end))
    find.f1 <- findOverlaps(gene, thefind)
    geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)],]
    
    #interval sequence
    
    output$seq_title_it <- renderText({
      if( length(geneinfo[, 1]) != 0  ){
        "The sequence of interval:"
      }else{
        NULL
      }
    })
    
    output$seq_it <- renderText({
      if( length(geneinfo[, 1]) != 0 ){
        df  <- subseq(fasta[names(fasta) == chr], as.numeric(start), as.numeric(end) )
        names(df) <- paste0(chr, ":", start, "-", end, " length = ", end - start +1)
        tmp.f11 <- file.path(tempdir(), "t11.fa")
        writeXStringSet(df, file = tmp.f11, width = 150)
        readLines(tmp.f11)
      }
      else{ NULL }
    }, sep = "\n")
    
    #fasta cds protein信息读取
    load(paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".cds.fasta.RData"))
    load(paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".protein.RData"))
    load(paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".cdna.fasta.RData"))
    
    #geneinfo
    
    output$geneinfo = DT::renderDataTable({
      if( length(geneinfo[, 1]) != 0 ){
        geneinfo
      }else{
        sendSweetAlert(
          session = session,
          title = "NO gene in interval!", type = "error",
          text = NULL
        )
        NULL 
      }}, escape = FALSE, rownames= FALSE, selection="single", options = list(pageLength = 10, autoWidth = TRUE, bSort=FALSE))

  
  
    #gene sequence
  
    output$gene_it <- renderText({
      if (is.null(input$geneinfo_rows_selected)) {
      } else {
        clicked <- input$geneinfo_rows_selected
        info <- data.frame(geneinfo[clicked,])
        gene <- subseq(fasta[info[1,2]], as.numeric(info[1,3]), as.numeric(info[1,4]))
        names(gene) <- paste0(names(gene),  ":", as.numeric(info[1,3]), "-", as.numeric(info[1,4]), " length = ", as.numeric(info[4]) - as.numeric(info[1,3]) +1)
        tmp.f4 <- file.path(tempdir(), "t4.fa")
        writeXStringSet(gene, file = tmp.f4, width = 150)
        readLines(tmp.f4) 
      }
    },sep = "\n")
    
    output$gene_title_it <- renderText({
      if (is.null(input$geneinfo_rows_selected)) {
        NULL
      } else {
        "The gene's sequence"
      }
    })
    
    #cds sequence
    output$cds_it <- renderText({
      if (is.null(input$geneinfo_rows_selected)) {
      } else {
        clicked <- input$geneinfo_rows_selected
        geneid <- data.frame(geneinfo[clicked,])[1,1]
        ditcs <- cds.info[grep(geneid, names(cds.info))]
        tmp.f6 <- file.path(tempdir(), "t6.fa")
        writeXStringSet(ditcs, file = tmp.f6, width = 150)
        readLines(tmp.f6)
      }
    },sep = "\n")
    
    output$cds_title_it <- renderText({
      if(is.null(input$geneinfo_rows_selected)){
        NULL
      }else{
        "CDS information"
      }
    })
    
    #cdna sequence
    output$cdna_it <- renderText({
      if (is.null(input$geneinfo_rows_selected)) {
        NULL
      } else {
        clicked <- input$geneinfo_rows_selected
        geneid <- data.frame(geneinfo[clicked,])[1,1]
        ditcd <- cdna.info[grep(geneid, names(cdna.info))]
        tmp.f6 <- file.path(tempdir(), "t6.fa")
        writeXStringSet(ditcd, file = tmp.f6, width = 150)
        readLines(tmp.f6)
      }
    },sep = "\n")
    
    output$cdna_title_it <- renderText({
      if(is.null(input$geneinfo_rows_selected)){
        NULL
      }else{
        "cDNA information"
      }
    })
    
    #proteion
    
    output$pro_it <- shiny::renderText({
      if (is.null(input$geneinfo_rows_selected)) {
      NULL
        }else{
      clicked <- input$geneinfo_rows_selected
      geneid <- data.frame(geneinfo[clicked,])[1,1]
      dpit <- protein[grep(geneid, names(protein))]
      tmp.f7 <- file.path(tempdir(), "t7.fa")
      writeXStringSet(dpit, file = tmp.f7, width = 150)
      readLines(tmp.f7)
    }}, sep = "\n")
    
    output$pro_title_it <- renderText({
      if( is.null(input$geneinfo_rows_selected) ){
        NULL
      }else{
        "Protein information"
      }
    })

  #click gff
  output$gffinfo_it <- DT::renderDataTable({
    if (is.null(input$geneinfo_rows_selected)){
    }else {
      clicked <- input$geneinfo_rows_selected
      geneid <- data.frame(geneinfo[clicked,])[1,1]
      gffile <- paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".",chr ,".gff.txt.gz")
      gf <- read.table(gffile, header = T, sep = "\t", as.is = T)
      gfinfo <- gf[grep(geneid, gf$Attributes), ]
      gfinfo <- gfinfo[, c(1:5, 9)]
      gfinfo
    }}, extensions = "Buttons", 
    rownames = F, 
    options = list(leftColumns = 8, 
                   scrollX = TRUE, dom = 'Bfrtip', 
                   buttons = c('copy', 'csv', 'excel', 'print')
    )
  )
  
  output$gffinfotitle_it <- renderText({
    if(is.null(input$geneinfo_rows_selected)){
      NULL
    }else{
      "The gff of gene for your select:"
    }
  })
  
  }
  
 )
  
  
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
        updateTextInput(session, "geneinterval", value = "chr2:20260371-22686979")

        
      })
    } else {NULL}
  })
}

shinyApp(ui, server)
