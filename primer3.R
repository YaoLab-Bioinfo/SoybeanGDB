ui <- fluidPage(
  title = HTML("<strong style='font-size:18px'>Primer3</strong>"),icon = icon("fire-alt"),
  
  sidebarPanel(
    HTML('<i class="fa fa-cog" aria-hidden="true"></i> <font size="4" color="black"><b>Parameter Selection:</b></font>'),
    fluidRow(
      column(4,selectInput("Chrprimer", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Chr:</b></font>'),choices = paste0("Chr", 1:20), selected = "Chr1")),
      column(4,textInput("upprimer", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Start:</b></font>'),value = "110000")),
      column(4,textInput("downprimer", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>End:</b></font>'),value = "111000"))
    ),
    textInput("PRIMER_OPT_SIZE", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Optimal primer size(bp):</b></font>'),value = "20"),
    sliderInput("PRIMER_SIZE", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Primer size range(bp):</b></font>'), min = 1, max = 35, 
                value = c(18, 25)),
    
    textInput("PRIMER_OPT_TM", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Optimal primer TM(℃):</b></font>'),value = "59.0"),
    
    fluidRow(
      column(6,textInput("PRIMER_MIN_TM", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Min primer TM(℃):</b></font>'),value = "57.0")),
      column(6,textInput("PRIMER_MAX_TM", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max primer TM(℃):</b></font>'),value = "62.0"))
    ),
    
    textInput("PRIMER_OPT_GC_PERCENT", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Optimal GC percent:</b></font>'),value = "50.0"),
    
    sliderInput("PRIMER_GC", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>GC percent range(bp):</b></font>'), min = 0, max = 100, 
                value = c(20, 80), step=0.1),
    
    textInput("PRIMER_MAX_NS_ACCEPTED", label = HTML("<i class='fa fa fa-circle' aria-hidden='true'></i> <font size='4' color='red'><b>Max #N's accepted:</b></font>"),value = "0"),
    
    fluidRow(
      column(6,textInput("PRIMER_MAX_POLY_X", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max Poly-X:</b></font>'),value = "10")),
      column(6,textInput("PRIMER_INTERNAL_MAX_POLY_X", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max Internal Poly-X:</b></font>'),value = "15"))
    ),
    fluidRow(
      column(6,textInput("PRIMER_MAX_SELF_ANY", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max Self Complementarity:</b></font>'),value = "45.0")),
      column(6,textInput("PRIMER_MAX_SELF_ANY_TH", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max Pair Complementarity:</b></font>'),value = "45.0"))
    ),
    fluidRow(
      column(6,textInput("PRIMER_MAX_SELF_END", label = HTML("<i class='fa fa fa-circle' aria-hidden='true'></i> <font size='3' color='red'><b>Max 3' Self Complementarity:</b></font>"),value = "35.0")),
      column(6,textInput("PRIMER_MAX_SELF_END_TH", label = HTML("<i class='fa fa fa-circle' aria-hidden='true'></i> <font size='3' color='red'><b>Max 3' Pair Complementarity:</b></font>"),value = "35.0"))
    ),
    textInput("PRIMER_PRODUCT_SIZE_RANGE", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Product Size Ranges:</b></font>'),value = "100-300,300-600,600-1000"),
    
    actionButton("submitprimer", strong("submit!",
                                        bsButton("qpr1", label="", icon=icon("question"), style="info", size="small")
    ), styleclass = "success"),
    conditionalPanel(condition="submitprimer != '0'", busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
    bsPopover("qpr1", "Whenever the genomic region or any option is updated, please click Go!",
              trigger = "focus")
    
  ),
  downloadButton("bulkdownloadprimerInfo.txt", "Download indels information"),
  mainPanel(
    dataTableOutput("primertable"),
    verbatimTextOutput("primerseq")
  )
)


server <- function(input, output, session) {
  observeEvent(input$submitprimer,{
    choicesequence <- paste0(gsub("C", "c", input$Chrprimer), ":", input$upprimer, "-", input$downprimer)
    myPos <- anaReg(choicesequence)
    
    if(validReg(myPos)){
      if (!is.null(myPos)) {
        chr <- myPos$chr
        start <- myPos$start
        end <- myPos$end
        load(paste0("./info/Zhonghuang_13." ,chr, ".fasta.RData"))
        seqprimer <- subseq(fasta, start, end)
        SEQUENCE_ID <- "example"
        SEQUENCE_TEMPLATE <- paste0("SEQUENCE_TEMPLATE=", seqprimer)
        PRIMER_OPT_SIZE <- paste0("PRIMER_OPT_SIZE=", input$PRIMER_OPT_SIZE)
        PRIMER_MAX_SIZE <- paste0("PRIMER_MAX_SIZE=", input$PRIMER_SIZE[2])
        PRIMER_MIN_SIZE <- paste0("PRIMER_MIN_SIZE=", input$PRIMER_SIZE[1])
        print(PRIMER_MIN_SIZE)
        print(PRIMER_MAX_SIZE)
        
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
        
        PRIMER_PRODUCT_SIZE_RANGE <- paste0("PRIMER_PRODUCT_SIZE_RANGE", input$PRIMER_PRODUCT_SIZE_RANGE)
        
        indelpos <- read.table(paste0("./info/Position/", input$Chrprimer, ".indel.position"), sep = "\t", header = T)
        snppos <- read.table(paste0("./info/Position/", input$Chrprimer, ".snp.position"), sep = "\t", header = T)
        snpnu <- snppos[snppos$POS <= end & snppos$POS >= start, ]
        indelnu <- indelpos[indelpos$POS <= end & indelpos$POS >= start, ]
        print(snpnu)
        print(indelnu)
        allnu <- rbind(snpnu, indelnu)
        allnu$POS <- allnu$POS - start
        print(allnu)
        SEQUENCE_TARGET <- paste0("SEQUENCE_TARGET=", str_c(allnu$POS, ",", allnu$length, collapse = " "))
        print(SEQUENCE_TARGET)
        if (length(allnu$POS) > 0){
          primerorder <- paste(SEQUENCE_TEMPLATE, PRIMER_OPT_SIZE, PRIMER_MAX_SIZE, PRIMER_MIN_SIZE,
                               PRIMER_OPT_TM, PRIMER_MAX_TM, PRIMER_MIN_TM,
                               PRIMER_OPT_GC_PERCENT, PRIMER_MAX_GC, PRIMER_MIN_GC,
                               PRIMER_MAX_NS_ACCEPTED, PRIMER_MAX_POLY_X, PRIMER_INTERNAL_MAX_POLY_X,
                               PRIMER_MAX_SELF_ANY, PRIMER_MAX_SELF_ANY_TH, PRIMER_MAX_SELF_END, PRIMER_MAX_SELF_END_TH, 
                               SEQUENCE_TARGET, "=", sep = "\n")
        }else{
          primerorder <- paste(SEQUENCE_TEMPLATE, PRIMER_OPT_SIZE, PRIMER_MAX_SIZE, PRIMER_MIN_SIZE,
                               PRIMER_OPT_TM, PRIMER_MAX_TM, PRIMER_MIN_TM,
                               PRIMER_OPT_GC_PERCENT, PRIMER_MAX_GC, PRIMER_MIN_GC,
                               PRIMER_MAX_NS_ACCEPTED, PRIMER_MAX_POLY_X, PRIMER_INTERNAL_MAX_POLY_X,
                               PRIMER_MAX_SELF_ANY,  PRIMER_MAX_SELF_ANY_TH, PRIMER_MAX_SELF_END, PRIMER_MAX_SELF_END_TH,
                               "=", sep = "\n")
        }
        print(input$PRIMER_SIZE)
        tmp.order <- file.path(tempdir(), "primer3.order")
        print(tmp.order)
        writeLines(primerorder, tmp.order)
        tmp.output <- file.path(tempdir(), "primer3.output")
        writeLines(primerorder, tmp.order, sep = "\n")
        system(paste0("primer3_core -format_output ", tmp.order, " > ", tmp.output))
        
        primer3output <- readLines(tmp.output)
        
        primertable <- primer3output[sort(c(grep("LEFT PRIMER", primer3output), grep("RIGHT PRIMER", primer3output)))]
        primertable[-c(1:2)] <- sub("^...", "", primertable[-c(1:2)])
        
        output$primertable <- renderDataTable({
          primertable <- read.table(text = primertable)
          primertable$Oligos <- paste(primertable$V1 , primertable$V2)
          primertable <- primertable[, c(11,3:10)]
          colnames(primertable) <- c("Oligos", "Start position", "Length", "Tm", "GC percent", "Self any", "Self end", "Hairpin", "Sequence")
          primertable
        }, escape = FALSE, options = list(pageLength = 6, autoWidth = TRUE, bSort=FALSE))
        
        
        output$primerseq <- renderText({
          startcut <- grep("INCLUDED REGION SIZE:", primer3output) + 1
          endcut <- grep("ADDITIONAL OLIGOS", primer3output) - 1
          primer3output[startcut:endcut][!primer3output[startcut:endcut] == ""]
        }, sep = "\n")
        
        
      }else{ NULL }
    }
    else{
      sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input genomic region or gene model in appropriate format!"
      )
    }
    
  })
  
  
}

shinyApp(ui, server)
