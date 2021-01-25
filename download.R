ui <-  fluidPage(
  tabPanel(
    "Download",icon = icon("search"),
    
    sidebarPanel(
      textInput("regBB", label = h5("Genomic region:",
                 bsButton("q1", label="", icon=icon("question"), style="info", size="small")),
                value = ""),
      
      bsPopover("q1", "A genomic region can be determined by chromosome positions or gene locus. For example, chr7:29506705-29659223 or SoyZH13_01G186100.",
                trigger = "focus"),
      
      actionButton("submit6", strong("Go!",
                  bsButton("q12", label="", icon=icon("question"), style="info", size="small")
      ), width = "90%", styleclass = "success"),
      
      actionButton("clearDOW", strong("Reset"), styleclass = "warning"),
      actionButton("DOWExam", strong("Load example"), styleclass = "info"),
      bsPopover("q12", "Whenever the genomic region or any option is updated, please click Go!",
                trigger = "focus"),
      
      p(h4("Select soya lines:",
                bsButton("qdl2", label="", icon=icon("question"), style="info", size="small"))),
      bsPopover("qdl2", "Only the chosen soya lines will be used.",
                trigger = "focus"),
      
      chooserInput("mychooserD", "Available frobs", "Selected frobs",
                   c(), all.soya.cho, size = 12, multiple = TRUE
      ),
      multiInput("down_mut_group", h4("Mutation types:",
                                      bsButton("qg1", label="", icon=icon("question"), style="info", size="small")),
                 choices = mutationtypes,
                 choiceValues = NULL,
                 width = 800),
      bsPopover("qdl1", "Only SNPs with selected mutation effects will be used.",
                trigger = "focus")
      ),
      br(),
      
      downloadButton("bulkdownloadsnpInfo.txt", "Download SNPs information"),
      downloadButton("bulkdownloadsnp.txt", "Download genotype data"),
      downloadButton("bulkdownloadgene.txt", "Download gene annotation"),
      h4("SNPs information in specified genomic region:"),
    mainPanel(
      dataTableOutput("mytable2")
    ))
    )



server <- function(input, output, session){
  # Bulk download genotypes of seleceted SNPs
  observe({
    if (input$submit6>0) {
      isolate({
        myPos <- anaReg(input$regBB)
        
        if (validReg(myPos)) {
        } else {
          js_string <- 'alert("Please input genomic region or gene model in appropriate format!");'
          session$sendCustomMessage(type='jsCode', list(value = js_string))
          myPos <- NULL
        }
        
        snp.info.down <<- NULL
        
        if (!is.null(myPos)) {
          output$mytable2 = renderDataTable({
            snp.info.down <<- snpInfo(chr=myPos$chr, start=myPos$start, end=myPos$end, 
                                      accession = input$mychooserD$selected, mutType = input$down_mut_group)
            snp.info.down[[2]]
          }, options = list(lengthMenu = c(5, 8, 10), pageLength = 5, searching = TRUE, autoWidth = TRUE), escape = FALSE
          )
          
          output$bulkdownloadsnp.txt <- downloadHandler(
            filename = function() { "down.snp.geno.txt" },
            content = function(file) {
              write.table(snp.info.down[[1]][[1]], file, sep="\t", quote=F)
            })
          
          # Bulk download information of SNPs
          output$bulkdownloadsnpInfo.txt <- downloadHandler(
            filename = function() { "down.snp.info.txt" },
            content = function(file) {
              write.table(snp.info.down[[2]], file, sep="\t", quote=F, row.names=F)
            })
          
          # Bulk download gene annotation
          output$bulkdownloadgene.txt <- downloadHandler(
            filename = function() { "down.gene.info.txt" },
            content = function(file) {
              gene.info <- gff[gff$chr==myPos$chr & gff$start>=myPos$start & gff$end<=myPos$end, ]
              write.table(gene.info, file, sep="\t", quote=F, row.names=F)
            })
        } else {
          NULL
        }
        
      })
    } else {NULL}
  })
  
  observe({
    if (input$clearDOW>0) {
      isolate({
        updateTextInput(session, "regBB", value="")
      })
    } else {NULL}
  })
  
  observe({
    if (input$DOWExam >0) {
      isolate({
        updateTextInput(session, "regBB", value="chr7:29506705-29659223")
      })
    } else {NULL}
  })
  } 
  

shinyApp(ui = ui, server = server)





