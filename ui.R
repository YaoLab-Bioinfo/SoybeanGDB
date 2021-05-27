library(shinyBS)


shinyUI(
  fluidPage(
    shinydisconnect::disconnectMessage(
      text = "Your session timed out, reload the application!",
      refresh = "Reload now",
      background = "#f89f43",
      colour = "white",
      overlayColour = "grey",
      overlayOpacity = 0.75,
      top = 250,
      refreshColour = "brown"
    ),
    #修改全局字体

    #tags$style('* {font-size:15px;font-family:sans-serif;}'),
    #tags$head(tags$style("#regB {font-size:20px;font-family:sans-serif;}")),
    tags$style(type='text/css', ".selectize-input { font-size: 15px;line-height: 15px;font-family:sans-serif; } .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
    #tags$style(type='text/css', ".irs-grid-text { font-size: 18pt; }"),
 
    navbarPage(
        title = HTML("<strong style='color:#000000'>SoybeanGDB</strong>"),
        windowTitle = "A comprehensive genome database of soybean",
        id = "The_page",
      #home
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Home</strong>"), icon = icon("home"), Homepage),
      
      
      
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>SNPs</strong>"),
        
      # Genome browse
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Browse</strong>"), icon = icon("window-maximize"),
        
        sidebarPanel(
          #text input
          tags$style("#regB {font-size:15px;font-family:sans-serif;}"),
          width = 3,
          tags$div(h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Browse SNPs</b></font>'),
                   bsButton("qgbv", label="", icon=icon("question"), style="info", size="small"))),
          bsPopover("qgbv", "For a specified genomic region or gene model, all the SNPs among user-selected soybean accessions were extracted and subjected to genome browser visualization", trigger = "focus"),

          textInput("regB", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                       bsButton("q6", label="", icon=icon("question"), style="info", size="small")),
                    value = "chr1:29765419-29793053"),
          
          bsPopover("q6", "A genomic region can be determined by chromosome positions or gene locus. For example, chr1:29765419-29793053 or SoyZH13_01G225600.",
                    trigger = "focus"),
          shinysky::actionButton("submit_browse", strong("Submit!",
                                         bsButton("q7", label="", icon=icon("question"), style="info", size="small")
          ), width = "90%", styleclass = "success"),
          bsPopover("q7", "Whenever the genomic region is updated, please click Submit!",
                    trigger = "focus"),
          shinysky::actionButton("clearGB", strong("Reset"), styleclass = "warning"),
          shinysky::actionButton("GBExam", strong("Load example"), styleclass = "info"),
          
          conditionalPanel(condition="input.submit_browse != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),

          sliderInput(inputId = "GBUP",  label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (bp)</b></font>'),
                                 bsButton("qg2", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 0, ticks = FALSE),
          bsPopover("qg2", "Extend the input genomic region to its upstream.",
                    trigger = "focus"),
          sliderInput("GBDOWN", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                   bsButton("qg4", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 0, ticks = FALSE),
          bsPopover("qg4", "Extend the input genomic region to its downstream.",
                    trigger = "focus"),

          shinyWidgets::multiInput("mychooserB", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
                                      bsButton("qg3", label="", icon=icon("question"), style="info", size="small")),
                     choices = all.soya.cho,
                     selected = all.soya.cho,
                     width = 800,
                     options = list(
                       enable_search = TRUE,
                       non_selected_header = "Choose from:",
                       selected_header = "You have selected:"
                     )
          ),
          bsPopover("qg3", "Only the chosen soybean accessions will be used.",
                    trigger = "focus"),
          fluidRow(
                  column(6, actionButton("browsenone1", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                  column(6, actionButton("browseall1", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                       ),
          
          shinyWidgets::multiInput("GB_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation effect</b></font>'),
                                        bsButton("qg1", label="", icon=icon("question"), style="info", size="small")),
                     choices = mutationtypes,
                     selected = mutationtypes,
                     width = 800,
                     options = list(
                       enable_search = TRUE,
                       non_selected_header = "Choose from:",
                       selected_header = "You have selected:"
                     )
          ),
          bsPopover("qg1", "Only SNPs with selected mutation effects will be used.",
                    trigger = "focus"),
          fluidRow(
            column(6, actionButton("browsenone2", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            column(6, actionButton("browseall2", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          ),
          
        ),
        

      mainPanel(
        width = 9,
        fluidRow(
          column(4, uiOutput("downloadBro01")),
          column(4, uiOutput("downloadBro02")),
          column(4, uiOutput("downloadBro03"))
          #column(4, downloadButton("downloadsnp.txt", style = "width:100%;", "Download genotype data", class = "buttDown")),
          #column(4, downloadButton("downloadsnpInfo.txt", style = "width:100%;", "Download SNPs information", class = "buttDown")),
          #column(4, downloadButton("downloadGB.pdf",  style = "width:100%;", "Download pdf-file", class = "buttDown")),
          #tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
        ),
        plotly::plotlyOutput("gbrowser", height = '600px', width = '100%'))

        
      ),
      
      # Bulk download of data
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Search</strong>"), icon = icon("search"),
        
        sidebarPanel(
          tags$style("#regBB {font-size:15px;font-family:sans-serif;}"),
          width = 3,
          fixedRow(
            column(12,
                   tags$div(h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Search SNPs</b></font>'),
                            bsButton("qqsi", label="", icon=icon("question"), style="info", size="small"))),
                   bsPopover("qqsi", "Search SNPs among 2898 accessions in an input genomic region or gene model", trigger = "focus")
            )
          ),
          
          
          textInput("regBB", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                        bsButton("q1", label="", icon=icon("question"), style="info", size="small")),
                    value = "chr7:29560705-29573051"),
          
          bsPopover("q1", "A genomic region can be determined by chromosome positions or gene locus. For example, chr7:29560705-29573051 or SoyZH13_01G186100.",
                    trigger = "focus"),

          shinyWidgets::multiInput("mychooserD", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
                                      bsButton("qdl2", label="", icon=icon("question"), style="info", size="small")),
                     choices = all.soya.cho,
                     selected = "G. Soja",
                     width = 800,
                     options = list(
                       enable_search = TRUE,
                       non_selected_header = "Choose from:",
                       selected_header = "You have selected:"
                     )
          ),
          bsPopover("qdl2", "Only the chosen soybean accessions will be used.",
                    trigger = "focus"),
          
          
          fluidRow(
            column(6, actionButton("snpsearchnone1", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            column(6, actionButton("snpsearchall1", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),

          ),
          br(),
          shinysky::actionButton("submit_SSNP", strong("Submit!",
                                                       bsButton("q12", label="", icon=icon("question"), style="info", size="small")
          ), width = "90%", styleclass = "success"),
          shinysky::actionButton("clearDOW", strong("Reset"), styleclass = "warning"),
          shinysky::actionButton("DOWExam", strong("Load example"), styleclass = "info"),
          bsPopover("q12", "Whenever the genomic region or any option is updated, please click Submit!",
                    trigger = "focus"),
          conditionalPanel(condition="input.submit_SSNP != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0))
          
        ),
        br(),
        mainPanel(
          width = 9,
          
          fluidRow(
            column(4, uiOutput("downloadSD01")),
            column(4, uiOutput("downloadSD02")),
            column(4, uiOutput("downloadSD03"))
            
            
            #column(4, downloadButton("bulkdownloadsnpInfo.txt", style = "width:100%;", "Download SNPs information", class = "buttDown")),
            #column(4, downloadButton("bulkdownloadsnp.txt", style = "width:100%;", "Download genotype data", class = "buttDown")),
            #column(4, downloadButton("bulkdownloadgene.txt", style = "width:100%;", "Download gene annotation", class = "buttDown")),
            #tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
          ),
          
          br(),
          DT::dataTableOutput("mytable2"),
          column(12, HTML(strrep(br(), 2))),
        )),
      
      # LDheatmap
      tabPanel(
        title = HTML("<strong style='font-size:20px'>LDheatmap</strong>"), icon = icon("fire"),
        
        sidebarPanel(
          tags$style("#regL {font-size:15px;font-family:sans-serif;}"),
          tags$style("#ldpos {font-size:15px;font-family:sans-serif;}"),
          width = 3,
          
          fixedRow(
            column(12,
                   tags$div(h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Linkage disequilibrium analysis</b></font>'),
                            bsButton("qlda", label="", icon=icon("question"), style="info", size="small"))),
                   bsPopover("qlda", " For a specified genomic region orgene model, a heat map could be created to display the pairwise linkage disequilibrium between different SNP sites", trigger = "focus")
            )
          ),
          
          textInput("regL", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                       bsButton("q5", label="", icon=icon("question"), style="info", size="small")),
                    value = "SoyZH13_01G002900"),
          
          bsPopover("q5", "A genomic region can be determined by chromosome positions or gene locus. For example, chr2:17603220-17604802 or SoyZH13_01G002900.",
                    trigger = "focus"),
          
          shinysky::actionButton("submitLD", strong("Submit!",
                                         bsButton("q8", label="", icon=icon("question"), style="info", size="small")
          ), styleclass = "success"),
          shinysky::actionButton("clearLD", strong("Reset"), styleclass = "warning"),
          shinysky::actionButton("LDExam", strong("Load example"), styleclass = "info"),
          conditionalPanel(condition="input.submit3 != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
          bsPopover("q8", "Whenever the genomic region is updated, please click Submit!",
                    trigger = "focus"),
          
          br(),
          
          h4(HTML('<i class="fa fa-cog"></i> <font size="5" color="red"><b>Plot options</b></font>')),
          
          radioButtons("flip", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Flip the figure</b></font>')), list( "TRUE" = 1, "FALSE" = 0)),
          
          conditionalPanel(
            condition = "input.flip==1",
            h4(checkboxInput("LDshowGene", label = p("Show gene model",
                                                   bsButton("qldg", label="", icon=icon("question"), style="info", size="small")), TRUE)),
            bsPopover("qldg", "Y:gene model height; W:gene model width",
                      trigger = "focus"),
            conditionalPanel(
              condition = "input.LDshowGene",
              fluidRow(
              column(6, numericInput("ldY", "Y:", value = 72)),
              column(6, numericInput("ldW", "W:", value = 72))
            ))
          ),
          
          conditionalPanel(
            condition = "input.flip==0",
            radioButtons("showText", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Print LD measurements</b></font>')), list("FALSE" =  0, "TRUE" = 1)),
            textInput("ldpos", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Label SNPs</b></font>')), value = "5, 8")
          ),
          
          radioButtons("ldcol",
                       label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Color</b></font>')), list("grey.colors(20)" = 1, "heat.colors(20)" = 2)
          ),
 
          sliderInput("ldUp", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (bp)</b></font>'),
                                 bsButton("ql4", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 0, ticks = FALSE),
          
          bsPopover("ql4", "Extend the input genomic region to its upstream.",
                    trigger = "focus"),
          
          sliderInput("ldDown", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                 bsButton("ql5", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 0, ticks = FALSE),
          
          bsPopover("ql5", "Extend the input genomic region to its downstream.",
                    trigger = "focus"),
          

          shinyWidgets::multiInput(
            "ld_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation effect</b></font>'),
                                       bsButton("ql1", label="", icon=icon("question"), style="info", size="small")),
            choices = mutationtypes,
            selected = mutationtypes,
            width = 800,
            options = list(
              enable_search = TRUE,
              non_selected_header = "Choose from:",
              selected_header = "You have selected:"
            )
          ),
          bsPopover("ql1", "Only SNPs with selected mutation effects will be used.",
                    trigger = "focus"),
          fluidRow(
            column(6, actionButton("ldnone2", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            column(6, actionButton("ldall2", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          ),
          
          shinyWidgets::multiInput("mychooserLD", p(h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
                                      bsButton("ql3", label="", icon=icon("question"), style="info", size="small"))),
                     choices = all.soya.cho,
                     selected = all.soya.cho,
                     width = 800,
                     options = list(
                       enable_search = TRUE,
                       non_selected_header = "Choose from:",
                       selected_header = "You have selected:"
                     )
          ), 
          
          bsPopover("ql3", "Only the chosen soybean accessions will be used.",
                    trigger = "focus"),
          
          fluidRow(
            column(6, actionButton("ldnone1", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            column(6, actionButton("ldall1", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          ),
          
          checkboxInput("ldSize", "Adjust plot size", FALSE),
          conditionalPanel(
            condition = "input.ldSize",
            numericInput("ldHeight", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 730),
            numericInput("ldWidth", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 1000)
          )
        ),
        
        mainPanel(
          width = 9,
          fluidRow(
            column(6, uiOutput("downloadLD01")),
            column(6, uiOutput("downloadLD02"))
            #column(6, downloadButton("downloadLD.pdf", style = "width:100%;", "Download pdf-file", class = "buttDown")),
            #column(6, downloadButton("downloadLD.svg", style = "width:100%;", "Download svg-file", class = "buttDown")),
            #tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
            ),

          plotOutput("ldheatmap", height = "800px", width = "100%")
          

        )
      ),
      
      
      # Nucleotide diversity
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Diversity</strong>"), icon = icon("hourglass"),
        
        sidebarPanel(
          tags$style("#regD {font-size:15px;font-family:sans-serif;}"),
          width = 3,
          fixedRow(
            column(12,
                   tags$div(h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Nucleotide diversity analysis</b></font>'),
                            bsButton("qnda", label="", icon=icon("question"), style="info", size="small"))),
                   bsPopover("qnda", "Calculate and demonstrate nucleotide diversities among subgroups of soybean accessions in specified genomic regions.", trigger = "focus")
            )
          ),
          
          textInput("regD", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                       bsButton("q3", label="", icon=icon("question"), style="info", size="small")),
                    value = "SoyZH13_12G067900"),
          
          bsPopover("q3", "A genomic region can be determined by chromosome positions or gene locus. For example, chr12:5609599-5630626 or SoyZH13_12G067900",
                    trigger = "focus"),
          
          shinysky::actionButton("submit4", strong("Submit!",
                                         bsButton("q10", label="", icon=icon("question"), style="info", size="small")
          ), styleclass = "success"),
          shinysky::actionButton("clearDIV", strong("Reset"), styleclass = "warning"),
          shinysky::actionButton("DIVExam", strong("Load example"), styleclass = "info"),
          conditionalPanel(condition="input.submit4 != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
          bsPopover("q10", "Whenever the genomic region or any plot option is updated, please click Submit!",
                    trigger = "focus"),
          
          br(),
          h4(HTML('<i class="fa fa-cog" aria-hidden="true"></i> <font size="5" color="red"><b>Plot options</b></font>')),
          
          numericInput("snpnumD", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Number of SNPs in each window</b></font>'),
                                     bsButton("qd6", label="", icon=icon("question"), style="info", size="small")
          ), value = 10, min = 5, max = 20),
          bsPopover("qd6", "A specified genomic region would be split into non-overlapping window so that each window contains specified number of SNPs. The nucleotide diversity of all soybean lines belong to the specified ecotypes in each window would be calculated.",
                    trigger = "focus"),

          shinyWidgets::pickerInput(
            inputId = "div_acc_group",
            label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Ecotypes to calculate diversity</b></font>')),
            choices = c("Improved cultivar", "Landrace", "G. Soja"),
            selected = c("Landrace", "G. Soja"),
            multiple = TRUE,
            options = list(style = "btn-primary")
          ),
          
          selectInput("nuc_numerator", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Numerator ecotype</b></font>'),
                                          bsButton("qd7", label="", icon=icon("question"), style="info", size="small")
          ), choices = c("G. Soja", "Improved cultivar", "Landrace")),
          bsPopover("qd7", "The nucleotide diversity of soybean lines belong to the Numerator ecotype would be divided by the nucleotide diversity of soybean lines belong to the Denominator ecotype for comparison.",
                    trigger = "focus"),
          selectInput("nuc_denominator", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Denominator ecotype</b></font>')), choices = 
                        c("Improved cultivar", "Landrace", "G. Soja")),
          
          tags$div(align = 'left',
                   class = 'multicol', style = "width: 100%",
                   shinyWidgets::multiInput("div_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation effect</b></font>'),
                                                  bsButton("qd1", label="", icon=icon("question"), style="info", size="small")),
                              choices = mutationtypes,
                              selected = mutationtypes,
                              width = 800,
                              options = list(
                                enable_search = TRUE,
                                non_selected_header = "Choose from:",
                                selected_header = "You have selected:"
                              )
                   ),
                   bsPopover("qd1", "Only SNPs with selected mutation effects will be used.",
                             trigger = "focus")
          ),
          fluidRow(
            column(6, actionButton("diversitynone2", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            column(6, actionButton("diversityldall2", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          ),
          
          
          sliderInput("divUp", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (bp)</b></font>'),
                                    bsButton("qd4", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 20000, ticks = FALSE),
          bsPopover("qd4", "Extend the input genomic region to its upstream.",
                    trigger = "focus"),
          
          sliderInput("divDown", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                    bsButton("qd5", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 20000, ticks = FALSE),
                    bsPopover("qd5", "Extend the input genomic region to its downstream",
                    trigger = "focus"),
          
          checkboxInput("divSize", "Adjust plot size", FALSE),
          conditionalPanel(
            condition = "input.divSize",
            numericInput("divHeight", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height:</b></font>'), value = 730),
            numericInput("divWidth", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width:</b></font>'), value = 1000)
          )
          
        ),
        
        mainPanel(
          width = 9,
          fluidRow(
            column(4, uiOutput("downloadDiv01")),
            column(4, uiOutput("downloadDiv02")),
            column(4, uiOutput("downloadDiv03"))
          ),
          
          # downloadButton("downloadDiv.pdf", "Download pdf-file"),
          # downloadButton("downloadDiv.svg", "Download svg-file"),
          # downloadButton("downloadDiv.txt", "Download TXT-file"),
          column(12, plotOutput("diversity", height = '800px', width = '100%'))
        )
        
      ),
      
      #AlleleFreq
      tabPanel(
        title = HTML("<strong style='font-size:20px'>AlleleFreq</strong>"), icon = icon("code-branch"),
        
        sidebarPanel(
          width = 3,
          fixedRow(
            column(12,
                   tags$div(h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Allele frequency analysis</b></font>'),
                               bsButton("qaf11", label="", icon=icon("question"), style="info", size="small"))),
                   bsPopover("qaf11", "Calculate and demonstrate allele frequency of input SNP sites.", trigger = "focus")
            )
          ),
          
          textAreaInput("af_snp_site", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Input SNP sites</b></font>'),
                                                  bsButton("qaf3", label="", icon=icon("question"), style="info", size="small")), 
                        width="100%", resize="vertical", height="150px", 
                        placeholder = "One SNP site in one row", 
                        value = "0133024709\n1403584545\n1403584761"
          ),
          bsPopover("qaf3", "Each SNP site should be a 10-digits integer and the first two digits represent the chromosome ID while the rest eight digits represent the genomic position of each SNP site. Each SNP site should take only one row!",
                    trigger = "focus"),
          
          
          shinyWidgets::pickerInput(
            inputId = "af_acc_group",
            label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Ecotypes to calculate allele frequency</b></font>')), 
            choices = c("Improved cultivar", "Landrace", "G. Soja"),
            selected = c("Improved cultivar", "Landrace", "G. Soja"),
            multiple = TRUE
          ),
          h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Allele colors</b></font>'),
                                bsButton("qaf2", label="", icon=icon("question"), style="info", size="small")),
          bsPopover("qaf2", "Colors for the major and minor allele in the pie chart respectively!", trigger = "focus"),
          
          fluidRow(
          column(6, RLumShiny::jscolorInput("jscolora1", h4(HTML('<i class="fa fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Major color</b></font>')), value = "#2E8FFF")),
          column(6, RLumShiny::jscolorInput("jscolora2", h4(HTML('<i class="fa fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Minor color</b></font>')), value = "#FFE74A"))
          ),

       
          numericInput("afHeight", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 650),
          numericInput("afWidth", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 800),
          
          shinysky::actionButton("submitaf1", strong("Submit!",
                                                     bsButton("qaf1", label="", icon=icon("question"), style="info", size="small")
          ), styleclass = "success"),
          shinysky::actionButton("clearAf", strong("Reset"), styleclass = "warning"),
          shinysky::actionButton("AfExam", strong("Load example"), styleclass = "info"),
          conditionalPanel(condition="input.submitaf1 != '0'", shinysky::busyIndicator(HTML("<p style='color:red;font-size:30px;'>Calculation In progress...</p>"), wait = 0)),
          bsPopover("qaf1", "Whenever the input SNP sites or any option is updated, please click Submit!",
                    trigger = "focus")
        ),
        
        mainPanel(
          width = 9,
          fluidRow(
            column(4, uiOutput("downloadAfq01")),
            column(4, uiOutput("downloadAfq02"))
          ),
          br(),
          br(),
          plotOutput("alleleFreq", height = "900px", width = "100%")
        )
        
      ),

      
      # Phylogenetic tree
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Phylogenetic</strong>"), icon = icon("cloud"),
        
        sidebarPanel(
          tags$style("#regP {font-size:15px;font-family:sans-serif;}"),
          width = 3,
          fixedRow(
            column(12,
                   h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Phylogenetic analysis</b></font>'),
                            bsButton("qpa", label="", icon=icon("question"), style="info", size="small") ),
                   bsPopover("qpa", " For a specified genomic region or gene model, a neighbor-joining (NJ) tree could be built based on a pairwise distance matrix derived from the simple matching distance for all SNP sites in this region.", trigger = "focus")
            )
          ),
          
          textInput("regP", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                       bsButton("q2", label="", icon=icon("question"), style="info", size="small")),
                    value = "SoyZH13_02G148200"),
          
          bsPopover("q2", "A genomic region can be determined by chromosome positions or gene locus. For example, chr2:17603220-17604802 or SoyZH13_02G148200",
                    trigger = "focus"),
          shinysky::actionButton("submit5", strong("Submit!",
                                         bsButton("q11", label="", icon=icon("question"), style="info", size="small")
          ), styleclass = "success"),
          shinysky::actionButton("clearPHY", strong("Reset"), styleclass = "warning"),
          shinysky::actionButton("PHYExam", strong("Load example"), styleclass = "info"),
          conditionalPanel(condition="input.submit5 != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
          bsPopover("q11", "Whenever the genomic region or any option is updated, please click Submit!",
                    trigger = "focus")
          ,
          br(),
          h4(HTML('<i class="fa fa-cog" aria-hidden="true"></i> <font size="5" color="red"><b>Plot options</b></font>')),
          
          sliderInput("phyUp", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (bp)</b></font>'),
                                 bsButton("qp4", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 2000, ticks = FALSE),
          bsPopover("qp4", "Extend the input genomic region to its upstream.",
                    trigger = "focus"),
          
          sliderInput("phyDown", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                 bsButton("qp5", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 2000, ticks = FALSE),
          bsPopover("qp5", "Extend the input genomic region to its downstream.",
                    trigger = "focus"),
          
          shinyWidgets::multiInput("mychooserPhy", p(h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
                                                        bsButton("qp3", label="", icon=icon("question"), style="info", size="small"))),
                                   choices = all.soya.cho,
                                   selected = all.soya.cho,
                                   width = 800,
                                   options = list(
                                     enable_search = TRUE,
                                     non_selected_header = "Choose from:",
                                     selected_header = "You have selected:"
                                   )
          ), 
          bsPopover("qp3", "Only the chosen soybean accessions will be used.",
                    trigger = "focus"),
          
          fluidRow(
            column(6, actionButton("phynone1", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            column(6, actionButton("phyall1", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          ),
          
          tags$div(align = 'left',
                   class = 'multicol', style = "width: 100%",
                   shinyWidgets::multiInput("phy_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation effects</b></font>'),
                                                  bsButton("qp1", label="", icon=icon("question"), style="info", size="small")),
                              choices = mutationtypes,
                              selected = mutationtypes,
                              width = 800,
                              options = list(
                                enable_search = TRUE,
                                non_selected_header = "Choose from:",
                                selected_header = "You have selected:"
                              )
                   ),
                   bsPopover("qp1", "Only SNPs with selected mutation effects will be used.",
                             trigger = "focus")
          ),
          
          fluidRow(
            column(6, actionButton("phynone2", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            column(6, actionButton("phyall2", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          ),
          
          checkboxInput("phySize", "Adjust plot size", FALSE),
          conditionalPanel(
            condition = "input.phySize",
            numericInput("phyHeight", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height:</b></font>'), value = 800),
            numericInput("phyWidth", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width:</b></font>'), value = 1000)
          )),      
          
          mainPanel(
            width = 9,
            fluidRow(
              column(5, uiOutput("downloadPhy01")),
              column(5, uiOutput("downloadPhy02"))
            ),
            
            # downloadButton("downloadPhylo.pdf", "Download pdf-file"),
            # downloadButton("downloadPhylo.nwk", "Download Newick-file"),
            shiny::uiOutput("phylo")
            
          )
        
      )
      
      ),

      #Indel
      
      tabPanel(
        title = HTML("<strong style='font-size:20px'>INDELs</strong>"),
        
        sidebarPanel(
          tags$style("#regi {font-size:15px;font-family:sans-serif;}"),
          width = 3,
          fixedRow(
            column(12,
                   h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Search INDELs</b></font>'),
                            bsButton("qii", label="", icon=icon("question"), style="info", size="small")),
                   bsPopover("qii", "Search INDELs among 2898 accessions in an input genomic region or gene model", trigger = "focus")
            )
          ),
          
          textInput("regi", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                       bsButton("qi1", label="", icon=icon("question"), style="info", size="small")),
                    value = "SoyZH13_01G186100"),
          
          bsPopover("qi1", "A genomic region can be determined by chromosome positions or gene locus. For example, chr1:29506705-29659223 or SoyZH13_01G186100.",
                    trigger = "focus"),
          

          shinyWidgets::multiInput("mychooseri", p(h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
                                          bsButton("qindel2", label="", icon=icon("question"), style="info", size="small"))),
                     choices = all.soya.cho,
                     selected = c("G. Soja"),
                     width = 800,
                     options = list(
                       enable_search = TRUE,
                       non_selected_header = "Choose from:",
                       selected_header = "You have selected:"
                     )
          ), 
          
          bsPopover("qindel2", "Only the chosen soybean accessions will be used.", trigger = "focus"),
          fluidRow(
            column(6, actionButton("indelnone1", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            column(6, actionButton("indelall1", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
          ),
          br(),
          shinysky::actionButton("submiti", strong("Submit!",
                                                   bsButton("qi2", label="", icon=icon("question"), style="info", size="small")
          ), width = "90%", styleclass = "success"),
          
          shinysky::actionButton("clearINDEL", strong("Reset"), styleclass = "warning"),
          shinysky::actionButton("INDELExam", strong("Load example"), styleclass = "info"),
          conditionalPanel(condition="input.submiti != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
          bsPopover("qi2", "Whenever the genomic region or any option is updated, please click Submit!",
                    trigger = "focus")
        ),
        
        mainPanel(
          width = 9,
          column(12,
          column(6, uiOutput("downloadindel01")),
          column(6, uiOutput("downloadindel02"))     
          
          #downloadButton("bulkdownloadindelInfo.txt", style = "width:90%;center", "Download indels information", class = "buttDown"),
          #downloadButton("bulkdownloadindelALLInfo.txt", style = "width:90%;center", "Download All indels information", class = "buttDown"),
          #tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
          ),
          htmlOutput("indeltabletitle"),
          DT::dataTableOutput("indeltable"),
          column(12, HTML(strrep(br(), 2))),
          tags$head(tags$style("#indeltabletitle{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                      }"
          )
          )
        )
      ),
      
      
      #Search sequence, cds, pro and gff information
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>Genomes</strong>"),
        tabPanel(
          
          title = HTML("<strong style='font-size:20px'>Search by gene ID</strong>"), icon = icon("id-card"),
          sidebarPanel(
            tags$style("#geneid {font-size:15px;font-family:sans-serif;}"),
            tags$style("#geneinterval {font-size:15px;font-family:sans-serif;}"),
            width = 3,
            fixedRow(
              column(12,
                     h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Search by gene ID</b></font>'),
                              bsButton("qsgsid", label="", icon=icon("question"), style="info", size="small")),
                     bsPopover("qsgsid", "Search any one of the 29 soybean genomes by gene ID", trigger = "focus")
              )
            ),
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("variety_ID", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select genome</b></font>')), 
                        list(`G. soja` = list("PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar` = list("Zhonghuang 13", "Williams 82", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                        "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                        "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1"),
                             `Landrace` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                               "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye")) ,
                        selected = "Zhonghuang 13"
            ),
            
            textInput("geneid", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Gene ID</b></font>')),
                      value = "SoyZH13_02G148200"),
            
            shinysky::actionButton("submit_GSID", strong("Submit!",
                                               bsButton("qsgs2", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            
            shinysky::actionButton("clearSERID", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("SERExamID", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="input.submit_GSID != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qsgs2", "Whenever the genomic region or any option is updated, please click Submit!",
                      trigger = "focus")
          ),
          mainPanel(
            width = 9,
            fluidRow(
              column(3, uiOutput("downloadgenesid01")),
              column(3, uiOutput("downloadgenesid02")), 
              column(3, uiOutput("downloadgenesid03")),
              column(3, uiOutput("downloadgenesid04"))
              #column(3, downloadButton("genesequence_ID.txt", "Download Gene Sequence", style = "width:100%;", class = "buttDown")),
              #column(3, downloadButton("cdssequence_ID.txt", "Download CDS Sequence", style = "width:100%;", class = "buttDown")),
              #column(3, downloadButton("cdnasequence_ID.txt", "Download cDNA Sequence", style = "width:100%;", class = "buttDown")),
              #column(3, downloadButton("prosequence_ID.txt", "Download Protein Sequence", style = "width:100%;", class = "buttDown")),
              #tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
            ),
            
            htmlOutput("geneticIDstructure_title"),
            uiOutput("geneticIDstructureif"),
            #plotOutput("geneticIDstructure", width = "100%", height = "100px", click = "plot_click"),
            tags$head(tags$style("#geneticIDstructure_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                       width = 100%;
                                      }"
            ),
            tags$style("#geneticIDstructureif{
                     width = 100%;
                     
          }")
            ),
            
            htmlOutput("seq_title"),
            verbatimTextOutput("seq"),
            tags$head(tags$style("#seq_title{color: red;
                                       font-size: 18px;
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

            
            htmlOutput("cds_title"),
            verbatimTextOutput("cds"),
            tags$head(tags$style("#cds_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#cds{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
          }")
            ),
            htmlOutput("cdna_title"),
            verbatimTextOutput("cdna"),
            tags$head(tags$style("#cdna_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#cdna{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
          }")
            ),
            htmlOutput("pro_title"),
            shiny::verbatimTextOutput("pro"),
            tags$head(tags$style("#pro_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#pro{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
                     
          }")
            ),
            
            htmlOutput("gffinfotitle"),
            DT::dataTableOutput("gffinfo"),
            column(12, HTML(strrep(br(), 2))),
            tags$head(tags$style("#gffinfotitle{color: red;
                                       font-style: bold;
                                       font-size: 18px;
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
          
          title = HTML("<strong style='font-size:20px'>Search by genome location</strong>"), icon = icon("search-location"),
          
          sidebarPanel(
            width = 3,
            fixedRow(
              column(12,
                     h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Search by genome location</b></font>'),
                              bsButton("qsgsit", label="", icon=icon("question"), style="info", size="small")),
                     bsPopover("qsgsit", "Search any one of the 29 soybean genomes by genome location", trigger = "focus")
              )
            ),
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("variety_IT", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select genome</b></font>')), 
                        list(`G. soja` = list("PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar` = list("Zhonghuang 13", "Williams 82", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                        "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                        "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1"),
                             `Landrace` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                               "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye")) ,
                        selected = "Zhonghuang 13"
            ),
            
            textInput("geneinterval", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>')),
                      value = "chr1:20260371-20686979"),
            
            shinysky::actionButton("submit_GSIT", strong("Submit!",
                                               bsButton("qsit2", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            
            shinysky::actionButton("clearSERIT", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("SERExamIT", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="input.submit_GSIT != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qsit2", "Whenever the genomic region or any option is updated, please click Submit!",
                      trigger = "focus")
            
          ),
          mainPanel(
            width = 9,
            fluidRow(
              column(3, uiOutput("downloadgenesit01")),
              column(3, uiOutput("downloadgenesit02")), 
              column(3, uiOutput("downloadgenesit03")),
              column(3, uiOutput("downloadgenesit04"))
              
              #column(3, downloadButton("sequence_IT.txt", "Download Interval Sequence", style = "width:100%;", class = "buttDown")),
              #column(3, downloadButton("cdssequence_IT.txt", "Download CDS Sequence", style = "width:100%;", class = "buttDown")),
              #column(3, downloadButton("cdnasequence_IT.txt", "Download cDNA Sequence", style = "width:100%;", class = "buttDown")),
              #column(3, downloadButton("prosequence_IT.txt", "Download Protein Sequence", style = "width:100%;", class = "buttDown"))
            ),

            htmlOutput("geneinfo_title"),
            column(12, DT::dataTableOutput("geneinfo")),
            tags$head(tags$style("#geneinfo_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
              tags$style("#geneinfo{
                     width = 100%;
                     max-height: 300px;
          }")
            ),
            
            htmlOutput("geneticITstructure_title"),
            uiOutput("geneticITstructureif"),
            #plotOutput("geneticITstructure", width = "100%", height = "100px"),
            
            tags$head(tags$style("#geneticITstructure_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            )
            
            ),
            
            htmlOutput("gene_title_it"),
            verbatimTextOutput("gene_it"),
            tags$head(tags$style("#gene_title_it{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#gene_it{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
          }")
            ),

            htmlOutput("cds_title_it"),
            verbatimTextOutput("cds_it"),
            tags$head(tags$style("#cds_title_it{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#cds_it{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
          }")
            ),
            htmlOutput("cdna_title_it"),
            verbatimTextOutput("cdna_it"),
            tags$head(tags$style("#cdna_title_it{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#cdna_it{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
          }")
            ),
            htmlOutput("pro_title_it"),
            shiny::verbatimTextOutput("pro_it"),
            tags$head(tags$style("#pro_title_it{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#pro_it{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
                     
          }")
            ),
            
            htmlOutput("gffinfotitle_it"),
            column(12,
                   DT::dataTableOutput("gffinfo_it")),
            tags$head(tags$style("#gffinfotitle_it{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#gffinfo_it{
                     width = 100%;
                     max-height: 300px;
          }")
            ),
            htmlOutput("repeatmasker_title_it"),
            column(12, DT::dataTableOutput("repeatmasker")),
            column(12, HTML(strrep(br(), 2))),
            tags$head(tags$style("#repeatmasker_title_it{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                          }"
            ),
            tags$style("#repeatmasker{
                     width = 100%;
                     max-height: 300px;
          }")
            )
          )
        )
      ),
      
      
      #TOOLS
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>Tools</strong>"), icon = icon("toolbox", lib = "font-awesome"),
      
      #Blast
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Blast</strong>"),
        icon = icon("rocket", class = NULL, lib = "font-awesome"),
        
        h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Search one or multiple soybean genomes by sequence similarity using BLAST</b></font>'),
                 bsButton("qBlastTitle", label="", icon=icon("question"), style="info", size="small")),
        bsPopover("qBlastTitle", title = Blast_Info_Title, content = NULL, trigger = "focus"),
        
        tabsetPanel(id = "BLAST_tab",
                    tabPanel("Input",
                             fixedRow(
                               column(5,
                                      selectInput("In_blast", label = tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Paste or upload input data?</font>'),
                                                                               bsButton("qBlastIn", label="", icon=icon("question"), style="info", size="small")
                                      ), choices = list("Paste input data" = "paste", 
                                                        "Upload input data" = "upload"), 
                                      selected = "paste"),
                                      bsPopover("qBlastIn", "The input data must be DNA sequence in fasta format.", trigger = "focus"),
                                      
                                      conditionalPanel(condition="input.In_blast == 'paste'", 
                                                       textAreaInput("BlastSeqPaste", label = HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Input sequence</font>'),
                                                                     value = "", resize = "vertical", height='400px', width = '200%',
                                                                     placeholder = "The sequence must be in fasta format")
                                      ),
                                      conditionalPanel(condition="input.In_blast == 'upload'", 
                                                       fileInput("BlastSeqUpload",
                                                                 label = h4("Upload file"), multiple = FALSE, width = "100%"),
                                      downloadButton("BLAST_Input.txt", "Example BLAST input data", style = "width:100%;", class = "buttDown")
                                      )
                               ),
                               column(4,
                                      shinyWidgets::multiInput(
                                        inputId = "BLASTdb",
                                        label = tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Choose BLAST databases</font>'),
                                                         bsButton("qblastDB", label="", icon=icon("question"), style="info", size="small")
                                        ),
                                        choices = c("Zhonghuang 13","PI 562565","PI 549046","PI 578357","W05",
                                                    "Zhutwinning2","Zi Hua No.4","Tong Shan Tian E Dan","58-161","PI 398296",
                                                    "Zhang Chun Man Cang Jin","Feng Di Huang","Tie Jia Si Li Huang","Shi Sheng Chang Ye",
                                                    "Williams 82","Xu Dou No.1","Tie Feng No.18","Ju Xuan No.23","Wan Dou No.28","Amsoy","Yu Dou No.22","Jin Dou No.23",
                                                    "Qi Huang No.34","Han Dou No.5","PI 548362","Ji Dou No.17","Dong Nong No.50","Hei He No.43","Ke Shan No.1"), 
                                        width = '100%',
                                        selected = NULL,
                                        options = list(
                                          enable_search = TRUE,
                                          non_selected_header = "Choose from:",
                                          selected_header = "You have selected:"
                                        )
                                      ),
                                      bsPopover("qblastDB", "Choose one or multiple BLAST databases to search against.",
                                                trigger = "focus")
                               ),
                               column(3,
                                      selectInput("program_database", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">Database type</font>'),
                                                                              bsButton("qBLASTpdata", label="", icon=icon("question"), style="info", size="small")),
                                                  choices=c("DNA database", "Protein database"), width = NULL),
                                      bsPopover("qBLASTpdata", "Set the type of database to search against.", 
                                                trigger = "focus"),
                                      textInput("BLASTev", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">E-value cutoff</font>'),
                                                                            bsButton("qBLASTev", label="", icon=icon("question"), style="info", size="small")), 
                                                value = "10", width = NULL, placeholder = NULL),
                                      bsPopover("qBLASTev", "Set E-value threshold to filter the BLAST output.",
                                                trigger = "focus"),
                                      
                                      conditionalPanel(condition="input.program_database == 'DNA database'", 
                                                       selectInput("programdna", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">Program</font>')), 
                                                                   choices=c("blastn","tblastn", "tblastx"), width = NULL),
                                      ),
                                      conditionalPanel(condition="input.program_database == 'Protein database'", 
                                                       selectInput("programpro", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">Program</font>')), 
                                                                   choices=c("blastp", "blastx"), width = NULL),
                                      ),
                    
                                      br(),
                                      
                                      shinysky::actionButton("submitBLAST", strong("BLAST!",
                                                                         bsButton("qBLASTGO", label="", icon=icon("question"), style="info", size="small")
                                      ), styleclass = "success"),
                                      shinysky::actionButton("clear3", strong("Reset"), styleclass = "warning"),
                                      shinysky::actionButton("blastExam", strong("Load example"), styleclass = "info"),
                                      conditionalPanel(condition="input.submitBLAST != '0'", shinysky::busyIndicator(HTML("<p style='color:red;font-size:30px;'>Calculation In progress...</p>"), wait = 0)),
                                      bsPopover("qBLASTGO", "Click this button to start the BLAST alignment!",
                                                trigger = "focus"),
                                      
                               )
                             )
                    ),
                    tabPanel("Output",
                             conditionalPanel(condition="input.submitBLAST > 0", 
                                              tags$div(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>BLAST output</b></font>'),
                                                       bsButton("qBLASTresultHelp", label="", icon=icon("question"), style="info", size="small")),
                                              bsPopover("qBLASTresultHelp", title = "Click on a row to check the details of the BLAST alignment!", trigger = "focus", content = NULL)
                             ),
                             
                             DT::dataTableOutput("BLASTresult"),
                             fixedRow(
                               column(1),
                               column(4,
                                      htmlOutput("Alignment"),
                                      tableOutput("clicked"),
                                      br(),
                                      br(),
                                      tags$head(tags$style("#Alignment{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                      }"
                                      )),
                                      tags$style("#clicked{
                                       width = 100%;
                                       padding: 6px 12px;
                                       white-space: pre-wrap;
                                      height: 300px;
                                      }"
                                      ),
                                      ),
                               column(7,
                                      br(),
                                      br(),
                                      tags$head(tags$style("#geneseq{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                      }"
                                      )),
                                      tags$style("#alignment{
                                       max-width = 100%;
                                       white-space: pre-wrap;
                                       max-height: 400px;
                                      }"
                                      ),
                                      verbatimTextOutput("alignment", placeholder = FALSE),
                                      br(),
                                      br()
                               )
                             )
                    )
                    
        )
      ),
      
      ##primer3
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Primer-Design</strong>"),icon = icon("drafting-compass"),
        
        sidebarPanel(
          tags$style("#upprimer {font-size:15px;font-family:sans-serif;}"),
          tags$style("#downprimer {font-size:15px;font-family:sans-serif;}"),
          tags$style("#PRIMER_OPT_SIZE {font-size:15px;font-family:sans-serif;}"),
          tags$style("#PRIMER_OPT_TM {font-size:15px;font-family:sans-serif;}"),
          tags$style("#PRIMER_MIN_TM {font-size:15px;font-family:sans-serif;}"),
          tags$style("#PRIMER_MAX_TM {font-size:15px;font-family:sans-serif;}"),
          tags$style("#PRIMER_OPT_GC_PERCENT {font-size:15px;font-family:sans-serif;}"),
          tags$style("#PRIMER_MAX_NS_ACCEPTED {font-size:15px;font-family:sans-serif;}"),
          tags$style("#PRIMER_MAX_POLY_X {font-size:15px;font-family:sans-serif;}"),
          tags$style("#PRIMER_INTERNAL_MAX_POLY_X {font-size:15px;font-family:sans-serif;}"),
          tags$style("#PRIMER_MAX_SELF_ANY {font-size:15px;font-family:sans-serif;}"),
          tags$style("#PRIMER_MAX_SELF_ANY_TH {font-size:15px;font-family:sans-serif;}"),
          tags$style("#PRIMER_MAX_SELF_END {font-size:15px;font-family:sans-serif;}"),
          tags$style("#PRIMER_MAX_SELF_END_TH {font-size:15px;font-family:sans-serif;}"),
          tags$style("#PRIMER_PRODUCT_SIZE_RANGE {font-size:15px;font-family:sans-serif;}"),
    
          fixedRow(
            column(12,
                   h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Desigin primer</b></font>'),
                            bsButton("qdp", label="", icon=icon("question"), style="info", size="small")),
                   bsPopover("qdp", "Design PCR primers for an input genomic region targeting SNPs and INDELs in this region.", trigger = "focus")
            )
          ),
          
          h4(HTML('<i class="fa fa fa-circle"></i> <font size="4" color="black"><b>Parameter settings</b></font>')),
          fluidRow(
            column(4, selectInput("Chrprimer", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Chr</b></font>')),
                                  choices = paste0("Chr", 1:20), selected = "Chr1")),
            column(4, textInput("upprimer", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Start</b></font>')),
                                value = "48000")),
            column(4, textInput("downprimer", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>End</b></font>')),
                                value = "49000"))
          ),
          
          textInput("PRIMER_OPT_SIZE", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Optimal primer size (bp)</b></font>'),
                                                  bsButton("qpr1", label="", icon=icon("question"), style="info", size="small")),value = "20"),
          bsPopover("qpr1", "Optimum length (in bases) of a primer. Primer3 will attempt to pick primers close to this length.",
                    trigger = "focus"),
          
          sliderInput("PRIMER_SIZE", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Primer size range (bp)</b></font>')
                                                , bsButton("qpr2", label="", icon=icon("question"), style="info", size="small")), min = 1, max = 35, 
                      value = c(18, 25)),
          bsPopover("qpr2", "Minimum acceptable length  and Maximum acceptable length (in bases) of a primer", trigger = "focus"),    
          
          textInput("PRIMER_OPT_TM", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Optimal primer TM (℃)</b></font>')
                                                , bsButton("qpr3", label = "", icon = icon("question"), style = "info", size = "small")),value = "59.0"),
          bsPopover("qpr3", "Optimum melting temperature (Celsius) for a primer.", trigger = "focus"),    
          
          fluidRow(
            column(6,textInput("PRIMER_MIN_TM", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Min primer TM (℃)</b></font>')
                                                           , bsButton("qpr4", label = "", icon = icon("question"), style = "info", size = "small")),value = "57.0"),
                   bsPopover("qpr4", "Minimum acceptable melting temperature (Celsius) for a primer oligo.", trigger = "focus")),
            column(6,textInput("PRIMER_MAX_TM", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max primer TM (℃)</b></font>')
                                                           , bsButton("qpr5", label = "", icon = icon("question"), style = "info", size = "small")),value = "62.0"),
                   bsPopover("qpr5", "Maximum acceptable melting temperature (Celsius) for a primer oligo.", trigger = "focus"))
          ),
          
          
          textInput("PRIMER_OPT_GC_PERCENT", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Optimal GC percent:</b></font>')
                                                        , bsButton("qpr6", label = "", icon = icon("question"), style = "info", size = "small")),value = "50.0"),
          bsPopover("qpr6", "Optimum GC percent.", trigger = "focus"),
          
          sliderInput("PRIMER_GC", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>GC percent range (bp)</b></font>')
                                              , bsButton("qpr7", label = "", icon = icon("question"), style = "info", size = "small")), min = 0, max = 100, 
                      value = c(20, 80), step=0.1),
          bsPopover("qpr7", "Minimum allowable percentage of Gs and Cs in any primer and Maximum allowable percentage of Gs and Cs in any primer generated by Primer.", trigger = "focus"),
          
          textInput("PRIMER_MAX_NS_ACCEPTED", label = h4(HTML("<i class='fa fa fa-circle' aria-hidden='true'></i> <font size='4' color='red'><b>Max #N's accepted</b></font>")
                                                         , bsButton("qpr8", label = "", icon = icon("question"), style = "info", size = "small")),value = "0"),
          bsPopover("qpr8", "Maximum number of unknown bases (N) allowable in any primer.", trigger = "focus"),
          
          
          fluidRow(
            column(6,textInput("PRIMER_MAX_POLY_X", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max Poly-X:</b></font>')
                                                               , bsButton("qpr9", label = "", icon = icon("question"), style = "info", size = "small")),value = "10"),
                   bsPopover("qpr9", "The maximum allowable length of a mononucleotide repeat, for example AAAAAA.", trigger = "focus")),
            column(6,textInput("PRIMER_INTERNAL_MAX_POLY_X", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max Internal Poly-X:</b></font>')
                                                                        , bsButton("qpr10", label = "", icon = icon("question"), style = "info", size = "small")),value = "15"),
                   bsPopover("qpr10", "Equivalent parameter of PRIMER_MAX_POLY_X for the internal oligo.", trigger = "focus"))
          ),
          fluidRow(
            column(6,textInput("PRIMER_MAX_SELF_ANY", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max Self Complementarity</b></font>')
                                                                 , bsButton("qpr11", label = "", icon = icon("question"), style = "info", size = "small")), value = "45.0"),
                   bsPopover("qpr11", "It is the maximum allowable local alignment score when testing a single primer for (local) self-complementarity.", trigger = "focus")),
            
            column(6,textInput("PRIMER_MAX_SELF_ANY_TH", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max Pair Complementarity</b></font>')
                                                                    , bsButton("qpr12", label = "", icon = icon("question"), style = "info", size = "small")), value = "45.0"),
                   bsPopover("qpr12", "The maximum primer pair complementarity.", trigger = "focus"))
          ),
          fluidRow(
            column(6,textInput("PRIMER_MAX_SELF_END", label = h4(HTML("<i class='fa fa fa-circle' aria-hidden='true'></i> <font size='3' color='red'><b>Max 3' Self Complementarity</b></font>")
                                                                 , bsButton("qpr13", label = "", icon = icon("question"), style = "info", size = "small")), value = "35.0")
                   , bsPopover("qpr13", "The maximum allowable 3'-anchored global.", trigger = "focus")),
            column(6,textInput("PRIMER_MAX_SELF_END_TH", label = h4(HTML("<i class='fa fa fa-circle' aria-hidden='true'></i> <font size='3' color='red'><b>Max 3' Pair Complementarity</b></font>")
                                                                    , bsButton("qpr14", label = "", icon = icon("question"), style = "info", size = "small")), value = "35.0")
                   , bsPopover("qpr14", "Same as PRIMER_MAX_SELF_END but is based on thermodynamical approach - the stability of structure is analyzed. The value of tag is expressed as melting temperature. ", trigger = "focus"))
          ),
          textInput("PRIMER_PRODUCT_SIZE_RANGE", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Product Size Ranges</b></font>')
                                                                    , bsButton("qpr15", label = "", icon = icon("question"), style = "info", size = "small")), value = "100-300,300-600,600-1000")
                    , bsPopover("qpr15", "If one desires PCR products in either the range from 100 to 300 bases or in the range from 300 to 600 (or 600 to 1000) bases then one would set this parameter to 100-300,300-600,600-1000.", trigger = "focus"),
          
          shinysky::actionButton("submitprimer", strong("submit!",
                                              bsButton("qprGO", label="", icon=icon("question"), style="info", size="small")
          ), styleclass = "success"),
          conditionalPanel(condition="submitprimer != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
          bsPopover("qprGO", "Whenever t he genomic region or any option is updated, please click Go!",
                    trigger = "focus")
          
        ),
        mainPanel(
          tags$style(HTML('#primertable tr:nth-child(4n+1) {background-color: skyblue !important;}')),
          tags$style(HTML('#primertable tr:nth-child(4n+2) {background-color: skyblue !important;}')),
          tags$style(HTML('#primertable tr:nth-child(4n+3) {background-color: pink !important;}')),
          tags$style(HTML('#primertable tr:nth-child(4n+0) {background-color: pink !important;}')),
          tags$style(HTML('#primertable th {background-color: grey !important;}')),
          column(12, DT::dataTableOutput("primertable")),
          htmlOutput("primerview"),
          column(12, verbatimTextOutput("primerseq")),
            tags$head(tags$style("#primerview{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#primertable{
                                   width = 100%;
                                   white-space: pre-wrap;
                                      }"
            ),
            tags$style("#primerseq{
                     width = 100%;
                     white-space: pre-wrap;
          
          }")
            ),

        )
      ),
      
      
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Orthologous</strong>"),icon = icon("tree"),
        
        sidebarPanel(
          fixedRow(
            column(12,
                   h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Orthologous Groups Search</b></font>'),
                            bsButton("qor1", label="", icon=icon("question"), style="info", size="small")),
                   bsPopover("qor1", "Orthologous Groups among 29 soybean genomes.", trigger = "focus")
            )
          ),
          textInput("ORT_ID", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Input gene model</b></font>'),
                                                  bsButton("qor2", label="", icon=icon("question"), style="info", size="small")),value = "SoyZH13_01G225600"),
          bsPopover("qor2", "Input gene model of any of the 29 soybean genomes.",
                    trigger = "focus"),
          shinysky::actionButton("ORT_sbumit", strong("Submit!"), styleclass = "success"),
          shinysky::actionButton("clearort", strong("Reset"), styleclass = "warning"),
          shinysky::actionButton("ortExam", strong("Load example"), styleclass = "info"),
          conditionalPanel(condition="ORT_sbumit != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0))
        ),
        mainPanel(
          textOutput("ortIDtitle"),
          DT::dataTableOutput("ortID") 
        )
      )
      
      
    ),

      tabPanel(
        title = HTML("<strong style='font-size:20px'>JBrowse</strong>"), icon = icon("window-restore"),
        h4(HTML('<i class="fas fa-dot-circle"></i> <font size="5" color="red"><b>JBrowse of 29 soybean genomes</b></font>'),
           bsButton("qjb1", label="", icon=icon("question"), style="info", size="small")),
        bsPopover("qjb1", "Click the name of a soybean genome to view the JBrowse", trigger = "focus"),
        DT::dataTableOutput('JBrowsetable')
        
      ),
    
    # Accession
    tabPanel(
      title = HTML("<strong style='font-size:20px'>Accession</strong>"), icon = icon("clipboard"),
      
      sidebarPanel(
        width = 3,
        shinyWidgets::multiInput("mychooserA", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
                                 bsButton("qa1", label="", icon=icon("question"), style="info", size="small")),
                                 choices = all.soya.cho,
                                 selected = c("Landrace"),
                                 width = 800,
                                 options = list(
                                   enable_search = TRUE,
                                   non_selected_header = "Choose from:",
                                   selected_header = "You have selected:"
                                 )
        ),
        
        bsPopover("qa1", "Only the chosen soybean accessions will be displayed.",
                  trigger = "focus"),
        fluidRow(
          column(6, actionButton("accessionnone1", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
          column(6, actionButton("accessionall1", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        )
      ),
      
      mainPanel(
        width = 9,
        fluidRow(

        column(6, downloadButton("soya.info.txt", style = "width:100%;", "Download information of all accessions", class = "buttDown")),
        column(6, downloadButton("sel.soya.info.txt", style = "width:100%;", "Download information of selected accessions", class = "buttDown")),
        tags$style(".buttDown{background-color:grey; color: white; font-size: 16px;}")
               ),
        HTML('<i class="fa fa-circle" aria-hidden="true" style="color:red"></i> <font size="5" color="red"><b>Information of selected soybean accessions</b></font>'),
        DT::dataTableOutput("mytable1")
      )
    ),
      
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>Help</strong>"), icon = icon("book"),
      
      ## About
      tabPanel(
        HTML("<strong style='font-size:20px'>About</strong>"),
        column(2),
        column(8, includeMarkdown("./About.md")),
        column(2)
        ),
      ##Link
      tabPanel(HTML("<strong style='font-size:20px'>Links</strong>"),
               column(2),
               column(8, includeMarkdown("./Links.md")),
               column(2)
      ),
      #29soybeans
      tabPanel(HTML("<strong style='font-size:20px'>Download</strong>"),
               column(2),
               column(8, includeMarkdown("Download.md")),
               column(2)
      )
      
      )
  )
 )
)

