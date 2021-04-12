library(shinyBS)


shinyUI(
  fluidPage(
    disconnectMessage(
      text = "Your session timed out, reload the application!",
      refresh = "Reload now",
      background = "#f89f43",
      colour = "white",
      overlayColour = "grey",
      overlayOpacity = 0.75,
      top = 250,
      refreshColour = "brown"
    ),
    #tags$style(type='text/css', ".selectize-input { font-size: 18px; line-height: 18px;} .selectize-dropdown { font-size: 18px; line-height: 18px; }"),
    #tags$style(type='text/css', ".irs-grid-text { font-size: 18pt; }"),
 
    navbarPage(
        header = "",
        title = HTML("<strong style='color:#000000'>SoybeanGDB</strong>"),
        windowTitle = "SNP database of 2898 soybean lines",
        id = "The_page",
      #home
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Home</strong>"), icon = icon("home"), Homepage),
      
      
      
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>SNP</strong>"),
        
      # Genome browse
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Browse</strong>"), icon = icon("window-maximize"),
        
        sidebarPanel(
          tags$div(HTML('<i class="fa fa-star" aria-hidden="true"></i> <font size="5" color="red"><b>Genome browse visualization</b></font>'),
                   bsButton("qgbv", label="", icon=icon("question"), style="info", size="small")),
          bsPopover("qgbv", "For a specified genomic region or gene model, all the SNPs among user-selected soybean accessions were extracted and subjected to genome browser visualization", trigger = "focus"),
          div(
            class="test_type",
          textInput("regB", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                       bsButton("q6", label="", icon=icon("question"), style="info", size="small")),
                    value = "chr1:29765419-29793053")
          ),
          
          bsPopover("q6", "A genomic region can be determined by chromosome positions or gene locus. For example, chr1:29765419-29793053 or SoyZH13_01G225600.",
                    trigger = "focus"),
          actionButton("submit_browse", strong("Submit!",
                                         bsButton("q7", label="", icon=icon("question"), style="info", size="small")
          ), width = "90%", styleclass = "success"),
          bsPopover("q7", "Whenever the genomic region is updated, please click Submit!",
                    trigger = "focus"),
          actionButton("clearGB", strong("Reset"), styleclass = "warning"),
          actionButton("GBExam", strong("Load example"), styleclass = "info"),
          
          conditionalPanel(condition="input.submit_browse != '0'", busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),

          sliderInput(inputId = "GBUP",  label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (bp)</b></font>'),
                                 bsButton("qg2", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 0, ticks = FALSE),
          bsPopover("qg2", "SNPs in the upstream of the specified genomic region will be used.",
                    trigger = "focus"),
          sliderInput("GBDOWN", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                   bsButton("qg4", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 0, ticks = FALSE),
          bsPopover("qg4", "SNPs in the downstream of the specified genomic region will be used.",
                    trigger = "focus"),

          multiInput("mychooserB", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
                                      bsButton("qg3", label="", icon=icon("question"), style="info", size="small")),
                     choices = all.soya.cho,
                     selected = all.soya.cho,
                     width = 800),
          bsPopover("qg3", "Only the chosen soybean accessions will be used.",
                    trigger = "focus"),
          fluidRow(
                  column(6, actionButton("browsenone1", strong("Clean all"), styleclass = "primary")),
                  column(6, actionButton("browseall1", strong("Select all"), styleclass = "primary"))
                       ),
          
          multiInput("GB_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation types</b></font>'),
                                        bsButton("qg1", label="", icon=icon("question"), style="info", size="small")),
                     choices = mutationtypes,
                     selected = mutationtypes,
                     width = 800),
          bsPopover("qg1", "Only SNPs with selected mutation effects will be used.",
                    trigger = "focus")
        ),
        

      mainPanel(
        fluidRow(
          column(4, downloadButton("downloadsnp.txt", style = "width:100%;", "Download genotype data")),
          column(4, downloadButton("downloadsnpInfo.txt", style = "width:100%;", "Download SNPs information")),
          column(4, downloadButton("downloadGB.pdf",  style = "width:100%;", "Download pdf-file"))
        ),
        plotlyOutput("gbrowser", height = '100%', width = '100%'))

        
      ),
      
      # Bulk download of data
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Search</strong>"), icon = icon("search"),
        
        sidebarPanel(
          fixedRow(
            column(12,
                   tags$div(HTML('<i class="fa fa-star" aria-hidden="true"></i> <font size="5" color="red"><b>Search SNP information</b></font>'),
                            bsButton("qqsi", label="", icon=icon("question"), style="info", size="small")),
                   bsPopover("qqsi", "Query the information for snp in Zhonghuang 13", trigger = "focus")
            )
          ),
          
          
          textInput("regBB", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                        bsButton("q1", label="", icon=icon("question"), style="info", size="small")),
                    value = "chr7:29506705-29659223"),
          
          bsPopover("q1", "A genomic region can be determined by chromosome positions or gene locus. For example, chr7:29506705-29659223 or SoyZH13_01G186100.",
                    trigger = "focus"),
          
          actionButton("submit_SSNP", strong("Submit!",
                                         bsButton("q12", label="", icon=icon("question"), style="info", size="small")
          ), width = "90%", styleclass = "success"),
          
          actionButton("clearDOW", strong("Reset"), styleclass = "warning"),
          actionButton("DOWExam", strong("Load example"), styleclass = "info"),
          bsPopover("q12", "Whenever the genomic region or any option is updated, please click Go!",
                    trigger = "focus"),
          conditionalPanel(condition="input.submit_SSNP != '0'", busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),

  

          multiInput("mychooserD", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean lines</b></font>'),
                                      bsButton("qdl2", label="", icon=icon("question"), style="info", size="small")),
                     choices = all.soya.cho,
                     selected = all.soya.cho,
                     width = 800),
          bsPopover("qdl2", "Only the chosen soybean lines will be used.",
                    trigger = "focus"),
          
          fluidRow(
            column(6, actionButton("snpsearchnone1", strong("Clean all"), styleclass = "primary")),
            column(6, actionButton("snpsearchall1", strong("Select all"), styleclass = "primary"))
          ),
          
          multiInput("down_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation types</b></font>'),
                                          bsButton("qg1", label="", icon=icon("question"), style="info", size="small")),
                     choices = mutationtypes,
                     choiceValues = NULL,
                     width = 800),
          bsPopover("qdl1", "Only SNPs with selected mutation effects will be used.",
                    trigger = "focus")
        ),
        br(),
        mainPanel(
          fluidRow(
            column(4, downloadButton("bulkdownloadsnpInfo.txt", style = "width:100%;", "Download SNPs information")),
            column(4, downloadButton("bulkdownloadsnp.txt", style = "width:100%;", "Download genotype data")),
            column(4, downloadButton("bulkdownloadgene.txt", style = "width:100%;", "Download gene annotation"))
          ),
          
          textOutput("snp_search"),
          tags$head(tags$style("#snp_search{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                      }"
          )
          ),
          dataTableOutput("mytable2")
        )),
      
      # LDheatmap
      tabPanel(
        title = HTML("<strong style='font-size:20px'>LDheatmap</strong>"), icon = icon("fire"),
        
        sidebarPanel(
          
          fixedRow(
            column(12,
                   tags$div(HTML('<i class="fa fa-star" aria-hidden="true"></i> <font size="5" color="red"><b>Linkage disequilibrium analysis</b></font>'),
                            bsButton("qlda", label="", icon=icon("question"), style="info", size="small")),
                   bsPopover("qlda", " For a specified genomic region orgene model, a heat map could be created to display the pairwise linkage disequilibrium between different SNP sites", trigger = "focus")
            )
          ),
          
          textInput("regL", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                       bsButton("q5", label="", icon=icon("question"), style="info", size="small")),
                    value = "SoyZH13_01G002900"),
          
          bsPopover("q5", "A genomic region can be determined by chromosome positions or gene locus. For example, chr2:17603220-17604802 or SoyZH13_01G002900.",
                    trigger = "focus"),
          
          actionButton("submit2", strong("Submit!",
                                         bsButton("q8", label="", icon=icon("question"), style="info", size="small")
          ), styleclass = "success"),
          actionButton("clearLD", strong("Reset"), styleclass = "warning"),
          actionButton("LDExam", strong("Load example"), styleclass = "info"),
          conditionalPanel(condition="input.submit3 != '0'", busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
          bsPopover("q8", "Whenever the genomic region is updated, please click Go!",
                    trigger = "focus"),
          
          br(),
          
          h4(HTML('<i class="fas fa-angle-double-down"></i> <font size="5" color="red"><b>Plot options</b></font>')),
          
          radioButtons("flip", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Flip the figure</b></font>'), list("FALSE" = 0, "TRUE" = 1)),
          
          conditionalPanel(
            condition = "input.flip==1",
            checkboxInput("LDshowGene", "Show gene model", FALSE),
            conditionalPanel(
              condition = "input.LDshowGene",
              numericInput("ldY", "Y:", value = 72),
              numericInput("ldW", "W:", value = 72)
            )
          ),
          
          conditionalPanel(
            condition = "input.flip==0",
            radioButtons("showText", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Print LD measurements</b></font>'), list("FALSE" =  0, "TRUE" = 1)),
            textInput("ldpos", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Label SNPs</b></font>'), value = "5, 8")
          ),
          
          radioButtons("ldcol",
                       label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Color</b></font>'), list("grey.colors(20)" = 1, "heat.colors(20)" = 2)
          ),
 
          sliderInput("ldUp", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (bp)</b></font>'),
                                 bsButton("ql4", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 0, ticks = FALSE),
          
          bsPopover("ql4", "SNPs in the upstream of the specified genomic region will be used.",
                    trigger = "focus"),
          
          sliderInput("ldDown", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                 bsButton("ql5", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 0, ticks = FALSE),
          
          bsPopover("ql5", "SNPs in the downstream of the specified genomic region will be used.",
                    trigger = "focus"),
          
          tags$div(align = 'left',
                   class = 'multicol', style = "width: 100%",
                   multiInput("LD_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation types</b></font>'),
                                                 bsButton("ql1", label="", icon=icon("question"), style="info", size="small")),
                              choices = mutationtypes,
                              choiceValues = NULL,
                              width = 800),
                   bsPopover("ql1", "Only SNPs with selected mutation effects will be used.",
                             trigger = "focus")
          ),



          multiInput("mychooserLD", p(h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean lines</b></font>'),
                                      bsButton("ql3", label="", icon=icon("question"), style="info", size="small"))),
                     choices = all.soya.cho,
                     selected = all.soya.cho,
                     width = 800), 
          
          bsPopover("ql3", "Only the chosen soybean lines will be used.",
                    trigger = "focus"),
          
          fluidRow(
            column(6, actionButton("ldnone1", strong("Clean all"), styleclass = "primary")),
            column(6, actionButton("ldall1", strong("Select all"), styleclass = "primary"))
          ),
          
          radioButtons("uploadLD", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>SNP sites to be retained</b></font>'),
                                      bsButton("ql2", label="", icon=icon("question"), style="info", size="small")), 
                       c("ALL" = "1", "Upload SNP sites file" = "2"), "1"),
          bsPopover("ql2", "A text file with SNP IDs (one ID per row) could be uploaded to screen the SNPs used in the analysis. Or else, all the SNPs in the specifid genomic region will be used.",
                    trigger = "focus"),
          conditionalPanel(condition="input.uploadLD == '2'",
                           fileInput("LD.snpsite", NULL, multiple = FALSE)),
          
          checkboxInput("ldSize", "Adjust plot size", FALSE),
          conditionalPanel(
            condition = "input.ldSize",
            numericInput("ldHeight", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height:</b></font>'), value = 550),
            numericInput("ldWidth", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width:</b></font>'), value = 750)
          )
        ),
        
        mainPanel(
          fluidRow(
            column(6, downloadButton("downloadLD.pdf", style = "width:100%;", "Download pdf-file")),
            column(6, downloadButton("downloadLD.svg", style = "width:100%;", "Download svg-file"))
            ),
          plotOutput("ldheatmap", height = '100%', width = '100%')
        )
      ),
      
      
      # Nucleotide diversity
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Diversity</strong>"), icon = icon("hourglass"),
        
        sidebarPanel(
          
          fixedRow(
            column(12,
                   tags$div(HTML('<i class="fa fa-star" aria-hidden="true"></i> <font size="5" color="red"><b>Nucleotide diversity analysis</b></font>'),
                            bsButton("qnda", label="", icon=icon("question"), style="info", size="small")),
                   bsPopover("qnda", "Functionalities were provided to calculate and demonstrate nucleotide diversities among subgroups of rice accessions in specified genomic regions.", trigger = "focus")
            )
          ),
          
          textInput("regD", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                       bsButton("q3", label="", icon=icon("question"), style="info", size="small")),
                    value = "SoyZH13_02G148200"),
          
          bsPopover("q3", "A genomic region can be determined by chromosome positions or gene locus. For example, chr1:17603220-17604802 or SoyZH13_02G148200",
                    trigger = "focus"),
          
          actionButton("submit4", strong("Submit!",
                                         bsButton("q10", label="", icon=icon("question"), style="info", size="small")
          ), styleclass = "success"),
          actionButton("clearDIV", strong("Reset"), styleclass = "warning"),
          actionButton("DIVExam", strong("Load example"), styleclass = "info"),
          conditionalPanel(condition="input.submit4 != '0'", busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
          bsPopover("q10", "Whenever the genomic region or any option is updated, please click Go!!",
                    trigger = "focus"),
          
          br(),
          HTML('<i class="fa fa-cog" aria-hidden="true"></i> <font size="4" color="red"><b>Plot options</b></font>'),
          
          numericInput("snpnumD", h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Number of SNPs in each window</b></font>'),
                                     bsButton("qd6", label="", icon=icon("question"), style="info", size="small")
          ), value = 10, min = 5, max = 20),
          bsPopover("qd6", "A specified genomic region would be split into non-overlapping window so that each window contains specified number of SNPs. The nucleotide diversity of all soybean lines belong to the specified ecotypes in each window would be calculated.",
                    trigger = "focus"),

          pickerInput(
            inputId = "div_acc_group",
            label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Ecotypes to calculate diversity</b></font>'), 
            choices = c("Improved cultivar", "Landrace", "G. Soja"),
            selected = c("Landrace", "G. Soja"),
            multiple = TRUE,
            options = list(style = "btn-primary")
          ),
          
          selectInput("nuc_numerator", h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Numerator ecotype</b></font>'),
                                          bsButton("qd7", label="", icon=icon("question"), style="info", size="small")
          ), choices = c("G. Soja", "Improved cultivar", "Landrace")),
          bsPopover("qd7", "The nucleotide diversity of soybean lines belong to the Numerator ecotype would be divided by the nucleotide diversity of soybean lines belong to the Denominator ecotype for comparison.",
                    trigger = "focus"),
          selectInput("nuc_denominator", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Denominator ecotype</b></font>'), choices = 
                        c("Improved cultivar", "Landrace", "G. Soja")),
          
          tags$div(align = 'left',
                   class = 'multicol', style = "width: 100%",
                   multiInput("div_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation types</b></font>'),
                                                  bsButton("qd1", label="", icon=icon("question"), style="info", size="small")),
                              choices = mutationtypes,
                              selected = mutationtypes,
                              width = 800),
                   bsPopover("qd1", "Only SNPs with selected mutation effects will be used.",
                             trigger = "focus")
          ),
          
          sliderInput("divUp", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (bp)</b></font>'),
                                    bsButton("qd4", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 20000, ticks = FALSE),
          bsPopover("qd4", "SNPs in the upstream of the specified genomic region will be used.",
                    trigger = "focus"),
          
          sliderInput("divDown", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                    bsButton("qd5", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 20000, ticks = FALSE),
                    bsPopover("qd5", "SNPs in the downstream of the specified genomic region will be used.",
                    trigger = "focus"),
          
          radioButtons("uploadDIV", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>SNP sites to be retained</b></font>'),
                                       bsButton("qd2", label="", icon=icon("question"), style="info", size="small")), 
                       c("ALL" = "1", "Upload SNP sites file" = "2"), "1"),
          bsPopover("qd2", "A text file with SNP IDs (one ID per row) could be uploaded to screen the SNPs used in the analysis. Or else, all the SNPs in the specifid genomic region will be used.",
                    trigger = "focus"),
          conditionalPanel(condition="input.uploadDIV == '2'",
                           fileInput("DIV.snpsite", NULL, multiple = FALSE)),
          
          checkboxInput("divSize", "Adjust plot size", FALSE),
          conditionalPanel(
            condition = "input.divSize",
            numericInput("divHeight", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height:</b></font>'), value = 550),
            numericInput("divWidth", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width:</b></font>'), value = 750)
          )
          
        ),
        
        mainPanel(
          fluidRow(
            column(4, uiOutput("downloadDiv01")),
            column(4, uiOutput("downloadDiv02")),
            column(4, uiOutput("downloadDiv03"))
          ),
          
          # downloadButton("downloadDiv.pdf", "Download pdf-file"),
          # downloadButton("downloadDiv.svg", "Download svg-file"),
          # downloadButton("downloadDiv.txt", "Download TXT-file"),
          plotOutput("diversity", height = '100%', width = '100%')
        )
        
      ),
      
      # Phylogenetic tree
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Phylogenetic</strong>"), icon = icon("cloud"),
        
        sidebarPanel(
          fixedRow(
            column(12,
                   tags$div(HTML('<i class="fa fa-star" aria-hidden="true"></i> <font size="5" color="red"><b>Phylogenetic analysis</b></font>'),
                            bsButton("qpa", label="", icon=icon("question"), style="info", size="small")),
                   bsPopover("qpa", " For a specified genomic region or gene model, a neighbor-joining (NJ) tree could be built based on a pairwise distance matrix derived from the simple matching distance for all SNP sites in this region (Supplementary methods).", trigger = "focus")
            )
          ),
          
          textInput("regP", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                       bsButton("q2", label="", icon=icon("question"), style="info", size="small")),
                    value = "SoyZH13_02G148200"),
          
          bsPopover("q2", "A genomic region can be determined by chromosome positions or gene locus. For example, chr2:17603220-17604802 or SoyZH13_01G186100",
                    trigger = "focus"),
          actionButton("submit5", strong("Submit!",
                                         bsButton("q11", label="", icon=icon("question"), style="info", size="small")
          ), styleclass = "success"),
          actionButton("clearPHY", strong("Reset"), styleclass = "warning"),
          actionButton("PHYExam", strong("Load example"), styleclass = "info"),
          conditionalPanel(condition="input.submit5 != '0'", busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
          bsPopover("q11", "Whenever the genomic region or any option is updated, please click Go!",
                    trigger = "focus")
          ,
          br(),
          HTML('<i class="fa fa-cog" aria-hidden="true"></i> <font size="4" color="red"><b>Plot options</b></font>'),
          
          sliderInput("phyUp", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (bp)</b></font>'),
                                 bsButton("qp4", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 1000, ticks = FALSE),
          bsPopover("qp4", "SNPs in the upstream of the specified genomic region will be used.",
                    trigger = "focus"),
          
          sliderInput("phyDown", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                 bsButton("qp5", label="", icon=icon("question"), style="info", size="small")
          ), min = 0, max = 50000, value = 1000, ticks = FALSE),
          
          bsPopover("qp5", "SNPs in the downstream of the specified genomic region will be used.",
                    trigger = "focus"),
          
          tags$div(align = 'left',
                   class = 'multicol', style = "width: 100%",
                   multiInput("phy_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation types</b></font>'),
                                                  bsButton("qp1", label="", icon=icon("question"), style="info", size="small")),
                              choices = mutationtypes,
                              choiceValues = NULL,
                              width = 800),
                   bsPopover("qp1", "Only SNPs with selected mutation effects will be used.",
                             trigger = "focus")
          ),

          
          multiInput("mychooserPhy", p(h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean lines</b></font>'),
                                         bsButton("qp3", label="", icon=icon("question"), style="info", size="small"))),
                     choices = all.soya.cho,
                     selected = all.soya.cho,
                     width = 800), 
          bsPopover("qp3", "Only the chosen soybean lines will be used.",
                    trigger = "focus"),
          
          fluidRow(
            column(6, actionButton("phynone1", strong("Clean all"), styleclass = "primary")),
            column(6, actionButton("phyall1", strong("Select all"), styleclass = "primary"))
          ),
          
          radioButtons("uploadPHY", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>SNP sites to be retained</b></font>'),
                                       bsButton("qp2", label="", icon=icon("question"), style="info", size="small")), 
                       c("ALL" = "1", "Upload SNP sites file" = "2"), "1"),
          bsPopover("qp2", "A text file with SNP IDs (one ID per row) could be uploaded to screen the SNPs used in the analysis. Or else, all the SNPs in the specifid genomic region will be used.",
                    trigger = "focus"),
          conditionalPanel(condition="input.uploadPHY == '2'",
                           fileInput("PHY.snpsite", NULL, multiple = FALSE)),
          
          checkboxInput("phySize", "Adjust plot size", FALSE),
          conditionalPanel(
            condition = "input.phySize",
            numericInput("phyHeight", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height:</b></font>'), value = 700),
            numericInput("phyWidth", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width:</b></font>'), value = 750)
          )),      
          
          mainPanel(
            fluidRow(
              column(5, uiOutput("downloadPhy01")),
              column(5, uiOutput("downloadPhy02"))
            ),
            
            # downloadButton("downloadPhylo.pdf", "Download pdf-file"),
            # downloadButton("downloadPhylo.nwk", "Download Newick-file"),
            plotOutput("phylo", height = '100%', width = '100%')
            
          )
        
      )
      ),

      #Indel
      
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Indel</strong>"),
        
        sidebarPanel(
          
          fixedRow(
            column(12,
                   tags$div(HTML('<i class="fa fa-star" aria-hidden="true"></i> <font size="5" color="red"><b>Indel information</b></font>'),
                            bsButton("qii", label="", icon=icon("question"), style="info", size="small")),
                   bsPopover("qii", "Query the information for indel in Zhonghuang 13", trigger = "focus")
            )
          ),
          
          textInput("regi", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                       bsButton("qi1", label="", icon=icon("question"), style="info", size="small")),
                    value = "SoyZH13_01G186100"),
          
          bsPopover("qi1", "A genomic region can be determined by chromosome positions or gene locus. For example, chr1:29506705-29659223 or SoyZH13_01G186100.",
                    trigger = "focus"),
          
          actionButton("submiti", strong("Submit!",
                                         bsButton("qi2", label="", icon=icon("question"), style="info", size="small")
          ), width = "90%", styleclass = "success"),
          
          actionButton("clearINDEL", strong("Reset"), styleclass = "warning"),
          actionButton("INDELExam", strong("Load example"), styleclass = "info"),
          conditionalPanel(condition="input.submiti != '0'", busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
          bsPopover("qi2", "Whenever the genomic region or any option is updated, please click Submit!",
                    trigger = "focus"),

          multiInput("mychooseri", p(h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean lines</b></font>'),
                                          bsButton("qdl2", label="", icon=icon("question"), style="info", size="small"))),
                     choices = all.soya.cho,
                     selected = c("Landrace"),
                     width = 800), 
          
          bsPopover("qdl2", "Only the chosen soybean lines will be used.",
                    trigger = "focus"),
          fluidRow(
            column(6, actionButton("indelnone1", strong("Clean all"), styleclass = "primary")),
            column(6, actionButton("indelall1", strong("Select all"), styleclass = "primary"))
          ),
        ),
        br(),
        mainPanel(
          downloadButton("bulkdownloadindelInfo.txt", style = "width:45%;center", "Download indels information"),

          textOutput("indeltabletitle"),
          DT::dataTableOutput("indeltable"),
          
          tags$head(tags$style("#indeltabletitle{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                      }"
          ),
          tags$style("#indeltable{
                     width = 100%;
                     max-height: 300px;
          }")
          )
        )
      ),
      
      

        
      #Search sequence, cds, pro and gff information
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>Genomes</strong>"),
        tabPanel(
          
          title = HTML("<strong style='font-size:20px'>Search by gene ID</strong>"), icon = icon("id-card"),
          sidebarPanel(
            fixedRow(
              column(12,
                     tags$div(HTML('<i class="fa fa-star" aria-hidden="true"></i> <font size="5" color="red"><b>Gene information</b></font>'),
                              bsButton("qsgsid", label="", icon=icon("question"), style="info", size="small")),
                     bsPopover("qsgsid", "Input gene to query base sequences and other information", trigger = "focus")
              )
            ),
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 400px; }"),
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
              column(3, downloadButton("genesequence_ID.txt", "Download Gene Sequence", style = "width:100%;", class = "buttDown")),
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
          
          title = HTML("<strong style='font-size:20px'>Search by genome location</strong>"), icon = icon("search-location"),
          
          sidebarPanel(
            
            fixedRow(
              column(12,
                     tags$div(HTML('<i class="fa fa-star" aria-hidden="true"></i> <font size="5" color="red"><b>Interval information</b></font>'),
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
              column(3, downloadButton("sequence_IT.txt", "Download Gene Sequence", style = "width:100%;", class = "buttDown")),
              column(3, downloadButton("cdssequence_IT.txt", "Download CDS Sequence", style = "width:100%;", class = "buttDown")),
              column(3, downloadButton("cdnasequence_IT.txt", "Download cDNA Sequence", style = "width:100%;", class = "buttDown")),
              column(3, downloadButton("prosequence_IT.txt", "Download Protein Sequence", style = "width:100%;", class = "buttDown"))
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
      ),
      #TOOLS
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>Tools</strong>"), icon = icon("toolbox", lib = "font-awesome"),
      
      #Blast
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Blast</strong>"),
        icon = icon("rocket", class = NULL, lib = "font-awesome"),
        
        tags$div(HTML('<i class="fa fa-star" aria-hidden="true"></i> <font size="5" color="red"><b>Search database by sequence similarity using BLAST</b></font>'),
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
                                      multiInput(
                                        inputId = "BLASTdb",
                                        label = tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Choose BLAST databases</font>'),
                                                         bsButton("qblastDB", label="", icon=icon("question"), style="info", size="small")
                                        ),
                                        choices = BLASTdb.fl$Accession, 
                                        width = '100%',
                                        selected = NULL,
                                        options = list(
                                          enable_search = TRUE,
                                          non_selected_header = "Choose from:",
                                          selected_header = "You have selected:"
                                        )
                                      ),
                                      bsPopover("qblastDB", "Choose one or multiple BLAST database to search against.",
                                                trigger = "focus")
                               ),
                               column(3,
                                      selectInput("program_database", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">Search database</font>'),
                                                                              bsButton("qBLASTpdata", label="", icon=icon("question"), style="info", size="small")),
                                                  choices=c("DNA database", "Protein database"), width = NULL),
                                      bsPopover("qBLASTpdata", "Choose search database.", 
                                                trigger = "focus"),
                                      textInput("BLASTev", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">E-value cutoff</font>'),
                                                                            bsButton("qBLASTev", label="", icon=icon("question"), style="info", size="small")), 
                                                value = "10", width = NULL, placeholder = NULL),
                                      bsPopover("qBLASTev", "Set E-value threshold to filter the BLAST output.",
                                                trigger = "focus"),
                                      
                                      conditionalPanel(condition="input.program_database == 'DNA database'", 
                                                       selectInput("programdna", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">Program</font>'),
                                                                   bsButton("qBLASThtd", label="", icon=icon("question"), style="info", size="small")), 
                                                                   choices=c("blastn","tblastn", "tblastx"), width = NULL),
                                      ),
                                      conditionalPanel(condition="input.program_database == 'Protein database'", 
                                                       selectInput("programpro", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">Program</font>'),
                                                                   bsButton("qBLASThtp", label="", icon=icon("question"), style="info", size="small")), 
                                                                   choices=c("blastp", "blastx"), width = NULL),
                                      ),
                                      bsPopover("qBLASThtd", "Choose alignment search tool.",
                                                trigger = "focus"),
                                      bsPopover("qBLASThtp", "Choose alignment search tool.",
                                                trigger = "focus"),
                    
                                      br(),
                                      
                                      actionButton("submitBLAST", strong("BLAST!",
                                                                         bsButton("qBLASTGO", label="", icon=icon("question"), style="info", size="small")
                                      ), styleclass = "success"),
                                      actionButton("clear3", strong("Reset"), styleclass = "warning"),
                                      actionButton("blastExam", strong("Load example"), styleclass = "info"),
                                      conditionalPanel(condition="input.submitBLAST != '0'", busyIndicator(HTML("<p style='color:red;font-size:30px;'>Calculation In progress...</p>"), wait = 0)),
                                      bsPopover("qBLASTGO", "Click this button to start the BLAST alignment!",
                                                trigger = "focus"),
                                      
                               )
                             )
                    ),
                    tabPanel("Output",
                             fixedRow(
                               column(4,
                                      downloadButton("BLASTresult.txt", "BLAST result", style = "width:100%;", class = "buttDown")
                               ),
                               column(4,
                                      downloadButton("blastDownIRFresult.txt", "Structure of Soybean in the BLAST result", style = "width:100%;", class = "buttDown")
                               ),
                               column(4,
                                      downloadButton("blastDownIRFfasta.txt", "Sequence of Soybean in the BLAST result", style = "width:100%;", class = "buttDown")
                               )
                             ),
                             
                             br(),
                             
                             conditionalPanel(condition="input.submitBLAST > 0", 
                                              tags$div(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>BLAST output</b></font>'),
                                                       bsButton("qBLASTresultHelp", label="", icon=icon("question"), style="info", size="small")),
                                              bsPopover("qBLASTresultHelp", title = "Click on a row to check the details of the BLAST alignment!", trigger = "focus", content = NULL)
                             ),
                             
                             DT::dataTableOutput("BLASTresult"),
                             fixedRow(
                               column(1),
                               column(4,
                                      textOutput("Alignment"),
                                      tableOutput("clicked"),
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
                                      textOutput("geneseq"),
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
                                      verbatimTextOutput("alignment", placeholder = FALSE)
                               )
                             )
                    )
                    
        )
      ),
      
      ##primer3
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Primer-Design</strong>"),icon = icon("drafting-compass"),
        
        sidebarPanel(
          
          fixedRow(
            column(12,
                   tags$div(HTML('<i class="fa fa-star" aria-hidden="true"></i> <font size="5" color="red"><b>Desigin primer</b></font>'),
                            bsButton("qdp", label="", icon=icon("question"), style="info", size="small")),
                   bsPopover("qdp", "Primer can be used for designing PCR primers, PCR is an essential and ubiquitous tool in genetics and molecular biology.", trigger = "focus")
            )
          ),
          
          HTML('<i class="fas fa-arrow-alt-circle-down"></i> <font size="4" color="black"><b>Parameter Selection</b></font>'),
          fluidRow(
            column(4,selectInput("Chrprimer", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Chr</b></font>'),choices = paste0("Chr", 1:20), selected = "Chr1")),
            column(4,textInput("upprimer", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Start</b></font>'),value = "48000")),
            column(4,textInput("downprimer", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>End</b></font>'),value = "49000"))
          ),
          
          textInput("PRIMER_OPT_SIZE", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Optimal primer size (bp)</b></font>'),
                                                  bsButton("qpr1", label="", icon=icon("question"), style="info", size="small")),value = "20"),
          bsPopover("qpr1", "Optimum length (in bases) of a primer. Primer3 will attempt to pick primers close to this length.",
                    trigger = "focus"),
          
          sliderInput("PRIMER_SIZE", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Primer size range (bp)</b></font>')
                                                , bsButton("qpr2", label="", icon=icon("question"), style="info", size="small")), min = 1, max = 35, 
                      value = c(18, 25)),
          bsPopover("qpr2", "Minimum acceptable length  and Maximum acceptable length (in bases) of a primer", trigger = "focus"),    
          
          textInput("PRIMER_OPT_TM", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Optimal primer TM (℃)</b></font>')
                                                , bsButton("qpr3", label = "", icon = icon("question"), style = "info", size = "small")),value = "59.0"),
          bsPopover("qpr3", "Optimum melting temperature (Celsius) for a primer.", trigger = "focus"),    
          
          fluidRow(
            column(6,textInput("PRIMER_MIN_TM", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Min primer TM (℃)</b></font>')
                                                           , bsButton("qpr4", label = "", icon = icon("question"), style = "info", size = "small")),value = "57.0"),
                   bsPopover("qpr4", "Minimum acceptable melting temperature (Celsius) for a primer oligo.", trigger = "focus")),
            column(6,textInput("PRIMER_MAX_TM", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max primer TM (℃)</b></font>')
                                                           , bsButton("qpr5", label = "", icon = icon("question"), style = "info", size = "small")),value = "62.0"),
                   bsPopover("qpr5", "Maximum acceptable melting temperature (Celsius) for a primer oligo.", trigger = "focus"))
          ),
          
          
          textInput("PRIMER_OPT_GC_PERCENT", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Optimal GC percent:</b></font>')
                                                        , bsButton("qpr6", label = "", icon = icon("question"), style = "info", size = "small")),value = "50.0"),
          bsPopover("qpr6", "Optimum GC percent.", trigger = "focus"),
          
          sliderInput("PRIMER_GC", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>GC percent range (bp)</b></font>')
                                              , bsButton("qpr7", label = "", icon = icon("question"), style = "info", size = "small")), min = 0, max = 100, 
                      value = c(20, 80), step=0.1),
          bsPopover("qpr7", "Minimum allowable percentage of Gs and Cs in any primer and Maximum allowable percentage of Gs and Cs in any primer generated by Primer.", trigger = "focus"),
          
          textInput("PRIMER_MAX_NS_ACCEPTED", label = h5(HTML("<i class='fa fa fa-circle' aria-hidden='true'></i> <font size='4' color='red'><b>Max #N's accepted</b></font>")
                                                         , bsButton("qpr8", label = "", icon = icon("question"), style = "info", size = "small")),value = "0"),
          bsPopover("qpr8", "Maximum number of unknown bases (N) allowable in any primer.", trigger = "focus"),
          
          
          fluidRow(
            column(6,textInput("PRIMER_MAX_POLY_X", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max Poly-X:</b></font>')
                                                               , bsButton("qpr9", label = "", icon = icon("question"), style = "info", size = "small")),value = "10"),
                   bsPopover("qpr9", "The maximum allowable length of a mononucleotide repeat, for example AAAAAA.", trigger = "focus")),
            column(6,textInput("PRIMER_INTERNAL_MAX_POLY_X", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max Internal Poly-X:</b></font>')
                                                                        , bsButton("qpr10", label = "", icon = icon("question"), style = "info", size = "small")),value = "15"),
                   bsPopover("qpr10", "Equivalent parameter of PRIMER_MAX_POLY_X for the internal oligo.", trigger = "focus"))
          ),
          fluidRow(
            column(6,textInput("PRIMER_MAX_SELF_ANY", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max Self Complementarity</b></font>')
                                                                 , bsButton("qpr11", label = "", icon = icon("question"), style = "info", size = "small")), value = "45.0"),
                   bsPopover("qpr11", "It is the maximum allowable local alignment score when testing a single primer for (local) self-complementarity.", trigger = "focus")),
            
            column(6,textInput("PRIMER_MAX_SELF_ANY_TH", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max Pair Complementarity</b></font>')
                                                                    , bsButton("qpr12", label = "", icon = icon("question"), style = "info", size = "small")), value = "45.0"),
                   bsPopover("qpr12", "The maximum primer pair complementarity.", trigger = "focus"))
          ),
          fluidRow(
            column(6,textInput("PRIMER_MAX_SELF_END", label = h5(HTML("<i class='fa fa fa-circle' aria-hidden='true'></i> <font size='3' color='red'><b>Max 3' Self Complementarity</b></font>")
                                                                 , bsButton("qpr13", label = "", icon = icon("question"), style = "info", size = "small")), value = "35.0")
                   , bsPopover("qpr13", "The maximum allowable 3'-anchored global.", trigger = "focus")),
            column(6,textInput("PRIMER_MAX_SELF_END_TH", label = h5(HTML("<i class='fa fa fa-circle' aria-hidden='true'></i> <font size='3' color='red'><b>Max 3' Pair Complementarity</b></font>")
                                                                    , bsButton("qpr14", label = "", icon = icon("question"), style = "info", size = "small")), value = "35.0")
                   , bsPopover("qpr14", "Same as PRIMER_MAX_SELF_END but is based on thermodynamical approach - the stability of structure is analyzed. The value of tag is expressed as melting temperature. ", trigger = "focus"))
          ),
          textInput("PRIMER_PRODUCT_SIZE_RANGE", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Product Size Ranges</b></font>')
                                                                    , bsButton("qpr15", label = "", icon = icon("question"), style = "info", size = "small")), value = "100-300,300-600,600-1000")
                    , bsPopover("qpr15", "If one desires PCR products in either the range from 100 to 300 bases or in the range from 300 to 600 (or 600 to 1000) bases then one would set this parameter to 100-300,300-600,600-1000.", trigger = "focus"),
          
          actionButton("submitprimer", strong("submit!",
                                              bsButton("qprGO", label="", icon=icon("question"), style="info", size="small")
          ), styleclass = "success"),
          conditionalPanel(condition="submitprimer != '0'", busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
          bsPopover("qprGO", "Whenever t he genomic region or any option is updated, please click Go!",
                    trigger = "focus")
          
        ),
        mainPanel(
          dataTableOutput("primertable"),
          verbatimTextOutput("primerseq")
        )
      )
    ),
      
      # Accession
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Accession</strong>"), icon = icon("clipboard"),
        
        sidebarPanel(
          downloadButton("soya.info.txt", style = "width:100%;", "Download information of all accessions"),

          multiInput("mychooserA", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean lines</b></font>'),
                                      bsButton("qa1", label="", icon=icon("question"), style="info", size="small")),
                     choices = all.soya.cho,
                     selected = c("Landrace"),
                     width = 800),
          
          bsPopover("qa1", "Only the chosen maize lines will be used.",
                    trigger = "focus"),
          fluidRow(
            column(6, actionButton("accessionnone1", strong("Clean all"), styleclass = "primary")),
            column(6, actionButton("accessionall1", strong("Select all"), styleclass = "primary"))
          ),
          
          br(),
          downloadButton("sel.soya.info.txt", style = "width:100%;", "Download information of selected accessions")
        ),
        
        mainPanel(
          h4(HTML('<i class="fa fa-table" aria-hidden="true"></i> <font size="4" color="red"><b>Information of selected soybean lines</b></font>')),
          dataTableOutput("mytable1")
        )
      ),
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>JBrowse</strong>"), icon = icon("window-restore"),

        tabPanel(HTML('<a href="https://venyao.xyz/jbrowse2/?session=share-I_pitcPW49&password=7w14E" target="_blank"> <strong style="font-size:20px">Zhonghuang 13</strong>')),
        

        tabPanel(HTML('<a href="https://venyao.xyz/jbrowse2/?session=share-iiCm9qQEzl&password=U3xj5" target="_blank"> <strong style="font-size:20px">58-161</strong>')),
        tabPanel(HTML('<a href="https://venyao.xyz/jbrowse2/?session=share-XzdAmf4gqC&password=4IzSe" target="_blank"> <strong style="font-size:20px">Amsoy</strong>'))
      ),
      
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>Help</strong>"), icon = icon("leanpub"),
      
      ## About
      tabPanel(
        HTML("<strong style='font-size:20px'>About</strong>"))
      )
    )
  )

)

