

library(shinythemes)
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
    navbarPage(
      
      
      title = HTML("<strong style='color:#000000'>SoyaSNPDB</strong>"),
      windowTitle = "SNP database of 2898 soya lines",
      
      #home
      tabPanel(
               title = HTML("<strong style='font-size:18px'>Home</strong>"), icon = icon("home"),Homepage),
      
      # Genome browser
      tabPanel(
        title = HTML("<strong style='font-size:18px'>Browser</strong>"), icon = icon("leaf"),
        
        tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});')),
                  tags$style(
                    HTML(
                      "
            #inputs-table {
            border-collapse: collapse;
            }
            
            #inputs-table td {
            padding: 3px;
            vertical-align: bottom;
            }

            .multicol .shiny-options-group{
                            -webkit-column-count: 2; /* Chrome, Safari, Opera */
            -moz-column-count: 2;    /* Firefox */
            column-count: 2;
            -moz-column-fill: balanced;
            -column-fill: balanced;
            }
            .checkbox{
            margin-top: 0px !important;
            -webkit-margin-after: 1px !important; 
            }

            "
                    ) #/ HTML
                  ) #/ style
        ), #/ head
        
        fluidRow(column(12,
                        class = "col-md-5",
                        style = "margin: 1px 1px 1px 1px",
                        tags$table(id = "inputs-table",
                                   style = "width: 100%",
                                   tags$tr(
                                     tags$td(style = "width: 60%",
                                             textInput("regB", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region:</b></font>'),
                                                                          bsButton("q6", label="", icon=icon("question"), style="info", size="small")),
                                                       value = ""),
                                             
                                             bsPopover("q6", "A genomic region can be determined by chromosome positions or gene locus. For example, chr1:29765419-29793053 or SoyZH13_01G225600.",
                                                       trigger = "focus")
                                     ), #/ column 1
                                     tags$td(style = "width: 40%; text-align: right",
                                             div(class = "form-group shiny-input-container",
                                                 actionButton("submit1", strong("Submit!",
                                                                                bsButton("q7", label="", icon=icon("question"), style="info", size="small")
                                                 ), width = "90%", styleclass = "success"),
                                                 conditionalPanel(condition="input.submit1 != '0'", busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
                                                 bsPopover("q7", "Whenever the genomic region is updated, please click Submit!",
                                                           trigger = "focus")
                                             )
                                     ), #/ column 2
                                     tags$td(style = "width: 40%; text-align: right",
                                             div(class = "form-group shiny-input-container",
                                                 actionButton("clearGB", strong("Reset"), styleclass = "warning"),
                                                 actionButton("GBExam", strong("Load example"), styleclass = "info")
                                             )
                                     )
                                   ) #/ tr
                        ) #/ table
        )),
        
        #      br(),
        
        fluidRow(
          column(3,
                 sliderInput("GBUP", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream:</b></font>'),
                                        bsButton("qg2", label="", icon=icon("question"), style="info", size="small")
                 ), min = 0, max = 50000, value = 0, ticks = FALSE),
                 bsPopover("qg2", "SNPs in the upstream of the specified genomic region will be used.",
                           trigger = "focus"),
                 sliderInput("GBDOWN", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream:</b></font>'),
                                          bsButton("qg4", label="", icon=icon("question"), style="info", size="small")
                 ), min = 0, max = 50000, value = 0, ticks = FALSE),
                 bsPopover("qg4", "SNPs in the downstream of the specified genomic region will be used.",
                           trigger = "focus")
          ),
          
          column(4,
                 p(h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soya accessions:</b></font>'),
                      bsButton("qg3", label="", icon=icon("question"), style="info", size="small"))),
                 bsPopover("qg3", "Only the chosen soya accessions will be used.",
                           trigger = "focus"),
                 
                 chooserInput("mychooserB", "Available frobs", "Selected frobs", c(),
                              all.soya.cho, size = 12, multiple = TRUE)
          ),
          
          
          column(5,
                 tags$div(align = 'left',
                          class = 'multicol', style = "width: 100%",
                          multiInput("GB_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation types:</b></font>'),
                                                        bsButton("qg1", label="", icon=icon("question"), style="info", size="small")),
                                     choices = mutationtypes,
                                     choiceValues = NULL,
                                     width = 800),
                          bsPopover("qg1", "Only SNPs with selected mutation effects will be used.",
                                    trigger = "focus")
                 )
          )
        ),
        
        downloadButton("downloadsnp.txt", "Download genotype data"),
        downloadButton("downloadsnpInfo.txt", "Download SNPs information"),
        downloadButton("downloadGB.pdf", "Download pdf-file"),
        
        plotlyOutput("gbrowser", height = '100%', width = '100%'),
        
        br()
        
      ),
      
      # LDheatmap
      tabPanel(
        title = HTML("<strong style='font-size:18px'>LDheatmap</strong>"), icon = icon("fire-alt"),
        
        sidebarPanel(
          textInput("regL", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region:</b></font>'),
                                       bsButton("q5", label="", icon=icon("question"), style="info", size="small")),
                    value = ""),
          
          bsPopover("q5", "A genomic region can be determined by chromosome positions or gene locus. For example, chr2:17603220-17604802 or SoyZH13_02G148200.",
                    trigger = "focus"),
          
          actionButton("submit2", strong("Go!",
                                         bsButton("q8", label="", icon=icon("question"), style="info", size="small")
          ), styleclass = "success"),
          actionButton("clearLD", strong("Reset"), styleclass = "warning"),
          actionButton("LDExam", strong("Load example"), styleclass = "info"),
          conditionalPanel(condition="input.submit3 != '0'", busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
          bsPopover("q8", "Whenever the genomic region is updated, please click Go!",
                    trigger = "focus"),
          
          br(),
          HTML('<i class="fa fa-cog" aria-hidden="true"></i> <font size="4" color="red"><b>Plot options</b></font>'),
          
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
            textInput("ldpos", "Label SNPs:", value = "5, 8")
          ),
          
          radioButtons("ldcol",
                       "Color", list("grey.colors(20)" = 1, "heat.colors(20)" = 2)
          ),
          
          numericInput("ldUp", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (kb):</b></font>'),
                                  bsButton("ql4", label="", icon=icon("question"), style="info", size="small")
          ), value = 0),
          bsPopover("ql4", "SNPs in the upstream of the specified genomic region will be used.",
                    trigger = "focus"),
          numericInput("ldDown", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (kb):</b></font>'),
                                    bsButton("ql5", label="", icon=icon("question"), style="info", size="small")
          ), value = 0),
          bsPopover("ql5", "SNPs in the downstream of the specified genomic region will be used.",
                    trigger = "focus"),
          
          tags$div(align = 'left',
                   class = 'multicol', style = "width: 100%",
                   multiInput("LD_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation types:</b></font>'),
                                                 bsButton("qg1", label="", icon=icon("question"), style="info", size="small")),
                              choices = mutationtypes,
                              choiceValues = NULL,
                              width = 800),
                   bsPopover("ql1", "Only SNPs with selected mutation effects will be used.",
                             trigger = "focus")
          ),
          
          p(h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soya lines:</b></font>'),
               bsButton("ql3", label="", icon=icon("question"), style="info", size="small"))),
          bsPopover("ql3", "Only the chosen soya lines will be used.",
                    trigger = "focus"),
          
          chooserInput("mychooserLD", "Available frobs", "Selected frobs",
                       c(), all.soya.cho, size = 12, multiple = TRUE
          ),
          
          radioButtons("uploadLD", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>SNP sites to be retained:</b></font>'),
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
          downloadButton("downloadLD.pdf", "Download pdf-file"),
          downloadButton("downloadLD.svg", "Download svg-file"),
          plotOutput("ldheatmap", height = '100%', width = '100%')
        )
      ),
      
      
      # Nucleotide diversity
      tabPanel(
        title = HTML("<strong style='font-size:18px'>Diversity</strong>"), icon = icon("hourglass"),
        
        sidebarPanel(
          textInput("regD", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region:</b></font>'),
                                       bsButton("q3", label="", icon=icon("question"), style="info", size="small")),
                    value = ""),
          
          bsPopover("q3", "A genomic region can be determined by chromosome positions or gene locus. For example, chr1:17603220-17604802 or SoyZH13_02G148200",
                    trigger = "focus"),
          
          actionButton("submit4", strong("Go!",
                                         bsButton("q10", label="", icon=icon("question"), style="info", size="small")
          ), styleclass = "success"),
          actionButton("clearDIV", strong("Reset"), styleclass = "warning"),
          actionButton("DIVExam", strong("Load example"), styleclass = "info"),
          conditionalPanel(condition="input.submit4 != '0'", busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
          bsPopover("q10", "Whenever the genomic region or any option is updated, please click Go!!",
                    trigger = "focus"),
          br(),
          HTML('<i class="fa fa-cog" aria-hidden="true"></i> <font size="4" color="red"><b>Plot options</b></font>'),
          
          numericInput("snpnumD", h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Number of SNPs in each window:</b></font>'),
                                     bsButton("qd6", label="", icon=icon("question"), style="info", size="small")
          ), value = 10, min = 5, max = 20),
          bsPopover("qd6", "A specified genomic region would be split into non-overlapping window so that each window contains specified number of SNPs. The nucleotide diversity of all soya lines belong to the specified ecotypes in each window would be calculated.",
                    trigger = "focus"),
          
          tags$div(align = 'left',
                   class = 'multicol', style = "width: 100%",
                   checkboxGroupInput("div_acc_group", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Ecotypes to calculate diversity:</b></font>'),
                                      choices = c("Improved cultivar", "Landrace", "G. Soja"),
                                      selected = c("Improved cultivar", "G. Soja")) 
          ),
          
          selectInput("nuc_numerator", h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Numerator ecotype:</b></font>'),
                                          bsButton("qd7", label="", icon=icon("question"), style="info", size="small")
          ), choices = 
            c("G. Soja", "Improved cultivar", "Landrace")),
          bsPopover("qd7", "The nucleotide diversity of soya lines belong to the Numerator ecotype would be divided by the nucleotide diversity of soya lines belong to the Denominator ecotype for comparison.",
                    trigger = "focus"),
          selectInput("nuc_denominator", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Denominator ecotype:</b></font>'), choices = 
                        c("Improved cultivar", "Landrace", "G. Soja")),
          
          tags$div(align = 'left',
                   class = 'multicol', style = "width: 100%",
                   multiInput("div_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation types:</b></font>'),
                                                  bsButton("qd1", label="", icon=icon("question"), style="info", size="small")),
                              choices = mutationtypes,
                              choiceValues = NULL,
                              width = 800),
                   bsPopover("qd1", "Only SNPs with selected mutation effects will be used.",
                             trigger = "focus")
          ),
          
          
          numericInput("divUp", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (kb):</b></font>'),
                                   bsButton("qd4", label="", icon=icon("question"), style="info", size="small")
          ), value = 20),
          bsPopover("qd4", "SNPs in the upstream of the specified genomic region will be used.",
                    trigger = "focus"),
          numericInput("divDown", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (kb):</b></font>'),
                                     bsButton("qd5", label="", icon=icon("question"), style="info", size="small")
          ), value = 20),
          bsPopover("qd5", "SNPs in the downstream of the specified genomic region will be used.",
                    trigger = "focus"),
          
          radioButtons("uploadDIV", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>SNP sites to be retained:</b></font>'),
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
            column(3, uiOutput("downloadDiv01")),
            column(3, uiOutput("downloadDiv02")),
            column(3, uiOutput("downloadDiv03"))
          ),
          
          # downloadButton("downloadDiv.pdf", "Download pdf-file"),
          # downloadButton("downloadDiv.svg", "Download svg-file"),
          # downloadButton("downloadDiv.txt", "Download TXT-file"),
          plotOutput("diversity", height = '100%', width = '100%')
        )
        
      ),
      
      # Phylogenetic tree
      tabPanel(
        title = HTML("<strong style='font-size:18px'>Phylogenetic</strong>"), icon = icon("cloud"),
        
        sidebarPanel(
          textInput("regP", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region:</b></font>'),
                                       bsButton("q2", label="", icon=icon("question"), style="info", size="small")),
                    value = ""),
          
          bsPopover("q2", "A genomic region can be determined by chromosome positions or gene locus. For example, chr2:17603220-17604802 or SoyZH13_01G186100",
                    trigger = "focus"),
          actionButton("submit5", strong("Go!",
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
          numericInput("phyUp", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (kb):</b></font>'),
                                   bsButton("qp4", label="", icon=icon("question"), style="info", size="small")
          ), value = 20),
          bsPopover("qp4", "SNPs in the upstream of the specified genomic region will be used.",
                    trigger = "focus"),
          numericInput("phyDown", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (kb):</b></font>'),
                                     bsButton("qp5", label="", icon=icon("question"), style="info", size="small")
          ), value = 20),
          bsPopover("qp5", "SNPs in the downstream of the specified genomic region will be used.",
                    trigger = "focus"),
          
          tags$div(align = 'left',
                   class = 'multicol', style = "width: 100%",
                   multiInput("phy_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation types:</b></font>'),
                                                  bsButton("qp1", label="", icon=icon("question"), style="info", size="small")),
                              choices = mutationtypes,
                              choiceValues = NULL,
                              width = 800),
                   bsPopover("qp1", "Only SNPs with selected mutation effects will be used.",
                             trigger = "focus")
          ),
          
          p(h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soya lines:</b></font>'),
               bsButton("qp3", label="", icon=icon("question"), style="info", size="small"))),
          bsPopover("qp3", "Only the chosen soya lines will be used.",
                    trigger = "focus"),
          
          chooserInput("mychooserPhy", "Available frobs", "Selected frobs",
                       c(), all.soya.cho, size = 12, multiple = TRUE
          ),
          
          radioButtons("uploadPHY", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>SNP sites to be retained:</b></font>'),
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
        
      ),
      
      # Accession
      tabPanel(
        title = HTML("<strong style='font-size:18px'>Accession</strong>"),
        
        sidebarPanel(
          downloadButton("soya.info.txt", "Download information of all accessions"),
          
          p(h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select maize lines:</b></font>'),
               bsButton("qa1", label="", icon=icon("question"), style="info", size="small"))),
          bsPopover("qa1", "Only the chosen maize lines will be used.",
                    trigger = "focus"),
          
          chooserInput("mychooserA", "Available frobs", "Selected frobs",
                       all.soya.cho, c("Landrace"), size = 10, multiple = TRUE
          ),
          
          br(),
          downloadButton("sel.soya.info.txt", "Download information of selected accessions")
        ),
        
        mainPanel(
          h4(HTML('<i class="fa fa-table" aria-hidden="true"></i> <font size="4" color="red"><b>Information of selected maize lines</b></font>')),
          dataTableOutput("mytable1")
        )
      ),
      
      #Search
      tabPanel(
        
        title = HTML("<strong style='font-size:18px'>Search</strong>"), icon = icon("search"),
        
        sidebarPanel(
          selectInput("variety", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>select variety:</b></font>'),
                                            bsButton("qs0", label="", icon=icon("question"), style="info", size="small")), 
                      list(`G.soja` = list("Zhonghuang 13", "PI 562565", "PI 549046", "PI 578357"),
                           `Improved cultivar` = list("Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                      "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                      "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1"),
                           `Landrace` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                             "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye")) ,
                      selected = "Zhonghuang 13"
          ),
          bsPopover("qs0", "Select a database that you would like to blast",
                    trigger = "focus"),
          
          
          selectInput("regs", label = tags$div(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region:</b></font>'),
                                               bsButton("qs1", label="", icon=icon("question"), style="info", size="small")), 
                      choices = list("Gene ID" = "Geneid", 
                                     "Chromosome interval" = "interval"), 
                      selected = "Geneid"),
          bsPopover("qs1", "A genomic region can be determined by chromosome positions or gene locus. For example, chr2:20260371-22686979 or SoyZH13_02G148200",
                    trigger = "focus"),
          
          conditionalPanel(condition="input.regs == 'Geneid'", 
                           textInput("geneid", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Gene ID:</b></font>')),
                                     value = "SoyZH13_02G148200")
          ),
          
          conditionalPanel(condition="input.regs == 'interval'", 
                           selectInput("Chrinterval", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Chr:</b></font>'),choices = paste0("chr", 1:20), selected = "chr1"),
                           textInput("upinterval", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Start:</b></font>'),value = "20260371"),
                           textInput("downinterval", label = HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>End:</b></font>'),value = "22686979")
          ),          
          
          
          
          actionButton("submit7", strong("submit!",
                                         bsButton("qs2", label="", icon=icon("question"), style="info", size="small")
          ), styleclass = "success"),
          actionButton("clearSER", strong("Reset"), styleclass = "warning"),
          actionButton("SERExam", strong("Load example"), styleclass = "info"),
          conditionalPanel(condition="input.submit7 != '0'", busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
          bsPopover("qs2", "Whenever the genomic region or any option is updated, please click Go!",
                    trigger = "focus")
        ),
        mainPanel(
          fluidRow(
            column(4, downloadButton("sequence.txt", "Download Sequence", style = "width:100%;", class = "buttDown")),
            column(4,downloadButton("genesequence.txt", "Download Gene Sequence", style = "width:100%;", class = "buttDown")),
            column(4,downloadButton("genesinfo.txt", "Download Gene Information", style = "width:100%;", class = "buttDown"))
          ),
          
          textOutput("seq_title"),
          verbatimTextOutput("seq"),
          tags$head(tags$style("#seq_title{color: red;
                                       font-size: 22px;
                                       font-style: bold;
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
                                      }"
          ),
          tags$style("#cds{
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
                                      }"
          ),
          tags$style("#pro{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
                     
          }")
          ),
          DT::dataTableOutput("geneinfo"),
          
          textOutput("searchgenetitle"),
          verbatimTextOutput("searchgenesequence"),
          tags$head(tags$style("#searchgenetitle{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                      }"
          ),
          tags$style("#searchgenesequence{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
          }")
          ),
          
          textOutput("searchcdstitle"),
          verbatimTextOutput("searchcdssequence"),
          tags$head(tags$style("#searchcdstitle{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                      }"
          ),
          tags$style("#searchcdssequence{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
          }")
          ),
          textOutput("searchprotitle"),
          verbatimTextOutput("searchprosequence"),
          tags$head(tags$style("#searchprotitle{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                      }"
          ),
          tags$style("#searchprosequence{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
          }")
          ),
          textOutput("searchgffinfotitle"),
          DT::dataTableOutput("searchgffinfo"),
          tags$head(tags$style("#searchgffinfotitle{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                      }"
          ),
          tags$style("#searchgffinfo{
                     width = 100%;
                     max-height: 300px;
          }")
          ),
          textOutput("gffinfotitle"),
          DT::dataTableOutput("gffinfo"),
          tags$head(tags$style("#gffinfotitle{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                      }"
          ),
          tags$style("#gffinfo{
                     width = 100%;
                     max-height: 300px;
          }")
          ),
          
        )
      ),
      
      #Blast
      tabPanel(
        title = HTML("<strong style='font-size:18px'>Blast</strong>"),
        icon = icon("rocket", class = NULL, lib = "font-awesome"),
        
        tags$div(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Search database by sequence similarity using BLAST</b></font>'),
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
                                                       textAreaInput("BlastSeqPaste", label = h4("Input sequence"),
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
                                        choices = NULL, width = '100%', 
                                        choiceNames = BLASTdb.fl$Accession,
                                        choiceValues = BLASTdb.fl$Accession,
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
                                      textInput("BLASTev", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">E-value cutoff</font>'),
                                                                            bsButton("qBLASTev", label="", icon=icon("question"), style="info", size="small")), 
                                                value = "10", width = NULL, placeholder = NULL),
                                      bsPopover("qBLASTev", "Set E-value threshold to filter the BLAST output.",
                                                trigger = "focus"),
                                      
                                      selectInput("program", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">Program:</font>'),
                                                                            bsButton("qBLASTht", label="", icon=icon("question"), style="info", size="small")), 
                                                  choices=c("blastn","tblastn", "blastp", "blastx", "tblastx"), width = NULL),
                                      bsPopover("qBLASTht", "Choose alignment search tool.",
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
                                      downloadButton("blastDownIRFresult.txt", "Structure of LIRs in the BLAST result", style = "width:100%;", class = "buttDown")
                               ),
                               column(4,
                                      downloadButton("blastDownIRFfasta.txt", "Sequence of LIRs in the BLAST result", style = "width:100%;", class = "buttDown")
                               )
                             ),
                             
                             br(),
                             
                             conditionalPanel(condition="input.submitBLAST > 0", 
                                              tags$div(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>BLAST output</b></font>'),
                                                       bsButton("qBLASTresultHelp", label="", icon=icon("question"), style="info", size="small")),
                                              bsPopover("qBLASTresultHelp", title = "Click on a row to check the details of the LIR and the BLAST alignment!", trigger = "focus", content = NULL)
                             ),
                             
                             DT::dataTableOutput("BLASTresult"),
                             fixedRow(
                               column(2),
                               column(2,
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
                               column(6,
                                      textOutput("geneseq"),
                                      tags$head(tags$style("#geneseq{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                      }"
                                      )),
                                      tags$style("#alignment{
                                       max-width = 100%;
                                       white-space: pre-wrap;
                                       max-height: 300px;
                                      }"
                                      ),
                                      verbatimTextOutput("alignment", placeholder = FALSE)
                               ),
                               column(2)
                             )
                    )
                    
        )
      ),
      
      # Bulk download of data
      tabPanel(
        title = HTML("<strong style='font-size:18px'>Download</strong>"),icon = icon("fire-alt"),
        
        sidebarPanel(
          textInput("regBB", label = h5(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region:</b></font>'),
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
          
          p(h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select soya lines:</b></font>'),
               bsButton("qdl2", label="", icon=icon("question"), style="info", size="small"))),
          bsPopover("qdl2", "Only the chosen soya lines will be used.",
                    trigger = "focus"),
          
          chooserInput("mychooserD", "Available frobs", "Selected frobs",
                       c(), all.soya.cho, size = 12, multiple = TRUE
          ),
          multiInput("down_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation types:</b></font>'),
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
        h4(HTML('<i class="fa fa-table" aria-hidden="true"></i> <font size="4" color="red"><b>SNPs information in specified genomic region:</b></font>')),
        mainPanel(
          dataTableOutput("mytable2")
        )),
      
      ## About
      tabPanel(
        HTML("<strong style='font-size:18px'>About</strong>"), icon = icon("leanpub"), includeMarkdown("About.md"))
      
    )
  )

)

