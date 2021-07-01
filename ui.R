
library(shinyBS)

shinyUI(
  fluidPage(
    titlePanel(title=div(
      img(src="headerN.png", height = 100, width = 100),
      span("SoybeanGDB:", style = "font-size:40px;color:white;"), 
      span("a comprehensive genome database of soybean", style = "font-size:30px;color:white;"),
      style = "background-color:#0073B7;margin-left: -15px;margin-right: -15px;margin-top: -20px;margin-bottom: -10px;"
    ), windowTitle = "Welcome to Soybean!"),
    
    includeCSS("www/footer.css"),
    
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
      title = "",
      windowTitle = "A comprehensive genome database of soybean",
      id = "The_page",
      #home
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Home</strong>"), icon = icon("home"), 
        Homepage
      ),
      
      #Search sequence, cds, pro and gff information
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>Genomes</strong>"),
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Search by gene ID</strong>"), icon = icon("id-card"),
          sidebarPanel(
            tags$style("#searchgeneid {font-size:15px;font-family:sans-serif;}"),
            width = 3,
            fixedRow(
              column(12,
                     h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Search by gene ID</b></font>'),
                        bsButton("qsgsid", label="", icon=icon("question"), style="info", size="small")),
                     bsPopover("qsgsid", "Search any one of the 29 soybean genomes by gene ID", trigger = "focus")
              )
            ),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("variety_ID", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>')), 
                        list(`G. soja` = list("PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar` = list("Zhonghuang 13", "Williams 82", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                        "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                        "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1"),
                             `Landrace` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                               "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye")) ,
                        selected = "Zhonghuang 13"
            ),
            
            textInput("searchgeneid", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Input Gene ID</b></font>')),
                      value = "SoyZH13_02G148200"),
            
            shinysky::actionButton("submit_GSID", strong("Submit!"), styleclass = "success"),
            
            shinysky::actionButton("clearSERID", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("SERExamID", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="input.submit_GSID != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0))
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
            br(), br(),
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
            ),
            uiOutput("zhanwei1")
          )
        ),
        
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Search by genome location</strong>"), icon = icon("search-location"),
          
          sidebarPanel(
            width = 3,
            tags$style("#geneinterval {font-size:15px;font-family:sans-serif;}"),
            fixedRow(
              column(12,
                     h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Search by genome location</b></font>'),
                        bsButton("qsgsit", label="", icon=icon("question"), style="info", size="small")),
                     bsPopover("qsgsit", "Search any one of the 29 soybean genomes by genome location", trigger = "focus")
              )
            ),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("variety_IT", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>')), 
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
            
            shinysky::actionButton("submit_GSIT", strong("Submit!"), styleclass = "success"),
            
            shinysky::actionButton("clearSERIT", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("SERExamIT", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="input.submit_GSIT != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0))
            
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
            br(), br(),
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
            ),
            uiOutput("zhanwei2")
          )
        ),
        
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Bulk search by gene IDs</strong>"), icon = icon("file-download"),
          
          sidebarPanel(
            width = 3,
            tags$style("#BDG_Paste {font-size:15px;font-family:sans-serif;}"),
            fixedRow(
              column(12,
                     h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Bulk search by gene IDs</b></font>'),
                        bsButton("qsbdg", label="", icon=icon("question"), style="info", size="small")),
                     bsPopover("qsbdg", "Search any one of the 29 soybean genomes by multiple gene IDs", trigger = "focus")
              )
            ),
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("variety_BDG", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>')), 
                        list(`G. soja` = list("PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar` = list("Zhonghuang 13", "Williams 82", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                        "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                        "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1"),
                             `Landrace` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                               "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye")),
                        selected = "Zhonghuang 13"
            ),
            
            textAreaInput("BDG_Paste", label = HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Input a list of gene model names</font>'),
                          value = "SoyZH13_03G069002\nSoyZH13_03G069001\nSoyZH13_03G069100\nSoyZH13_01G071100", resize = "vertical", height='200px', width = '200%',
                          placeholder = "The Gene ID must in the database"),
            
            shinysky::actionButton("submit_BDG", strong("Submit!"), styleclass = "success"),
            shinysky::actionButton("clearBDG", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("BDGExam", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="input.submit_BDG != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0))
            
          ),
          mainPanel(
            width = 9,
            br(),
            fluidRow(
              column(3, uiOutput("downloadBDG01")),
              column(3, uiOutput("downloadBDG02")), 
              column(3, uiOutput("downloadBDG03")),
              column(3, uiOutput("downloadBDG04"))
            ),
            br(),
            
            htmlOutput("gffinfotitle_BDG"),
            DT::dataTableOutput("gffinfo_BDG"),
            br(), br(),
            tags$head(tags$style("#gffinfotitle_BDG{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#gffinfo_BDG{
                     width = 100%;
                     max-height: 300px;
          }")
            ),
            uiOutput("zhanwei3")
          )
        )
      ),
      
      
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>SNPs</strong>"),
        
        # Genome browse
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Browse</strong>"), icon = icon("folder-open-o"),
          
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
            bsPopover("qg4", "Extend the input genomic region to its downstream.", trigger = "focus"),
            
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
            bsPopover("qg3", "Only the chosen soybean accessions will be used.", trigger = "focus"),
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
            
            bsPopover("qg1", "Only SNPs with selected mutation effects will be used.", trigger = "focus"),
            fluidRow(
              column(6, actionButton("browsenone2", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
              column(6, actionButton("browseall2", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
            )
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
            plotly::plotlyOutput("gbrowser", height = '600px', width = '100%')
          )
        ),
        
        #Search SNP information
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Search</strong>"), icon = icon("search"),
          
          sidebarPanel(
            tags$style("#regBB {font-size:15px;font-family:sans-serif;}"),
            width = 3,
            fixedRow(
              column(12,
                     tags$div(h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Search SNPs</b></font>'),
                                 bsButton("qqsi", label="", icon=icon("question"), style="info", size="small"))),
                     bsPopover("qqsi", "Search SNPs among 2898 soybean accessions in an input genomic region or gene model.", trigger = "focus")
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
            bsPopover("qdl2", "Only the chosen soybean accessions will be used.", trigger = "focus"),
            
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
            br(),br()
          )
        ),
        
        
        # LDheatmap
        tabPanel(
          title = HTML("<strong style='font-size:20px'>LDheatmap</strong>"), icon = icon("project-diagram"),
          
          sidebarPanel(
            tags$style("#regL {font-size:15px;font-family:sans-serif;}"),
            tags$style("#ldpos {font-size:15px;font-family:sans-serif;}"),
            width = 3,
            
            fixedRow(
              column(12,
                     tags$div(h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>Linkage disequilibrium analysis</b></font>'),
                                 bsButton("qlda", label="", icon=icon("question"), style="info", size="small"))),
                     bsPopover("qlda", " For a specified genomic region or gene model, a heat map can be created to display the pairwise linkage disequilibrium between different SNP sites", trigger = "focus")
              )
            ),
            
            textInput("regL", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                         bsButton("q5", label="", icon=icon("question"), style="info", size="small")),
                      value = "SoyZH13_09G103313"),
            
            bsPopover("q5", "A genomic region can be determined by chromosome positions or gene locus. For example, chr9:24221992-24225930 or SoyZH13_09G103313.",
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
              bsPopover("qldg", "Y:gene model height; W:gene model width", trigger = "focus"),
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
            bsPopover("ql4", "Extend the input genomic region to its upstream.", trigger = "focus"),
            
            sliderInput("ldDown", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                     bsButton("ql5", label="", icon=icon("question"), style="info", size="small")
            ), min = 0, max = 50000, value = 0, ticks = FALSE),
            bsPopover("ql5", "Extend the input genomic region to its downstream.", trigger = "focus"),
            
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
            bsPopover("ql1", "Only SNPs with selected mutation effects will be used.", trigger = "focus"),
            
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
            
            bsPopover("ql3", "Only the chosen soybean accessions will be used.", trigger = "focus"),
            
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
          title = HTML("<strong style='font-size:20px'>Diversity</strong>"), icon = icon("chart-area"),
          
          sidebarPanel(
            tags$style("#regD {font-size:15px;font-family:sans-serif;}"),
            tags$style("#snpnumD {font-size:15px;font-family:sans-serif;}"),
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
            bsPopover("q10", "Whenever the genomic region or any plot option is updated, please click Submit!", trigger = "focus"),
            
            br(),
            h4(HTML('<i class="fa fa-cog" aria-hidden="true"></i> <font size="5" color="red"><b>Plot options</b></font>')),
            
            numericInput("snpnumD", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Number of SNPs in each window</b></font>'),
                                       bsButton("qd6", label="", icon=icon("question"), style="info", size="small")
            ), value = 10, min = 5, max = 20),
            bsPopover("qd6", "A specified genomic region would be split into non-overlapping window so that each window contains specified number of SNPs. The nucleotide diversity of all soybean accessions belong to the specified ecotypes in each window would be calculated.",
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
            bsPopover("qd7", "The nucleotide diversity of soybean accessions belong to the Numerator ecotype would be divided by the nucleotide diversity of soybean accessions belong to the Denominator ecotype for comparison.",
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
            ), min = 0, max = 50000, value = 10000, ticks = FALSE),
            bsPopover("qd4", "Extend the input genomic region to its upstream.", trigger = "focus"),
            
            sliderInput("divDown", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                      bsButton("qd5", label="", icon=icon("question"), style="info", size="small")
            ), min = 0, max = 50000, value = 10000, ticks = FALSE),
            bsPopover("qd5", "Extend the input genomic region to its downstream", trigger = "focus"),
            
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
          title = HTML("<strong style='font-size:20px'>AlleleFreq</strong>"), icon = icon("chart-pie"),
          
          sidebarPanel(
            width = 3,
            tags$style("#af_snp_site {font-size:15px;font-family:sans-serif;}"),
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
            bsPopover("qaf3", "Each SNP site should be a 10-digits integer. The first two digits represent the chromosome ID while the rest eight digits represent the genomic position of each SNP site. Each SNP site should take only one row!",
                      trigger = "focus"),
            
            shinyWidgets::pickerInput(
              inputId = "af_acc_group",
              label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Ecotypes to calculate allele frequency</b></font>')), 
              choices = c("Improved cultivar", "Landrace", "G. Soja"),
              selected = c("Improved cultivar", "Landrace", "G. Soja"),
              multiple = TRUE
            ),
            selectInput("jscolora", h4(HTML('<i class="fa fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Allele colors</b></font>'),
                                       bsButton("qaf2", label="", icon=icon("question"), style="info", size="small")), 
                        choices = c("steelblue, yellow2", "mediumspringgreen, mediumvioletred", "orchid, palegreen", "cornflowerblue, forestgreen"), selected = "steelblue, yellow2" ),
            bsPopover("qaf2", "Colors for the major and minor allele in the pie chart, respectively!", trigger = "focus"),
            
            numericInput("afHeight", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 650),
            numericInput("afWidth", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 800),
            
            shinysky::actionButton("submitaf1", strong("Submit!",
                                                       bsButton("qaf1", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            
            shinysky::actionButton("clearAf", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("AfExam", strong("Load example"), styleclass = "info"),
            
            conditionalPanel(condition="input.submitaf1 != '0'", shinysky::busyIndicator(HTML("<p style='color:red;font-size:30px;'>Calculation In progress...</p>"), wait = 0)),
            bsPopover("qaf1", "Whenever the input SNP sites or any option is updated, please click Submit!", trigger = "focus")
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
        )
        # Phylogenetic tree

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
                   bsPopover("qii", "Search INDELs among 2898 soybean accessions in an input genomic region or gene model", trigger = "focus")
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
          fluidRow(
            column(6, uiOutput("downloadindel01", style = "width:100%;")),
            column(6, uiOutput("downloadindel02", style = "width:100%;")),    
            
            #column(6, downloadButton("bulkdownloadindelInfo.txt", style = "width:100%;", "Download indels information", class = "buttDown")),
            #column(6, downloadButton("bulkdownloadindelALLInfo.txt", style = "width:100%;", "Download All indels information", class = "buttDown"))
            #tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
          ),
          br(),
          htmlOutput("indeltabletitle"),
          DT::dataTableOutput("indeltable"),
          br(), br(),
          tags$head(tags$style("#indeltabletitle{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                      }"
          )
          )
        )
      ),
      
      
      #TOOLS
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>Tools</strong>"), icon = icon("toolbox", lib = "font-awesome"),
        
        #Blast
        tabPanel(
          title = HTML("<strong style='font-size:20px'>BLAST</strong>"),
          icon = icon("rocket", class = NULL, lib = "font-awesome"),
          tags$style("#BLASTev {font-size:15px;font-family:sans-serif;}"),
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
                                        bsPopover("qBlastIn", "The input data must be DNA sequences in fasta format.", trigger = "focus"),
                                        
                                        conditionalPanel(condition="input.In_blast == 'paste'", 
                                                         textAreaInput("BlastSeqPaste", label = HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Input sequence</font>'),
                                                                       value = "", resize = "vertical", height='400px', width = '200%',
                                                                       placeholder = "The input sequences must be in fasta format")
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
                                                    choices=c("Genome Sequence", "Protein Sequence", "Gene Sequence", "CDS sequence"), width = NULL),
                                        bsPopover("qBLASTpdata", "Set the type of BLAST database to search against.", 
                                                  trigger = "focus"),
                                        textInput("BLASTev", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">E-value cutoff</font>'),
                                                                              bsButton("qBLASTev", label="", icon=icon("question"), style="info", size="small")), 
                                                  value = "10", width = NULL, placeholder = NULL),
                                        bsPopover("qBLASTev", "Set E-value threshold to filter the BLAST output.",
                                                  trigger = "focus"),
                                        
                                        conditionalPanel(condition="input.program_database == 'Genome Sequence'", 
                                                         selectInput("programdna", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">Program</font>')), 
                                                                     choices=c("blastn","tblastn", "tblastx"), width = NULL),
                                        ),
                                        conditionalPanel(condition="input.program_database == 'Protein Sequence'", 
                                                         selectInput("programpro", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">Program</font>')), 
                                                                     choices=c("blastp", "blastx"), width = NULL),
                                        ),
                                        conditionalPanel(condition="input.program_database == 'Gene Sequence'", 
                                                         selectInput("programgene", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">Program</font>')), 
                                                                     choices=c("blastn","tblastn", "tblastx"), width = NULL),
                                        ),
                                        conditionalPanel(condition="input.program_database == 'CDS sequence'", 
                                                         selectInput("programcds", label = tags$div(HTML('<i class="fa fa-play"></i> <font size="4" color="red">Program</font>')), 
                                                                     choices=c("blastn","tblastn", "tblastx"), width = NULL),
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
                               br(),
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
                                          width: 800px;
                                          padding: 6px 12px; 
                                          white-space: pre-wrap;
                                          height: 400px;
                                          background: white;
                                        }"
                                        ),
                                        shinycssloaders::withSpinner(verbatimTextOutput("alignment", placeholder = FALSE)),
                                        br(),
                                        br()
                                 )
                               )
                      )
          )
        ),
        
        
        ##primer3
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Primer-Design</strong>"), icon = icon("drafting-compass"),
          
          sidebarPanel(
            tags$style("#primergene {font-size:15px;font-family:sans-serif;}"),
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
            
            
            textInput("primergene", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Genomic region</b></font>')
                                               , bsButton("qpr0", label = "", icon = icon("question"), style = "info", size = "small")),value = "SoyZH13_02G148200"),
            bsPopover("qpr0", "A genomic region can be determined by chromosome positions or gene locus. For example, chr2:17603220-17604802 or SoyZH13_02G148200.", trigger = "focus"),
            
            sliderInput("PRIMER_OPT_SIZE", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Optimal primer size (bp)</b></font>'),
                                                      bsButton("qpr1", label="", icon=icon("question"), style="info", size="small")),min = 16, max = 30, value = 20),
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
            bsPopover("qpr7", "Minimum allowed percentage of Gs and Cs in any primer and Maximum allowable percentage of Gs and Cs in any primer generated by Primer.", trigger = "focus"),
            
            textInput("PRIMER_MAX_NS_ACCEPTED", label = h4(HTML("<i class='fa fa fa-circle' aria-hidden='true'></i> <font size='4' color='red'><b>Max #N's accepted</b></font>")
                                                           , bsButton("qpr8", label = "", icon = icon("question"), style = "info", size = "small")),value = "0"),
            bsPopover("qpr8", "Maximum number of unknown bases (N) allowable in any primer.", trigger = "focus"),
            
            fluidRow(
              column(6,textInput("PRIMER_MAX_POLY_X", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max Poly-X</b></font>')
                                                                 , bsButton("qpr9", label = "", icon = icon("question"), style = "info", size = "small")),value = "10"),
                     bsPopover("qpr9", "The maximum allowable length of a mononucleotide repeat, for example AAAAAA.", trigger = "focus")),
              column(6,textInput("PRIMER_INTERNAL_MAX_POLY_X", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Max Internal Poly-X</b></font>')
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
                     , bsPopover("qpr13", "The maximum allowable 3\\'-anchored global.", trigger = "focus")),
              column(6,textInput("PRIMER_MAX_SELF_END_TH", label = h4(HTML("<i class='fa fa fa-circle' aria-hidden='true'></i> <font size='3' color='red'><b>Max 3' Pair Complementarity</b></font>")
                                                                      , bsButton("qpr14", label = "", icon = icon("question"), style = "info", size = "small")), value = "35.0")
                     , bsPopover("qpr14", "Same as PRIMER_MAX_SELF_END but is based on thermodynamical approach - the stability of structure is analyzed. The value of tag is expressed as melting temperature. ", trigger = "focus"))
            ),
            
            textInput("PRIMER_PRODUCT_SIZE_RANGE", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="3" color="red"><b>Product Size Ranges</b></font>')
                                                              , bsButton("qpr15", label = "", icon = icon("question"), style = "info", size = "small")), value = "100-300,300-600,600-1000")
            , bsPopover("qpr15", "If one desires PCR products in either the range from 100 to 300 bases or in the range from 300 to 600 (or 600 to 1000) bases then one would set this parameter to 100-300,300-600,600-1000.", trigger = "focus"),
            
            shinysky::actionButton("submitprimer", strong("Submit!",
                                                          bsButton("qprGO", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            shinysky::actionButton("clearprimer", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("primerExam", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="submitprimer != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qprGO", "Whenever the genomic region or any option is updated, please click Go!",
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
            )
          )
        ),
        
        
        # Orthologous
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Orthologous</strong>"),icon = icon("people-arrows"),
          tags$style("#ORT_ID {font-size:15px;font-family:sans-serif;}"),
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
            DT::dataTableOutput("ortID"),
            br(),
            br(),
            uiOutput("zhanwei4")
          )
        ),
        
        
        #GO Annotation
        tabPanel(
          title = HTML("<strong style='font-size:20px'>GO Annotation</strong>"),icon = icon("sitemap"),
          sidebarPanel(
            width = 3,
            tags$style("#GOGENErePaste {font-size:15px;font-family:sans-serif;}"),
            h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>GO Annotation</b></font>'),
               bsButton("qGOre0", label="", icon=icon("question"), style="info", size="small")),
            bsPopover("qGOre0", "Perform GO annotation for input gene sets.", trigger = "focus"),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("GO_variety_ID", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>'),
                                                    bsButton("qGOre1", label="", icon=icon("question"), style="info", size="small")), 
                        list(`G.soja` = list("PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar` = list("Zhonghuang 13", "Williams 82", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                        "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                        "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1"),
                             `Landrace` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                               "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye")) ,
                        selected = "Zhonghuang 13"
            ),
            bsPopover("qGOre1", "Select a genome for the input gene set.",
                      trigger = "focus"),
            
            selectInput("In_GOre", label = tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Paste or upload input data?</font>'),
                                                    bsButton("qGOre2", label="", icon=icon("question"), style="info", size="small")
            ), choices = list("Paste input data" = "paste2", 
                              "Upload input data" = "upload2"), 
            selected = "paste"),
            bsPopover("qGOre2", "The input data must be multiple gene model names.", trigger = "focus"),
            
            conditionalPanel(condition="input.In_GOre == 'paste2'", 
                             textAreaInput("GOGENErePaste", label = HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Input a list of gene model names</font>'),
                                           value = "", resize = "vertical", height='200px', width = '200%',
                                           placeholder = "One Gene ID in one row.")
            ),
            conditionalPanel(condition="input.In_GOre == 'upload2'", 
                             fileInput("GOGENEreUpload",
                                       label = h4("Upload file"), multiple = FALSE, width = "100%"),
                             downloadButton("GOre_Input.txt", "Example input gene set", style = "width:100%;", class = "buttDown")
            ),
            
            sliderInput("GOnrow", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Maximum number of GO terms</b></font>'),
                                     bsButton("qGOid4", label="", icon=icon("question"), style="info", size="small")
            ), min = 1, max = 50, value = 30, ticks = FALSE),
            bsPopover("qGOid4", "Maximum number of GO terms to display",
                      trigger = "focus"),
            
            shinysky::actionButton("GOre_sbumit", strong("Submit!",
                                                         bsButton("qGO3", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            
            shinysky::actionButton("clearGOre", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("GOreExam", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="ORT_sbumit != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qGO3", "Click this button to start the annotation!",
                      trigger = "focus"),
            
            numericInput("goanheight", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>'), value = 800),
            numericInput("goanwidth", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>'), value = 1300)
          ),
          
          mainPanel(
            width = 9,
            htmlOutput("GOidout_title"),
            DT::dataTableOutput("GOidout"),
            br(),
            uiOutput("GOre1barplotif"),
            #plotOutput("GOidbarplot", height = "800px", width = "100%"),
            br(), br()
          )
        ),
        
        #GO Enrichment
        tabPanel(
          title = HTML("<strong style='font-size:20px'>GO Enrichment</strong>"),icon = icon("sitemap"),
          
          sidebarPanel(
            width = 3,
            tags$style("#GOGENEgePaste {font-size:15px;font-family:sans-serif;}"),
            h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>GO enrichment analysis</b></font>'),
               bsButton("qGOge0", label="", icon=icon("question"), style="info", size="small")),
            bsPopover("qGOge0", "Perform GO enrichment analysis for input gene sets.", trigger = "focus"),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("GOge_variety_ID", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>'),
                                                      bsButton("qGOge1", label="", icon=icon("question"), style="info", size="small")), 
                        list(`G.soja` = list("PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar` = list("Zhonghuang 13", "Williams 82", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                        "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                        "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1"),
                             `Landrace` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                               "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye")) ,
                        selected = "Zhonghuang 13"
            ),
            bsPopover("qGOge1", "Select a genome for the input gene set.", trigger = "focus"),
            
            selectInput("In_GOge", label = tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Paste or upload input data?</font>'),
                                                    bsButton("qGOge2", label="", icon=icon("question"), style="info", size="small")
            ), choices = list("Paste input data" = "paste3", 
                              "Upload input data" = "upload3"), selected = "paste"),
            bsPopover("qGOge2", "The input data must be multiple gene model names.", trigger = "focus"),
            
            conditionalPanel(condition="input.In_GOge == 'paste3'", 
                             textAreaInput("GOGENEgePaste", label = HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Input a list of gene model names</font>'),
                                           value = "", resize = "vertical", height='200px', width = '200%',
                                           placeholder = "One gene ID in one row.")
            ),
            conditionalPanel(condition="input.In_GOge == 'upload3'", 
                             fileInput("GOGENEgeUpload",
                                       label = h4("Upload file"), multiple = FALSE, width = "100%"),
                             downloadButton("GOge_Input.txt", "Example input gene set", style = "width:100%;", class = "buttDown")
            ),
            
            sliderInput("GOgenrow", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Maximum number of GO terms</b></font>'),
                                       bsButton("qGOge3", label="", icon=icon("question"), style="info", size="small")
            ), min = 1, max = 50, value = 30, ticks = FALSE),
            bsPopover("qGOge3", "Maximum number of GO terms to display", trigger = "focus"),
            
            sliderInput("GOp", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>P-adjust</b></font>'),
                                  bsButton("qGOid5", label="", icon=icon("question"), style="info", size="small")
            ), min = 0.01, max = 1, value = 0.05, ticks = FALSE),
            bsPopover("qGOid5", "Maximum adjusted p value allowed.",
                      trigger = "focus"),
            
            sliderInput("GOq", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Q-value</b></font>'),
                                  bsButton("qGOid6", label="", icon=icon("question"), style="info", size="small")
            ), min = 0.01, max = 1, value = 0.05, ticks = FALSE),
            bsPopover("qGOid6", "Maximum Q value allowed.",
                      trigger = "focus"),
            shinysky::actionButton("GOge_sbumit", strong("Submit!",
                                                         bsButton("qge4", label="", icon=icon("question"), style="info", size="small")), styleclass = "success"),
            
            shinysky::actionButton("clearGOge", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("GOgeExam", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="GOge_sbumit != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qge4", "Click this button to start the analysis!",
                      trigger = "focus"),
            fluidRow(
              h5(HTML('<i class="fa fa-cog"></i> <font size="5" color="red"><b>Molecular Function</b></font>')),
              column(6,numericInput("goheight1", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>'), value = 400)),
              column(6,numericInput("gowidth1", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>'), value = 1300))
            ),
            fluidRow(
              h5(HTML('<i class="fa fa-cog"></i> <font size="5" color="red"><b>Biological Process</b></font>')),
              column(6,numericInput("goheight2", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>'), value = 400)),
              column(6,numericInput("gowidth2", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>'), value = 1300))
            ),
            fluidRow(
              h5(HTML('<i class="fa fa-cog"></i> <font size="5" color="red"><b>Cellular Component</b></font>')),
              column(6,numericInput("goheight3", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>'), value = 300)),
              column(6,numericInput("gowidth3", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>'), value = 1300))
            )
          ),
          
          mainPanel(
            width = 9,
            htmlOutput("GOgeout1_title"),
            DT::dataTableOutput("GOgeout1"),
            uiOutput("GOge1barplotif"),
            #plotOutput("GOge1barplot", height = "800px", width = "100%"),
            htmlOutput("GOgeout2_title"),
            DT::dataTableOutput("GOgeout2"),
            uiOutput("GOge2barplotif"),
            #plotOutput("GOge2barplot", height = "800px", width = "100%"),
            htmlOutput("GOgeout3_title"),
            DT::dataTableOutput("GOgeout3"),
            uiOutput("GOge3barplotif"),
            #plotOutput("GOge3barplot", height = "800px", width = "100%"),
            br(), br()
          )
        ),
        
        #KEGG Annotation
        tabPanel(
          title = HTML("<strong style='font-size:20px'>KEGG Annotation</strong>"),icon = icon("kaggle"),
          
          sidebarPanel(
            width = 3,
            tags$style("#KEGG_GENErePaste {font-size:15px;font-family:sans-serif;}"),
            h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>KEGG Annotation</b></font>'),
               bsButton("qKEre0", label="", icon=icon("question"), style="info", size="small")),
            bsPopover("qKEre0", "Perform KEGG annotation for input gene sets.", trigger = "focus"),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("KE_variety_ID", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>'),
                                                    bsButton("qKEre1", label="", icon=icon("question"), style="info", size="small")), 
                        list(`G.soja` = list("PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar` = list("Zhonghuang 13", "Williams 82", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                        "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                        "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1"),
                             `Landrace` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                               "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye")) ,
                        selected = "Zhonghuang 13"
            ),
            bsPopover("qKEre1", "Select a genome for the input gene set.",
                      trigger = "focus"),
            
            selectInput("In_KEre", label = tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Paste or upload input data?</font>'),
                                                    bsButton("qKEre2", label="", icon=icon("question"), style="info", size="small")
            ), choices = list("Paste input data" = "paste2", 
                              "Upload input data" = "upload2"), 
            selected = "paste"),
            bsPopover("qKEre2", "The input data must be multiple gene model names.", trigger = "focus"),
            
            conditionalPanel(condition="input.In_KEre == 'paste2'", 
                             textAreaInput("KEGG_GENErePaste", label = HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Input a list of gene model names</font>'),
                                           value = "", resize = "vertical", height='200px', width = '200%',
                                           placeholder = "One gene ID in one row.")
            ),
            conditionalPanel(condition="input.In_KEre == 'upload2'", 
                             fileInput("KEGG_GENEreUpload",
                                       label = h4("Upload file"), multiple = FALSE, width = "100%"),
                             downloadButton("KOre_Input.txt", "Example input gene set", style = "width:100%;", class = "buttDown")
            ),
            sliderInput("KOnrow", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Maximum number of KEGG pathways</b></font>'),
                                     bsButton("qKEid4", label="", icon=icon("question"), style="info", size="small")
            ), min = 1, max = 50, value = 30, ticks = FALSE),
            bsPopover("qKEid4", "Maximum number of KEGG pathways to display",
                      trigger = "focus"),
            
            
            shinysky::actionButton("KEGGre_sbumit", strong("Submit!",
                                                           bsButton("qKO3", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            shinysky::actionButton("clearKOre", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("KOreExam", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="ORT_sbumit != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qKO3", "Click this button to start the annotation!",
                      trigger = "focus"),
            numericInput("kegganheight", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>'), value = 800),
            numericInput("kegganwidth", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>'), value = 1300),
            
          ),
          mainPanel(
            width = 9,
            htmlOutput("KOidout_title"),
            DT::dataTableOutput("KOidout"),
            br(),
            uiOutput("KOidbarplotif"),
            #plotOutput("KOidbarplot", height = "800px", width = "100%"),
            br(), br()
          )
        ),
        
        #KEGG Enrichment
        tabPanel(
          title = HTML("<strong style='font-size:20px'>KEGG Enrichment</strong>"),icon = icon("kaggle"),
          
          sidebarPanel(
            width = 3,
            tags$style("#KEGG_GENErePaste1 {font-size:15px;font-family:sans-serif;}"),
            h4(HTML('<i class="fas fa-dot-circle" aria-hidden="true"></i> <font size="5" color="red"><b>KEGG enrichment analysis</b></font>'),
               bsButton("qKE0", label="", icon=icon("question"), style="info", size="small")),
            bsPopover("qKE0", "Perform enrichment analysis on gene sets.", trigger = "focus"),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("KEGG_variety_ID", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>'),
                                                      bsButton("qKE1", label="", icon=icon("question"), style="info", size="small")), 
                        list(`G.soja` = list("PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar` = list("Zhonghuang 13", "Williams 82", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                        "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                        "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1"),
                             `Landrace` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                               "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye")) ,
                        selected = "Zhonghuang 13"
            ),
            bsPopover("qKE1", "Select a genome for the input gene set.",
                      trigger = "focus"),
            
            selectInput("In_KE", label = tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Paste or upload input data?</font>'),
                                                  bsButton("qKE2", label="", icon=icon("question"), style="info", size="small")
            ), choices = list("Paste input data" = "paste4", 
                              "Upload input data" = "upload4"), 
            selected = "paste4"),
            bsPopover("qKE2", "The input data must be multiple gene model names.", trigger = "focus"),
            
            conditionalPanel(condition="input.In_KE == 'paste4'", 
                             textAreaInput("KEGG_GENErePaste1", label = HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Input a list of gene model names</font>'),
                                           value = "", resize = "vertical", height='200px', width = '200%',
                                           placeholder = "One gene ID in one row.")
            ),
            conditionalPanel(condition="input.In_KE == 'upload4'", 
                             fileInput("KEGG_GENEreUpload1",
                                       label = h4("Upload file"), multiple = FALSE, width = "100%"),
                             downloadButton("KO_Input.txt", "Example input gene set", style = "width:100%;", class = "buttDown")
            ),
            
            sliderInput("KEnrow", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Maximum number of KEGG pathways</b></font>'),
                                     bsButton("qKEid3", label="", icon=icon("question"), style="info", size="small")
            ), min = 1, max = 50, value = 30, ticks = FALSE),
            bsPopover("qKEid3", "Maximum number of KEGG pathways to display",
                      trigger = "focus"),
            
            sliderInput("KEp", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>P-adjust</b></font>'),
                                  bsButton("qKEid5", label="", icon=icon("question"), style="info", size="small")
            ), min = 0.01, max = 1, value = 0.05, ticks = FALSE),
            bsPopover("qKEid5", "Maximum adjusted p value allowed.",
                      trigger = "focus"),
            
            sliderInput("KEq", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Q-value</b></font>'),
                                  bsButton("qKEid6", label="", icon=icon("question"), style="info", size="small")
            ), min = 0.01, max = 1, value = 0.05, ticks = FALSE),
            bsPopover("qKEid6", "Maximum Q value allowed.",
                      trigger = "focus"),
            shinysky::actionButton("KEGG_sbumit", strong("Submit!",
                                                         bsButton("qKE3", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            shinysky::actionButton("clearKO", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("KOExam", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="ORT_sbumit != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qKE3", "Click this button to start the analysis!",
                      trigger = "focus"),
            numericInput("keggheight", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>'), value = 800),
            numericInput("keggwidth", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>'), value = 1300)
          ),
          
          mainPanel(
            width = 9,
            htmlOutput("KEidout_title"),
            DT::dataTableOutput("KEidout"),
            br(),
            uiOutput("KEidbarplotif"),
            #plotOutput("KEidbarplot", height = "800px", width = "100%"),
            br(), br()
          )
        )
      ),
      
      
      # JBrowse
      tabPanel(
        title = HTML("<strong style='font-size:20px'>JBrowse</strong>"), icon = icon("list"),
        h4(HTML('<i class="fas fa-dot-circle"></i> <font size="5" color="red"><b>JBrowse of 29 soybean genomes</b></font>'),
           bsButton("qjb1", label="", icon=icon("question"), style="info", size="small")),
        bsPopover("qjb1", "Click the name of a soybean genome to view in JBrowse", trigger = "focus"),
        DT::dataTableOutput('JBrowsetable'),
        br()
      ),
      
      
      #Expression
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>Expression</strong>"),
        #Gene expression
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Gene expression analysis</strong>"),
          sidebarPanel(
            #Expression pattern of protein coding genes in 27 different soybean samples
            tags$style("#gtsgeneid {font-size:15px;font-family:sans-serif;}"),
            width = 3,
            textInput("gtsgeneid", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Input gene ID</b></font>'), 
                                              bsButton("qsgts1", label="", icon=icon("question"), style="info", size="small")),
                      value = "SoyZH13_05G201900"),
            bsPopover("qsgts1", "Input a gene ID of the Zhonghuang 13 genome!",
                      trigger = "focus"),
            shinyWidgets::multiInput("Gexp_mut_group", h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Choose tissues/stages</b></font>'),
                                                          bsButton("qsgex", label="", icon=icon("question"), style="info", size="small")),
                                     choices = c("cotyledon-1", "root", "stem", "leafbud-1", "leafbud-2", "leaf-1", "cotyledon-2", "stem-2", "leafbud-3", "leaf-2", "flo-1", "shoot_meristem", "flo-2", "flo-3", "flo-4", "flo-5", "pod&seed-1", "pod&seed-2", "pod&seed-3", "pod-1", "pod-2", "pod-3", "seed-1", "seed-2", "seed-3", "seed-4", "seed-5"),
                                     selected = c("cotyledon-1", "root", "stem", "leafbud-1", "leafbud-2", "leaf-1", "cotyledon-2", "stem-2", "leafbud-3", "leaf-2", "flo-1", "shoot_meristem", "flo-2", "flo-3", "flo-4", "flo-5", "pod&seed-1", "pod&seed-2", "pod&seed-3", "pod-1", "pod-2", "pod-3", "seed-1", "seed-2", "seed-3", "seed-4", "seed-5"),
                                     width = 800,
                                     options = list(
                                       enable_search = TRUE,
                                       non_selected_header = "Choose from:",
                                       selected_header = "You have selected:"
                                     )
            ),
            
            fluidRow(
              column(6, actionButton("accessiongexnone", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
              column(6, actionButton("accessiongexall", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
            ),
            br(),
            
            shinyWidgets::actionBttn("Searchname_description", "Information of 27 collected samples", 
                                     block = TRUE, size = "sm", style="unite", color="default")
            ,
            bsModal("name-description", "Information of 27 collected samples", "Searchname_description", size = "large", 
                   DT::dataTableOutput("expressionname1", width = "100%")
            ),
            
            br(),
            
            bsPopover("qsgex", "Choose one or multiple tissues and development stages.",
                      trigger = "focus"),
            shinysky::actionButton("submit_GSgst", strong("Submit!",
                                                          bsButton("qsgts2", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            
            shinysky::actionButton("cleargst", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("gstExamID", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="submit_GSgst != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qsgts2", "Click this button to start the analysis!",
                      trigger = "focus"),
            br(),
            numericInput("gstheight", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>'), value = 800),
            numericInput("gstwidth", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>'), value = 1300)
            
          ),
          
          mainPanel(
            width = 9,
            htmlOutput("gstout_title"),
            DT::dataTableOutput("gstoutresult", width = "100%"),
            uiOutput("gstbarplotif")
            #plotOutput("gstbarplot", height = "800px", width = "100%")
          )
        ),
        
        #Gene co-correlation
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Gene co-expression analysis</strong>"),
          sidebarPanel(
            #Expression pattern of protein coding genes in 27 different soybean samples
            width = 3,
            tags$style("#genecorPaste {font-size:15px;font-family:sans-serif;}"),
            textAreaInput("genecorPaste", label = h4(HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Input a list of gene model names</b></font>'), 
                                                     bsButton("qsgcor1", label="", icon=icon("question"), style="info", size="small")),
                          value = "", resize = "vertical", height='200px', width = '200%',
                          placeholder = "One Gene ID in one row"),
            bsPopover("qsgcor1", "Input at least two Gene IDs of the Zhonghuang 13 genome!", trigger = "focus"),
            shinysky::actionButton("submit_GSgcor", strong("Submit!",
                                                           bsButton("qsgcor2", label="", icon=icon("question"), style="info", size="small")), styleclass = "success"),
            shinysky::actionButton("cleargcor", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("gcorExamID", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="submit_GSgcor != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qsgcor2", "Click this button to start the analysis!",
                      trigger = "focus"),
            numericInput("genecorheight", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>'), value = 1000),
            numericInput("genecorwidth", HTML('<i class="fa fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>'), value = 1000),
          ),
          
          mainPanel(
            width = 9,
            htmlOutput("gcorout_title"),
            DT::dataTableOutput("gcordata", width = "100%"),
            uiOutput("genecorplotif"),
            br(),
            br()
            #plotOutput("genecorplot", height = "800px", width = "100%"),
          )
        )
      ),
      
      
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>Help</strong>"), icon = icon("book"),
        
        # Accession
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Accessions</strong>"),
          
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
            HTML('<i class="fa fa-circle" aria-hidden="true" style="color:red"></i> <font size="5" color="red"><b>Information of selected soybean accessions</b></font>'),
            DT::dataTableOutput("mytable1"),
            br(),
            br()
          )
        ),
        
        # Data source
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Data source</strong>"),
          column(2),
          column(8,
                 h4(HTML('<i class="fas fa-dot-circle"></i> <font size="5" color="red"><b>29 soybean genomes</b></font>')),
                 DT::dataTableOutput('Downloadtable', width = "100%"),
                 br(),
                 
                 h4(HTML('<i class="fas fa-dot-circle"></i> <font size="5" color="red"><b>SNPs & INDELs of 2898 soybean accessions</b></font>')),
                 h4(a("https://ngdc.cncb.ac.cn/gvm/getProjectDetail?project=GVM000063", href="https://ngdc.cncb.ac.cn/gvm/getProjectDetail?project=GVM000063", target="_blank")),
                 br(),
                 
                 h4(HTML('<i class="fas fa-dot-circle"></i> <font size="5" color="red"><b>Expression level of protein-coding genes in the genome of Zhonghuang 13</b></font>')),
                 h4(a("https://link.springer.com/article/10.1007/s11427-019-9822-2", href="https://link.springer.com/article/10.1007/s11427-019-9822-2", target="_blank")),
                 
                 br(),
                 br()
          ),
          column(2)
        ),
        
        # Link
        tabPanel(HTML("<strong style='font-size:20px'>Links</strong>"),
                 column(2),
                 column(8, includeMarkdown("./Links.md")),
                 column(2)
        ),
        
        # Tutorial
        tabPanel(HTML("<strong style='font-size:20px'>Tutorial</strong>"),
                 sidebarPanel(
                   style = "position:fixed;width:23%;",
                   h4("Table of contents"),
                   width = 3,
                   includeMarkdown("Tutorial_toc.md")
                 ),
                 
                 mainPanel(width = 9,
                           column(10, includeMarkdown("Tutorial.md")),
                           column(2)
                 )
        )
      ),
      
      footer = footerTagList
    )
 )
)

