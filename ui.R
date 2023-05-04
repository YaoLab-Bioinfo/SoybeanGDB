
library(shinyBS)

shinyUI(
  fluidPage(
    tags$head(includeHTML(("google-analytics.html"))),
    titlePanel(title=div(
      img(src="headerN.png", height = 100, width = 100),
      span("SoybeanGDB:", style = "font-size:40px;color:white;"), 
      span("a comprehensive genome database of soybean", style = "font-size:30px;color:white;"),
      style = "background-color:#0073B7;margin-left: -15px;margin-right: -15px;margin-top: -20px;margin-bottom: -10px;"
    ), windowTitle = "Welcome to SoybeanGDB!"),
    
    includeCSS("www/footer.css"),
    #includeScript("./www/script.js"),
    
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
          title = HTML("<strong style='font-size:20px'>Search by gene IDs</strong>"), icon = icon("id-card"),
          
          sidebarPanel(
            width = 3,
            tags$style("#BDG_Paste {font-size:15px;font-family:sans-serif;}"),
            fixedRow(
              column(12,
                     h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Search by gene IDs</b></font>'),
                        bsButton("qsgsid", label="", icon=icon("question"), style="info", size="small")),
                     bsPopover("qsgsid", "Search any one of the 39 soybean genomes by gene IDs", trigger = "focus")
              )
            ),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("variety_BDG", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>')), 
                        list(`Wild soybean (Glycine soja)` = list("PI 483463", "PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar (Glycine max)` = list("Zhonghuang 13", "Williams 82", "Lee", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                        "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                        "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1", "Tianlong1", "Hwangkeum"),
                             `Landrace (Glycine max)` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                               "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye"),
                             `Perennial Glycine` = list("Glycine cyrtoloba", "Glycine dolichocarpa", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3"),
                             `All genomes` = list("All genomes")
                             ) ,
                        selected = "Zhonghuang 13"
            ),
            conditionalPanel(condition="input.variety_BDG == 'All genomes'", 
                             textInput("BDG_Paste1", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input a single gene ID</b></font>')), value = "SoyZH13_01G071100"),
                             
            ),
            conditionalPanel(condition="input.variety_BDG != 'All genomes'", 
                             textAreaInput("BDG_Paste", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input a list of gene model names</b></font>')),
                                           value = "SoyZH13_03G069003\nSoyZH13_03G069001\nSoyZH13_03G069100\nSoyZH13_01G071100", resize = "vertical", height='200px', width = '200%',
                                           placeholder = "The Gene ID must in the database"),
                             
            ),
            
            
            shinysky::actionButton("submit_GSID", strong("Submit!", 
                                                         bsButton("qsgsid1", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            bsPopover("qsgsid1", "Whenever the Gene ID is updated, please click Submit!",
                      trigger = "focus"),
            shinysky::actionButton("clearSERID", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("SERExamID", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="input.submit_GSID != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0))
          ),
          
          mainPanel(
            width = 9,
            fluidRow(
              column(3, uiOutput("downloadBDG01")),
              column(3, uiOutput("downloadBDG02")), 
              column(3, uiOutput("downloadBDG03")),
              column(3, uiOutput("downloadBDG04"))
            ),
            
            htmlOutput("geneinfoid_title"),
            column(12, DT::dataTableOutput("geneinfoid")),
            tags$head(tags$style("#geneinfoid_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#geneinfoid{
                     width = 100%;
                     max-height: 300px;
            }")
            ),
            
            htmlOutput("geneticIDstructure_title"),
            uiOutput("geneticIDstructureif"),
            #plotOutput("geneticIDstructure", width = "100%", height = "100px"),
            
            tags$head(tags$style("#geneticIDstructure_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            )),
            
            htmlOutput("gene_title_id"),
            verbatimTextOutput("gene_id"),
            tags$head(tags$style("#gene_title_id{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#gene_id{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
            }")
            ),
            
            htmlOutput("cds_title_id"),
            verbatimTextOutput("cds_id"),
            tags$head(tags$style("#cds_title_id{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#cds_id{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
            }")
            ),
            
            htmlOutput("cdna_title_id"),
            verbatimTextOutput("cdna_id"),
            tags$head(tags$style("#cdna_title_id{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#cdna_id{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
            }")
            ),
            
            htmlOutput("pro_title_id"),
            shiny::verbatimTextOutput("pro_id"),
            tags$head(tags$style("#pro_title_id{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#pro_id{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
                     
            }")
            ),
            
            htmlOutput("Functionaltitle_id"),
            column(12,
                   DT::dataTableOutput("Functional_id")),
            tags$head(tags$style("#Functionaltitle_id{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#Functional_id{
                     width = 100%;
                     max-height: 300px;
            }")
            ),
            
            htmlOutput("gffinfotitle_id"),
            column(12,
                   DT::dataTableOutput("gffinfo_id")),
            tags$head(tags$style("#gffinfotitle_id{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#gffinfo_id{
                     width = 100%;
                     max-height: 300px;
            }")
            ),
            column(12, br() ),
            uiOutput("zhanwei3")
          )
        ),
        
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Search by genome location</strong>"), icon = icon("search-location"),
          
          sidebarPanel(
            width = 3,
            tags$style("#geneinterval {font-size:15px;font-family:sans-serif;}"),
            fixedRow(
              column(12,
                     h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Search by genome location</b></font>'),
                        bsButton("qsgsit", label="", icon=icon("question"), style="info", size="small")),
                     bsPopover("qsgsit", "Search any one of the 39 soybean genomes by genome location", trigger = "focus")
              )
            ),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("variety_IT", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>')), 
                        list(`Wild soybean (Glycine soja)` = list("PI 483463", "PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar (Glycine max)` = list("Zhonghuang 13", "Williams 82", "Lee", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                                      "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                                      "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1", "Tianlong1", "Hwangkeum"),
                             `Landrace (Glycine max)` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                                             "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye"),
                             `Perennial Glycine` = list("Glycine cyrtoloba", "Glycine dolichocarpa", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3") ) ,
                        selected = "Zhonghuang 13"
            ),
            
            textInput("geneinterval", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input a genomic region</b></font>')),
                      value = "chr1:20260371-20686979"),
            
            shinysky::actionButton("submit_GSIT", strong("Submit!", 
                                                         bsButton("qsgsit1", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            
            bsPopover("qsgsit1", "Whenever the  genome location is updated, please click Submit!",
                      trigger = "focus"),
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
            )),
            
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
            
            
            htmlOutput("Functional_it_title"),
            column(12,
                   DT::dataTableOutput("Functional_it_id")),
            tags$head(tags$style("#Functional_it_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#Functional_it_id{
                     width = 100%;
                     max-height: 300px;
            }")
            ),

            htmlOutput("repeatmasker_title_it"),
            column(12, DT::dataTableOutput("repeatmasker")),
            column(12, br()),
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
          title = HTML("<strong style='font-size:20px'>Genomic distribution of genes</strong>"),icon = icon("location-arrow"),
          sidebarPanel(
            width = 3,
            tags$style("#GL_Paste {font-size:15px;font-family:sans-serif;}"),
            h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic distribution of genes</b></font>'),
               bsButton("qGL0", label="", icon=icon("question"), style="info", size="small")),
            bsPopover("qGL0", "Visualize genomic distribution of genes", trigger = "focus"),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            
            selectInput("variety_GL", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>')), 
                        list(`Wild soybean (Glycine soja)` = list("PI 483463", "PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar (Glycine max)` = list("Zhonghuang 13", "Williams 82", "Lee", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                                      "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                                      "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1", "Tianlong1", "Hwangkeum"),
                             `Landrace (Glycine max)` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                                             "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye"),
                             `Perennial Glycine` = list("Glycine cyrtoloba", "Glycine dolichocarpa", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3")),
                        selected = "Zhonghuang 13"
            ),
            
            textAreaInput("GL_Paste", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input a list of gene model names</b></font>')),
                          value = "SoyZH13_20G124201\nSoyZH13_14G131103\nSoyZH13_10G049500\nSoyZH13_01G054202\nSoyZH13_13G043301\nSoyZH13_10G214500\nSoyZH13_05G061800\nSoyZH13_11G118400\nSoyZH13_02G038800\nSoyZH13_16G155300\nSoyZH13_13G319400\nSoyZH13_19G153500\nSoyZH13_20G135100\nSoyZH13_18G142622\nSoyZH13_12G186900\nSoyZH13_11G123500\nSoyZH13_18G024500\nSoyZH13_20G078300\nSoyZH13_02G231900\nSoyZH13_03G179600",
                          resize = "vertical", height='200px', width = '200%',
                          placeholder = "The Gene ID must in the database"),
            
            
            shinysky::actionButton("GL_sbumit", strong("Submit!",
                                                       bsButton("qGL3", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            shinysky::actionButton("cleargl", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("glExam", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="GL_sbumit != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qGL3", "Whenever the list of Gene IDs is updated, please click Submit!",
                      trigger = "focus"),
            br(),
            numericInput("genelocheight", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 1000),
            numericInput("genelocwidth", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 1000)
          ),
          
          mainPanel(
            width = 9,
            br(),
            column(6, uiOutput("downloadgeneloc1")), 
            column(6, uiOutput("downloadgeneloc2")),
            #downloadButton("downloadSL.pdf", "Download pdf-file"),
            #downloadButton("downloadGL.svg", "Download svg-file"),
            uiOutput("genelocplotif"),
            #plotOutput("GLplot", height = "800px", width = "100%"),
            br(),
            br(),
            br()
          )
        ),
        
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Transcription factors/regulators</strong>"),icon = icon("house-user"),
          sidebarPanel(
            tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});')),
                      tags$style(
                        HTML(
                          "
                        .multicol{
                        -webkit-column-count: 5; /* Chrome, Safari, Opera */
                        -moz-column-count: 5;    /* Firefox */
                        column-count: 5;
                        column-gap: 40px;
          
                        }
                        "
                        ) #/ HTML
                      ) #/ style
            ), #/ head
            
            width = 3,
            
            h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Transcription factors/regulators</b></font>'),
               bsButton("qtrf0", label="", icon=icon("question"), style="info", size="small")),
            bsPopover("qtrf0", "Genome-wide Prediction and Classification of Soybean Transcription Factors (TF) and Transcriptional Regulators (TR)", trigger = "focus"),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            
            selectInput("variety_trf", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>')), 
                        list(`Wild soybean (Glycine soja)` = list("PI 483463", "PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar (Glycine max)` = list("Zhonghuang 13", "Williams 82", "Lee", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                                      "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                                      "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1", "Tianlong1", "Hwangkeum"),
                             `Landrace (Glycine max)` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                                             "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye"),
                             `Perennial Glycine` = list("Glycine cyrtoloba", "Glycine dolichocarpa", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3") ),
                        selected = "Zhonghuang 13"
            ),
            conditionalPanel(condition="ORT_sbumit != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0))
          ),
          
          mainPanel(
            width = 9,
            column(12,htmlOutput("trfaccessions_title")),
            column(12,uiOutput("trfaccessions")),
            tags$head(tags$style("#trfaccessions_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            )),
            column(12,br()),
            column(12,htmlOutput("trftitle_BDG")),
            column(12,DT::dataTableOutput("trf_BDG")),
            tags$head(tags$style("#trftitle_BDG{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#trf_BDG{
                     width = 100%;
                     max-height: 300px;
            }")
            ),
            htmlOutput("description_title_trf"),
            verbatimTextOutput("description_trf"),
            tags$head(tags$style("#description_title_trf{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#description_trf{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
            }")
            ),
            
            htmlOutput("gene_title_trf"),
            verbatimTextOutput("gene_trf"),
            tags$head(tags$style("#gene_title_trf{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#gene_trf{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
            }")
            ),
            
            htmlOutput("cds_title_trf"),
            verbatimTextOutput("cds_trf"),
            tags$head(tags$style("#cds_title_trf{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#cds_trf{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
            }")
            ),
            
            htmlOutput("cdna_title_trf"),
            verbatimTextOutput("cdna_trf"),
            tags$head(tags$style("#cdna_title_trf{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#cdna_trf{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
            }")
            ),
            
            htmlOutput("pro_title_trf"),
            shiny::verbatimTextOutput("pro_trf"),
            tags$head(tags$style("#pro_title_trf{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#pro_trf{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
                     
            }")
            ),
            
            htmlOutput("gffinfotitle_trf"),
            column(12,
                   DT::dataTableOutput("gffinfo_trf")),
            tags$head(tags$style("#gffinfotitle_trf{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#gffinfo_trf{
                     width = 100%;
                     max-height: 300px;
            }")
            ),
            column(12,br()),
            column(12,br())
          )
        ),
        
        #Genome synteny
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Genome synteny</strong>"),icon = icon("bezier-curve"),
          sidebarPanel(
            width = 3,
            h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Genome synteny</b></font>'),
               bsButton("qgsy0", label="", icon=icon("question"), style="info", size="small")),
            bsPopover("qgsy0", "Browse syntenic chromosome regions between different soybean genomes.", trigger = "focus"),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("variety_gsy1", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Choose a reference genome</b></font>')), 
                        c("Zhonghuang 13", "Williams 82"),
                        selected = "Zhonghuang 13"
            ),
            
            conditionalPanel(condition="input.variety_gsy1 == 'Zhonghuang 13'", 
                             selectInput("variety_gsy21", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Choose a genome to query</b></font>')), 
                                         list(`Wild soybean (Glycine soja)` = list("PI 483463", "PI 562565", "PI 549046", "PI 578357", "W05"),
                                              `Improved cultivar (Glycine max)` = list("Williams 82", "Lee", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                                                       "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                                                       "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1", "Tianlong1", "Hwangkeum"),
                                              `Landrace (Glycine max)` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                                                              "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye"),
                                              `Perennial Glycine` = list("Glycine cyrtoloba", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3") ),
                                         selected = "W05"
                             )
            ),
            conditionalPanel(condition="input.variety_gsy1 == 'Williams 82'", 
                             selectInput("variety_gsy22", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Choose a genome to query</b></font>')), 
                                         list(`Wild soybean (Glycine soja)` = list("PI 483463", "PI 562565", "PI 549046", "PI 578357", "W05"),
                                              `Improved cultivar (Glycine max)` = list("Zhonghuang 13", "Lee", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                                                       "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                                                       "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1", "Tianlong1", "Hwangkeum"),
                                              `Landrace (Glycine max)` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                                                              "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye"),
                                              `Perennial Glycine` = list("Glycine cyrtoloba", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3") ),
                                         selected = "W05"
                             )
                             
            ),
            
            selectInput("variety_gsy3", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Choose a chromosome</b></font>')), 
                        c(paste0("chr", 1:20)),
                        selected = "chr1"
            ),
            
            conditionalPanel(condition="submit_gsy != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            shinysky::actionButton("submit_gsy", strong("Submit!"), styleclass = "success")
          ),
          
          mainPanel(
            width = 9,
            uiOutput("zhanwei5"),
            column(12,htmlOutput("gsy_BDG_title")),
            column(12,DT::dataTableOutput("gsy_BDG")),
            tags$head(tags$style("#trfaccessions_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            )),
            column(12,htmlOutput("genomedata1_title")),
            column(12,DT::dataTableOutput("genomedata1")),
            tags$head(tags$style("#trfaccessions_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            )),
            column(12,htmlOutput("genomedata2_title")),
            column(12,DT::dataTableOutput("genomedata2")),
            
            column(12,br()),
            column(12,br())
          )
        ),
        
        #Structural variations
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Structural variations</strong>"), icon = icon("sliders-h"),
          
          sidebarPanel(
            width = 3,
            h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Structural variations</b></font>'),
               bsButton("qgsv0", label="", icon=icon("question"), style="info", size="small")),
            bsPopover("qgsv0", "Browse structural variations identified between different soybean genomes.", trigger = "focus"),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("variety_gsv1", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Choose a reference genome</b></font>')), 
                        c("Zhonghuang 13", "Williams 82"),
                        selected = "Zhonghuang 13"
            ),
            
            conditionalPanel(condition="input.variety_gsv1 == 'Zhonghuang 13'", 
                             selectInput("variety_gsv21", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Choose a genome to query</b></font>')), 
                                         list(`Wild soybean (Glycine soja)` = list("PI 483463", "PI 562565", "PI 549046", "PI 578357", "W05"),
                                              `Improved cultivar (Glycine max)` = list("Williams 82", "Lee", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                                                       "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                                                       "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1", "Tianlong1", "Hwangkeum"),
                                              `Landrace (Glycine max)` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                                                              "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye"),
                                              `Perennial Glycine` = list("Glycine cyrtoloba", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3") ),
                                         selected = "W05"
                             ),
            ),
            conditionalPanel(condition="input.variety_gsv1 == 'Williams 82'", 
                             selectInput("variety_gsv22", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Choose a genome to query</b></font>')), 
                                         list(`Wild soybean (Glycine soja)` = list("PI 483463", "PI 562565", "PI 549046", "PI 578357", "W05"),
                                              `Improved cultivar (Glycine max)` = list("Zhonghuang 13", "Lee", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                                                       "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                                                       "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1", "Tianlong1", "Hwangkeum"),
                                              `Landrace (Glycine max)` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                                                              "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye"),
                                              `Perennial Glycine` = list("Glycine cyrtoloba", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3") ),
                                         selected = "W05"
                             ),
                             
            ),
            
            selectInput("variety_gsv3", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Choose a chromosome</b></font>')), 
                        c(paste0("chr", 1:20)),
                        selected = "chr1"
            ),
            conditionalPanel(condition="submit_gsv != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            shinysky::actionButton("submit_gsv", strong("Submit!"), styleclass = "success")
          ),
          
          mainPanel(
            uiOutput("genestructure_tab_show")
            # tabsetPanel(id = "genestructure_tab",
            #             tabPanel("Duplication",
            #                      fixedRow(
            #                        DT::dataTableOutput("dupresult"),
            #                        column(12,htmlOutput("dupdata1_title")),
            #                        column(12,DT::dataTableOutput("dupdata1")),
            #                        tags$head(tags$style("#trfaccessions_title{color: red;
            #                            font-style: bold;
            #                            font-size: 18px;
            #                            font-family:Times New Roman;
            #                           }"
            #                        )),
            #                        column(12,htmlOutput("dupdata2_title")),
            #                        column(12,DT::dataTableOutput("dupdata2")))
            #                      
            #             ),
            #             
            #             tabPanel("Inversion",
            #                      fixedRow(
            #                        DT::dataTableOutput("invresult"),
            #                        column(12,htmlOutput("invdata1_title")),
            #                        column(12,DT::dataTableOutput("invdata1")),
            #                        tags$head(tags$style("#trfaccessions_title{color: red;
            #                            font-style: bold;
            #                            font-size: 18px;
            #                            font-family:Times New Roman;
            #                           }"
            #                        )),
            #                        column(12,htmlOutput("invdata2_title")),
            #                        column(12,DT::dataTableOutput("invdata2")))
            #             ),
            #             
            #             tabPanel("Tandem repeat",
            #                      fixedRow(
            #                        DT::dataTableOutput("tdmresult"),
            #                        column(12,htmlOutput("tdmdata1_title")),
            #                        column(12,DT::dataTableOutput("tdmdata1")),
            #                        tags$head(tags$style("#trfaccessions_title{color: red;
            #                            font-style: bold;
            #                            font-size: 18px;
            #                            font-family:Times New Roman;
            #                           }"
            #                        )),
            #                        column(12,htmlOutput("tdmdata2_title")),
            #                        column(12,DT::dataTableOutput("tdmdata2")))
            #             ),
            #             
            #             tabPanel("Translocation",
            #                      fixedRow(
            #                        DT::dataTableOutput("transresult"),
            #                        column(12,htmlOutput("transdata1_title")),
            #                        column(12,DT::dataTableOutput("transdata1")),
            #                        tags$head(tags$style("#trfaccessions_title{color: red;
            #                            font-style: bold;
            #                            font-size: 18px;
            #                            font-family:Times New Roman;
            #                           }"
            #                        )),
            #                        column(12,htmlOutput("transdata2_title")),
            #                        column(12,DT::dataTableOutput("transdata2")))
            #             ),
            #             
            #             tabPanel("Deletion",
            #                      fixedRow(
            #                        DT::dataTableOutput("delresult"),
            #                        column(12,htmlOutput("deldata1_title")),
            #                        column(12,DT::dataTableOutput("deldata1")),
            #                        tags$head(tags$style("#trfaccessions_title{color: red;
            #                            font-style: bold;
            #                            font-size: 18px;
            #                            font-family:Times New Roman;
            #                           }"
            #                        )),
            #                        column(12,htmlOutput("deldata2_title")),
            #                        column(12,DT::dataTableOutput("deldata2")))
            #             ),
            #             
            #             tabPanel("Insertion",
            #                      fixedRow(
            #                        DT::dataTableOutput("insresult"),
            #                        column(12,htmlOutput("insdata1_title")),
            #                        column(12,DT::dataTableOutput("insdata1")),
            #                        tags$head(tags$style("#trfaccessions_title{color: red;
            #                            font-style: bold;
            #                            font-size: 18px;
            #                            font-family:Times New Roman;
            #                           }"
            #                        )),
            #                        column(12,htmlOutput("insdata2_title")),
            #                        column(12,DT::dataTableOutput("insdata2")))
            #             ),
            #             br()
            # )
          )
        )
      ),
      
      # JBrowse
      tabPanel(
        title = HTML("<strong style='font-size:20px'>JBrowse</strong>"), icon = icon("list"),
        h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>JBrowse of 39 soybean genomes</b></font>'),
           bsButton("qjb1", label="", icon=icon("question"), style="info", size="small")),
        bsPopover("qjb1", "Click the name of a soybean genome to view in JBrowse", trigger = "focus"),
        DT::dataTableOutput('JBrowsetable'),
        br()
      ),
      
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>SNPs</strong>"),
        
        # Genome browse
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Browse</strong>"), icon = icon("folder-open"),
          tabsetPanel(id = "Browse_key",
                      tabPanel(HTML("<strong style='font-size:18px'>Information of 2898 soybean accessions</strong>"),
          sidebarPanel(
            #text input
            tags$style("#regB {font-size:15px;font-family:sans-serif;}"),
            width = 3,
            tags$div(h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Browse SNPs</b></font>'),
                        bsButton("qgbv", label="", icon=icon("question"), style="info", size="small"))),
            bsPopover("qgbv", "For a specified genomic region or gene model, all the SNPs among user-selected soybean accessions were extracted and subjected to genome browser visualization", trigger = "focus"),
            
            textInput("regB", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
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
            
            sliderInput(inputId = "GBUP",  label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (bp)</b></font>'),
                                                      bsButton("qg2", label="", icon=icon("question"), style="info", size="small")
            ), min = 0, max = 50000, value = 0, ticks = FALSE),
            bsPopover("qg2", "Extend the input genomic region to its upstream.",
                      trigger = "focus"),
            
            sliderInput("GBDOWN", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                     bsButton("qg4", label="", icon=icon("question"), style="info", size="small")
            ), min = 0, max = 50000, value = 0, ticks = FALSE),
            bsPopover("qg4", "Extend the input genomic region to its downstream.", trigger = "focus"),
            
            shinyWidgets::multiInput("mychooserB", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
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
            
            shinyWidgets::multiInput("GB_mut_group", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation effect</b></font>'),
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
        
        tabPanel(HTML("<strong style='font-size:18px'>Information of 481soybean accessions</strong>"),
                 sidebarPanel(
                   #text input
                   tags$style("#regB82 {font-size:15px;font-family:sans-serif;}"),
                   width = 3,
                   tags$div(h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Browse SNPs</b></font>'),
                               bsButton("qgbv82", label="", icon=icon("question"), style="info", size="small"))),
                   bsPopover("qgbv82", "For a specified genomic region or gene model, all the SNPs among user-selected soybean accessions were extracted and subjected to genome browser visualization", trigger = "focus"),
                   
                   textInput("regB82", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                                  bsButton("q682", label="", icon=icon("question"), style="info", size="small")),
                             value = "chr15:1000000-1011111"),
                   
                   bsPopover("q682", "A genomic region can be determined by chromosome positions or gene locus. For example, chr15:1000000-1011111 or Glyma.01G000050.",
                             trigger = "focus"),
                   
                   shinysky::actionButton("submit_browse82", strong("Submit!",
                                                                    bsButton("q782", label="", icon=icon("question"), style="info", size="small")
                   ), width = "90%", styleclass = "success"),
                   bsPopover("q782", "Whenever the genomic region is updated, please click Submit!",
                             trigger = "focus"),
                   
                   shinysky::actionButton("clearGB82", strong("Reset"), styleclass = "warning"),
                   shinysky::actionButton("GBExam82", strong("Load example"), styleclass = "info"),
                   
                   conditionalPanel(condition="input.submit_browse82 != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
                   
                   sliderInput(inputId = "GBUP82",  label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (bp)</b></font>'),
                                                               bsButton("qg282", label="", icon=icon("question"), style="info", size="small")
                   ), min = 0, max = 50000, value = 0, ticks = FALSE),
                   bsPopover("qg282", "Extend the input genomic region to its upstream.",
                             trigger = "focus"),
                   
                   sliderInput("GBDOWN82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                              bsButton("qg482", label="", icon=icon("question"), style="info", size="small")
                   ), min = 0, max = 50000, value = 0, ticks = FALSE),
                   bsPopover("qg482", "Extend the input genomic region to its downstream.", trigger = "focus"),
                   
                   shinyWidgets::multiInput("mychooserB82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
                                                               bsButton("qg382", label="", icon=icon("question"), style="info", size="small")),
                                            choices = all.soya.cho82,
                                            selected = all.soya.cho82,
                                            width = 800,
                                            options = list(
                                              enable_search = TRUE,
                                              non_selected_header = "Choose from:",
                                              selected_header = "You have selected:"
                                            )
                   ),
                   
                   bsPopover("qg382", "Only the chosen soybean accessions will be used.", trigger = "focus"),
                   fluidRow(
                     column(6, actionButton("browsenone182", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                     column(6, actionButton("browseall182", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                   ),
                   
                   shinyWidgets::multiInput("GB_mut_group82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation effect</b></font>'),
                                                                 bsButton("qg182", label="", icon=icon("question"), style="info", size="small")),
                                            choices = mutationtypes82,
                                            selected = mutationtypes82,
                                            width = 800,
                                            options = list(
                                              enable_search = TRUE,
                                              non_selected_header = "Choose from:",
                                              selected_header = "You have selected:"
                                            )
                   ),
                   
                   bsPopover("qg182", "Only SNPs with selected mutation effects will be used.", trigger = "focus"),
                   fluidRow(
                     column(6, actionButton("browsenone282", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                     column(6, actionButton("browseall282", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                   )
                 ),
                 
                 mainPanel(
                   width = 9,
                   fluidRow(
                     column(4, uiOutput("downloadBro0182")),
                     column(4, uiOutput("downloadBro0282")),
                     column(4, uiOutput("downloadBro0382"))
                     #column(4, downloadButton("downloadsnp.txt", style = "width:100%;", "Download genotype data", class = "buttDown")),
                     #column(4, downloadButton("downloadsnpInfo.txt", style = "width:100%;", "Download SNPs information", class = "buttDown")),
                     #column(4, downloadButton("downloadGB.pdf",  style = "width:100%;", "Download pdf-file", class = "buttDown")),
                     #tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
                   ),
                   
                   plotly::plotlyOutput("gbrowser82", height = '600px', width = '100%')
                 )
        )))
        ,

        tabPanel(
          title = HTML("<strong style='font-size:20px'>Search</strong>"), icon = icon("search"), 
          tabsetPanel(id = "Search_key",
                      tabPanel(HTML("<strong style='font-size:18px'>SNPs among 2898 soybean accessions</strong>"), 
          sidebarPanel(
            tags$style("#regBB {font-size:15px;font-family:sans-serif;}"),
            tags$style("#regi481 {font-size:15px;font-family:sans-serif;}"),
            
            width = 3,
            fixedRow(
              column(12,
                     tags$div(h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Search SNPs</b></font>'),
                                 bsButton("qqsi", label="", icon=icon("question"), style="info", size="small"))),
                     bsPopover("qqsi", "Search SNPs among 2898 soybean accessions in an input genomic region or gene model.", trigger = "focus")
              )
            ),
            
              textInput("regBB", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                            bsButton("q1", label="", icon=icon("question"), style="info", size="small")),
                        value = "chr7:29560705-29573051"),
              bsPopover("q1", "A genomic region can be determined by chromosome positions or gene locus. For example, chr7:29560705-29573051 or SoyZH13_01G186100.",
                        trigger = "focus"),
              shinyWidgets::multiInput("mychooserD", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
                                                        bsButton("qdl2", label="", icon=icon("question"), style="info", size="small")),
                                       choices = all.soya.cho,
                                       selected = "Glycine soja",
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
                        trigger = "focus")
           
            
            
          ),
          
          conditionalPanel(condition="input.submit_SSNP != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
          mainPanel(
            width = 9,
            
            
            fluidRow(
              column(4, uiOutput("downloadSD01")),
              column(4, uiOutput("downloadSD02")),
              column(4, uiOutput("downloadSD03")),
              
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
        tabPanel(HTML("<strong style='font-size:18px'>SNPs among 481 soybean accessions</strong>"), 
                 sidebarPanel(
                   tags$style("#regBB {font-size:15px;font-family:sans-serif;}"),
                   tags$style("#regi481 {font-size:15px;font-family:sans-serif;}"),
                   
                   width = 3,
                   fixedRow(
                     column(12,
                            tags$div(h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Search SNPs</b></font>'),
                                        bsButton("qqsi_1", label="", icon=icon("question"), style="info", size="small"))),
                            bsPopover("qqsi_1", "Search SNPs among 481 soybean accessions in an input genomic region or gene model.", trigger = "focus")
                     )
                   ),
                 
                     
                     textInput("regi481", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                                     bsButton("qi481", label="", icon=icon("question"), style="info", size="small")),
                               value = "chr1:29506705-29659223"),
                     
                     bsPopover("qi481", "A genomic region can be determined by chromosome positions. For example, chr1:29506705-29659223.",
                               trigger = "focus"),
                     
                     shinyWidgets::multiInput("mychooseri481", p(h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
                                                                    bsButton("qsnp482", label="", icon=icon("question"), style="info", size="small"))),
                                              choices = soynm,
                                              selected = soynm[grepl("HN", soynm)],
                                              width = 800,
                                              options = list(
                                                enable_search = TRUE,
                                                non_selected_header = "Choose from:",
                                                selected_header = "You have selected:"
                                              )
                     ), 
                     
                     bsPopover("qsnp482", "Only the chosen soybean accessions will be used.", trigger = "focus"),
                     fluidRow(
                       column(6, actionButton("snpnone481", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                       column(6, actionButton("snpall481", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                     ),
                     br(),
                     shinysky::actionButton("submiti481", strong("Submit!",
                                                                 bsButton("qi2", label="", icon=icon("question"), style="info", size="small")
                     ), width = "90%", styleclass = "success"),
                     
                     shinysky::actionButton("clearSNP481", strong("Reset"), styleclass = "warning"),
                     shinysky::actionButton("SNPExam481", strong("Load example"), styleclass = "info"),
                     bsPopover("qi2", "Whenever the genomic region or any option is updated, please click Submit!",
                               trigger = "focus")
                   ),
                   
                 conditionalPanel(condition="input.submiti481 != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
                 
                    mainPanel(
                   width = 9,
                     fluidRow(
                       #column(6, uiOutput("downloadSNP01", style = "width:100%;")),
                       column(12, uiOutput("downloadSNP02", style = "width:100%;")),  
                       
                       #column(6, downloadButton("bulkdownloadSNPInfo.txt", style = "width:100%;", "Download SNPs information", class = "buttDown")),
                       #column(6, downloadButton("bulkdownloadSNPALLInfo.txt", style = "width:100%;", "Download All SNPs information", class = "buttDown"))
                       #tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
                     ),
                     br(),
                     
                     htmlOutput("snptabletitle481"),
                     DT::dataTableOutput("snptable481"),
                     br(), br(),
                     tags$head(tags$style("#snptabletitle481{color: red;
                                       font-size: 22px;
                                       font-style: bold;
                                      }"
                     )
                     )
                   
                   
                 ))
        
        
        
        )),
        
        
        # LDheatmap
        tabPanel(
          title = HTML("<strong style='font-size:20px'>LDheatmap</strong>"), icon = icon("project-diagram"),
          tabsetPanel(id = "LDheatmap_key",
                      tabPanel(HTML("<strong style='font-size:18px'>Information of 2898 soybean accessions</strong>"),
                               
                               
          sidebarPanel(
            tags$style("#regL {font-size:15px;font-family:sans-serif;}"),
            tags$style("#ldpos {font-size:15px;font-family:sans-serif;}"),
            width = 3,
            
            fixedRow(
              column(12,
                     tags$div(h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Linkage disequilibrium analysis</b></font>'),
                                 bsButton("qlda", label="", icon=icon("question"), style="info", size="small"))),
                     bsPopover("qlda", " For a specified genomic region or gene model, a heat map can be created to display the pairwise linkage disequilibrium between different SNP sites", trigger = "focus")
              )
            ),
            
            textInput("regL", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
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
            radioButtons("flip", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Flip the figure</b></font>')), list( "TRUE" = 1, "FALSE" = 0)),
            
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
              radioButtons("showText", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Print LD measurements</b></font>')), list("FALSE" =  0, "TRUE" = 1)),
              textInput("ldpos", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Label SNPs</b></font>')), value = "5, 8")
            ),
            
            radioButtons("ldcol",
                         label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Color</b></font>')), list("grey.colors(20)" = 1, "heat.colors(20)" = 2)
            ),
            
            sliderInput("ldUp", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (bp)</b></font>'),
                                   bsButton("ql4", label="", icon=icon("question"), style="info", size="small")
            ), min = 0, max = 50000, value = 0, ticks = FALSE),
            bsPopover("ql4", "Extend the input genomic region to its upstream.", trigger = "focus"),
            
            sliderInput("ldDown", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                     bsButton("ql5", label="", icon=icon("question"), style="info", size="small")
            ), min = 0, max = 50000, value = 0, ticks = FALSE),
            bsPopover("ql5", "Extend the input genomic region to its downstream.", trigger = "focus"),
            
            shinyWidgets::multiInput(
              "ld_mut_group", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation effect</b></font>'),
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
            
            shinyWidgets::multiInput("mychooserLD", p(h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
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
              numericInput("ldHeight", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 730),
              numericInput("ldWidth", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 1000)
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
        
        tabPanel(HTML("<strong style='font-size:18px'>Information of 481 soybean accessions</strong>"),
                 
                 
                 sidebarPanel(
                   tags$style("#regL82 {font-size:15px;font-family:sans-serif;}"),
                   tags$style("#ldpos82 {font-size:15px;font-family:sans-serif;}"),
                   width = 3,
                   
                   fixedRow(
                     column(12,
                            tags$div(h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Linkage disequilibrium analysis</b></font>'),
                                        bsButton("qlda82", label="", icon=icon("question"), style="info", size="small"))),
                            bsPopover("qlda82", " For a specified genomic region or gene model, a heat map can be created to display the pairwise linkage disequilibrium between different SNP sites", trigger = "focus")
                     )
                   ),
                   
                   textInput("regL82", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                                   bsButton("q582", label="", icon=icon("question"), style="info", size="small")),
                             value = "chr15:1000000-1011111"),
                   
                   bsPopover("q582", "A genomic region can be determined by chromosome positions or gene locus. For example, chr15:1000000-1011111 or Glyma.01G000050.",
                             trigger = "focus"),
                   
                   shinysky::actionButton("submitLD82", strong("Submit!",
                                                                bsButton("q882", label="", icon=icon("question"), style="info", size="small")
                   ), styleclass = "success"),
                   
                   shinysky::actionButton("clearLD82", strong("Reset"), styleclass = "warning"),
                   shinysky::actionButton("LDExam82", strong("Load example"), styleclass = "info"),
                   
                   conditionalPanel(condition="input.submit3 != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
                   bsPopover("q882", "Whenever the genomic region is updated, please click Submit!",
                             trigger = "focus"),
                   
                   br(),
                   
                   h4(HTML('<i class="fa fa-cog"></i> <font size="5" color="red"><b>Plot options</b></font>')),
                   radioButtons("flip82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Flip the figure</b></font>')), list( "TRUE" = 1, "FALSE" = 0)),
                   
                   conditionalPanel(
                     condition = "input.flip82==1",
                     h4(checkboxInput("LDshowGene82", label = p("Show gene model",
                                                                 bsButton("qldg82", label="", icon=icon("question"), style="info", size="small")), TRUE)),
                     bsPopover("qldg82", "Y:gene model height; W:gene model width", trigger = "focus"),
                     conditionalPanel(
                       condition = "input.LDshowGene82",
                       fluidRow(
                         column(6, numericInput("ldY82", "Y:", value = 72)),
                         column(6, numericInput("ldW82", "W:", value = 72))
                       ))
                   ),
                   
                   conditionalPanel(
                     condition = "input.flip82==0",
                     radioButtons("showText82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Print LD measurements</b></font>')), list("FALSE" =  0, "TRUE" = 1)),
                     textInput("ldpos82", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Label SNPs</b></font>')), value = "5, 8")
                   ),
                   
                   radioButtons("ldcol82",
                                label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Color</b></font>')), list("grey.colors(20)" = 1, "heat.colors(20)" = 2)
                   ),
                   
                   sliderInput("ldUp82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (bp)</b></font>'),
                                             bsButton("ql482", label="", icon=icon("question"), style="info", size="small")
                   ), min = 0, max = 50000, value = 0, ticks = FALSE),
                   bsPopover("ql482", "Extend the input genomic region to its upstream.", trigger = "focus"),
                   
                   sliderInput("ldDown82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                               bsButton("ql582", label="", icon=icon("question"), style="info", size="small")
                   ), min = 0, max = 50000, value = 0, ticks = FALSE),
                   bsPopover("ql582", "Extend the input genomic region to its downstream.", trigger = "focus"),
                   
                   shinyWidgets::multiInput(
                     "ld_mut_group82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation effect</b></font>'),
                                           bsButton("ql182", label="", icon=icon("question"), style="info", size="small")),
                     choices = mutationtypes82,
                     selected = mutationtypes82,
                     width = 800,
                     options = list(
                       enable_search = TRUE,
                       non_selected_header = "Choose from:",
                       selected_header = "You have selected:"
                     )
                   ),
                   bsPopover("ql182", "Only SNPs with selected mutation effects will be used.", trigger = "focus"),
                   
                   fluidRow(
                     column(6, actionButton("ldnone282", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                     column(6, actionButton("ldall282", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                   ),
                   
                   shinyWidgets::multiInput("mychooserLD82", p(h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
                                                                   bsButton("ql382", label="", icon=icon("question"), style="info", size="small"))),
                                            choices = all.soya.cho82,
                                            selected = all.soya.cho82,
                                            width = 800,
                                            options = list(
                                              enable_search = TRUE,
                                              non_selected_header = "Choose from:",
                                              selected_header = "You have selected:"
                                            )
                   ), 
                   
                   bsPopover("ql382", "Only the chosen soybean accessions will be used.", trigger = "focus"),
                   
                   fluidRow(
                     column(6, actionButton("ldnone182", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                     column(6, actionButton("ldall182", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                   ),
                   
                   checkboxInput("ldSize82", "Adjust plot size", FALSE),
                   conditionalPanel(
                     condition = "input.ldSize82",
                     numericInput("ldHeight82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 730),
                     numericInput("ldWidth82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 1000)
                   )
                 ),
                 
                 mainPanel(
                   width = 9,
                   fluidRow(
                     column(6, uiOutput("downloadLD0182")),
                     column(6, uiOutput("downloadLD0282"))
                     #column(6, downloadButton("downloadLD.pdf", style = "width:100%;", "Download pdf-file", class = "buttDown")),
                     #column(6, downloadButton("downloadLD.svg", style = "width:100%;", "Download svg-file", class = "buttDown")),
                     #tags$style(".buttDown{background-color:black; color: white; font-size: 16px;}")
                   ),
                   
                   plotOutput("ldheatmap82", height = "800px", width = "100%")
                 )
        ))),
        
        
        
        # Nucleotide diversity
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Diversity</strong>"), icon = icon("chart-area"),
          tabsetPanel(id = "Diversity_key",
                      tabPanel(HTML("<strong style='font-size:18px'>Information of 2898 soybean accessions</strong>"),
                               
          sidebarPanel(
            tags$style("#regD {font-size:15px;font-family:sans-serif;}"),
            tags$style("#snpnumD {font-size:15px;font-family:sans-serif;}"),
            width = 3,
            
            fixedRow(
              column(12,
                     tags$div(h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Nucleotide diversity analysis</b></font>'),
                                 bsButton("qnda", label="", icon=icon("question"), style="info", size="small"))),
                     bsPopover("qnda", "Calculate and demonstrate nucleotide diversities among subgroups of soybean accessions in specified genomic regions.", trigger = "focus")
              )
            ),
            
            textInput("regD", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
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
            
            numericInput("snpnumD", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Number of SNPs in each window</b></font>'),
                                       bsButton("qd6", label="", icon=icon("question"), style="info", size="small")
            ), value = 10, min = 5, max = 20),
            bsPopover("qd6", "A specified genomic region would be split into non-overlapping window so that each window contains specified number of SNPs. The nucleotide diversity of all soybean accessions belong to the specified ecotypes in each window would be calculated.",
                      trigger = "focus"),
            
            shinyWidgets::pickerInput(
              inputId = "div_acc_group",
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Ecotypes to calculate diversity</b></font>')),
              choices = c("Glycine soja", "Landrace", "Improved cultivar"),
              selected = c("Glycine soja", "Landrace"),
              multiple = TRUE,
              options = list(style = "btn-primary")
            ),
            
            selectInput("nuc_numerator", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Numerator ecotype</b></font>'),
                                            bsButton("qd7", label="", icon=icon("question"), style="info", size="small")
            ), choices = c("Glycine soja", "Improved cultivar", "Landrace")),
            bsPopover("qd7", "The nucleotide diversity of soybean accessions belong to the Numerator ecotype would be divided by the nucleotide diversity of soybean accessions belong to the Denominator ecotype for comparison.",
                      trigger = "focus"),
            selectInput("nuc_denominator", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Denominator ecotype</b></font>')), choices = 
                          c("Improved cultivar", "Landrace", "Glycine soja")),
            
            shinyWidgets::multiInput("div_mut_group", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation effect</b></font>'),
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
                      trigger = "focus"),
            
            fluidRow(
              column(6, actionButton("diversitynone2", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
              column(6, actionButton("diversityldall2", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
            ),
            
            sliderInput("divUp", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (bp)</b></font>'),
                                    bsButton("qd4", label="", icon=icon("question"), style="info", size="small")
            ), min = 0, max = 50000, value = 10000, ticks = FALSE),
            bsPopover("qd4", "Extend the input genomic region to its upstream.", trigger = "focus"),
            
            sliderInput("divDown", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                      bsButton("qd5", label="", icon=icon("question"), style="info", size="small")
            ), min = 0, max = 50000, value = 10000, ticks = FALSE),
            bsPopover("qd5", "Extend the input genomic region to its downstream", trigger = "focus"),
            
            checkboxInput("divSize", "Adjust plot size", FALSE),
            conditionalPanel(
              condition = "input.divSize",
              numericInput("divHeight", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 730),
              numericInput("divWidth", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 1000)
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
        )
      ,
      tabPanel(HTML("<strong style='font-size:18px'>Information of 481 soybean accessions</strong>"),
               
               sidebarPanel(
                 tags$style("#regD82 {font-size:15px;font-family:sans-serif;}"),
                 tags$style("#snpnumD82 {font-size:15px;font-family:sans-serif;}"),
                 width = 3,
                 
                 fixedRow(
                   column(12,
                          tags$div(h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Nucleotide diversity analysis</b></font>'),
                                      bsButton("qnda82", label="", icon=icon("question"), style="info", size="small"))),
                          bsPopover("qnda82", "Calculate and demonstrate nucleotide diversities among subgroups of soybean accessions in specified genomic regions.", trigger = "focus")
                   )
                 ),
                 
                 textInput("regD82", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                                bsButton("q382", label="", icon=icon("question"), style="info", size="small")),
                           value = "chr15:1000000-1011111"),
                 
                 bsPopover("q382", "A genomic region can be determined by chromosome positions or gene locus. For example, chr15:1000000-1011111 or Glyma.01G000050",
                           trigger = "focus"),
                 
                 shinysky::actionButton("submit482", strong("Submit!",
                                                            bsButton("q1082", label="", icon=icon("question"), style="info", size="small")
                 ), styleclass = "success"),
                 
                 shinysky::actionButton("clearDIV82", strong("Reset"), styleclass = "warning"),
                 shinysky::actionButton("DIVExam82", strong("Load example"), styleclass = "info"),
                 
                 conditionalPanel(condition="input.submit482 != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
                 bsPopover("q1082", "Whenever the genomic region or any plot option is updated, please click Submit!", trigger = "focus"),
                 
                 br(),
                 h4(HTML('<i class="fa fa-cog" aria-hidden="true"></i> <font size="5" color="red"><b>Plot options</b></font>')),
                 
                 numericInput("snpnumD82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Number of SNPs in each window</b></font>'),
                                              bsButton("qd682", label="", icon=icon("question"), style="info", size="small")
                 ), value = 10, min = 5, max = 20),
                 bsPopover("qd682", "A specified genomic region would be split into non-overlapping window so that each window contains specified number of SNPs. The nucleotide diversity of all soybean accessions belong to the specified ecotypes in each window would be calculated.",
                           trigger = "focus"),
                 
                 shinyWidgets::pickerInput(
                   inputId = "div_acc_group82",
                   label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Ecotypes to calculate diversity</b></font>')),
                   choices = c("Glycine soja", "Landraces and elites"),
                   selected = c("Glycine soja", "Landraces and elites"),
                   multiple = TRUE,
                   options = list(style = "btn-primary")
                 ),
                 
                 selectInput("nuc_numerator82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Numerator ecotype</b></font>'),
                                                   bsButton("qd782", label="", icon=icon("question"), style="info", size="small")
                 ), choices = c("Glycine soja", "Landraces and elites")),
                 bsPopover("qd782", "The nucleotide diversity of soybean accessions belong to the Numerator ecotype would be divided by the nucleotide diversity of soybean accessions belong to the Denominator ecotype for comparison.",
                           trigger = "focus"),
                 selectInput("nuc_denominator82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Denominator ecotype</b></font>')), choices = 
                               c("Landraces and elites", "Glycine soja")),
                 
                 shinyWidgets::multiInput("div_mut_group82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Mutation effect</b></font>'),
                                                                bsButton("qd182", label="", icon=icon("question"), style="info", size="small")),
                                          choices = mutationtypes82,
                                          selected = mutationtypes82,
                                          width = 800,
                                          options = list(
                                            enable_search = TRUE,
                                            non_selected_header = "Choose from:",
                                            selected_header = "You have selected:"
                                          )
                 ),
                 bsPopover("qd182", "Only SNPs with selected mutation effects will be used.",
                           trigger = "focus"),
                 
                 fluidRow(
                   column(6, actionButton("diversitynone282", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                   column(6, actionButton("diversityldall282", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                 ),
                 
                 sliderInput("divUp82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Upstream (bp)</b></font>'),
                                           bsButton("qd482", label="", icon=icon("question"), style="info", size="small")
                 ), min = 0, max = 50000, value = 10000, ticks = FALSE),
                 bsPopover("qd482", "Extend the input genomic region to its upstream.", trigger = "focus"),
                 
                 sliderInput("divDown82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Downstream (bp)</b></font>'),
                                             bsButton("qd582", label="", icon=icon("question"), style="info", size="small")
                 ), min = 0, max = 50000, value = 10000, ticks = FALSE),
                 bsPopover("qd582", "Extend the input genomic region to its downstream", trigger = "focus"),
                 
                 checkboxInput("divSize82", "Adjust plot size", FALSE),
                 conditionalPanel(
                   condition = "input.divSize82",
                   numericInput("divHeight82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 730),
                   numericInput("divWidth82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 1000)
                 )
               ),
               
               mainPanel(
                 width = 9,
                 fluidRow(
                   column(4, uiOutput("downloadDiv0182")),
                   column(4, uiOutput("downloadDiv0282")),
                   column(4, uiOutput("downloadDiv0382"))
                 ),
                 
                 # downloadButton("downloadDiv.pdf", "Download pdf-file"),
                 # downloadButton("downloadDiv.svg", "Download svg-file"),
                 # downloadButton("downloadDiv.txt", "Download TXT-file"),
                 column(12, plotOutput("diversity82", height = '800px', width = '100%'))
               )
      )
    )),  
        #AlleleFreq
        tabPanel(
          title = HTML("<strong style='font-size:20px'>AlleleFreq</strong>"), icon = icon("chart-pie"),
          tabsetPanel(id = "AlleleFreq_key",
                      tabPanel(HTML("<strong style='font-size:18px'>Information of 2898 soybean accessions</strong>"),
          sidebarPanel(
            width = 3,
            tags$style("#af_snp_site {font-size:15px;font-family:sans-serif;}"),
            fixedRow(
              column(12,
                     tags$div(h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Allele frequency analysis</b></font>'),
                                 bsButton("qaf11", label="", icon=icon("question"), style="info", size="small"))),
                     bsPopover("qaf11", "Calculate and demonstrate allele frequency of input SNP sites.", trigger = "focus")
              )
            ),
            
            textAreaInput("af_snp_site", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input SNP sites</b></font>'),
                                                    bsButton("qaf3", label="", icon=icon("question"), style="info", size="small")), 
                          width="100%", resize="vertical", height="150px", 
                          placeholder = "One SNP site in one row", 
                          value = "0133024709\n1403584545\n1403584761"
            ),
            bsPopover("qaf3", "Each SNP site should be a 10-digits integer. The first two digits represent the chromosome ID while the rest eight digits represent the genomic position of each SNP site. Each SNP site should take only one row!",
                      trigger = "focus"),
            
            shinyWidgets::pickerInput(
              inputId = "af_acc_group",
              label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Ecotypes to calculate allele frequency</b></font>')), 
              choices = c("Glycine soja", "Landrace", "Improved cultivar"),
              selected = c("Glycine soja", "Landrace", "Improved cultivar"),
              multiple = TRUE
            ),
            selectInput("jscolora", h4(HTML('<i class="fa fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Allele colors</b></font>'),
                                       bsButton("qaf2", label="", icon=icon("question"), style="info", size="small")), 
                        choices = c("steelblue, yellow2", "mediumspringgreen, mediumvioletred", "orchid, palegreen", "cornflowerblue, forestgreen"), selected = "steelblue, yellow2" ),
            bsPopover("qaf2", "Colors for the major and minor allele in the pie chart, respectively!", trigger = "focus"),
            
            numericInput("afHeight", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 650),
            numericInput("afWidth", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 800),
            
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
              column(4, uiOutput("downloadAfq02")),
              column(4, uiOutput("downloadAfq03"))
            ),
            
            br(),
            br(),
            
            plotOutput("alleleFreq", height = "900px", width = "100%")
          )
        ),
    
        tabPanel(HTML("<strong style='font-size:18px'>Information of 481 soybean accessions</strong>"),
                 sidebarPanel(
                   width = 3,
                   tags$style("#af_snp_site82 {font-size:15px;font-family:sans-serif;}"),
                   fixedRow(
                     column(12,
                            tags$div(h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Allele frequency analysis</b></font>'),
                                        bsButton("qaf1182", label="", icon=icon("question"), style="info", size="small"))),
                            bsPopover("qaf1182", "Calculate and demonstrate allele frequency of input SNP sites.", trigger = "focus")
                     )
                   ),
                   
                   textAreaInput("af_snp_site82", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input SNP sites</b></font>'),
                                                             bsButton("qaf382", label="", icon=icon("question"), style="info", size="small")), 
                                 width="100%", resize="vertical", height="150px", 
                                 placeholder = "One SNP site in one row", 
                                 value = "0300000762\n2015000614\n2015001173"
                   ),
                   bsPopover("qaf382", "Each SNP site should be a 10-digits integer. The first two digits represent the chromosome ID while the rest eight digits represent the genomic position of each SNP site. Each SNP site should take only one row!",
                             trigger = "focus"),
                   
                   shinyWidgets::pickerInput(
                     inputId = "af_acc_group82",
                     label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Ecotypes to calculate allele frequency</b></font>')), 
                     choices = c("Glycine soja", "Landraces and elites"),
                     selected = c("Glycine soja", "Landraces and elites"),
                     multiple = TRUE
                   ),
                   selectInput("jscolora82", h4(HTML('<i class="fa fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Allele colors</b></font>'),
                                                bsButton("qaf282", label="", icon=icon("question"), style="info", size="small")), 
                               choices = c("steelblue, yellow2", "mediumspringgreen, mediumvioletred", "orchid, palegreen", "cornflowerblue, forestgreen"), selected = "steelblue, yellow2" ),
                   bsPopover("qaf282", "Colors for the major and minor allele in the pie chart, respectively!", trigger = "focus"),
                   
                   numericInput("afHeight82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 650),
                   numericInput("afWidth82", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 800),
                   
                   shinysky::actionButton("submitaf182", strong("Submit!",
                                                                bsButton("qaf182", label="", icon=icon("question"), style="info", size="small")
                   ), styleclass = "success"),
                   
                   shinysky::actionButton("clearAf82", strong("Reset"), styleclass = "warning"),
                   shinysky::actionButton("AfExam82", strong("Load example"), styleclass = "info"),
                   
                   conditionalPanel(condition="input.submitaf182 != '0'", shinysky::busyIndicator(HTML("<p style='color:red;font-size:30px;'>Calculation In progress...</p>"), wait = 0)),
                   bsPopover("qaf182", "Whenever the input SNP sites or any option is updated, please click Submit!", trigger = "focus")
                 ),
                 
                 mainPanel(
                   width = 9,
                   
                   fluidRow(
                     column(4, uiOutput("downloadAfq0182")),
                     column(4, uiOutput("downloadAfq0282")),
                     column(4, uiOutput("downloadAfq0382"))
                   ),
                   
                   br(),
                   br(),
                   
                   plotOutput("alleleFreq82", height = "900px", width = "100%")
                 ))
    ))
        # Phylogenetic tree
        
        # SNP
        
      ),
      
      
      #Indel
      tabPanel(
        title = HTML("<strong style='font-size:20px'>INDELs</strong>"),
        
        sidebarPanel(
          tags$style("#regi {font-size:15px;font-family:sans-serif;}"),
          width = 3,
          fixedRow(
            column(12,
                   h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Search INDELs</b></font>'),
                      bsButton("qii", label="", icon=icon("question"), style="info", size="small")),
                   bsPopover("qii", "Search INDELs among 2898 soybean accessions in an input genomic region or gene model", trigger = "focus")
            )
          ),
          
          textInput("regi", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Genomic region</b></font>'),
                                       bsButton("qi1", label="", icon=icon("question"), style="info", size="small")),
                    value = "SoyZH13_01G186100"),
          
          bsPopover("qi1", "A genomic region can be determined by chromosome positions or gene locus. For example, chr1:29506705-29659223 or SoyZH13_01G186100.",
                    trigger = "focus"),
          
          shinyWidgets::multiInput("mychooseri", p(h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
                                                      bsButton("qindel2", label="", icon=icon("question"), style="info", size="small"))),
                                   choices = all.soya.cho,
                                   selected = c("Glycine soja"),
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

      #Expression
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>Expression</strong>"),
        #Gene expression
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Gene expression analysis</strong>"),
          sidebarPanel(
            #Expression of Zhonghuang 13
            tags$style("#gtsgeneid {font-size:15px;font-family:sans-serif;}"),
            width = 3,
            
            h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Gene expression analysis</b></font>'),
               bsButton("qsgts0", label="", icon=icon("question"), style="info", size="small")),
            bsPopover("qsgts0", "Inspect expression levels of input genes in different tissues/stages.", trigger = "focus"),

            selectInput("expression_genome", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select a dataset</b></font>'),
                                                    bsButton("qexpression1", label="", icon=icon("question"), style="info", size="small")), 
                        list("Expression data of Zhonghuang 13", 
                             "Expression data of A81-356022",
                             "Expression data of W05",
                             "Transcriptomes data of 102 soybean accessions"
                             ),
                            
                             #"Expression of seed development"),
                        selected = "Expression data of Zhonghuang 13"
            ),
            bsPopover("qexpression1", "Select a dataset for the input gene set.",
                      trigger = "focus"),
            
            
            tags$style("#gtsgenePaste {font-size:15px;font-family:sans-serif;}"),
            textAreaInput("gtsgenePaste", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input a list of gene model names</b></font>'), 
                                                     bsButton("qsgts1", label="", icon=icon("question"), style="info", size="small")),
                          value = "", resize = "vertical", height='200px', width = '200%',
                          placeholder = "One Gene ID in one row"),
            
            bsPopover("qsgts1", "Input a list gene ID of genome!",
                      trigger = "focus"),
            
            conditionalPanel(condition="input.expression_genome == 'Expression data of Zhonghuang 13'", 
                 shinyWidgets::multiInput("Gexp_mut_groupA", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Choose tissues/stages</b></font>'),
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
                 bsPopover("qsgex", "Choose one or multiple tissues and development stages.",
                           trigger = "focus"),
                             
            ),
            conditionalPanel(condition="input.expression_genome == 'Expression data of A81-356022'", 
                 shinyWidgets::multiInput("Gexp_mut_groupB", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Choose tissues/stages</b></font>'),
                            bsButton("qsgex1", label="", icon=icon("question"), style="info", size="small")),
                            choices = c("young_leaf", "flower", "one.cm.pod", "pod.shell.10DAF", "pod.shell.14DAF", "seed.10DAF", "seed.14DAF", "seed.21DAF", "seed.25DAF", "seed.28DAF", "seed.35DAF", "seed.42DAF", "root", "nodule"),
                            selected = c("young_leaf", "flower", "one.cm.pod", "pod.shell.10DAF", "pod.shell.14DAF", "seed.10DAF", "seed.14DAF", "seed.21DAF", "seed.25DAF", "seed.28DAF", "seed.35DAF", "seed.42DAF", "root", "nodule"),
                            width = 800,
                            options = list(
                            enable_search = TRUE,
                            non_selected_header = "Choose from:",
                            selected_header = "You have selected:"
                            )
                  ),
                 bsPopover("qsgex1", "Choose one or multiple tissues and development stages.",trigger = "focus"),
            ),
            
            #seed development in the cultivar Williams
            conditionalPanel(condition="input.expression_genome == 'Expression data of W05'",
                             shinyWidgets::multiInput("Gexp_mut_groupC", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Choose tissues/stages</b></font>'),
                                                      bsButton("qsgex", label="", icon=icon("question"), style="info", size="small")),
                                                      choices = c("Trifoliate", "Leaves", "Roots"),
                                                      selected = c("Trifoliate", "Leaves", "Roots"),
                                                      width = 800,
                                                      options = list(
                                                      enable_search = TRUE,
                                                      non_selected_header = "Choose from:",
                                                      selected_header = "You have selected:"
                                                      )
                             )
            ),
            conditionalPanel(condition="input.expression_genome == 'Transcriptomes data of 102 soybean accessions'", 
                  shinyWidgets::multiInput("Gexp_mut_groupD", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Choose accessions</b></font>'),
                            bsButton("qsgex2", label="", icon=icon("question"), style="info", size="small")),
                                  choices = c("Anderson", "Young", "Williams82", "Lawrence", "A.K.(Harrow)", "Capital", "Dunfield", "Illini", "Kanro", "Korean", "Lincoln", "Mandarin", "Mukden", "Richland", "Haberlandt", "Ogden", "Ralsoy", "Roanoke", "S-100", "Tokyo", "Amcor", "Beeson", "Century", "Blackhawk", "Bonus", "Pella", "Calland", "Chippewa", "Clark", "Corsoy", "Cumberland", "Oakland", "Merit", "Douglas", "Ford", "Harcor", "Harosoy", "Shelby", "Hawkeye", "Kent", "Perry", "Wayne", "Williams", "Woodworth", "Zane", "Hill", "Jackson", "Centennial", "Hood", "Tracy", "Brim", "Dare", "Pickett", "Ransom", "Davis", "Gasoy17", "Holladay", "NC-Roy", "5601T", "NC-Raleigh", "PI88.788", "TN05-3027", "4J105-3-4", "5M20-2-5-2", "CL0J095-4-6", "CL0J173-6-8", "HS6-3976", "Prohio", "LD00-3309", "LD01-5907", "LD02-4485", "LD02-9050", "Magellan", "Maverick", "S06-13640", "NE3001", "Skylla", "U03-100612", "LG03-2979", "LG03-3191", "LG04-4717", "LG05-4292", "LG05-4317", "LG05-4464", "LG05-4832", "LG90-2550", "LG92-1255", "LG94-1128", "LG94-1906", "LG97-7012", "LG98-1605", "LG00-3377", "LG04-6000", "PI398.881", "PI427.136", "PI437.169B", "PI507.681B", "PI518.751", "PI561.370", "PI404.188A", "PI574.486", "IA3023"),
                                  selected = c("Anderson", "Young", "Williams82", "Lawrence", "A.K.(Harrow)"),
                                  width = 800,
                                  options = list(
                                  enable_search = TRUE,
                                  non_selected_header = "Choose from:",
                                  selected_header = "You have selected:"
                                  ),

                      ),
            bsPopover("qsgex2", "Choose one or multiple accessions.",trigger = "focus"),
            ),

            
            
            

            fluidRow(
              column(6, actionButton("accessiongexnone", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
              column(6, actionButton("accessiongexall", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
            ),
            br(),
            
            conditionalPanel(condition="input.expression_genome == 'Expression data of Zhonghuang 13'", 
                             shinyWidgets::actionBttn("Searchname_description", "Information of 27 collected samples", 
                                                      block = TRUE, size = "sm", style="unite", color="default"),
                             bsModal("name-description", "Information of 27 collected samples", "Searchname_description", size = "large", 
                                     DT::dataTableOutput("expressionname1", width = "100%"),
                                     HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Title:</b></font>'),
                                     HTML('<i aria-hidden="true"></i> <font size="4" color="grey"><b>Update soybean Zhonghuang 13 genome to a golden reference</b></font>'),
                                     br(),
                                     HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>doi:</b></font>'),
                                     HTML('<i aria-hidden="true"></i> <font size="4" color="grey"><a href="https://doi.org/10.1007/s11427-019-9822-2">doi.org/10.1007/s11427-019-9822-2</a></font>')
                             )
            ),
            conditionalPanel(condition="input.expression_genome == 'Expression data of A81-356022'", 
                             shinyWidgets::actionBttn("Searchname_description2", "Information of 14 collected samples", 
                                                      block = TRUE, size = "sm", style="unite", color="default"),
                             bsModal("name-description2", "Information of 14 collected samples", "Searchname_description2", size = "large", 
                                     DT::dataTableOutput("expressionname2", width = "100%"),
                                     HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Title:</b></font>'),
                                     HTML('<i aria-hidden="true"></i> <font size="4" color="grey"><b>RNA-Seq Atlas of Glycine max: A guide to the soybean transcriptome</b></font>'),
                                     br(),
                                     HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>doi:</b></font>'),
                                     HTML('<i aria-hidden="true"></i> <font size="4" color="grey"><a href="https://doi.org/10.1186/1471-2229-10-160">doi.org/10.1186/1471-2164-13-310</a></font>')
                             )
            ),
            
            conditionalPanel(condition="input.expression_genome == 'Expression data of W05'",
                             shinyWidgets::actionBttn("Searchname_description3", "Information of 3 collected samples",
                                                      block = TRUE, size = "sm", style="unite", color="default"),
                             bsModal("name-description3", "Information of 3 collected samples", "Searchname_description3", size = "large",
                                     DT::dataTableOutput("expressionname3", width = "100%"),
                                     HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Title:</b></font>'),
                                     HTML('<i aria-hidden="true"></i> <font size="4" color="grey"><b>Identification of a novel salt tolerance gene in wild soybean by whole-genome sequencing</b></font>'),
                                     br(),
                                     HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>doi:</b></font>'),
                                     HTML('<i aria-hidden="true"></i> <font size="4" color="grey"><a href="https://doi.org/10.1038/ncomms5340">doi.org/10.1038/ncomms5340</a></font>')
                             )
            ),
            
            
            conditionalPanel(condition="input.expression_genome == 'Transcriptomes data of 102 soybean accessions'", 
                             shinyWidgets::actionBttn("Searchname_description4", "Information of 102 soybean accessions", 
                                                      block = TRUE, size = "sm", style="unite", color="default"),
                             bsModal("name-description4", "Information of 102 soybean accessions", "Searchname_description4", size = "large", 
                                     #DT::dataTableOutput("expressionname4", width = "100%"),
                                     HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Title:</b></font>'),
                                     HTML('<i aria-hidden="true"></i> <font size="4" color="grey"><b>TWAS results are complementary to and less affected by linkage disequilibrium than GWAS</b></font>'),
                                     br(),
                                     HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>doi:</b></font>'),
                                     HTML('<i aria-hidden="true"></i> <font size="4" color="grey"><a href="https://doi.org/10.1093/plphys/kiab161">doi.org/10.1093/plphys/kiab161</a></font>'),
                                     br(),
                                     HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Reference genome:</b></font>'),
                                     HTML('<i aria-hidden="true"></i> <font size="4" color="grey"><b>G. max Williams 82 V2</b></font>')
                             )
                             
            ),
            
            

            

            br(),
            

            shinysky::actionButton("submit_GSgst", strong("Submit!",
                                                          bsButton("qsgts2", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            
            shinysky::actionButton("cleargst", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("gstExamID", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="submit_GSgst != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qsgts2", "Click this button to start the analysis!",
                      trigger = "focus"),
            br(),
            numericInput("gstheight", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 1000),
            numericInput("gstwidth", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 1300),
            #numericInput("gstlabsize", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Lab font size</b></font>')), value = 15),
            numericInput("gstfontsize", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Font size</b></font>')), value = 15)
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
            #Expression of Zhonghuang 13
            width = 3,
            
            h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Gene co-expression analysis</b></font>'),
               bsButton("qsgcor0", label="", icon=icon("question"), style="info", size="small")),
            bsPopover("qsgcor0", "Perform co-expression analysis of genes across multiple tissues/samples.", trigger = "focus"),
            
            selectInput("cor_expression_genome", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select a dataset</b></font>'),
                                                        bsButton("qcorexpression1", label="", icon=icon("question"), style="info", size="small")), 
                        list("Expression data of Zhonghuang 13", "Expression data of A81-356022", "Expression data of W05", "Transcriptomes data of 102 soybean accessions"),
                        selected = "Expression data of Zhonghuang 13"
            ),
            bsPopover("qcorexpression1", "Select a genome for the input gene set.",
                      trigger = "focus"),
            
            
            tags$style("#genecorPaste {font-size:15px;font-family:sans-serif;}"),
            textAreaInput("genecorPaste", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input a list of gene model names</b></font>'), 
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
            numericInput("genecorheight", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 1000),
            numericInput("genecorwidth", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 1000)
          ),
          
          mainPanel(
            width = 9,
            htmlOutput("gcorout_title"),
            DT::dataTableOutput("gcordata", width = "100%"),
            uiOutput("genecorplotif"),
            br(), br()
            #plotOutput("genecorplot", height = "800px", width = "100%"),
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
          h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Search one or multiple soybean genomes by sequence similarity using BLAST</b></font>'),
             bsButton("qBlastTitle", label="", icon=icon("question"), style="info", size="small")),
          bsPopover("qBlastTitle", title = Blast_Info_Title, content = NULL, trigger = "focus"),
          
          tabsetPanel(id = "BLAST_tab",
                      tabPanel("Input",
                               fixedRow(
                                 column(5,
                                        selectInput("In_blast", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Paste or upload input data?</b></font>'),
                                                                                 bsButton("qBlastIn", label="", icon=icon("question"), style="info", size="small")
                                        ), choices = list("Paste input data" = "paste", 
                                                          "Upload input data" = "upload"), 
                                        selected = "paste"),
                                        bsPopover("qBlastIn", "The input data must be DNA sequences or Protein sequences in fasta format.", trigger = "focus"),
                                        
                                        conditionalPanel(condition="input.In_blast == 'paste'", 
                                                         textAreaInput("BlastSeqPaste", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input sequence</b></font>')),
                                                                       value = "", resize = "vertical", height='400px', width = '200%',
                                                                       placeholder = "The input sequences must be in fasta format")
                                        ),
                                        conditionalPanel(condition="input.In_blast == 'upload'", 
                                                         fileInput("BlastSeqUpload",
                                                                   label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Upload file</b></font>')), multiple = FALSE, width = "100%"),
                                                         downloadButton("BLAST_Input.txt", "Example BLAST input data", style = "width:100%;", class = "buttDown")
                                        )
                                 ),
                                 
                                 column(4,
                                        shinyWidgets::multiInput(
                                          inputId = "BLASTdb",
                                          label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Choose BLAST databases</b></font>'),
                                                           bsButton("qblastDB", label="", icon=icon("question"), style="info", size="small")
                                          ),
                                          choices = c("Zhonghuang 13","PI 562565","PI 549046","PI 578357","W05",
                                                      "PI 483463", "Lee", 
                                                      "Zhutwinning2","Zi Hua No.4","Tong Shan Tian E Dan","58-161","PI 398296",
                                                      "Zhang Chun Man Cang Jin","Feng Di Huang","Tie Jia Si Li Huang","Shi Sheng Chang Ye",
                                                      "Williams 82","Xu Dou No.1","Tie Feng No.18","Ju Xuan No.23","Wan Dou No.28","Amsoy","Yu Dou No.22","Jin Dou No.23",
                                                      "Qi Huang No.34","Han Dou No.5","PI 548362","Ji Dou No.17","Dong Nong No.50","Hei He No.43","Ke Shan No.1", "Tianlong1", "Hwangkeum",
                                                      "Glycine cyrtoloba", "Glycine dolichocarpa", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3"), 
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
                                        selectInput("program_database", label = h4(HTML('<i class="fa fa-play"></i> <font size="4" color="red"><b>Database type</b></font>'),
                                                                                         bsButton("qBLASTpdata", label="", icon=icon("question"), style="info", size="small")),
                                                    choices=c("Genome Sequence", "Protein Sequence", "Gene Sequence", "CDS sequence"), width = NULL),
                                        bsPopover("qBLASTpdata", "Set the type of BLAST database to search against.", 
                                                  trigger = "focus"),
                                        textInput("BLASTev", label = h4(HTML('<i class="fa fa-play"></i> <font size="4" color="red"><b>E-value cutoff</b></font>'),
                                                                              bsButton("qBLASTev", label="", icon=icon("question"), style="info", size="small")), 
                                                  value = "10", width = NULL, placeholder = NULL),
                                        bsPopover("qBLASTev", "Set E-value threshold to filter the BLAST output.",
                                                  trigger = "focus"),
                                        
                                        conditionalPanel(condition="input.program_database == 'Genome Sequence'", 
                                                         selectInput("programdna", label = h4(HTML('<i class="fa fa-play"></i> <font size="4" color="red"><b>Program</b></font>')), 
                                                                     choices=c("blastn","tblastn", "tblastx"), width = NULL),
                                        ),
                                        conditionalPanel(condition="input.program_database == 'Protein Sequence'", 
                                                         selectInput("programpro", label = h4(HTML('<i class="fa fa-play"></i> <font size="4" color="red"><b>Program</b></font>')), 
                                                                     choices=c("blastp", "blastx"), width = NULL),
                                        ),
                                        conditionalPanel(condition="input.program_database == 'Gene Sequence'", 
                                                         selectInput("programgene", label = h4(HTML('<i class="fa fa-play"></i> <font size="4" color="red"><b>Program</b></font>')), 
                                                                     choices=c("blastn","tblastn", "tblastx"), width = NULL),
                                        ),
                                        conditionalPanel(condition="input.program_database == 'CDS sequence'", 
                                                         selectInput("programcds", label = h4(HTML('<i class="fa fa-play"></i> <font size="4" color="red"><b>Program</b></font>')), 
                                                                     choices=c("blastn","tblastn", "tblastx"), width = NULL),
                                        ),
                                        #Filter (Low-complexity)
                                        checkboxInput("Filtercomplexity", "Filter low complexity region", FALSE),
                                        conditionalPanel(condition="input.programdna == 'blastn' & input.program_database == 'Genome Sequence'", 
                                                         checkboxInput("Filtershortsequence1", "Short sequence", FALSE)
                                        ),
                                        conditionalPanel(condition="input.programpro == 'blastp' & input.program_database == 'Protein Sequence'", 
                                                         checkboxInput("Filtershortsequence2", "Short sequence", FALSE)
                                        ),
                                        conditionalPanel(condition="input.programgene == 'blastn' & input.program_database == 'Gene Sequence'", 
                                                         checkboxInput("Filtershortsequence3", "Short sequence", FALSE)
                                        ),
                                        conditionalPanel(condition="input.programcds == 'blastn' & input.program_database == 'CDS sequence'", 
                                                         checkboxInput("Filtershortsequence4", "Short sequence", FALSE)
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
                               # conditionalPanel(condition="input.submitBLAST > 0", 
                               #                  tags$div(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>BLAST output</b></font>'),
                               #                           bsButton("qBLASTresultHelp", label="", icon=icon("question"), style="info", size="small")),
                               #                  bsPopover("qBLASTresultHelp", title = "Click on a row to check the details of the BLAST alignment!", trigger = "focus", content = NULL)
                               # ),
                               br(),
                               DT::dataTableOutput("BLASTresult"),
                               
                               fixedRow(
                                 column(1),
                                 column(4,
                                        htmlOutput("Alignment"),
                                        tableOutput("clicked"),
                                        br(),
                                        br(),
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
                                       max-height: 500px;
                                      }"
                                        )
                                 ),
                                 column(6,
                                        br(),
                                        br(),
                                       tags$style("#alignment{
                                        
                                         # width = 100%;
                                         # max-height: 400px;
                                         # white-space: pre-wrap;
                                         # background: white;
            
                                          # width: 800px;
                                          width: 100%;
                                          padding: 6px 12px;
                                          #white-space: pre-wrap;
                                          max-height: 400px;
                                          background: white;
                                        }"
                                        ),
                                        uiOutput("blastalignmentif"),
                                        #shinycssloaders::withSpinner(verbatimTextOutput("alignment", placeholder = FALSE)), 
                                        br(),
                                        br(),
                                        br(),
                                        br()
                                 ),
                                 column(1)
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
                     h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Desigin primer</b></font>'),
                        bsButton("qdp", label="", icon=icon("question"), style="info", size="small")),
                     bsPopover("qdp", "Design PCR primers for an input genomic region targeting SNPs and INDELs in this region.", trigger = "focus")
              )
            ),
            
            h4(HTML('<i class="fa fa-play"></i> <font size="4" color="black"><b>Parameter settings</b></font>')),
            
            
            textInput("primergene", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Genomic region</b></font>')
                                               , bsButton("qpr0", label = "", icon = icon("question"), style = "info", size = "small")),value = "SoyZH13_02G148200"),
            bsPopover("qpr0", "A genomic region can be determined by chromosome positions or gene locus. For example, chr2:17603220-17604802 or SoyZH13_02G148200.", trigger = "focus"),
            
            sliderInput("PRIMER_OPT_SIZE", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Optimal primer size (bp)</b></font>'),
                                                      bsButton("qpr1", label="", icon=icon("question"), style="info", size="small")),min = 16, max = 30, value = 20),
            bsPopover("qpr1", "Optimum length (in bases) of a primer. Primer3 will attempt to pick primers close to this length.",
                      trigger = "focus"),
            
            sliderInput("PRIMER_SIZE", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Primer size range (bp)</b></font>')
                                                  , bsButton("qpr2", label="", icon=icon("question"), style="info", size="small")), min = 1, max = 35, 
                        value = c(18, 25)),
            bsPopover("qpr2", "Minimum acceptable length  and Maximum acceptable length (in bases) of a primer", trigger = "focus"),    
            
            textInput("PRIMER_OPT_TM", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Optimal primer TM (℃)</b></font>')
                                                  , bsButton("qpr3", label = "", icon = icon("question"), style = "info", size = "small")),value = "59.0"),
            bsPopover("qpr3", "Optimum melting temperature (Celsius) for a primer.", trigger = "focus"),    
            
            fluidRow(
              column(6,textInput("PRIMER_MIN_TM", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Min primer TM (℃)</b></font>')
                                                             , bsButton("qpr4", label = "", icon = icon("question"), style = "info", size = "small")),value = "57.0"),
                     bsPopover("qpr4", "Minimum acceptable melting temperature (Celsius) for a primer oligo.", trigger = "focus")),
              column(6,textInput("PRIMER_MAX_TM", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Max primer TM (℃)</b></font>')
                                                             , bsButton("qpr5", label = "", icon = icon("question"), style = "info", size = "small")),value = "62.0"),
                     bsPopover("qpr5", "Maximum acceptable melting temperature (Celsius) for a primer oligo.", trigger = "focus"))
            ),
            
            textInput("PRIMER_OPT_GC_PERCENT", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Optimal GC percent:</b></font>')
                                                          , bsButton("qpr6", label = "", icon = icon("question"), style = "info", size = "small")),value = "50.0"),
            bsPopover("qpr6", "Optimum GC percent.", trigger = "focus"),
            
            sliderInput("PRIMER_GC", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>GC percent range (bp)</b></font>')
                                                , bsButton("qpr7", label = "", icon = icon("question"), style = "info", size = "small")), min = 0, max = 100, 
                        value = c(20, 80), step=0.1),
            bsPopover("qpr7", "Minimum allowed percentage of Gs and Cs in any primer and Maximum allowable percentage of Gs and Cs in any primer generated by Primer.", trigger = "focus"),
            
            textInput("PRIMER_MAX_NS_ACCEPTED", label = h4(HTML("<i class='fa fa-play' aria-hidden='true'></i> <font size='4' color='red'><b>Max #N's accepted</b></font>")
                                                           , bsButton("qpr8", label = "", icon = icon("question"), style = "info", size = "small")),value = "0"),
            bsPopover("qpr8", "Maximum number of unknown bases (N) allowable in any primer.", trigger = "focus"),
            
            fluidRow(
              column(6,textInput("PRIMER_MAX_POLY_X", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Max Poly-X</b></font>')
                                                                 , bsButton("qpr9", label = "", icon = icon("question"), style = "info", size = "small")),value = "10"),
                     bsPopover("qpr9", "The maximum allowable length of a mononucleotide repeat, for example AAAAAA.", trigger = "focus")),
              column(6,textInput("PRIMER_INTERNAL_MAX_POLY_X", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Max Internal Poly-X</b></font>')
                                                                          , bsButton("qpr10", label = "", icon = icon("question"), style = "info", size = "small")),value = "15"),
                     bsPopover("qpr10", "Equivalent parameter of PRIMER_MAX_POLY_X for the internal oligo.", trigger = "focus"))
            ),
            
            fluidRow(
              column(6,textInput("PRIMER_MAX_SELF_ANY", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Max Self Complementarity</b></font>')
                                                                   , bsButton("qpr11", label = "", icon = icon("question"), style = "info", size = "small")), value = "45.0"),
                     bsPopover("qpr11", "It is the maximum allowable local alignment score when testing a single primer for (local) self-complementarity.", trigger = "focus")),
              
              column(6,textInput("PRIMER_MAX_SELF_ANY_TH", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Max Pair Complementarity</b></font>')
                                                                      , bsButton("qpr12", label = "", icon = icon("question"), style = "info", size = "small")), value = "45.0"),
                     bsPopover("qpr12", "The maximum primer pair complementarity.", trigger = "focus"))
            ),
            
            fluidRow(
              column(6,textInput("PRIMER_MAX_SELF_END", label = h4(HTML("<i class='fa fa-play' aria-hidden='true'></i> <font size='3' color='red'><b>Max 3' Self Complementarity</b></font>")
                                                                   , bsButton("qpr13", label = "", icon = icon("question"), style = "info", size = "small")), value = "35.0")
                     , bsPopover("qpr13", "The maximum allowable 3\\'-anchored global.", trigger = "focus")),
              column(6,textInput("PRIMER_MAX_SELF_END_TH", label = h4(HTML("<i class='fa fa-play' aria-hidden='true'></i> <font size='3' color='red'><b>Max 3' Pair Complementarity</b></font>")
                                                                      , bsButton("qpr14", label = "", icon = icon("question"), style = "info", size = "small")), value = "35.0")
                     , bsPopover("qpr14", "Same as PRIMER_MAX_SELF_END but is based on thermodynamical approach - the stability of structure is analyzed. The value of tag is expressed as melting temperature. ", trigger = "focus"))
            ),
            
            textInput("PRIMER_PRODUCT_SIZE_RANGE", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="3" color="red"><b>Product Size Ranges</b></font>')
                                                              , bsButton("qpr15", label = "", icon = icon("question"), style = "info", size = "small")), value = "100-300,300-600,600-1000"), 
            bsPopover("qpr15", "If one desires PCR products in either the range from 100 to 300 bases or in the range from 300 to 600 (or 600 to 1000) bases then one would set this parameter to 100-300,300-600,600-1000.", trigger = "focus"),
            
            shinysky::actionButton("submitprimer", strong("Submit!",
                                                          bsButton("qprGO", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            shinysky::actionButton("clearprimer", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("primerExam", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="submitprimer != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qprGO", "Whenever the genomic region or any option is updated, please click Submit!",
                      trigger = "focus")
          ),
          
          mainPanel(
            # tags$style(HTML('#primertable tr:nth-child(4n+1) {background-color: skyblue !important;}')),
            # tags$style(HTML('#primertable tr:nth-child(4n+2) {background-color: skyblue !important;}')),
            # tags$style(HTML('#primertable tr:nth-child(4n+3) {background-color: pink !important;}')),
            # tags$style(HTML('#primertable tr:nth-child(4n+0) {background-color: pink !important;}')),
            tags$style(HTML('#primertable tr:nth-child(1) {background-color: skyblue !important;}')),
            tags$style(HTML('#primertable tr:nth-child(2) {background-color: skyblue !important;}')),
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
                     h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Orthologous Groups Search</b></font>'),
                        bsButton("qor1", label="", icon=icon("question"), style="info", size="small")),
                     bsPopover("qor1", "Orthologous Groups among 39 soybean genomes.", trigger = "focus")
              )
            ),
            # textInput("ORT_ID", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input a gene model</b></font>'),
            #                                bsButton("qor2", label="", icon=icon("question"), style="info", size="small")),value = "SoyZH13_01G225600"),
            
            textAreaInput("ORT_ID", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input a list of gene model names</b></font>')),
                          value = "SoyZH13_20G016601\nSoyZH13_10G058900\nSoyZH13_05G061800\nSoyZH13_18G189300\nSoyZH13_11G118400\nSoyZH13_16G065200\nSoyZH13_02G001200\nSoyZH13_20G007200\nSoyZH13_19G070100\nSoyZH13_16G092700\nSoyZH13_13G319000\nSoyZH13_11G230700\nSoyZH13_11G235100\nSoyZH13_12G154400",
                          resize = "vertical", height='200px', width = '200%',
                          bsButton("qor2", label="", icon=icon("question"), style="info", size="small"),
                          placeholder = "The Gene ID must in the database"),
            
            bsPopover("qor2", "Input a list gene model of any of the 39 soybean genomes.",
                      trigger = "focus"),
            checkboxInput("shownum", "Show the numbers", TRUE),
            shinysky::actionButton("ORT_sbumit", strong("Submit!"), styleclass = "success"),
            shinysky::actionButton("clearort", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("ortExam", strong("Load example"), styleclass = "info"),
            
            conditionalPanel(condition="ORT_sbumit != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0))
          ),
          
          mainPanel(
            textOutput("ortIDtitle"),
            DT::dataTableOutput("ortID"),
            uiOutput("orthplotif"),
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
            h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>GO Annotation</b></font>'),
               bsButton("qGOre0", label="", icon=icon("question"), style="info", size="small")),
            bsPopover("qGOre0", "Perform GO annotation for input gene sets.", trigger = "focus"),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("GO_variety_ID", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>'),
                                                    bsButton("qGOre1", label="", icon=icon("question"), style="info", size="small")), 
                        list(`Wild soybean (Glycine soja)` = list("PI 483463", "PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar (Glycine max)` = list("Zhonghuang 13", "Williams 82", "Lee", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                                      "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                                      "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1", "Tianlong1", "Hwangkeum"),
                             `Landrace (Glycine max)` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                                             "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye"),
                             `Perennial Glycine` = list("Glycine cyrtoloba", "Glycine dolichocarpa", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3") ) ,
                        selected = "Zhonghuang 13"
            ),
            bsPopover("qGOre1", "Select a genome for the input gene set.",
                      trigger = "focus"),
            
            selectInput("In_GOre", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Paste or upload input data?</b></font>'),
                                                    bsButton("qGOre2", label="", icon=icon("question"), style="info", size="small")
            ), choices = list("Paste input data" = "paste2", 
                              "Upload input data" = "upload2"), 
            selected = "paste"),
            bsPopover("qGOre2", "The input data must be multiple gene model names.", trigger = "focus"),
            
            conditionalPanel(condition="input.In_GOre == 'paste2'", 
                             textAreaInput("GOGENErePaste", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input a list of gene model names</b></font>')),
                                           value = "", resize = "vertical", height='200px', width = '200%',
                                           placeholder = "One Gene ID in one row.")
            ),
            conditionalPanel(condition="input.In_GOre == 'upload2'", 
                             fileInput("GOGENEreUpload",
                                       label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Upload file</b></font>')), multiple = FALSE, width = "100%"),
                             downloadButton("GOre_Input.txt", "Example input gene set", style = "width:100%;", class = "buttDown")
            ),
            
            sliderInput("GOnrow", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Maximum number of GO terms</b></font>'),
                                     bsButton("qGOid4", label="", icon=icon("question"), style="info", size="small")
            ), min = 1, max = 50, value = 30, ticks = FALSE),
            bsPopover("qGOid4", "Maximum number of GO terms to display",
                      trigger = "focus"),
            
            shinysky::actionButton("GOre_sbumit", strong("Submit!",
                                                         bsButton("qGO3", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            
            shinysky::actionButton("clearGOre", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("GOreExam", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="GOre_sbumit != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qGO3", "Click this button to start the annotation!",
                      trigger = "focus"),
            
            numericInput("goanheight", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 800),
            numericInput("goanwidth", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 1300)
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
            h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>GO enrichment analysis</b></font>'),
               bsButton("qGOge0", label="", icon=icon("question"), style="info", size="small")),
            bsPopover("qGOge0", "Perform GO enrichment analysis for input gene sets.", trigger = "focus"),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("GOge_variety_ID", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>'),
                                                      bsButton("qGOge1", label="", icon=icon("question"), style="info", size="small")), 
                        list(`Wild soybean (Glycine soja)` = list("PI 483463", "PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar (Glycine max)` = list("Zhonghuang 13", "Williams 82", "Lee", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                                      "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                                      "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1", "Tianlong1", "Hwangkeum"),
                             `Landrace (Glycine max)` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                                             "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye"),
                             `Perennial Glycine` = list("Glycine cyrtoloba", "Glycine dolichocarpa", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3") ) ,
                        selected = "Zhonghuang 13"
            ),
            bsPopover("qGOge1", "Select a genome for the input gene set.", trigger = "focus"),
            
            selectInput("In_GOge", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Paste or upload input data?</b></font>'),
                                                    bsButton("qGOge2", label="", icon=icon("question"), style="info", size="small")
            ), choices = list("Paste input data" = "paste3", 
                              "Upload input data" = "upload3"), selected = "paste"),
            bsPopover("qGOge2", "The input data must be multiple gene model names.", trigger = "focus"),
            
            conditionalPanel(condition="input.In_GOge == 'paste3'", 
                             textAreaInput("GOGENEgePaste", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input a list of gene model names</b></font>')),
                                           value = "", resize = "vertical", height='200px', width = '200%',
                                           placeholder = "One gene ID in one row.")
            ),
            conditionalPanel(condition="input.In_GOge == 'upload3'", 
                             fileInput("GOGENEgeUpload",
                                       label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Upload file</b></font>')), multiple = FALSE, width = "100%"),
                             downloadButton("GOge_Input.txt", "Example input gene set", style = "width:100%;", class = "buttDown")
            ),
            
            sliderInput("GOgenrow", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Maximum number of GO terms</b></font>'),
                                       bsButton("qGOge3", label="", icon=icon("question"), style="info", size="small")
            ), min = 1, max = 50, value = 30, ticks = FALSE),
            bsPopover("qGOge3", "Maximum number of GO terms to display", trigger = "focus"),
            
            sliderInput("GOp", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>P-adjust</b></font>'),
                                  bsButton("qGOid5", label="", icon=icon("question"), style="info", size="small")
            ), min = 0.01, max = 1, value = 0.05, ticks = FALSE),
            bsPopover("qGOid5", "Maximum adjusted p value allowed.",
                      trigger = "focus"),
            
            sliderInput("GOq", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Q-value</b></font>'),
                                  bsButton("qGOid6", label="", icon=icon("question"), style="info", size="small")
            ), min = 0.01, max = 1, value = 0.05, ticks = FALSE),
            bsPopover("qGOid6", "Maximum Q value allowed.",
                      trigger = "focus"),
            shinysky::actionButton("GOge_sbumit", strong("Submit!",
                                                         bsButton("qge4", label="", icon=icon("question"), style="info", size="small")), styleclass = "success"),
            
            shinysky::actionButton("clearGOge", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("GOgeExam", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="GOge_sbumit != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qge4", "Click this button to start the analysis!", trigger = "focus"),
            
            fluidRow(
              column(12,h5(HTML('<i class="fa fa-cog"></i> <font size="5" color="red"><b>Molecular Function</b></font>'))),
              column(6,numericInput("goheight1", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 400)),
              column(6,numericInput("gowidth1", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 1300))
            ),
            fluidRow(
              column(12,h5(HTML('<i class="fa fa-cog"></i> <font size="5" color="red"><b>Biological Process</b></font>'))),
              column(6,numericInput("goheight2", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 400)),
              column(6,numericInput("gowidth2", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 1300))
            ),
            fluidRow(
              column(12,h5(HTML('<i class="fa fa-cog"></i> <font size="5" color="red"><b>Cellular Component</b></font>'))),
              column(6,numericInput("goheight3", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 300)),
              column(6,numericInput("gowidth3", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 1300))
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
            h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>KEGG Annotation</b></font>'),
               bsButton("qKEre0", label="", icon=icon("question"), style="info", size="small")),
            bsPopover("qKEre0", "Perform KEGG annotation for input gene sets.", trigger = "focus"),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("KE_variety_ID", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>'),
                                                    bsButton("qKEre1", label="", icon=icon("question"), style="info", size="small")), 
                        list(`Wild soybean (Glycine soja)` = list("PI 483463", "PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar (Glycine max)` = list("Zhonghuang 13", "Williams 82", "Lee", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                                      "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                                      "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1", "Tianlong1", "Hwangkeum"),
                             `Landrace (Glycine max)` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                                             "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye"),
                             `Perennial Glycine` = list("Glycine cyrtoloba", "Glycine dolichocarpa", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3") ) ,
                        selected = "Zhonghuang 13"
            ),
            bsPopover("qKEre1", "Select a genome for the input gene set.", trigger = "focus"),
            
            selectInput("In_KEre", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Paste or upload input data?</b></font>'),
                                                    bsButton("qKEre2", label="", icon=icon("question"), style="info", size="small")
            ), choices = list("Paste input data" = "paste2", 
                              "Upload input data" = "upload2"), 
            selected = "paste"),
            bsPopover("qKEre2", "The input data must be multiple gene model names.", trigger = "focus"),
            
            conditionalPanel(condition="input.In_KEre == 'paste2'", 
                             textAreaInput("KEGG_GENErePaste", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input a list of gene model names</b></font>')),
                                           value = "", resize = "vertical", height='200px', width = '200%',
                                           placeholder = "One gene ID in one row.")
            ),
            conditionalPanel(condition="input.In_KEre == 'upload2'", 
                             fileInput("KEGG_GENEreUpload",
                                       label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Upload file</b></font>')), multiple = FALSE, width = "100%"),
                             downloadButton("KOre_Input.txt", "Example input gene set", style = "width:100%;", class = "buttDown")
            ),
            sliderInput("KOnrow", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Maximum number of KEGG pathways</b></font>'),
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
            numericInput("kegganheight", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 800),
            numericInput("kegganwidth", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 1300)
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
            h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>KEGG enrichment analysis</b></font>'),
               bsButton("qKE0", label="", icon=icon("question"), style="info", size="small")),
            bsPopover("qKE0", "Perform KEGG enrichment analysis for input gene sets.", trigger = "focus"),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            selectInput("KEGG_variety_ID", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select a genome</b></font>'),
                                                      bsButton("qKE1", label="", icon=icon("question"), style="info", size="small")), 
                        list(`Wild soybean (Glycine soja)` = list("PI 483463", "PI 562565", "PI 549046", "PI 578357", "W05"),
                             `Improved cultivar (Glycine max)` = list("Zhonghuang 13", "Williams 82", "Lee", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                                      "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                                      "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1", "Tianlong1", "Hwangkeum"),
                             `Landrace (Glycine max)` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                                             "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye"),
                             `Perennial Glycine` = list("Glycine cyrtoloba", "Glycine dolichocarpa", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3") ) ,
                        selected = "Zhonghuang 13"
            ),
            bsPopover("qKE1", "Select a genome for the input gene set.", trigger = "focus"),
            
            selectInput("In_KE", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Paste or upload input data?</b></font>'),
                                                  bsButton("qKE2", label="", icon=icon("question"), style="info", size="small")
            ), choices = list("Paste input data" = "paste4", 
                              "Upload input data" = "upload4"), 
            selected = "paste4"),
            bsPopover("qKE2", "The input data must be multiple gene model names.", trigger = "focus"),
            
            conditionalPanel(condition="input.In_KE == 'paste4'", 
                             textAreaInput("KEGG_GENErePaste1", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input a list of gene model names</b></font>')),
                                           value = "", resize = "vertical", height='200px', width = '200%',
                                           placeholder = "One gene ID in one row.")
            ),
            conditionalPanel(condition="input.In_KE == 'upload4'", 
                             fileInput("KEGG_GENEreUpload1",
                                       label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Upload file</b></font>')), multiple = FALSE, width = "100%"),
                             downloadButton("KO_Input.txt", "Example input gene set", style = "width:100%;", class = "buttDown")
            ),
            
            sliderInput("KEnrow", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Maximum number of KEGG pathways</b></font>'),
                                     bsButton("qKEid3", label="", icon=icon("question"), style="info", size="small")
            ), min = 1, max = 50, value = 30, ticks = FALSE),
            bsPopover("qKEid3", "Maximum number of KEGG pathways to display",
                      trigger = "focus"),
            
            sliderInput("KEp", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>P-adjust</b></font>'),
                                  bsButton("qKEid5", label="", icon=icon("question"), style="info", size="small")
            ), min = 0.01, max = 1, value = 0.05, ticks = FALSE),
            bsPopover("qKEid5", "Maximum adjusted p value allowed.",
                      trigger = "focus"),
            
            sliderInput("KEq", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Q-value</b></font>'),
                                  bsButton("qKEid6", label="", icon=icon("question"), style="info", size="small")
            ), min = 0.01, max = 1, value = 0.05, ticks = FALSE),
            bsPopover("qKEid6", "Maximum Q value allowed.",
                      trigger = "focus"),
            shinysky::actionButton("KEGG_sbumit", strong("Submit!",
                                                         bsButton("qKE3", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            shinysky::actionButton("clearKO", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("KOExam", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="KEGG_sbumit != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
            bsPopover("qKE3", "Click this button to start the analysis!",
                      trigger = "focus"),
            numericInput("keggheight", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot height</b></font>')), value = 800),
            numericInput("keggwidth", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Plot width</b></font>')), value = 1300)
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
        ),
        #ID covernt
        tabPanel(
          title = HTML("<strong style='font-size:20px'>ID Converter</strong>"), icon = icon("id-card"),
          
          sidebarPanel(
            width = 4,
            tags$style("#IDCOV_idcov {font-size:15px;font-family:sans-serif;}"),
            fixedRow(
              column(12,
                     h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>ID Converter</b></font>'),
                        bsButton("qsgsid", label="", icon=icon("question"), style="info", size="small")),
                     bsPopover("qsgsid", "Search any one of the 39 soybean genomes by gene IDs", trigger = "focus")
              )
            ),
            
            tags$style(type='text/css', ".selectize-dropdown-content {max-height: 600px; }"),
            fluidRow(
              column(
                width = 1,
                h4("From")
              ),
              column(5,selectInput("variety_idcov",label = NULL,
                                   list(`Wild soybean (Glycine soja)` = list("PI 483463", "PI 562565", "PI 549046", "PI 578357", "W05"),
                                        `Improved cultivar (Glycine max)` = list("Zhonghuang 13", "Williams 82", "Lee", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                                                 "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                                                 "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1", "Tianlong1", "Hwangkeum"),
                                        `Landrace (Glycine max)` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                                                        "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye"),
                                        `Perennial Glycine` = list("Glycine cyrtoloba", "Glycine dolichocarpa", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3")
                                   ) ,
                                   selected = "Zhonghuang 13"
              )),
              column(
                width = 1,
                h4("To")
              ),
              column(5,selectInput("variety_idcov_1", label = NULL,
                                   list(`Wild soybean (Glycine soja)` = list("PI 483463", "PI 562565", "PI 549046", "PI 578357", "W05"),
                                        `Improved cultivar (Glycine max)` = list("Zhonghuang 13", "Williams 82", "Lee", "Xu Dou No.1", "Tie Feng No.18", "Ju Xuan No.23", "Wan Dou No.28", "Amsoy",
                                                                                 "Yu Dou No.22", "Jin Dou No.23", "Qi Huang No.34", "Han Dou No.5", "PI 548362",
                                                                                 "Ji Dou No.17", "Dong Nong No.50", "Hei He No.43", "Ke Shan No.1", "Tianlong1", "Hwangkeum"),
                                        `Landrace (Glycine max)` = list("Zhutwinning2", "Zi Hua No.4", "Tong Shan Tian E Dan", "58-161", "PI 398296",
                                                                        "Zhang Chun Man Cang Jin", "Feng Di Huang", "Tie Jia Si Li Huang", "Shi Sheng Chang Ye"),
                                        `Perennial Glycine` = list("Glycine cyrtoloba", "Glycine dolichocarpa", "Glycine falcata", "Glycine stenophita", "Glycine syndetika", "Glycine tomentella D3")
                                   ) ,
                                   selected = "Williams 82"
              ))
            ),
            
            textAreaInput("IDCOV_idcov", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input a list of gene model names</b></font>'), 
            ),
            value = "SoyZH13_01G071100\nSoyZH13_01G123300", resize = "vertical", height='200px', width = '200%',
            placeholder = "One Gene ID in one row"),
            
            # 
            # textAreaInput("IDCOV_idcov", label = h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Input Gene ID</b></font>'),
            #                                     bsButton("qsgeneid1", label="", icon=icon("question"), style="info", size="small")), value = "SoyZH13_01G071100"),
            
            
            
            
            shinysky::actionButton("submit_GSidcov", strong("Submit!", 
                                                            bsButton("qsgsid1", label="", icon=icon("question"), style="info", size="small")
            ), styleclass = "success"),
            bsPopover("qsgsid1", "Whenever the Gene ID is updated, please click Submit!",
                      trigger = "focus"),
            shinysky::actionButton("clearSERIDcov", strong("Reset"), styleclass = "warning"),
            shinysky::actionButton("SERExamIDcov", strong("Load example"), styleclass = "info"),
            conditionalPanel(condition="input.submit_GSidcov != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0))
          ),
          
          mainPanel(
            width = 8,
            fluidRow(
              column(2, uiOutput("downloadIDCOV01")),
              column(2, uiOutput("downloadIDCOV02")), 
              column(2, uiOutput("downloadIDCOV03")),
              column(2, uiOutput("downloadIDCOV04"))
            ),
            
            htmlOutput("geneinfoidCOV_title"),
            column(12, DT::dataTableOutput("geneinfoidCOV")),
            tags$head(tags$style("#geneinfoidCOV_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#geneinfoidCOV{
                     width = 100%;
                     max-height: 300px;
            }")
            ),
            
            htmlOutput("geneticIDstructureCOV_title"),
            uiOutput("geneticIDstructureCOVif"),
            #plotOutput("geneticIDstructureCOV", width = "100%", height = "100px"),
            
            tags$head(tags$style("#geneticIDstructureCOV_title{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            )),
            
            htmlOutput("gene_titleCOV_id"),
            verbatimTextOutput("gene_idCOV"),
            tags$head(tags$style("#gene_titleCOV_id{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#gene_idCOV{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
            }")
            ),
            
            htmlOutput("cds_titleCOV_id"),
            verbatimTextOutput("cds_idCOV"),
            tags$head(tags$style("#cds_titleCOV_id{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#cds_idCOV{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
            }")
            ),
            
            htmlOutput("cdna_titleCOV_id"),
            verbatimTextOutput("cdna_idCOV"),
            tags$head(tags$style("#cdna_titleCOV_id{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#cdna_idCOV{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
            }")
            ),
            
            htmlOutput("pro_titleCOV_id"),
            shiny::verbatimTextOutput("pro_idCOV"),
            tags$head(tags$style("#pro_titleCOV_id{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#pro_idCOV{
                     width = 100%;
                     max-height: 300px;
                     white-space: pre-wrap;
                     
                     
            }")
            ),
            
            htmlOutput("Functionaltitle_idCOV"),
            column(12,
                   DT::dataTableOutput("Functional_idCOV")),
            tags$head(tags$style("#Functionaltitle_idCOV{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#Functional_idCOV{
                     width = 100%;
                     max-height: 300px;
            }")
            ),
            
            htmlOutput("gffinfotitle_idCOV"),
            column(12,
                   DT::dataTableOutput("gffinfo_idCOV")),
            tags$head(tags$style("#gffinfotitle_idCOV{color: red;
                                       font-style: bold;
                                       font-size: 18px;
                                       font-family:Times New Roman;
                                      }"
            ),
            tags$style("#gffinfo_idCOV{
                     width = 100%;
                     max-height: 300px;
            }")
            ),
            column(12, br() ),
            uiOutput("zhanwei3COV")
          )
        )
        
        
        
      ),
      
      # Accession
      tabPanel(
        title = HTML("<strong style='font-size:20px'>Accessions</strong>"),
        tabsetPanel(id = "Accessions_key",
                    tabPanel(HTML("<strong style='font-size:18px'>Information of 2898 soybean accessions</strong>"),
        sidebarPanel(
          width = 3,
          
         
          shinyWidgets::multiInput("mychooserA", h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
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
      tabPanel(HTML("<strong style='font-size:18px'>Information of 481 soybean accessions</strong>"),
                 sidebarPanel(
                   width = 3,
                     shinyWidgets::multiInput("mychooserAB", p(h4(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red"><b>Select soybean accessions</b></font>'),
                                                                  bsButton("qa2", label="", icon=icon("question"), style="info", size="small"))),
                                              choices = soynm,
                                              selected = soynm,
                                              width = 800,
                                              options = list(
                                                enable_search = TRUE,
                                                non_selected_header = "Choose from:",
                                                selected_header = "You have selected:"
                                              )
                     ), 
                     
                     
                     bsPopover("qa2", "Only the chosen soybean accessions will be displayed.",
                               trigger = "focus"),
                     fluidRow(
                       column(6, actionButton("accessionnone2", strong("Deselect all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                       column(6, actionButton("accessionall2", strong("Select all"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                     )
                   
                 ),
                 
                 mainPanel(
                   width = 9,
                   HTML('<i class="fa fa-circle" aria-hidden="true" style="color:red"></i> <font size="5" color="red"><b>Information of selected soybean accessions</b></font>'),
                   
                     DT::dataTableOutput("mytable12"),
                   br(),
                   br()
                 )
      ))),
      
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>Download</strong>"), icon = icon("download"),
        tabPanel(
          HTML("<strong style='font-size:20px'>Genome</strong>"),
          DT::dataTableOutput("Download_table_out1")
        ),
        tabPanel(
          HTML("<strong style='font-size:20px'>TF/TR&TE</strong>"),
          DT::dataTableOutput("Download_table_out2")
        ),
        tabPanel(
          HTML("<strong style='font-size:20px'>GO&KEGG</strong>"),
          DT::dataTableOutput("Download_table_out3")
        )
        
        
        
        # tabPanel(
        #   HTML("<strong style='font-size:20px'>Genome data</strong>"),
        #   
        #   
        #   tabsetPanel(id = "download_1",
        #               tabPanel(h4("Fasta"),
        #                        DT::dataTableOutput("fasta_table_out")
        #               ),
        #               tabPanel(h4("Transposable elements"),
        #                        DT::dataTableOutput("TE_table_out")
        #               ),
        #               tabPanel(h4("GFF"),
        #                        DT::dataTableOutput("gff_table_out")
        #               )
        #   )
        # ),
        
        # tabPanel(
        #   HTML("<strong style='font-size:20px'>BLASTdb</strong>"),
        #   
        #   
        #   tabsetPanel(id = "download_2",
        #               tabPanel(h4("Genome"),
        #                        DT::dataTableOutput("genomeTable", width = "100%")
        #               ),
        #               tabPanel(h4("CDS"),
        #                        DT::dataTableOutput("cdsTable")
        #               ),
        #               tabPanel(h4("Gene"),
        #                        DT::dataTableOutput("GeneTable")
        #               ),
        #               tabPanel(h4("Protein"),
        #                        DT::dataTableOutput("ProteinTable")
        #               )
        #   )
        # )
      ),
      
      
      navbarMenu(
        title = HTML("<strong style='font-size:20px'>Help</strong>"), icon = icon("book"),
        # Data source
        tabPanel(
          title = HTML("<strong style='font-size:20px'>Data source</strong>"),
          column(2),
          column(8,
                 h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>39 soybean genomes</b></font>')),
                 DT::dataTableOutput('Downloadtable', width = "100%"),
                 br(),
                 h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>SNPs among 2898 soybean accessions</b></font>')),
                 h4(a("https://ngdc.cncb.ac.cn/gvm/getProjectDetail?project=GVM000063", href="https://ngdc.cncb.ac.cn/gvm/getProjectDetail?project=GVM000063", target="_blank")),
                 br(),
                 
                 h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>SNPs among 481 soybean accessions</b></font>')),
                 h4(a("https://doi.org/10.1038/s41597-021-00834-w", href="https://doi.org/10.1038/s41597-021-00834-w", target="_blank")),
                 br(),
                 
                 h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Expression profile of protein-coding genes in the genome of Zhonghuang 13</b></font>')),
                 h4(a("https://link.springer.com/article/10.1007/s11427-019-9822-2", href="https://link.springer.com/article/10.1007/s11427-019-9822-2", target="_blank")),
                 br(),
                 
                 h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Expression profile of protein-coding genes in the genome of A81-356022</b></font>')),
                 h4(a("https://bmcplantbiol.biomedcentral.com/articles/10.1186/1471-2229-10-160", href="https://bmcplantbiol.biomedcentral.com/articles/10.1186/1471-2229-10-160", target="_blank")),
                 br(),
                 
                 h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Expression profile of protein-coding genes in the genome of W05</b></font>')),
                 h4(a("https://www.nature.com/articles/ncomms5340", href="https://www.nature.com/articles/ncomms5340", target="_blank")),
                 br(),
                 
                 h4(HTML('<i class="fa fa-circle" aria-hidden="true"></i> <font size="4" color="red"><b>Transcriptomes data of 102 soybean accessions</b></font>')),
                 h4(a("https://bmcplantbiol.biomedcentral.com/articles/10.1186/1471-2229-10-160", href="https://bmcplantbiol.biomedcentral.com/articles/10.1186/1471-2229-10-160", target="_blank")),
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
        ),
        # NEWS
        tabPanel(HTML("<strong style='font-size:20px'>News</strong>"),
                 column(2),
                 column(8, includeMarkdown("./NEWS.md")),
                 column(2)
        ),
        
        tabPanel(HTML("<strong style='font-size:20px'>Contact</strong>"),
                 column(2),
                 column(8, includeMarkdown("./Contact.md")),
                 column(2)
        )
        
        
      ),
      tabPanel(HTML("<strong style='font-size:20px'>Literature</strong>"),
               column(2),
               column(8, includeMarkdown("./Literature.md")),
               column(2)
      ),
      
      
      footer = footerTagList
    )
 )
)

