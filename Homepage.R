
Homepage <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(disable = T),
  shinydashboard::dashboardSidebar(disable = T),
  shinydashboard::dashboardBody(
    tags$head(tags$style("section.content { overflow-y: hidden; }")),
    column(
      width = 10,
      offset = 1,
      textBox(
        width = 12,
        p(strong("SoybeanGDB"), "is a comprehensive database providing informatics service to researchers focused on the genetic and genomic studies of soybean. A total of", strong("39 high-quality soybean genomes"), "were collected, including a Chinese soybean", strong(em("Glycine max")), strong("[L.] Merr. cv. Zhonghuang 13."),
          "High-quality SNPs and INDELs among 2898 soybean accessions based on the genome of Zhonghuang 13. High-quality SNPs among 481 soybean accessions based on the genome of William82.")
      )
    ),
    
    column(
      width = 10,
      offset = 1,
      sectionBox(
        title = "Statistics",
        # fluidRow(
        #   valueBox("39", "High quality soybean genomes", width = 3),
        #   valueBox("2898", "Soybean accessions",  width = 3),
        #   valueBox("15,446,616", "High quality SNPs", width = 3),
        #   valueBox("4,136,231", "High quality INDELs", width = 3)
        # ),
        fluidRow(
          box(width = 3,
              tags$style("#Link_JBrowse_up {font-size:30px;;}"),
              shinyWidgets::actionBttn("Link_JBrowse_up", "39", 
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("High quality soybean genomes")
          ),
          
          box(width = 3,
              tags$style("#Link_Accessions {font-size:30px;;}"),
              shinyWidgets::actionBttn("Link_Accessions", "2898 + 481",
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Soybean accessions")
          ),
          
          box(width = 3,
              tags$style("#Browse_botton_up {font-size:28px;;}"),
              shinyWidgets::actionBttn("Browse_botton_up", "15,446,616 + 7,792,974", 
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("High quality SNPs")
          ),
          box(width = 3,
              tags$style("#Link_Indel_up {font-size:30px;;}"),
              shinyWidgets::actionBttn("Link_Indel_up", "4,136,231",
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("High quality INDELs")
          )
          
          
        )
        
      )
    ),
    
    column(
      width = 10,
      offset = 1,
      sectionBox(
        title = "Functionalities of SoybeanGDB",
        fluidRow(
          box(width = 4,
              shinyWidgets::actionBttn("Link_GeneInfoID", "Genome -> Search by gene IDs", 
                                       icon = icon("id-card", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search 39 soybean genomes by gene ID")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_GeneInfoIT", "Genome -> Search by location", 
                                       icon = icon("search-location", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search 39 soybean genomes by location")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_Genetrf", "Genome -> Transcription factors/regulators", 
                                       icon = icon("house-user", class = NULL),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Transcription factors/regulators in 39 soybean genomes")
          )
        ),
        
        fluidRow(
          box(width = 4,
              shinyWidgets::actionBttn("Link_Genegsy", "Genome -> Genome synteny", 
                                       icon = icon("bezier-curve", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Syntenic regions between different soybean genomes")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_Genegsvr", "Genome -> Structural variations", 
                                       icon = icon("sliders-h", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search structural variations by location")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_JBrowse", "JBrowse", 
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("JBrowse of 39 soybean genomes")
          )
        ),
        
        fluidRow(
          
          box(width = 4,
              shinyWidgets::actionBttn("Browse_botton", "SNP -> Browse", 
                                       icon = icon("folder-open-o", class = NULL),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Browse SNPs among 2898/481 soybean accessions")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_SnpInfo", "SNP -> Search", 
                                       icon = icon("search", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search SNPs among 2898/481 soybean accessions")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Linkage_anal", "SNP -> LDheatmap", 
                                       icon = icon("project-diagram", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Linkage disequilibrium analysis between SNPs in a genomic region")
          )
        ),
        
        fluidRow(
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_diversity", "SNP -> Nucleotide diversity", 
                                       icon = icon("chart-area", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Nucleotide diversity analysis among different groups of soybean accessions")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_AlleleFreq", "SNP -> Allele frequency", 
                                       icon = icon("chart-pie", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Allele frequency analysis of user-input SNP sites")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_Indel", "INDELs",
                                       
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search INDELs among 2898 soybean accessions")
          )
          
        ),
        
        fluidRow(
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_Geneexpressionlevel", "Gene expression analysis", 
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Expression analysis of protein-coding genes in diverse tissues/stages")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_Geneexpressioncorrelationanalysis", "Co-expression analysis", 
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Co-expression analysis of genes")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_blast", "BLAST", 
                                       icon = icon("rocket", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search 39 soybean genomes using BLAST")
          )
    
        ),
        
        fluidRow(
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_Primer", "Primer Design", 
                                       icon = icon("drafting-compass", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Design primers based on the Zhonghuang 13 genome")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_Orthologous", "Orthologous", 
                                       icon = icon("people-arrows", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Orthologous groups among 39 soybean genomes")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_GOAnnotation", "GO Annotation", 
                                       icon = icon("sitemap", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("GO annotation of input gene sets")
          )
          
        ),
        
        fluidRow(
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_GOEnrichment", "GO Enrichment", 
                                       icon = icon("sitemap", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("GO enrichment analysis of input gene sets")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_KEGGanootation", "KEGG Annotation", 
                                       icon = icon("kaggle", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("KEGG annotation of input gene sets")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_KEGGEnrichment", "KEGG Enrichment", 
                                       icon = icon("kaggle", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("KEGG enrichment analysis of input gene sets")
          )
          
        ) 
      )
    )
  )
)

