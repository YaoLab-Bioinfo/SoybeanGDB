Homepage <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(disable = T),
  shinydashboard::dashboardSidebar(disable = T),
  shinydashboard::dashboardBody(
    tags$head(tags$style("section.content { overflow-y: hidden; }")),
    column(
      width = 10,
      offset = 1,
      titleBox(title = p("SoybeanGDB: A comprehensive genome database of soybean"))
    ),
    column(
      width = 10,
      offset = 1,
      textBox(
        width = 12,
        p("A total of 29 high-quality soybean genomes were collected, including a Chinese soybean", strong(em("Glycine max")), strong("[L.] Merr. cv. Zhonghuang 13."),
          "High-quality SNPs and INDELs were identified among 2898 soybean accessions based on the genome of Zhonghuang 13.")
      )
    ),

    column(
      width = 10,
      offset = 1,
      sectionBox(
        title = "Statistics",
        fluidRow(
          valueBox("2898", "Soybean germplasm collection",  width = 3),
          valueBox("15,446,616", "High quality SNPS", width = 3),
          valueBox("4,136,231", "High quality INDELS", width = 3),
          valueBox("29", "High quality soybean genomes", width = 3)
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
              shinyWidgets::actionBttn("Browse_botton", "Browse SNPs", 
                         icon = icon("window-maximize", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Browse SNPs among 2898 soybean accessions")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_SnpInfo", "Search SNPs", 
                         icon = icon("search", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search SNPs among 2898 soybean accessions")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Linkage_anal", "LDheatmap", 
                         icon = icon("fire", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Linkage disequilibrium analysis between SNPs in a genomic region")
          )
        ),
        
        fluidRow(
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_diversity", "Nucleotide diversity", 
                         icon = icon("hourglass", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Nucleotide diversity analysis among different groups of soybean accessions")
          ),
          box(width = 4,
              shinyWidgets::actionBttn("Link_Phylogenetic", "Phylogenetic analysis", 
                         icon = icon("cloud", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Phylogenetic analysis based on SNPs in a genomic region")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_AlleleFreq", "Allele frequency", 
                                       icon = icon("code-branch", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Allele frequency analysis of user-input SNP sites")
          )
          
        ),
        
        fluidRow(
          box(width = 4,
              shinyWidgets::actionBttn("Link_Indel", "INDELs",
                                       
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search INDELs among 2898 soybean accessions")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_GeneInfoID", "Search by gene ID", 
                         icon = icon("id-card", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search 29 soybean genomes by gene ID")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_GeneInfoIT", "Search by genome location", 
                         icon = icon("search-location", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search 29 soybean genomes by location")
          )

        ),
        
        fluidRow(
          box(width = 4,
              shinyWidgets::actionBttn("Link_blast", "BLAST", 
                                       icon = icon("rocket", class = NULL, lib = "font-awesome"),
                                       block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search 29 soybean genomes using BLAST")
          ),
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_Primer", "Primer Design", 
                         icon = icon("drafting-compass", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Design primers based on the genome of Zhonghuang 13")
          ),
          
          
          box(width = 4,
              shinyWidgets::actionBttn("Link_Orthologous", "Orthologous", 
                         icon = icon("tree", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Orthologous Groups among 29 soybean genomes")
          )
        ) 
      )
    )
  )
)
