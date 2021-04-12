library(shinydashboard)
Homepage <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(
    tags$head(tags$style("section.content { overflow-y: hidden; }")),
    column(
      width = 10,
      offset = 1,
      titleBox(title = p("SoybeanGDB: SNP Database of", em("Glycine max (Linn.) Merr.")))
    ),
    column(
      width = 10,
      offset = 1,
      textBox(
        width = 12,
        p("The SNP database of", em("Glycine max (Linn.) Merr."), "(SoybeanGDB) is an interactive web-based platform and set of analytic tools for effcient retrieve and analysis of SNPs among 2898 soybean germplasm accessions based on the data reported by previous research(", a("Tian et al., 2019", href = "https://doi.org/10.1016/j.cell.2020.05.023", target = "_blank"), ")")
      )
    ),

    column(
      width = 10,
      offset = 1,
      sectionBox(
        title = "Statistics",
        fluidRow(
          valueBox("2898", "Germplasm Collections",  width = 4),
          valueBox("15,446,616", "High quality SNPS", width = 4),          
          valueBox("4,136,231", "High quality INDELS", width = 4)
        ),
        fluidRow(
          valueBox("103", "G. Soja", width = 4),
          valueBox("1747", "Improved cultivar", width = 4),
          valueBox("1048", "Landrace", width = 4),
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
              actionBttn("Browse_botton", "Browse", 
                         icon = icon("folder-open-o", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Browse Soybean by genomes")
          ),
          
          box(width = 4,
              actionBttn("Link_SnpInfo", "Search", 
                         icon = icon("upload", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search SNP information")
          ),
          
          box(width = 4,
              actionBttn("Linkage_anal", "LDheatmap", 
                         icon = icon("search", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Linkage disequilibrium measurements between single nucleotide polymorphisms within a genomic region")
          )
        ),
        
        fluidRow(
          
          box(width = 4,
              actionBttn("Link_diversity", "Diversity", 
                         icon = icon("search", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Diversity")
          ),
          box(width = 4,
              actionBttn("Link_Phylogenetic", "Phylogenetic", 
                         icon = icon("cogs", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Phylogenetic")
          ),
          
          box(width = 4,
              actionBttn("Link_Indel", "Indel",
                         icon = icon("eercast", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search Indel information")
          )
          
        ),
        
        fluidRow(
          
          box(width = 4,
              actionBttn("Link_GeneInfoID", "Gene ID", 
                         icon = icon("bullseye", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search soybean Genomes information")
          ),
          
          box(width = 4,
              actionBttn("Link_GeneInfoIT", "Gene Location", 
                         icon = icon("bullseye", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search soybean Genomes information")
          ),
          
          box(width = 4,
              actionBttn("Link_blast", "BLAST", 
                         icon = icon("rocket", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Choose accession to blast")
          ),
        ),
        
        fluidRow(
          box(width = 4,
              actionBttn("Link_Primer", "Primer", 
                         icon = icon("file-image-o", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Design primer by Zhonghuang 13 genome")
          ),
          
          box(width = 4,
              actionBttn("Link_JBrowse", "JBrowse2", 
                         icon = icon("bullseye", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Browse 27 genomes information")
          ),
          
          box(width = 4,
              actionBttn("Link_Accession", "Accession", 
                         icon = icon("rocket", class = NULL, lib = "font-awesome"),
                         block = TRUE, size = "lg", style="unite", color="default"),
              h4("Search soybean Accession")
          )
        ) 
      )
    )
  )
)
