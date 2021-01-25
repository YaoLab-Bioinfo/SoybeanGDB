Homepage <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(
    column(
      width = 10,
      offset = 1,
      titleBox(title = p("SoyaSNPDB: A web server for SNP database of", em("Glycine max (Linn.) Merr.")))
    ),
    column(
      width = 10,
      offset = 1,
      textBox(
        width = 12,
        p("The SNP database of", em("Glycine max (Linn.) Merr."), "(SoyaSNPDB) is an interactive web-based platform and set of analytic tools for effcient retrieve and analysis of SNPs among 2898 soybean germplasm accessions based on the data reported by previous research(", a("Tian et al., 2019", href = "https://doi.org/10.1016/j.cell.2020.05.023", target = "_blank"), ")")
      )
    ),
    
    column(
      width = 10,
      offset = 1,
      titleBox(title = "Tutorial of SoyaSNPDB")
    ),
    column(
      width = 10,
      offset = 1,
      textBox(
        width = 12,
        p("To facilitate user access to SoyaSNPDB, We have written a Help manual, you can click this link to get the Help manual, or click the Help page to use.")
      )
    ),

    column(
      width = 10,
      offset = 1,
      sectionBox(
        title = "What's Inside",
        fluidRow(
          valueBox("15,446,616", "High quality SNPS", width = 4),
          valueBox("2898", "Germplasm Collections", color = "purple", width = 4),
          valueBox(26, "High average coverage depth", color = "yellow", width = 4)
        ),
        fluidRow(
          valueBox("103", "G. Soja", color = "fuchsia", width = 4),
          valueBox("1747", "Improved cultivar", color = "navy", width = 4),
          valueBox("1048", "Landrace", color = "olive", width = 4),
        )
      )
    ),

    column(
      width = 10,
      offset = 1,
      sectionBox(
        title = "Functionalities of SoyaSNPDB",
          fluidRow(
            module_Box(
              width = 4, height='210px',
              title = "Browse",
              imgSrc = "browser.png",
              text = "Browse long inverted repeats identified in 424 eukaryotic genomes for the sequences, structures of LIRs and the overlaps between LIRs and genes."
            ),
            module_Box(
              width = 4, height='210px',
              title = "LDheatmap",
              imgSrc = "SearchByReg.png",
              text = "Search LIRBase for long inverted repeats in a specific genome by genomic locations."
            ),
			module_Box(
              width = 4, height='210px',
              title = "Diversity",
              imgSrc = "SearchByLIRID.png",
              text = "Search LIRBase for long inverted repeats in a specific genome by the identifiers of long inverted repeats."
            )
          ),
          fluidRow(
            module_Box(
              width = 4, height='230px',
              title = "Phylogenetic",
              imgSrc = "BLAST.png",
              text = "Search LIRBase by sequence similarity using BLAST."
            ),
			module_Box(
              width = 4, height='230px',
              title = "Accession",
              imgSrc = "Annotate.png",
              text = "Detect and annotate long inverted repeats in user-uploaded DNA sequences."
            ),
			module_Box(
      			  width = 4, height='230px',
      			  title = "Search",
      			  imgSrc = "Annotate.png",
      			  text = "Detect and annotate long inverted repeats in user-uploaded DNA sequences."
			      )
          ),
		  fluidRow(
		        module_Box(
              width = 4, height='210px',
              title = "Blast",
              imgSrc = "Quantify.png",
              text = "Align small RNA sequencing data to LIRs of a specific genome to detect the origination of small RNAs from LIRs and quantify the expression level of small RNAs and LIRs."
            ),
            module_Box(
              width = 4, height='210px',
              title = "Download",
              imgSrc = "DESeq.png",
              text = "Perform differential expression analysis of long inverted repeats or small RNAs between different biological samples/tissues."
            ),
			module_Box(
              width = 4, height='210px',
              title = "About",
              imgSrc = "Visualize.png",
              text = "Predict and visualize the secondary structure of potential long hpRNA encoded by a long inverted repeat."
            )
          )
      )
    ),

    fluidRow(
      column(
        width = 10,
        offset = 1,
        box(
          title = span(strong("About soyaSNPDB"), style = "font-size:20px"),
          width = 12,
          solidHeader = TRUE,
          collapsible = FALSE,
          status = "warning",
          includeMarkdown("About.md")
        )
      )
    )
  )
)
