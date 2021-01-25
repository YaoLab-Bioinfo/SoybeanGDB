
# options(shiny.maxRequestSize = 200*1024^2)

shinyServer(function(input, output, session) {
  
  # GBrowser
  observe({
    if (input$submit1>0) {
      isolate({
        myPos <- anaReg(input$regB)
        
        if (validReg(myPos)) {
          if (!is.null(myPos)) {
            snp.info <- snpInfo(chr=myPos$chr, start=myPos$start - input$GBUP, end=myPos$end + input$GBDOWN, 
                                accession = input$mychooserB$selected, mutType = input$GB_mut_group)
          } else {
            snp.info <- NULL
          }
          
          if (is.null(snp.info) || nrow(snp.info[[1]][[1]]) < 1) {
            sendSweetAlert(
              session = session,
              title = "Error input!", type = "error",
              text = "No SNPs are detected in the specified genomic region or the specified genomic region is too large!"
            )
          } else {
            GBplot <<- NULL
            output$gbrowser <- renderPlotly({
              GBplot <<- GBrowser(chr=myPos$chr, start=myPos$start - input$GBUP, 
                                  end=myPos$end + input$GBDOWN,
                                  accession = input$mychooserB$selected,
                                  mutType = input$GB_mut_group)
              GBplot[[2]]
            })
            
            ## Download PDF file of GBrowser
            output$downloadGB.pdf <- downloadHandler(
              filename <- function() { paste('GBrowser.pdf') },
              content <- function(file) {
                pdf(file, width = 900/72, height = 300/72)
                grid.draw(GBplot[[1]])
                dev.off()
              }, contentType = 'application/pdf')
            
            # Download genotypes of seleceted SNPs
            output$downloadsnp.txt <- downloadHandler(
              filename = function() { "snp.geno.txt" },
              content = function(file) {
                write.table(snp.info[[1]][[1]], file, sep="\t", quote=F)
              })
            
            # Download information of SNPs
            output$downloadsnpInfo.txt <- downloadHandler(
              filename = function() { "snp.info.txt" },
              content = function(file) {
                write.table(snp.info[[2]], file, sep="\t", quote=F, row.names=F)
              })
          }
        } else {
          sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please input genomic region or gene model in appropriate format!"
          )
        }
      })
    } else {
      NULL
    }
  })
  observe({
    if (input$clearGB>0) {
      isolate({
        updateTextInput(session, "regB", value="")
      })
    } else {NULL}
  })
  
  observe({
    if (input$GBExam >0) {
      isolate({
        updateTextInput(session, "regB", value="chr1:29765419-29793053")
      })
    } else {NULL}
  })
  
  

  # LDheatmap
  observe({
    if (input$submit2>0) {
      isolate({
        ld.height <<- input$ldHeight
        ld.width <<- input$ldWidth
        myPos <- anaReg(input$regL)
        
        if (validReg(myPos)) {
          if (!is.null(myPos)) {
            snp.reg <- fetchSnp(chr=myPos$chr, start=myPos$start - input$ldUp * 1000, 
                              end=myPos$end + input$ldDown * 1000, accession = input$mychooserLD$selected,
                              mutType = input$ld_mut_group)[[1]]
          } else {
            snp.reg <- NULL
          }
          
          if (is.null(snp.reg) || nrow(snp.reg) < 5) {
            sendSweetAlert(
              session = session,
              title = "Error input!", type = "error",
              text = "Too few SNPs are detected in the specified genomic region or the specified genomic region is too large!"
            )
          } else {
            snp.pos <- as.numeric(unlist(strsplit(input$ldpos, split=",")))
            
            if (input$uploadLD == 1) {
              ld.snp.site <- NULL
            } else {
              if (!is.null(input$LD.snpsite)) {
                ld.snp.site <- readLines(input$LD.snpsite$datapath)
              } else {
                ld.snp.site <- NULL
              }
            }
            
            output$ldheatmap <- renderPlot({
              if (input$flip == "0") {
                ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp * 1000, end=myPos$end + input$ldDown * 1000, text=c(FALSE, TRUE)[as.numeric(input$showText)+1],
                           snp.pos=snp.pos, gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                           col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                           mutType = input$ld_mut_group, accession = input$mychooserLD$selected, 
                           snpSites = ld.snp.site)
              } else if (input$flip == "1") {
                if (input$LDshowGene) {
                  ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp * 1000, end=myPos$end + input$ldDown * 1000, text=FALSE,
                             snp.pos=snp.pos, ld.y=input$ldY/100, ld.w=input$ldW/100, gene=TRUE, 
                             col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                             mutType = input$ld_mut_group, accession = input$mychooserLD$selected, 
                             snpSites = ld.snp.site)
                } else {
                  ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp * 1000, end=myPos$end + input$ldDown * 1000, text=FALSE,
                             gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                             col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                             mutType = input$ld_mut_group, accession = input$mychooserLD$selected, 
                             snpSites = ld.snp.site)
                }
              }
              
            }, height = ld.height, width = ld.width)
          }
        } else {
          sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please input genomic region or gene model in appropriate format!" 
          )
        }
      })
    } else {
      NULL
    }
  })
  
  observe({
    if (input$clearLD>0) {
      isolate({
        updateTextInput(session, "regL", value="")
      })
    } else {NULL}
  })
  
  observe({
    if (input$LDExam >0) {
      isolate({
        updateTextInput(session, "regL", value="SoyZH13_02G148200")
      })
    } else {NULL}
  })
  
  ## Download PDF file of LDheatmap
  output$downloadLD.pdf <- downloadHandler(
    filename <- function() { paste('LDheatmap.pdf') },
    content <- function(file) {
      withProgress(message='Calculation in progress...',value = 0, detail = 'This may take a while...', {
        myPos <- anaReg(input$regL)
        
        snp.reg <- fetchSnp(chr=myPos$chr, start=myPos$start - input$ldUp * 1000, 
                            end=myPos$end + input$ldDown * 1000, accession = input$mychooserLD$selected,
                            mutType = input$ld_mut_group)[[1]]
        if (nrow(snp.reg) < 5) {
          js_string <- 'alert("Too few SNPs in specified genomic region!");'
          session$sendCustomMessage(type='jsCode', list(value = js_string))
        } else {
          pdf(file, width = input$ldWidth/72, height = input$ldHeight/72, onefile = FALSE)
          
          snp.pos <- as.numeric(unlist(strsplit(input$ldpos, split=",")))
          
          if (input$uploadLD == 1) {
            ld.snp.site <- NULL
          } else {
            if (!is.null(input$LD.snpsite)) {
              ld.snp.site <- readLines(input$LD.snpsite$datapath)
            } else {
              ld.snp.site <- NULL
            }
          }
          
          if (input$flip == "0") {
            ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp * 1000, end=myPos$end + input$ldDown * 1000, text=c(FALSE, TRUE)[as.numeric(input$showText)+1],
                       snp.pos=snp.pos, gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                       col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                       mutType = input$ld_mut_group, accession = input$mychooserLD$selected, 
                       snpSites = ld.snp.site)
          } else if (input$flip == "1") {
            if (input$LDshowGene) {
              ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp * 1000, end=myPos$end + input$ldDown * 1000, text=FALSE,
                         snp.pos=snp.pos, ld.y=input$ldY/100, ld.w=input$ldW/100, gene=TRUE, 
                         col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                         mutType = input$ld_mut_group, accession = input$mychooserLD$selected, 
                         snpSites = ld.snp.site)
            } else {
              ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp * 1000, end=myPos$end + input$ldDown * 1000, text=FALSE,
                         gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                         col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                         mutType = input$ld_mut_group, accession = input$mychooserLD$selected, 
                         snpSites = ld.snp.site)
            }
          }
          dev.off()
        }
        
      })
      
    }, contentType = 'application/pdf')
  
  ## Download SVG file of LDheatmap
  output$downloadLD.svg <- downloadHandler(
    filename <- function() { paste('LDheatmap.svg') },
    content <- function(file) {
      withProgress(message='Calculation in progress...',value = 0, detail = 'This may take a while...', {
        myPos <- anaReg(input$regL)
        snp.reg <- fetchSnp(chr=myPos$chr, start=myPos$start - input$ldUp * 1000, 
                            end=myPos$end + input$ldDown * 1000, accession = input$mychooserLD$selected,
                            mutType = input$ld_mut_group)[[1]]
        if (nrow(snp.reg) < 5) {
          js_string <- 'alert("Too few SNPs in specified genomic region!");'
          session$sendCustomMessage(type='jsCode', list(value = js_string))
        } else {
          svg(file, width = input$ldWidth/72, height = input$ldHeight/72)
          
          snp.pos <- as.numeric(unlist(strsplit(input$ldpos, split=",")))
          
          if (input$uploadLD == 1) {
            ld.snp.site <- NULL
          } else {
            if (!is.null(input$LD.snpsite)) {
              ld.snp.site <- readLines(input$LD.snpsite$datapath)
            } else {
              ld.snp.site <- NULL
            }
          }
          
          if (input$flip == "0") {
            ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp * 1000, end=myPos$end + input$ldDown * 1000, text=c(FALSE, TRUE)[as.numeric(input$showText)+1],
                       snp.pos=snp.pos, gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                       col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                       mutType = input$ld_mut_group, accession = input$mychooserLD$selected, 
                       snpSites = ld.snp.site)
          } else if (input$flip == "1") {
            if (input$LDshowGene) {
              ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp * 1000, end=myPos$end + input$ldDown * 1000, text=FALSE,
                         snp.pos=snp.pos, ld.y=input$ldY/100, ld.w=input$ldW/100, gene=TRUE, 
                         col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                         mutType = input$ld_mut_group, accession = input$mychooserLD$selected, 
                         snpSites = ld.snp.site)
            } else {
              ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp * 1000, end=myPos$end + input$ldDown * 1000, text=FALSE,
                         gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                         col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                         mutType = input$ld_mut_group, accession = input$mychooserLD$selected, 
                         snpSites = ld.snp.site)
            }
          }
          dev.off()
        }
        
      })
      
    }, contentType = 'image/svg')
  
  
  
  # Diversity
  
  observe({
    if (input$submit4>0) {
      isolate({
        div.height <<- input$divHeight
        div.width <<- input$divWidth
        
        myPos <- anaReg(input$regD)
        
        if (validReg(myPos)) {
        
        div.up <- input$divUp * 1000
        div.down <- input$divDown * 1000
        div.group <- input$div_acc_group
        div.step <- input$snpnumD
        div.numerator <- input$nuc_numerator
        div.denominator <- input$nuc_denominator
        div.mut.group <- input$div_mut_group
        
        if (input$uploadDIV == 1) {
          div.snp.site <- NULL
        } else {
          if (!is.null(input$DIV.snpsite)) {
            div.snp.site <- readLines(input$DIV.snpsite$datapath)
          } else {
            div.snp.site <- NULL
          }
        }
        
        if (!is.null(myPos)) {
          snp.reg <- fetchSnp(chr=myPos$chr, start=myPos$start - div.up, end=myPos$end + div.down,
                              mutType=input$div_mut_group)[[1]]
        } else {
          snp.reg <- NULL
        }
        
        if (is.null(snp.reg) || nrow(snp.reg) < 10) {
          js_string <- 'alert("No SNPs are detected in the specified genomic region or the specified genomic region is too large!");'
          session$sendCustomMessage(type='jsCode', list(value = js_string))
        } else {
          nuc.div.plot <<- NULL
          output$diversity <- renderPlot({
            nuc.div.plot <<- nucDiv(chr=myPos$chr, nuc.start=myPos$start - div.up, nuc.end=myPos$end + div.down, 
                                    groups = div.group, step = div.step,
                                    numerator = div.numerator, denominator = div.denominator, 
                                    mutType = div.mut.group, snpSites = div.snp.site)
            grid.draw(grid.arrange(nuc.div.plot[[1]], nuc.div.plot[[2]], ncol=1, heights=c(2.3, 1)))
          }, height = div.height, width = div.width)
          
          ## Download PDF file of Diversity
          output$downloadDiv01 <- renderUI({
            req(input$submit4, nuc.div.plot)
            downloadButton("downloadDiv.pdf", "Download pdf-file")
          })
          
          output$downloadDiv.pdf <- downloadHandler(
            filename <- function() { paste('diversity.pdf') },
            content <- function(file) {
              pdf(file, width = input$divWidth/72, height = input$divHeight/72)
              grid.draw(grid.arrange(nuc.div.plot[[1]], nuc.div.plot[[2]], ncol=1, heights=c(2.3, 1)))
              
              dev.off()
            }, contentType = 'application/pdf')
          
          ## Download SVG file of Diversity
          output$downloadDiv02 <- renderUI({
            req(input$submit4, nuc.div.plot)
            downloadButton("downloadDiv.svg", "Download svg-file")
          })
          
          output$downloadDiv.svg <- downloadHandler(
            filename <- function() { paste('diversity.svg') },
            content <- function(file) {
              svg(file, width = input$divWidth/72, height = input$divHeight/72)
              grid.draw(grid.arrange(nuc.div.plot[[1]], nuc.div.plot[[2]], ncol=1, heights=c(2.3, 1)))
              
              dev.off()
            }, contentType = 'image/svg')
          
          ## Download TXT file of diversity
          output$downloadDiv03 <- renderUI({
            req(input$submit4)
            downloadButton("downloadDiv.txt", "Download TXT-file")
          })
          
          output$downloadDiv.txt <- downloadHandler(
            filename <- function() { paste('diversity.txt') },
            content <- function(file) {
              write.table(diVTxt, file, sep="\t", quote=F, row.names = F)
            }, contentType = 'text/plain')
          
          
        }
        }else{
          sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please input genomic region or gene model in appropriate format!"
          )
        } 
      })
    } else {
      NULL
    }
  })
  
  observe({
    if (input$clearDIV>0) {
      isolate({
        updateTextInput(session, "regD", value="")
      })
    } else {NULL}
  })
  
  observe({
    if (input$DIVExam >0) {
      isolate({
        updateTextInput(session, "regD", value="SoyZH13_02G148200")
      })
    } else {NULL}
  })
  
  
  # phylogenetics
  observe({
    if (input$submit5>0) {
      isolate({
        phy.height <<- input$phyHeight
        phy.width <<- input$phyWidth
        phy.up <- input$phyUp * 1000
        phy.down <- input$phyDown * 1000
        
        myPos <- anaReg(input$regP)
        
        if (validReg(myPos)) {
          
        phy.soya <- input$mychooserPhy$selected
        phy.mut.group <- input$phy_mut_group
        
        if (input$uploadPHY == 1) {
          phy.snp.site <- NULL
        } else {
          if (!is.null(input$PHY.snpsite)) {
            phy.snp.site <- readLines(input$PHY.snpsite$datapath)
          } else {
            phy.snp.site <- NULL
          }
        }
        
        if (!is.null(myPos)) {
          snp.reg <- fetchSnp(chr=myPos$chr, start=myPos$start - phy.up, end=myPos$end + phy.down,
                              accession=phy.soya, mutType=phy.mut.group)[[1]]
        } else {
          snp.reg <- NULL
        }
        
        if (is.null(snp.reg) || nrow(snp.reg) < 10) {
          sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "No SNPs are detected in the specified genomic region or the specified genomic region is too large!"
          )
        } else {
          output$phylo <- renderPlot({
            phylo(chr=myPos$chr, start=myPos$start-phy.up, end=myPos$end+phy.down,
                  accession=phy.soya, mutType=phy.mut.group, snpSites = phy.snp.site)
          }, height = phy.height, width = phy.width)
          
          ## Download PDF file of phylogenetics
          output$downloadPhy01 <- renderUI({
            req(input$submit5)
            downloadButton("downloadPhylo.pdf", "Download pdf-file")
          })
          
          output$downloadPhylo.pdf <- downloadHandler(
            filename <- function() { paste('phylogenetics.pdf') },
            content <- function(file) {
              pdf(file, width = input$phyWidth/72, height = input$phyHeight/72)
              dev.off()
            }, contentType = 'application/pdf')
          
          ## Download NWK file of phylogenetics
          output$downloadPhy02 <- renderUI({
            req(input$submit5)
            downloadButton("downloadPhylo.nwk", "Download Newick-file")
          })
          
          output$downloadPhylo.nwk <- downloadHandler(
            filename <- function() { paste('phylogenetics.nwk') },
            content <- function(file) {
              write.tree(treNwk, file)
            }, contentType = 'text/plain')
        }
        } else{
          sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please input genomic region or gene model in appropriate format!"
          )
        }
      })
    } else {
      NULL
    }
  })
  
  observe({
    if (input$clearPHY>0) {
      isolate({
        updateTextInput(session, "regP", value="")
      })
    } else {NULL}
  })
  
  observe({
    if (input$PHYExam >0) {
      isolate({
        updateTextInput(session, "regP", value="SoyZH13_01G186100")
      })
    } else {NULL}
  })
  

 
  # Accession

  output$soya.info.txt <- downloadHandler(
    filename = function() { "soya.info.txt" },
    content = function(file) {
      write.table(soya.info, file, sep = "\t", quote=FALSE, row.names = FALSE)
    }, contentType = 'text/plain')
  
  
  output$sel.soya.info.txt <- downloadHandler(
    filename = function() { "sel.soya.info.txt" },
    content = function(file) {
      accession <- input$mychooserA$selected
      accession <- gsub(",.+", "", accession)
      accession <- sapply(accession, function(x){
        if (x %in% c("Improved cultivar", "Landrace", "G. Soja")) {
          x.dat <- readLines(paste0("./data/", x, ".soya.txt"))
          return(x.dat)
        } else {
          return(x)
        }
      })
      accession <- unique(unlist(accession))
      
      write.table(soya.info[soya.info$ID %in% accession, ], 
                  file, sep = "\t", quote=FALSE, row.names = FALSE)
    }, contentType = 'text/plain')
  
  
  output$mytable1 = renderDataTable({
    accession <- input$mychooserA$selected
    accession <- gsub(",.+", "", accession)
    accession <- sapply(accession, function(x){
      if (x %in% c("Improved cultivar", "Landrace", "G. Soja")) {
        x.dat <- readLines(paste0("./data/", x, ".soya.txt"))
        return(x.dat)
      } else {
        return(x)
      }
    })
    accession <- unique(unlist(accession))
    
    soya.info[soya.info$ID %in% accession, ]
  }, options = list(lengthMenu = c(5, 8, 10), pageLength = 5, searching = TRUE, autoWidth = FALSE), escape = FALSE
  )
  

  
  #search gene or interval information
  
  
  observeEvent(input$submit7,{
    if (input$regs == "interval") {
      chr <- input$Chrinterval
      start <- as.numeric(input$upinterval)
      end <- as.numeric(input$downinterval)
      geneid <- ""
    }else if (input$regs == "Geneid"){ 
      load(paste0("./info/", gsub(" ", "_", input$variety), ".gene.info.RData"))
      start <- gene.info$start[gene.info$id == input$geneid]
      end <- gene.info$end[gene.info$id == input$geneid]
      chr <- gene.info$chr[gene.info$id == input$geneid]
      geneid <- input$geneid
    }
    load(paste0("./info/",gsub(" ", "_", input$variety), ".",chr, ".fasta.RData"))
    
    if (start > width(fasta[chr]) | end > width(fasta[chr])){
      sendSweetAlert(
        session = session,
        title = "The input value exceeds the length of the chromosome!", type = "error",
        text = NULL
      )
      NULL 
    }else{
      load(paste0("./info/", gsub(" ", "_", input$variety), ".protein.RData"))
      cds.info <- readAAStringSet(paste0("./info/", gsub(" ", "_", input$variety), ".",chr ,".cds.info.fasta"))
      start <- as.integer(start)
      end <- as.integer(end)
      df <- subseq(fasta[chr], start, end)
      names(df) <- paste0(geneid, " ",names(df), ":", start, "-", end, " length = ", end - start +1)
      tmp.f1 <- file.path(tempdir(), "t1.fa")
      
      output$seq <- renderText({
        writeXStringSet(df, file = tmp.f1, width = 150)
        readLines(tmp.f1)
      }, sep = "\n")
      
      if( geneid %in% gene.info$id ){
        #protein
        dt <- protein[grep(geneid, names(protein))]
        output$pro <- shiny::renderText({
          tmp.f2 <- file.path(tempdir(), "t2.fa")
          writeXStringSet(dt, file = tmp.f2, width = 150)
          readLines(tmp.f2)
        }, sep = "\n")
        
        output$pro_title <- renderText({
          "The pro sequence of gene:"
        })
        #cds
        dc <- cds.info[grep(geneid, names(cds.info))]
        output$cds <- renderText({
          tmp.f3 <- file.path(tempdir(), "t3.fa")
          writeXStringSet(dc, file = tmp.f3, width = 150)
          readLines(tmp.f3)
        }, sep = "\n")
        
        output$cds_title <- renderText({
          "The cds sequence of gene:"
        })
        
        output$geneinfo = DT::renderDataTable({
          NULL
        })
        
        gffile <- paste0("./info/GFF/", gsub(" ", "_", input$variety), ".",chr ,".gff.txt")
        gf <- read.table(gffile, header = T, sep = "\t", as.is = T)
        
        output$gffinfo <- DT::renderDataTable({
          gfinfo <- gf[grep(geneid, gf$Attributes), ]
          gfinfo
        }, extensions = "Buttons", rownames = F, options = list(leftColumns = 8, scrollX = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'print'))
        )
        
        output$gffinfotitle <- renderText({
          if(is.null(gf)){
            NULL
            }else{
            "The gene's gff information"
            }
        })
        
        
      } else {
        output$gffinfotitle <- NULL
        output$gffinfo <- NULL
        output$pro <- NULL
        output$cds <- NULL
        output$cds_title <- renderText({
          NULL
        })
        output$pro_title <- renderText({
          NULL
        })
        
        gene <- GRanges(seqnames = gene.info$chr, IRanges(start = gene.info$start, end = gene.info$end))
        thefind <- GRanges(seqnames = chr, IRanges(start = start, end = end))
        find.f1 <- findOverlaps(gene, thefind)
        geneinfo <- gene.info[gene.info$id %in% gene.info$id[c(find.f1@from)],]
        
        output$geneinfo = DT::renderDataTable({
          geneinfo
        }, escape = FALSE, rownames= FALSE, selection="single", options = list(pageLength = 10, autoWidth = TRUE, bSort=FALSE))
      }
      
      output$seq_title <- renderText({
        "The sequence of gene or interval:"
      })
      
      
      output$searchgenetitle <- renderText({
        if (is.null(input$geneinfo_rows_selected)) {
          NULL
        } else {
          "The gene's sequence"
        }
      })
      
      output$searchgenesequence <- renderText({
        if (is.null(input$geneinfo_rows_selected)) {
        } else {
          clicked <- input$geneinfo_rows_selected
          info <- data.frame(geneinfo[clicked,])
          gene <- subseq(fasta[info[1,2]], as.numeric(info[1,3]), as.numeric(info[1,4]))
          names(gene) <- paste0(names(gene),  ":", as.numeric(info[1,3]), "-", as.numeric(info[1,4]), " length = ", as.numeric(info[4]) - as.numeric(info[1,3]) +1)
          tmp.f4 <- file.path(tempdir(), "t4.fa")
          writeXStringSet(gene, file = tmp.f4, width = 150)
          readLines(tmp.f4) 
        }
      },sep = "\n")
      
      output$searchcdstitle <- renderText({
        if (is.null(input$geneinfo_rows_selected)) {
          NULL
        } else {
          "The cds's sequence"
        }
      })
      
      output$searchcdssequence <- renderText({
        if (is.null(input$geneinfo_rows_selected)) {
        } else {
          clicked <- input$geneinfo_rows_selected
          geneid <- data.frame(geneinfo[clicked,])[1,1]
          dsc <- cds.info[grep(geneid, names(cds.info))]
          tmp.f5 <- file.path(tempdir(), "t5.fa")
          writeXStringSet(dsc, file = tmp.f5, width = 150)
          readLines(tmp.f5)
        }
      },sep = "\n")
      
      output$searchprotitle <- renderText({
        if (is.null(input$geneinfo_rows_selected)) {
          NULL
        } else {
          "The protein's sequence"
        }
      })
      
      output$searchprosequence <- renderText({
        if (is.null(input$geneinfo_rows_selected)) {
        } else {
          clicked <- input$geneinfo_rows_selected
          geneid <- data.frame(geneinfo[clicked,])[1,1]
          dsp <- protein[grep(geneid, names(cds.info))]
          tmp.f6 <- file.path(tempdir(), "t6.fa")
          writeXStringSet(dsp, file = tmp.f6, width = 150)
          readLines(tmp.f6)
        }
      },sep = "\n")
      
      #gff for select
      output$searchgffinfo <- DT::renderDataTable({
        if (is.null(input$geneinfo_rows_selected)){
        }else {
          clicked <- input$geneinfo_rows_selected
          geneid <- data.frame(geneinfo[clicked,])[1,1]
          gffile <- paste0("./info/GFF/", gsub(" ", "_", input$variety), ".",chr ,".gff.txt")
          gf <- read.table(gffile, header = T, sep = "\t", as.is = T)
          gfinfo <- gf[grep(geneid, gf$Attributes), ]
          gfinfo
        }}, extensions = "Buttons", 
        rownames = F, 
        options = list(leftColumns = 8, 
                       scrollX = TRUE, dom = 'Bfrtip', 
                       buttons = c('copy', 'csv', 'excel', 'print')
        )
      )
      
      output$searchgffinfotitle <- renderText({
        if(is.null(input$geneinfo_rows_selected)){
          NULL
        }else{
        "The gff of gene for your select:"
          }
      })
      
      
      
      output$sequence.txt <- downloadHandler(
        filename <- function() { paste('The_sequence.txt') },
        content <- function(file) {
          writeXStringSet(df, file, width = 150)
        }, contentType = 'text/plain'
      )
      
      output$genesequence.txt <- downloadHandler(
        filename <- function() { paste('The_gene_sequence.txt') },
        content <- function(file) {
          writeXStringSet(dt, file, width = 150)
        }, contentType = 'text/plain'
      )
      
      output$genesinfo.txt <- downloadHandler(
        filename <- function() { paste('The_gene_infomation.txt') },
        content <- function(file) {
          write.table(gene.info[gene.info$id %in% gene.info$id[c(find.f1@from)],], file, sep = "\t", col.names = T, row.names = F, quote = F)
        }, contentType = 'text/plain'
      )
    }
    
    
    
    observe({
      if (input$clearSER>0) {
        isolate({
          updateTextInput(session, "upinterval", value = "")
          updateSelectInput(session, "Chrinterval", selected = "")
          updateTextInput(session, "downinterval", value = "")
          updateTextInput(session, "geneid", value = "SoyZH13_02G148200")
        })
      } else {NULL}
    })
    
    observe({
      if (input$SERExam >0) {
        isolate({
          updateTextInput(session, "geneid", value="SoyZH13_02G148200")
          updateSelectInput(session, "variety", selected = "Zhonghuang 13")
          updateSelectInput(session, "regs", selected = "Geneid")
        })
      } else {NULL}
    })
  })
  
  
  
  #blast
    blast.result <- eventReactive(input$submitBLAST, {
    blast.in.seq <- ""
    if (input$In_blast == "paste") {
      blast.in.seq <- input$BlastSeqPaste
      blast.in.seq <- gsub("^\\s+", "", blast.in.seq)
      blast.in.seq <- gsub("\\s+$", "", blast.in.seq)
    } else if (input$In_blast == "upload") {
      blast.in.seq <- readLines(input$BlastSeqUpload$datapath)
    }
    
    blast.in.file <- gsub("\\s+", "-", Sys.time())
    blast.in.file <- gsub(":", "-", blast.in.file)
    blast.in.file <- paste0(blast.in.file, ".fasta")
    blast.in.file <- file.path(tempdir(), blast.in.file)
    writeLines(blast.in.seq, con = blast.in.file)
    
    blast.db <- input$BLASTdb
    blast.db <- gsub(" ", "_", blast.db)
    blast.db <- paste0("www/BLASTN/", blast.db)
    blast.db.fl <- paste0(blast.db, ".nhr")
    if (!file.exists(blast.db.fl)) {
      sendSweetAlert(
        session = session,
        title = "BLAST database not found!", type = "error",
        text = NULL
      )
      NULL
    } else {
      blast.out.file <- paste0(blast.in.file, ".blast.out")
      blast.cmds <- paste0(input$program," -query ", blast.in.file," -db ", '"', paste(blast.db, sep=" ", collapse = " "), '"', " -evalue ",
                           input$BLASTev, " -outfmt 5", " -out ", blast.out.file)
      system(blast.cmds, ignore.stdout = TRUE, ignore.stderr = TRUE)
      xmlParse(blast.out.file)}
  })
  
  #makes the datatable 
  blastedResults <- reactive({
    if (is.null(blast.result())){
      
    } else {
      xmltop = xmlRoot(blast.result())
      
      #the first chunk is for multi-fastas
      x <- which(sapply(blast.result()["//Iteration//Iteration_hits//Hit//Hsp//Hsp_num"], xmlValue) == "1")
      y <- sapply(blast.result()["//Iteration//Iteration_hits//Hit//Hit_def"], xmlValue)
      x1 <- c(x[-1], length(sapply(blast.result()["//Iteration//Iteration_hits//Hit//Hsp//Hsp_num"], xmlValue))+1)
      z <- sapply(blast.result()["//Iteration//Iteration_hits//Hit//Hit_def"], xmlValue)
      d <- sapply(blast.result()["//Iteration//Iteration_hits//Hit//Hit_len"], xmlValue)
      results <- xpathApply(blast.result(), '//Iteration',function(row){
        qseqid <- getNodeSet(row, 'Iteration_query-def') %>% sapply(., xmlValue)
        qlen <- getNodeSet(row, 'Iteration_query-len') %>% sapply(., xmlValue)
        qstart <- getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_query-from')  %>% sapply(., xmlValue)
        qend <- getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_query-to')  %>% sapply(., xmlValue)
        sseqid <- rep(z, x1-x)
        sslen <- rep(d, x1-x)
        sstart <- getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_hit-from')  %>% sapply(., xmlValue)
        send <- getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_hit-to')  %>% sapply(., xmlValue)
        bitscore <- getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_bit-score')  %>% sapply(., xmlValue)
        evalue <- getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_evalue')  %>% sapply(., xmlValue)
        gaps <- getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_gaps')  %>% sapply(., xmlValue)
        cbind(qseqid, qlen, sseqid, sslen, qstart, qend, sstart, send, bitscore, evalue, gaps)
      })
      #this ensures that NAs get added for no hits
      results <-  rbind.fill(lapply(results,function(y){as.data.frame((y),stringsAsFactors=FALSE)}))
    }
  })
 
  output$BLASTresult <- DT::renderDataTable({
    if (is.null(blastedResults())) {
      blastedResults <- data.frame("V1"="No BLAST hits found!")
      colnames(blastedResults) <- ""
      blastedResults
    } else {
      blastedResults()
    }
  }, escape = FALSE, rownames= FALSE, selection="single", options = list(pageLength = 10, autoWidth = TRUE, bSort=FALSE))
  
  # Update Tab Panel
  observe({
    if (input$submitBLAST >0) {
      isolate({
        if (!is.null(blast.result())) {
          updateTabsetPanel(session, 'BLAST_tab', selected = 'Output')
        } else {
          NULL
        }
      })
    } else {NULL}
  })
  
  output$geneseq <- renderText({
    if (is.null(input$BLASTresult_rows_selected) || is.null(blast.result()) ) {
      
    } else {
      "The gene's sequence"
    }
  })
  
  output$alignment <- renderText({
    if(is.null(input$BLASTresult_rows_selected) || is.null(blast.result()) ){
      NULL
    }
    else{
      
      xmltop = xmlRoot(blast.result())
      
      clicked = input$BLASTresult_rows_selected
      #loop over the xml to get the alignments
      align <- xpathApply(blast.result(), '//Iteration',function(row){
        top <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_qseq') %>% sapply(., xmlValue)
        mid <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_midline') %>% sapply(., xmlValue)
        bottom <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_hseq') %>% sapply(., xmlValue)
        rbind(top,mid,bottom)
      })
      
      #split the alignments every 100 carachters to get a "wrapped look"
      alignx <- do.call("cbind", align)
      splits <- strsplit(gsub("(.{100})", "\\1,", alignx[1:3,clicked]),",")
      
      #paste them together with returns '\n' on the breaks
      split_out <- lapply(1:length(splits[[1]]),function(i){
        rbind(paste0("Q-",splits[[1]][i],"\n"),paste0("M-",splits[[2]][i],"\n"),paste0("H-",splits[[3]][i],"\n"))
      })
      split_out[[1]][1] <- paste0(" ", split_out[[1]][1])
      unlist(split_out)
    }
  })
  
  #this chunk gets the alignemnt information from a clicked row
  output$clicked <- renderTable({
    if(is.null(input$BLASTresult_rows_selected)){}
    else{
      
      clicked = input$BLASTresult_rows_selected
      tableout<- data.frame(blastedResults()[clicked,])
      tableout <- t(tableout)
      names(tableout) <- c("")
      rownames(tableout) <- c("Query ID","Query length", "Subject ID", "Subject length", "Qstart", "Qend", "Sstart", "Send", "Bit-score", "evalue", "gaps")
      colnames(tableout) <- NULL
      data.frame(tableout)
    }
  },rownames =T,colnames =F)
  
  
  output$choicegene <- renderText({
    if (is.null(input$BLASTresult_rows_selected) || is.null(blast.result()) ) {
      
    } else {
      "The sequenc in the gene"
    }
  })
  output$Alignment <- renderText({
    if (is.null(input$BLASTresult_rows_selected) || is.null(blast.result()) ) {
      
    } else {
      "Alignment"
    }
  })
  
  ## Download BLAST example input
  output$BLAST_Input.txt <- downloadHandler(
    filename <- function() { paste('BLAST_example_input.txt') },
    content <- function(file) {
      writeLines(exam1.fa, con=file)
    }, contentType = 'text/plain'
  )
  
  ## Download BLAST result
  output$BLASTresult.txt <- downloadHandler(
    filename <- function() { paste('BLAST_Input.txt') },
    content <- function(file) {
      fwrite(blastedResults(), file, sep="\t", quote=F)
    }, contentType = 'text/plain'
  )
  
  ##reset
  observe({
    if (input$clear3 >0) {
      isolate({
        updateSelectInput(session, "In_blast", selected = "paste")
        updateTextAreaInput(session, "BlastSeqPaste", value="")
        updateMultiInput(session, "BLASTdb", selected = "")
      })
    } else {NULL}
  })
  
  #load example
  observe({
    if (input$blastExam >0) {
      isolate({
        updateSelectInput(session, "In_blast", selected = "paste")
        updateTextAreaInput(session, "BlastSeqPaste", value = paste(exam1.fa, collapse = "\n"))
        updateMultiInput(session, "BLASTdb", selected = c("Zhonghuang 13"))
      })
    } else {NULL}
  })
  

  # Bulk download genotypes of seleceted SNPs
  observe({
    if (input$submit6>0) {
      isolate({
        myPos <- anaReg(input$regBB)
        
        if (validReg(myPos)) {
        } else {
          sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please input genomic region or gene model in appropriate format!"
          )
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
  

  
})

