
# options(shiny.maxRequestSize = 200*1024^2)

shinyServer(function(input, output, session) {
  
    observeEvent(input$go, {
    updateTabsetPanel(session, "tabset", "Plot")
  })

  
  # Home-browser
  observeEvent(input$Browse_botton, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Browse</strong>"))
  })
  
    #Home-Indel
  observeEvent(input$Link_Indel, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Indel</strong>"))
  })
  
  #Home-LDHeatmap
  observeEvent(input$Linkage_anal, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>LDheatmap</strong>"))
  })
  
  #Home-Diversity
  observeEvent(input$Link_diversity, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Diversity</strong>"))
  })

  
  #Home-Phylogenetic
  observeEvent(input$Link_Phylogenetic, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Phylogenetic</strong>"))
  })
  
  #Home-SnpInfo
  observeEvent(input$Link_SnpInfo, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Search</strong>"))
  })

  #Home-GeneInfoID
  observeEvent(input$Link_GeneInfoID, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Search by gene ID</strong>"))
  })
  
  #Home-GeneInfoIT
  observeEvent(input$Link_GeneInfoIT, {
    updateNavbarPage(session, "The_page", selected =HTML("<strong style='font-size:20px'>Search by genome location</strong>"))
  })
 
  #Home-BLAST
  observeEvent(input$Link_blast, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Blast</strong>"))
  })
  
  #Home-Primer
  observeEvent(input$Link_Primer, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Primer</strong>"))
  })
  
  #Home-Accession
  observeEvent(input$Link_Accession, {
    updateNavbarPage(session, "The_page", selected = HTML("<strong style='font-size:20px'>Accession</strong>"))
  })
  
  #Home-JBrowse2
  output$Link <- renderUI({
    a(HTML('<strong style="font-size:20px">JBrowse2</strong>'), href="http://google.com", target="_blank")
  })
  
  

  # GBrowser
  observe({
    if (input$submit_browse>0) {
      isolate({
        myPos <- anaReg(input$regB)
        
        if (validReg(myPos)) {
          if (!is.null(myPos)) {
            snp.info <- snpInfo(chr=myPos$chr, start=myPos$start - input$GBUP, end=myPos$end + input$GBDOWN, 
                                accession = gsub(",.+", "", input$mychooserB), mutType = input$GB_mut_group)
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
                                  accession = gsub(",.+", "", input$mychooserB),
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
  
  observeEvent(input$browseall1, {
    updateMultiInput(
      session = session,
      inputId = "mychooserB",
      selected = all.soya.cho
    )
  })
  
  observeEvent(input$browsenone1, {
    updateMultiInput(
      session = session,
      inputId = "mychooserB",
      selected = character(0)
    )
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
  
  # Search for seleceted SNPs
  observe({
    if (input$submit_SSNP>0) {
      isolate({
        myPos <- anaReg(input$regBB)
        
        if (validReg(myPos)) {
          snp.info.down <<- NULL
          
          if (!is.null(myPos)) {
            
            output$snp_search <- renderText({
              "SNPs information in specified Genomic region"
            })
            output$mytable2 = renderDataTable({
              snp.info.down <<- snpInfo(chr=myPos$chr, start=myPos$start, end=myPos$end, 
                                        accession = gsub(",.+", "", input$mychooserD), mutType = input$down_mut_group)
              serchSNP <- snp.info.down[[2]][, c(1:3,5:7)]
              colnames(serchSNP) <- c("SNPID", "Major", "Minor", "Reference", "Alternative", "Effect")
              serchSNP
            }, options = list(lengthMenu = c(5, 8, 10), pageLength = 15, searching = TRUE, autoWidth = TRUE), escape = FALSE
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
          
        } else {
          sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please input genomic region or gene model in appropriate format!"
          )
        }
        
      })
    } else {NULL}
  })
  
  observeEvent(input$snpsearchall1, {
    updateMultiInput(
      session = session,
      inputId = "mychooserD",
      selected = all.soya.cho
    )
  })
  
  observeEvent(input$snpsearchnone1, {
    updateMultiInput(
      session = session,
      inputId = "mychooserD",
      selected = character(0)
    )
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

  # LDheatmap
  observe({
    library(LDheatmap)
    if (input$submit2>0) {
      isolate({
        ld.height <<- input$ldHeight
        ld.width <<- input$ldWidth
        myPos <- anaReg(input$regL)
        
        if (validReg(myPos)) {
          if (!is.null(myPos)) {
            snp.reg <- fetchSnp(chr=myPos$chr, start=myPos$start - input$ldUp, 
                              end=myPos$end + input$ldDown, accession = gsub(",.+", "", input$mychooserLD),
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
                ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=c(FALSE, TRUE)[as.numeric(input$showText)+1],
                           snp.pos=snp.pos, gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                           col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                           mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD),
                           snpSites = ld.snp.site)
              } else if (input$flip == "1") {
                if (input$LDshowGene) {
                  ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=FALSE,
                             snp.pos=snp.pos, ld.y=input$ldY/100, ld.w=input$ldW/100, gene=TRUE, 
                             col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                             mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD), 
                             snpSites = ld.snp.site)
                } else {
                  ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=FALSE,
                             gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                             col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                             mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD), 
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
  
  observeEvent(input$ldall1, {
    updateMultiInput(
      session = session,
      inputId = "mychooserLD",
      selected = all.soya.cho
    )
  })
  
  observeEvent(input$ldnone1, {
    updateMultiInput(
      session = session,
      inputId = "mychooserLD",
      selected = character(0)
    )
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
        updateTextInput(session, "regL", value="SoyZH13_01G002900")
      })
    } else {NULL}
  })
  
  
  ## Download PDF file of LDheatmap
  output$downloadLD.pdf <- downloadHandler(
    filename <- function() { paste('LDheatmap.pdf') },
    content <- function(file) {
      withProgress(message='Calculation in progress...',value = 0, detail = 'This may take a while...', {
        myPos <- anaReg(input$regL)
        
        snp.reg <- fetchSnp(chr=myPos$chr, start=myPos$start - input$ldUp, 
                            end=myPos$end + input$ldDown, accession = gsub(",.+", "", input$mychooserLD),
                            mutType = input$ld_mut_group)[[1]]
        if (nrow(snp.reg) < 5) {
          sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Too few SNPs in specified genomic region!"
          )
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
            ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=c(FALSE, TRUE)[as.numeric(input$showText)+1],
                       snp.pos=snp.pos, gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                       col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                       mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD), 
                       snpSites = ld.snp.site)
          } else if (input$flip == "1") {
            if (input$LDshowGene) {
              ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=FALSE,
                         snp.pos=snp.pos, ld.y=input$ldY/100, ld.w=input$ldW/100, gene=TRUE, 
                         col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                         mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD), 
                         snpSites = ld.snp.site)
            } else {
              ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=FALSE,
                         gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                         col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                         mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD), 
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
        snp.reg <- fetchSnp(chr=myPos$chr, start=myPos$start - input$ldUp, 
                            end=myPos$end + input$ldDown, accession = gsub(",.+", "", input$mychooserLD),
                            mutType = input$ld_mut_group)[[1]]
        if (nrow(snp.reg) < 5) {
          sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Too few SNPs in specified genomic region!"
          )
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
            ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=c(FALSE, TRUE)[as.numeric(input$showText)+1],
                       snp.pos=snp.pos, gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                       col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                       mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD), 
                       snpSites = ld.snp.site)
          } else if (input$flip == "1") {
            if (input$LDshowGene) {
              ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=FALSE,
                         snp.pos=snp.pos, ld.y=input$ldY/100, ld.w=input$ldW/100, gene=TRUE, 
                         col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                         mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD), 
                         snpSites = ld.snp.site)
            } else {
              ld.heatmap(chr=myPos$chr, start=myPos$start - input$ldUp, end=myPos$end + input$ldDown, text=FALSE,
                         gene=FALSE, flip=c(FALSE, TRUE)[as.numeric(input$flip)+1],
                         col=list(grey.colors(20), heat.colors(20))[[as.numeric(input$ldcol)]],
                         mutType = input$ld_mut_group, accession = gsub(",.+", "", input$mychooserLD), 
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
        
        div.up <- input$divUp
        div.down <- input$divDown
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
          sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Too little SNPs are detected in the specified genomic region or the specified genomic region is too large!"
          )
        } else {
          nuc.div.plot <<- NULL
          output$diversity <- renderPlot({
            nuc.div.plot <<- nucDiv(chr = myPos$chr, nuc.start = myPos$start - div.up, nuc.end = myPos$end + div.down, 
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
    library(ggtree)
    if (input$submit5>0) {
      isolate({
        phy.height <<- input$phyHeight
        phy.width <<- input$phyWidth
        phy.up <- input$phyUp
        phy.down <- input$phyDown
        
        myPos <- anaReg(input$regP)
        
        if (validReg(myPos)) {
          
        phy.soya <- gsub(",.+", "", input$mychooserPhy)
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
              print(figurecp)
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
  
  observeEvent(input$phyall1, {
    updateMultiInput(
      session = session,
      inputId = "mychooserPhy",
      selected = all.soya.cho
    )
  })
  
  observeEvent(input$phynone1, {
    updateMultiInput(
      session = session,
      inputId = "mychooserPhy",
      selected = character(0)
    )
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

  #search gene or interval information

  observeEvent(input$submit_GSID,{
    library(GenomicRanges)
    load(paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".gene.info.RData"))
    #判断用户输入数据 chr start end
    geneid <- input$geneid
    start <- gene_info_s$start[gene_info_s$id == input$geneid]
    end <- gene_info_s$end[gene_info_s$id == input$geneid]
    chr <- gene_info_s$chr[gene_info_s$id == input$geneid]
    
    
    if(identical(chr, character(0))){
      sendSweetAlert(
        session = session,
        title = "Yor input ID is wrong!", type = "error",
        text = NULL
      )
      NULL 
      
    }else{
      fasta <- readDNAStringSet(paste0("./info/",gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".",chr, ".fasta.gz"))
      #判断长度
      if (start > width(fasta[chr]) | end > width(fasta[chr]) | end - start > 10000000 | !(end - start > 0) | is.na(end + start) ){
        sendSweetAlert(
          session = session,
          title = "The length of the chromosome is not right!", type = "error",
          text = NULL
        )
        NULL 
      }else{
        #fasta cds protein信息读取
        load(paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".cds.fasta.RData"))
        load(paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".protein.RData"))
        load(paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".cdna.fasta.RData"))
        
        #输入为GENEID
        if( geneid %in% gene_info_s$id ){
          
          gfinfo <- gene_info_s[gene_info_s$id == geneid, ]
          
          gfinfo$seq[gfinfo$strand == "+"] <- as.character(subseq(fasta[gfinfo[gfinfo$strand == "+",]$chr], gfinfo[gfinfo$strand == "+",]$start, gfinfo[gfinfo$strand == "+",]$end))
          gfinfo$seq[gfinfo$strand == "-"] <- as.character(reverseComplement(subseq(fasta[gfinfo[gfinfo$strand == "-",]$chr], gfinfo[gfinfo$strand == "-",]$start, gfinfo[gfinfo$strand == "-",]$end)))
          
          df <- DNAStringSet(gfinfo$seq)
          names(df) <- paste0(geneid, " ",gfinfo$chr, ":", start, "-", end, " length = ", end - start +1)
          tmp.f1 <- file.path(tempdir(), "t1.fa")
          
          output$seq <- renderText({
            writeXStringSet(df, file = tmp.f1, width = 150)
            readLines(tmp.f1)
          }, sep = "\n")
          output$seq_title <- renderText({
            if( is.null(df) ){
              NULL
            }else{
              "Gene sequence"
            }
          })
          
          #protein
          dp <- protein[grep(geneid, names(protein))]
          output$pro <- shiny::renderText({
            tmp.f2 <- file.path(tempdir(), "t2.fa")
            writeXStringSet(dp, file = tmp.f2, width = 150)
            readLines(tmp.f2)
          }, sep = "\n")
          
          output$pro_title <- renderText({
            if( is.null(dp) ){
              NULL
            }else{
              "Protein information"
            }
          })
          
          #cds
          dc <- cds.info[grep(geneid, names(cds.info))]
          
          output$cds <- renderText({
            tmp.f3 <- file.path(tempdir(), "t3.fa")
            writeXStringSet(dc, file = tmp.f3, width = 150)
            readLines(tmp.f3)
          }, sep = "\n")
          
          output$cds_title <- renderText({
            if(is.null(dc)){
              NULL
            }else{
              "CDS information"
            }            
          })
          
          #cdna
          dcdna <- cdna.info[grep(geneid, names(cdna.info))]
          output$cdna_title <- renderText({
            if(is.null(dcdna)){
              NULL
            }else{
              "cDNA information"
            }
          })
          
          output$cdna <- renderText({
            
            tmp.f4 <- file.path(tempdir(), "t4.fa")
            writeXStringSet(dcdna, file = tmp.f4, width = 150)
            readLines(tmp.f4)
          }, sep = "\n")
          
          output$cds_title <- renderText({
            "The cds sequence of gene:"
          })
          
          gffile <- paste0("./info/", gsub(" ", "_", input$variety_ID), "/", gsub(" ", "_", input$variety_ID), ".",chr ,".gff.txt.gz")
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
        }else{
          sendSweetAlert(
            session = session,
            title = "Your ID is wrong", type = "error",
            text = NULL
          )
          NULL 
        }
        
        
      }
    }  
    
    output$genesequence_ID.txt <- downloadHandler(
      filename <- function() { paste('The_gene_sequence.txt') },
      content <- function(file) {
        writeXStringSet(df, file, width = 150)
      }, contentType = 'text/plain'
    )
    
    output$cdssequence_ID.txt <- downloadHandler(
      filename <- function() { paste('The_cds_sequence.txt') },
      content <- function(file) {
        writeXStringSet(dc, file, width = 150)
      }, contentType = 'text/plain'
    )
    
    output$cdnasequence_ID.txt <- downloadHandler(
      filename <- function() { paste('The_cdna_sequence.txt') },
      content <- function(file) {
        writeXStringSet(dcdna, file, width = 150)
      }, contentType = 'text/plain'
    )
    
    output$prosequence_ID.txt <- downloadHandler(
      filename <- function() { paste('The_pro_sequence.txt') },
      content <- function(file) {
        writeXStringSet(dp, file, width = 150)
      }, contentType = 'text/plain'
    )
  }) 
  
  observeEvent(input$indelall1, {
    updateMultiInput(
      session = session,
      inputId = "mychooseri",
      selected = all.soya.cho
    )
  })
  
  observeEvent(input$indelnone1, {
    updateMultiInput(
      session = session,
      inputId = "mychooseri",
      selected = character(0)
    )
  })
  
  observe({
    if (input$clearSERID>0) {
      isolate({
        updateTextInput(session, "geneid", value = "")
      })
    } else {NULL}
  })
  
  observe({
    if (input$SERExamID >0) {
      isolate({
        updateSelectInput(session, "variety_ID", selected = "Zhonghuang 13")
        updateTextInput(session, "geneid", value = "SoyZH13_02G148200")
        
        
      })
    } else {NULL}
  })
  
  
  observeEvent(input$submit_GSIT,{
    library(GenomicRanges)
    library(IRanges)
    load(paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".gene.info.RData"))
    chr <- gsub(":.+", "", input$geneinterval)
    start <- as.numeric(gsub("\\s","", strsplit(gsub(".+:", "", input$geneinterval),"-")[[1]]))[1]
    end <- as.numeric(gsub("\\s","", strsplit(gsub(".+:", "", input$geneinterval),"-")[[1]]))[2]
    
    if( !(chr %in% paste0("chr", 1:20)) ){
      sendSweetAlert(
        session = session,
        title = "Please input true chromosome", type = "error",
        text = NULL
      )
      NULL
    }else{
    fasta <- readDNAStringSet(paste0("./info/",gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".",chr, ".fasta.gz"))
    
    #interval sequence
    if (start > width(fasta[chr]) | end > width(fasta[chr]) | end - start > 10000000 | !(end - start > 0) | is.na(end + start) ){
      sendSweetAlert(
        session = session,
        title = "The length of the chromosome is not right!", type = "error",
        text = NULL
      )
      NULL 
    }else{
      gene <- GRanges(seqnames = gene_info_s$chr, IRanges(start = gene_info_s$start, end = gene_info_s$end))
      thefind <- GRanges(seqnames = chr, IRanges(start = start, end = end))
      find.f1 <- findOverlaps(gene, thefind)
      geneinfo <- gene_info_s[gene_info_s$id %in% gene_info_s$id[c(find.f1@from)],]
      
    output$seq_title_it <- renderText({
      "The sequence of interval"
    })
    
    output$seq_it <- renderText({
        ditf  <- subseq(fasta[names(fasta) == chr], as.numeric(start), as.numeric(end) )
        names(ditf) <- paste0(chr, ":", start, "-", end, " length = ", end - start + 1)
        tmp.f11 <- file.path(tempdir(), "t11.fa")
        writeXStringSet(ditf, file = tmp.f11, width = 150)
        readLines(tmp.f11)
    }, sep = "\n")
    
    #fasta cds protein信息读取
    load(paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".cds.fasta.RData"))
    load(paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".protein.RData"))
    load(paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".cdna.fasta.RData"))
    
    #geneinfo
    output$geneinfo = DT::renderDataTable({
      if( length(geneinfo[, 1]) != 0 ){
        geneinfo
      }else{
        geneinfo <- data.frame("V1"="No gene in this location!")
        colnames(geneinfo) <- ""
        geneinfo
      }}, escape = FALSE, rownames= FALSE, selection="single", options = list(pageLength = 10, autoWidth = TRUE, bSort=FALSE))
    
    
    
    #gene sequence
    if( length(geneinfo[, 1]) != 0 ){
    output$gene_it <- renderText({
      if (is.null(input$geneinfo_rows_selected)) {
      } else {
        clicked <- input$geneinfo_rows_selected
        info <- data.frame(geneinfo[clicked,])
        info$seq[info$strand == "+"] <- as.character(subseq(fasta[info[info$strand == "+",]$chr], info[info$strand == "+",]$start, info[info$strand == "+",]$end))
        info$seq[info$strand == "-"] <- as.character(reverseComplement(subseq(fasta[info[info$strand == "-",]$chr], info[info$strand == "-",]$start, info[info$strand == "-",]$end)))
        gene <- DNAStringSet(info$seq)
        names(gene) <- paste0(names(gene),  ":", as.numeric(info[1,3]), "-", as.numeric(info[1,4]), " length = ", as.numeric(info[4]) - as.numeric(info[1,3]) +1)
        tmp.f4 <- file.path(tempdir(), "t4.fa")
        writeXStringSet(gene, file = tmp.f4, width = 150)
        readLines(tmp.f4) 
      }
    },sep = "\n")
    
    output$gene_title_it <- renderText({
      if (is.null(input$geneinfo_rows_selected)) {
        NULL
      } else {
        "The gene's sequence"
      }
    })
    
    #cds sequence
    output$cds_it <- renderText({
      if (is.null(input$geneinfo_rows_selected)) {
      } else {
        clicked <- input$geneinfo_rows_selected
        geneid <- data.frame(geneinfo[clicked,])[1,1]
        ditcs <- cds.info[grep(geneid, names(cds.info))]
        tmp.f6 <- file.path(tempdir(), "t6.fa")
        writeXStringSet(ditcs, file = tmp.f6, width = 150)
        readLines(tmp.f6)
      }
    },sep = "\n")
    
    output$cds_title_it <- renderText({
      if(is.null(input$geneinfo_rows_selected)){
        NULL
      }else{
        "CDS information"
      }
    })
    
    #cdna sequence
    output$cdna_it <- renderText({
      if (is.null(input$geneinfo_rows_selected)) {
        NULL
      } else {
        clicked <- input$geneinfo_rows_selected
        geneid <- data.frame(geneinfo[clicked,])[1,1]
        ditcd <- cdna.info[grep(geneid, names(cdna.info))]
        tmp.f6 <- file.path(tempdir(), "t6.fa")
        writeXStringSet(ditcd, file = tmp.f6, width = 150)
        readLines(tmp.f6)
      }
    },sep = "\n")
    
    output$cdna_title_it <- renderText({
      if(is.null(input$geneinfo_rows_selected)){
        NULL
      }else{
        "cDNA information"
      }
    })
    
    #proteion
    
    output$pro_it <- shiny::renderText({
      if (is.null(input$geneinfo_rows_selected)) {
        NULL
      }else{
        clicked <- input$geneinfo_rows_selected
        geneid <- data.frame(geneinfo[clicked,])[1,1]
        ditp <- protein[grep(geneid, names(protein))]
        tmp.f7 <- file.path(tempdir(), "t7.fa")
        writeXStringSet(ditp, file = tmp.f7, width = 150)
        readLines(tmp.f7)
      }}, sep = "\n")
    
    output$pro_title_it <- renderText({
      if( is.null(input$geneinfo_rows_selected) ){
        NULL
      }else{
        "Protein information"
      }
    })
    
    #click gff
    output$gffinfo_it <- DT::renderDataTable({
      if (is.null(input$geneinfo_rows_selected)){
      }else {
        clicked <- input$geneinfo_rows_selected
        geneid <- data.frame(geneinfo[clicked,])[1,1]
        gffile <- paste0("./info/", gsub(" ", "_", input$variety_IT), "/", gsub(" ", "_", input$variety_IT), ".",chr ,".gff.txt.gz")
        gf <- read.table(gffile, header = T, sep = "\t", as.is = T)
        gfinfo <- gf[grep(geneid, gf$Attributes), ]
        gfinfo <- gfinfo[, c(1:5, 9)]
        gfinfo
      }}, extensions = "Buttons", 
      rownames = F, 
      options = list(leftColumns = 8, 
                     scrollX = TRUE, dom = 'Bfrtip', 
                     buttons = c('copy', 'csv', 'excel', 'print')
      )
    )
    
    output$gffinfotitle_it <- renderText({
      if(is.null(input$geneinfo_rows_selected)){
        NULL
      }else{
        "The gff of gene for your select:"
      }
    })
    
    output$sequence_IT.txt <- downloadHandler(
      filename <- function() { paste('The_sequence.txt') },
      content <- function(file) {
        ditf  <- subseq(fasta[names(fasta) == chr], as.numeric(start), as.numeric(end) )
        names(ditf) <- paste0(chr, ":", start, "-", end, " length = ", end - start +1)
        writeXStringSet(ditf, file, width = 150)
      }, contentType = 'text/plain'
    )
    
    output$cdssequence_IT.txt <- downloadHandler(
      filename <- function() { paste('The_cds_sequence.txt') },
      content <- function(file) {
        clicked <- input$geneinfo_rows_selected
        geneid <- data.frame(geneinfo[clicked,])[1,1]
        ditcs <- cds.info[grep(geneid, names(cds.info))]
        writeXStringSet(ditcs, file, width = 150)
      }, contentType = 'text/plain'
    )
    
    output$cdnasequence_IT.txt <- downloadHandler(
      filename <- function() { paste('The_cdna_sequence.txt') },
      content <- function(file) {
        clicked <- input$geneinfo_rows_selected
        geneid <- data.frame(geneinfo[clicked,])[1,1]
        ditcd <- cdna.info[grep(geneid, names(cdna.info))]
        writeXStringSet(ditcd, file, width = 150)
      }, contentType = 'text/plain'
    )
    
    output$prosequence_IT.txt <- downloadHandler(
      filename <- function() { paste('The_pro_infomation.txt') },
      content <- function(file) {
        clicked <- input$geneinfo_rows_selected
        geneid <- data.frame(geneinfo[clicked,])[1,1]
        writeXStringSet(ditp, file, width = 150)
      }, contentType = 'text/plain'
    )
    }else{
      output$sequence_IT.txt <- downloadHandler(
        filename <- function() { paste('The_sequence.txt') },
        content <- function(file) {
          ditf  <- subseq(fasta[names(fasta) == chr], as.numeric(start), as.numeric(end) )
          names(ditf) <- paste0(chr, ":", start, "-", end, " length = ", end - start +1)
          writeXStringSet(ditf, file, width = 150)
        }, contentType = 'text/plain'
      )
      
      output$cdssequence_IT.txt <- downloadHandler(
        filename <- function() { paste('The_cds_sequence.txt') },
        content <- function(file) {
          writeLines("NO gene in this location", file)
        }, contentType = 'text/plain'
      )
      
      output$cdnasequence_IT.txt <- downloadHandler(
        filename <- function() { paste('The_cdna_sequence.txt') },
        content <- function(file) {
          writeLines("NO gene in this location", file)
        }, contentType = 'text/plain'
      )
      
      output$prosequence_IT.txt <- downloadHandler(
        filename <- function() { paste('The_pro_infomation.txt') },
        content <- function(file) {
          writeLines("NO gene in this location", file)
        }, contentType = 'text/plain'
      )
     }
    
    }
   }
  })
  
  
  
  
  observe({
    if (input$clearSERIT>0) {
      isolate({
        updateTextInput(session, "geneinterval", value = "")
      })
    } else {NULL}
  })
  
  observe({
    if (input$SERExamIT >0) {
      isolate({
        updateSelectInput(session, "variety_IT", selected = "Zhonghuang 13")
        updateTextInput(session, "geneinterval", value = "chr2:20260371-22686979")
        
        
      })
    } else {NULL}
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
    
    if (blast.in.seq == ""){
      sendSweetAlert(
        session = session,
        title = "Please input sequence!", type = "error",
        text = NULL
      )
      NULL 
    }else{
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
      
      if (input$program_database == "DNA database"){
        blastmethod <- input$programdna
      }else{
        blastmethod <- input$programpro
      }
      
      blast.out.file <- paste0(blast.in.file, ".blast.out")
      blast.cmds <- paste0(blastmethod," -query ", blast.in.file," -db ", '"', paste(blast.db, sep=" ", collapse = " "), '"', " -evalue ",
                           input$BLASTev, " -outfmt 5", " -out ", blast.out.file)
      
      system(blast.cmds, ignore.stdout = TRUE, ignore.stderr = TRUE)

      xmlParse(blast.out.file)}
    }   
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
        length <- getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_align-len')  %>% sapply(., xmlValue)
        
        identity <- getNodeSet(row, 'Iteration_hits//Hit//Hsp//Hsp_identity')  %>% sapply(., xmlValue)
        pident <- round(as.integer(identity) / as.integer(length) * 100, 2)
        cbind(qseqid, qlen, sseqid, sslen, qstart, qend, sstart, send, bitscore, evalue, gaps, pident, length)
      })
      #this ensures that NAs get added for no hits
      results <-  rbind.fill(lapply(results,function(y){as.data.frame((y),stringsAsFactors=FALSE)}))
      if (ncol(results) != 13) {
        results <- NULL
      }
      na.omit(results)
    }
  })
 
  output$BLASTresult <- DT::renderDataTable({
    if (is.null(blastedResults()) || is.null(blast.result())) {
      blastedResults <- data.frame("V1"="No BLAST hits found!")
      colnames(blastedResults) <- ""
      blastedResults
    } else {
      
      blastedResults()
    }
  }, escape = FALSE, rownames= FALSE, selection="single", filter = 'top', options = list(pageLength = 10, autoWidth = TRUE, bSort=FALSE))
  
  
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
    if (is.null(input$BLASTresult_rows_selected) || is.null(blastedResults()) ) {
      NULL
    } else {
      "The gene's sequence"
    }
  })
  
  output$alignment <- renderText({
    if(is.null(input$BLASTresult_rows_selected) || is.null(blastedResults()) ){
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
        rbind(paste0("Q:",splits[[1]][i],"\n"),paste0("M:",splits[[2]][i],"\n"),paste0("H-",splits[[3]][i],"\n"), "\n")
      })
      split_out[[1]][1] <- paste0(" ", split_out[[1]][1])
      unlist(split_out)
    }
  })
  
  #this chunk gets the alignemnt information from a clicked row
  output$clicked <- renderTable({
    if(is.null(input$BLASTresult_rows_selected) || is.null(blastedResults())){
      NULL
    }
    else{
      
      clicked = input$BLASTresult_rows_selected
      tableout<- data.frame(blastedResults()[clicked,])
      tableout <- t(tableout)
      names(tableout) <- c("")
      rownames(tableout) <- c("query ID","query length", "subject ID", "subject length", "query start", "query end", 
                              "subject start", "subject end", "bit-score", "e-value", "gaps", "percentage of identical matches", 
                              "alignment length")
      colnames(tableout) <- NULL
      data.frame(tableout)
    }
  },rownames =T,colnames =F)
  
  
  output$choicegene <- renderText({
    if (is.null(input$BLASTresult_rows_selected) || is.null(blastedResults()) ) {
      
    } else {
      "The sequenc in the gene"
    }
  })
  output$Alignment <- renderText({
    if (is.null(input$BLASTresult_rows_selected) || is.null(blastedResults()) ) {
      
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

  
  #INDEL
  
  
  observeEvent(input$submiti,{
    myPos <- anaReg(input$regi)
    if(validReg(myPos)){
      if (!is.null(myPos)) {
        chr <- as.character(myPos$chr)
        start <- as.numeric(myPos$start)
        end <- as.numeric(myPos$end)
        tmp.fg <- file.path(tempdir(), "tg.txt")
        tabix <- paste0("tabix ./indel/", chr, ".delete.gz ", gsub("c", "C", chr), ":", start, "-", end, " > ", tmp.fg)
        system(tabix)
        gffindel <- read.table(tmp.fg, sep = "\t", as.is = T, header = F)
        names(gffindel) <- c("chr", "pos", "ref", "alt" , paste0("s", 1:2898))
        
        accession = input$mychooseri
        accession <- sapply(accession, function(x){
          if (x %in% c("Improved cultivar", "Landrace", "G. Soja")) {
            x.dat <- readLines(paste0("./data/", x, ".soya.txt"))
            return(x.dat)
          } else {
            return(x)
          }
        })
        
        choosei <- unique(gsub(",.+", "", unlist(accession)))
        output$indeltable <- DT::renderDataTable({
          indel <- gffindel[, c(1:4, as.numeric(gsub("s", "", choosei)) + 4)]
          indel[,c(1:4, order(as.numeric(gsub("s", "", choosei)), decreasing = F) + 4)]
        }, 
        extensions = "FixedColumns", rownames = F, 
        options = list(leftColumns = 8, scrollX = TRUE, dom = 'Bfrtip')
        )
        
        output$indeltabletitle <- renderText({
          if( is.null(gffindel) ){
            "The sequence of interval"
          }else{
            NULL
          }
        })
        
        output$bulkdownloadindelInfo.txt <- downloadHandler(
          filename <- function() { paste('The_indelinfo.txt') },
          content <- function(file) {
            indel <- gffindel[, c(1:4, as.numeric(gsub("s", "", choosei)) + 4)]
            write.table(indel[,c(1:4, order(as.numeric(gsub("s", "", choosei)), decreasing = F) + 4)], file, col.names = T, row.names = F, quote = F, sep = "\t")
          }, contentType = 'text/plain'
        )
        
        
      }else{ NULL }
    }
    else{
      sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input genomic region or gene model in appropriate format!"
      )
    }
    
  })
  
  observe({
    if (input$clearINDEL>0) {
      isolate({
        updateTextInput(session, "regi", value="")
      })
    } else {NULL}
  })
  
  observe({
    if (input$INDELExam >0) {
      isolate({
        updateTextInput(session, "regi", value="SoyZH13_01G186100")
        
      })
    } else {NULL}
  })
  
  
  #primer3
  
  observeEvent(input$submitprimer,{
    choicesequence <- paste0(gsub("C", "c", input$Chrprimer), ":", input$upprimer, "-", input$downprimer)
    myPos <- anaReg(choicesequence)
    library(GenomicRanges)
    if(validReg(myPos)){
      if (!is.null(myPos)) {
        chr <- myPos$chr
        start <- myPos$start
        end <- myPos$end
        fasta <- readDNAStringSet(paste0("./info/Zhonghuang_13/Zhonghuang_13." ,chr, ".fasta.gz"))
        seqprimer <- subseq(fasta, start, end)
        SEQUENCE_ID <- "example"
        SEQUENCE_TEMPLATE <- paste0("SEQUENCE_TEMPLATE=", seqprimer)
        PRIMER_OPT_SIZE <- paste0("PRIMER_OPT_SIZE=", input$PRIMER_OPT_SIZE)
        PRIMER_MAX_SIZE <- paste0("PRIMER_MAX_SIZE=", input$PRIMER_SIZE[2])
        PRIMER_MIN_SIZE <- paste0("PRIMER_MIN_SIZE=", input$PRIMER_SIZE[1])
        
        PRIMER_OPT_TM <- paste0("PRIMER_OPT_TM=", input$PRIMER_OPT_TM)
        PRIMER_MAX_TM <- paste0("PRIMER_MAX_TM=", input$PRIMER_MAX_TM)
        PRIMER_MIN_TM <- paste0("PRIMER_MIN_TM=", input$PRIMER_MIN_TM)
        
        PRIMER_OPT_GC_PERCENT <- paste0("PRIMER_OPT_GC_PERCENT=", input$PRIMER_OPT_GC_PERCENT)
        PRIMER_MAX_GC <- paste0("PRIMER_MAX_GC=", input$PRIMER_GC[2])
        PRIMER_MIN_GC <- paste0("PRIMER_MIN_GC=", input$PRIMER_GC[1])
        
        PRIMER_MAX_NS_ACCEPTED <- paste0("PRIMER_MAX_NS_ACCEPTED=", input$PRIMER_MAX_NS_ACCEPTED)
        PRIMER_MAX_POLY_X <- paste0("PRIMER_MAX_POLY_X=", input$PRIMER_MAX_POLY_X)
        PRIMER_INTERNAL_MAX_POLY_X <- paste0("PRIMER_INTERNAL_MAX_POLY_X=", input$PRIMER_INTERNAL_MAX_POLY_X)
        PRIMER_MAX_SELF_ANY <-  paste0("PRIMER_MAX_SELF_ANY=", input$PRIMER_MAX_SELF_ANY)
        PRIMER_MAX_SELF_ANY_TH <- paste0("PRIMER_MAX_SELF_ANY_TH=", input$PRIMER_MAX_SELF_ANY_TH)
        
        PRIMER_MAX_SELF_END<- paste0("PRIMER_MAX_SELF_END=", input$PRIMER_MAX_SELF_END)
        PRIMER_MAX_SELF_END_TH <- paste0("PRIMER_MAX_SELF_END_TH=", input$PRIMER_MAX_SELF_END_TH)
        
        PRIMER_PRODUCT_SIZE_RANGE <- paste0("PRIMER_PRODUCT_SIZE_RANGE", input$PRIMER_PRODUCT_SIZE_RANGE)
        
        indelpos <- data.table::fread(paste0("./info/Position/", input$Chrprimer, ".indel.position"), sep = "\t", header = T, data.table = F)
        snppos <- data.table::fread(paste0("./info/Position/", input$Chrprimer, ".snp.position"), sep = "\t", header = T, data.table = F)
        
        snpnu <- snppos[snppos$POS <= end & snppos$POS >= start, ]
        indelnu <- indelpos[indelpos$POS <= end & indelpos$POS >= start, ]
        allnu <- rbind(snpnu, indelnu)
        allnu$POS <- allnu$POS - start
        SEQUENCE_TARGET <- paste0("SEQUENCE_TARGET=", str_c(allnu$POS, ",", allnu$length, collapse = " "))
        if (length(allnu$POS) > 0){
          primerorder <- paste(SEQUENCE_TEMPLATE, PRIMER_OPT_SIZE, PRIMER_MAX_SIZE, PRIMER_MIN_SIZE,
                               PRIMER_OPT_TM, PRIMER_MAX_TM, PRIMER_MIN_TM,
                               PRIMER_OPT_GC_PERCENT, PRIMER_MAX_GC, PRIMER_MIN_GC,
                               PRIMER_MAX_NS_ACCEPTED, PRIMER_MAX_POLY_X, PRIMER_INTERNAL_MAX_POLY_X,
                               PRIMER_MAX_SELF_ANY, PRIMER_MAX_SELF_ANY_TH, PRIMER_MAX_SELF_END, PRIMER_MAX_SELF_END_TH, 
                               SEQUENCE_TARGET, "=", sep = "\n")
        }else{
          primerorder <- paste(SEQUENCE_TEMPLATE, PRIMER_OPT_SIZE, PRIMER_MAX_SIZE, PRIMER_MIN_SIZE,
                               PRIMER_OPT_TM, PRIMER_MAX_TM, PRIMER_MIN_TM,
                               PRIMER_OPT_GC_PERCENT, PRIMER_MAX_GC, PRIMER_MIN_GC,
                               PRIMER_MAX_NS_ACCEPTED, PRIMER_MAX_POLY_X, PRIMER_INTERNAL_MAX_POLY_X,
                               PRIMER_MAX_SELF_ANY,  PRIMER_MAX_SELF_ANY_TH, PRIMER_MAX_SELF_END, PRIMER_MAX_SELF_END_TH,
                               "=", sep = "\n")
        }
        tmp.order <- file.path(tempdir(), "primer3.order")
        writeLines(primerorder, tmp.order)
        tmp.output <- file.path(tempdir(), "primer3.output")
        writeLines(primerorder, tmp.order, sep = "\n")
        system(paste0("primer3_core -format_output ", tmp.order, " > ", tmp.output))
        primer3output <- readLines(tmp.output)
        
        primertable <- primer3output[sort(c(grep("LEFT PRIMER", primer3output), grep("RIGHT PRIMER", primer3output)))]
        primertable[-c(1:2)] <- sub("^...", "", primertable[-c(1:2)])
        
        if ( length(primertable > 0)){
          output$primertable <- renderDataTable({
            primertable <- read.table(text = primertable)
            primertable$Oligos <- paste(primertable$V1 , primertable$V2)
            primertable <- primertable[, c(11,3:10)]
            colnames(primertable) <- c("Oligos", "Start position", "Length", "Tm", "GC percent", "Self any", "Self end", "Hairpin", "Sequence")
            primertable
          }, escape = FALSE, options = list(pageLength = 6, autoWidth = TRUE, bSort=FALSE))
          
          
          output$primerseq <- renderText({
            startcut <- grep("INCLUDED REGION SIZE:", primer3output) + 1
            endcut <- grep("ADDITIONAL OLIGOS", primer3output) - 1
            primer3output[startcut:endcut][!primer3output[startcut:endcut] == ""]
          }, sep = "\n")
        }else{
          sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Can't design primer by your input location!"
          )
          
        }
        
      }else{ 
        NULL 
        }
    }
    else{
      sendSweetAlert(
        session = session,
        title = "Error input!", type = "error",
        text = "Please input genomic region or gene model in appropriate format!"
      )
    }
    
  })
  
  #JBrowseR


  
  
  
  # Accession
  
  output$soya.info.txt <- downloadHandler(
    filename = function() { "soya.info.txt" },
    content = function(file) {
      write.table(soya.info, file, sep = "\t", quote=FALSE, row.names = FALSE)
    }, contentType = 'text/plain')
  
  
  output$sel.soya.info.txt <- downloadHandler(
    filename = function() { "sel.soya.info.txt" },
    content = function(file) {
      accession <- input$mychooserA
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
    accession <- input$mychooserA
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
  }, options = list(lengthMenu = c(5, 8, 10), pageLength = 10, searching = TRUE, autoWidth = FALSE), escape = FALSE
  )
  
  observeEvent(input$accessionall1, {
    updateMultiInput(
      session = session,
      inputId = "mychooserA",
      selected = all.soya.cho
    )
  })
  
  observeEvent(input$accessionnone1, {
    updateMultiInput(
      session = session,
      inputId = "mychooserA",
      selected = character(0)
    )
  })
 
})

