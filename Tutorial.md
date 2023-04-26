<div align='center' ><font size='70'>Tutorial of SoybeanGDB</font></div>

>&emsp;&emsp;**SoybeanGDB** is a comprehensive genome database to accelerate functional genomic and population genetic studies in soybean.

>&emsp;&emsp;**SoybeanGDB** employs 39 high-quality soybean genomes, 15,446,616 high-quality SNPs (single nucleotide polymorphism) and 4,136,231 high-quality Indels (small insertions/deletions) among 2898 soybean accessions identified based on next-generation sequencing data. To help users, a variety of versatile analytic tools including JBrowse, BLAST, GO/KEGG annotation, GO/KEGG enrichment analysis, Primer designing, etc., are implemented in **SoybeanGDB**. **SoybeanGDB** is deployed at ([https://venyao.xyz/SoybeanGDB/](https://venyao.xyz/SoybeanGDB/)) for online use.

>&emsp;&emsp;The homepage of **SoybeanGDB** displays the main functionalities of SoybeanGDB (**Figure 1**).  

>(1) Search 39 high-quality soybean genomes by gene IDs.  
>(2) Search 39 high-quality soybean genomes by genome locations.  
>(3) Search 39 high-quality soybean genomes using BLAST.  
>(4) Browse transcription factors/regulators in a genome.  
>(5) Browse syntenic regions between different soybean genomes.  
>(6) Browse structural variations by location.  
>(7) JBrowse visualization of 39 high-quality soybean genomes.  
>(8) Browse and visualize high-quality SNPs among 2898 soybean accessions.  
>(9) Search and retrieve SNPs among 2898 soybean accessions.  
>(10) Conduct linkage disequilibrium analysis between SNPs in a genomic region.  
>(11) Conduct nucleotide diversity analysis among different groups of soybean accessions to identify genes under selection during domestication and modern breeding.  
>(12) Calculate and visualize allele frequency of user-input SNP sites.  
>(13) Search and retrieve INDELs among 2898 soybean accessions.  
>(14) Conduct expression and co-expression analysis of genes in the genome of Zhonghuang 13.  
>(15) Design primers based on the Zhonghuang 13 genome targeting SNPs and Indels in user-input genomic region.  
>(16) Search and retrieve orthologous gene groups among 39 soybean genomes.  
>(17) Conduct GO (gene ontology) annotation/enrichment analysis of user-input protein-coding genes in any of the 39 genomes.  
>(18) Conduct KEGG annotation/enrichment analysis of user-input protein-coding genes in any of the 39 genomes.  

<div align=center><img src="Fig1.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 1. The home page of SoybeanGDB</font></div>  

## **1. Search a genome by gene IDs** <a name="tp1"></a>

>&emsp;&emsp;Users can input multiple gene IDs for any of the 39 soybean genomes to obtain the annotations of all input genes. Annotations of all genes can be viewed in a table, which can be exported as a csv or excel file (**Figure 2**). The gene sequences, CDS sequences, cDNA sequences, and the protein sequences can be download as plain text files.
<div align=center><img src="Fig2.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 2. Search a genome by multiple gene IDs</font></div>  

## **2. Search a genome by location** <a name="tp2"></a>
>&emsp;&emsp;Users can input a genomic region to view and retrieve the information of genes and transposable elements in any of the 39 soybean genomes. Check the example input data to enter a genomic region in appropriate format (**Figure 3**). Steps to search any genome by location are shown in **Figure 3**.
<div align=center><img src="Fig3.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 3. Genes and transposable elements in chr1:20260371-20686979 of Zhonghuang 13</font></div>  

## **3. Genomic distribution of genes** <a name="tp3"></a>
>&emsp;&emsp;Users can input multiple gene IDs for any of the 39 soybean genomes to visualize the chromosome location of all input genes (Figure 4).
<div align=center><img src="Fig4.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 4. Genomic distribution of genes</font></div>  

## **4. Browse transcription factors/regulators of a genome** <a name="tp4"></a>
>&emsp;&emsp;Users can browse transcription factors/regulators annotated in any of the 39 soybean genomes. Steps to browse the annotated transcription factors/regulators are shown in **Figure 5**.
<div align=center><img src="Fig5.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 5. Transcription factors/regulators of Zhonghuang 13</font></div>  

## **5. Genome synteny** <a name="tp5"></a>
>&emsp;&emsp;This function is designed for users to browse the syntenic regions between two soybean genomes. Syntenic blocks will be displayed in a table in the main panel. Genes in the syntenic blocks of the reference/query genome will be listed (**Figure 6**).
<div align=center><img src="Fig6.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 6. Syntenic chromosome regions between Zhonghuang 13 and W05</font></div>  

## **6. Structural variations** <a name="tp6"></a>
>&emsp;&emsp;This function is designed for users to browse the structural variations identified between two soybean genomes. Different types of structural variations, including duplications, inversions, tandem repeats, translocations, deletions, and insertions, will be displayed in different panels (**Figure 7**).
<div align=center><img src="Fig7.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 7. Structural variations between Zhonghuang 13 and W05</font></div>  

## **7. JBrowse** <a name="tp7"></a>
>&emsp;&emsp;Jbrowser of all 39 high-quality soybean genomes were built to view the genome sequence, protein-coding genes, transposable elements, as well as the GO and KEGG annotations of protein-coding genes (**Figure 8**; **Figure 9**). These information are displayed in different tracks of the JBrowser.  
<div align=center><img src="Fig8.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 8. The JBrowse menu of SoybeanGDB</font></div>  
<div align=center><img src="Fig9.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 9. JBrowse of Zhonghuang 13</font></div>  

## **8. Browse SNPs in a user-input genomic region** <a name="tp8"></a>
>&emsp;&emsp;Users can browse and download SNPs among 2898 soybean accessions by inputting a single gene ID or genomic region in appropriate format, such as "SoyZH13_09G103313" or "chr1:29765419-29793053" (**Figure 10**). After clicking the "Submit" button, SNPs will be visualized in the main panel, with different SNPs represented as inverted-triangles in different colors. The result can be further filtered by selecting soybean accessions or setting the mutation effect of SNPs. The result can also be downloaded by clicking the download buttons "Genotype data", "SNPs information" and "PDF-file" at the top of the main panel.
<div align=center><img src="Fig10.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 10. Browse and visualize SNPs in chr1:29765419-29793053</font></div>  

## **9. Search and retrieve SNPs in a user-input genomic region** <a name="tp9"></a>
>&emsp;&emsp;Users can search and retrieve SNPs among 2898 soybean accessions by inputting a single gene ID or genomic region in appropriate format, such as "chr7:29560705-29573051" (**Figure 11**). After clicking the "Submit" button, the genotypes of selected soybean accessions at SNP sites located in the user-input genomic region will be displayed in the main panel. In addition, SNPs information, genotype data and gene annotations in user-input genomic region can be downloaded using the download buttons at the top of the main panel.
<div align=center><img src="Fig11.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 11. SNPs retrieved in genomic region chr7:29560705-29573051</font></div>  

## **10. Linkage disequilibrium analysis of SNPs** <a name="tp10"></a>
>&emsp;&emsp;In this menu, a heatmap can be created to display the linkage disequilibrium between pair-wise SNP sites in a user-input genomic region. Essential steps to conduct linkage disequilibrium analysis is shown in **Figure 12**. Several options are provided to tune the appearance of the heatmap including figure flipping and colors (**Figure 12**). Finally, the heatmap can be downloaded in PDF or SVG format.
<div align=center><img src="Fig12.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 12. Linkage disequilibrium analysis of SoyZH13_09G103313</font></div>  

## **11. Nucleotide diversity analysis of SNPs** <a name="tp11"></a>
>&emsp;&emsp;The ‘‘Diversity" submenu under the "SNPs" menu provides the functionality to calculate nucleotide diversity among groups of soybean accessions in a user-input genomic region. Taking SoyZH13_12G067900 as an example, the results can be adjusted by sequentially setting the widgets "Number of SNPs in each window", "Ecotypes to calculate diversity", "Numerator ecotype", "Denominator ecotype", "Mutation effect" and "Upstream/Downstream". After clicking the **Submit!** button, the results can be visualized in the main panel (**Figure 13**). The results can also be downloaded as a PDF, SVG or TXT file.
<div align=center><img src="Fig13.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 13. Nucleotide diversity analysis of SoyZH13_12G067900</font></div>  

## **12. Allele frequency analysis of user-input SNP sites** <a name="tp12"></a>
>&emsp;&emsp;In this menu, allele frequency of user-input SNP sites across different soybean ecotypes (improved cultivar, landrace, and G. soja) can be calculated and visualized (**Figure 14**). Several parameters are provided to tune the appearance of the result plot, including the colors used for the major and minor allele and the plot size. After clicking the **Submit!** button, the results would be visualized in the main panel, which can be exported in PDF or SVG format.
<div align=center><img src="Fig14.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 14. Allele frequency analysis of user-input SNP sites</font></div>  

## **13. Search and retrieve INDELs** <a name="tp13"></a>
>&emsp;&emsp;Users can search and retrieve high-quality INDELs among 2898 soybean accessions for any input gene ID or genomic region. Steps to search Indels are shown in **Figure 15**.
<div align=center><img src="Fig15.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 15. Search and retrieve INDELs located in the genic region of SoyZH13_01G186100</font></div>  

## **14. Gene expression analysis** <a name="tp14"></a>
>&emsp;&emsp;The expression levels of protein-coding genes in the genome of Zhonghuang 13 across 27 different tissues/stages were collected in SoybeanGDB. Using the "Gene expression analysis" functionality, the expression level of any user-input gene can be retrieved as a table and visualized as a heatmap (**Figure 16**). The expression level of the input gene can be downloaded as a csv or excel file. The size of the heatmap can be adjusted using provided widgets in the sidebar panel.  
<div align=center><img src="Fig16.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 16. Expression profile of SoyZH13_05G201900</font></div>  

## **15. Gene co-expression analysis** <a name="tp15"></a>
>&emsp;&emsp;Based on the expression levels of protein-coding genes in the genome of Zhonghuang 13 across 27 different tissues/stages, we implemented a functionality in SoybeanGDB for users to perform co-expression analysis of input genes. For a user-input gene list, the expression correlation coefficient between the input genes and all expressed genes were calculated and displayed in a table, which can be downloaded as csv or excel files (**Figure 17**). The expression correlation coefficient can also be visualized as a heatmap. Essential steps to conduct co-expression analysis are shown in **Figure 17**.  
<div align=center><img src="Fig17.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 17. Gene co-expression analysis</font></div>  

## **16. Search a genome by BLAST** <a name="tp16"></a>
>&emsp;&emsp;Under this menu, users can search one or multiple soybean genomes by sequence similarity utilizing BLAST (**Figure 18**). The input sequences must be in fasta format. The genome sequences, gene sequences, CDS sequences, and protein sequences of any one or multiple of the 39 high-quality genomes can be searched by BLAST. After clicking the **Submit** button, BLAST alignment would be conducted and the results can be viewed in the **Output** panel (**Figure 19**).
<div align=center><img src="Fig18.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 18. Steps to perform BLAST</font></div>  
<div align=center><img src="Fig19.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 19. BLAST result viewed in the output panel</font></div>  

## **17. Design primers for a user-input genomic region or gene locus** <a name="tp17"></a>
>&emsp;&emsp;Using this functionality, users can design primers for any input genomic region or gene locus in the genome of Zhonghuang 13, targeting SNPs and Indels in this region (**Figure 20**). Primer3 was utilized to design the primers. Many options of Primer3 are implemented as graphical interface for users to set appropriate parameters for primer designing. Five best candidate primers would be displayed in a result table, with each column representing diverse features of the designed primers. The detailed information of the best primers is displayed below the table, including the template sequence, the primers sequence and the SNPs/INDELs in the input region.
<div align=center><img src="Fig20.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 20. Design primers for a user-input genomic region</font></div>  

## **18. Search orthologous gene groups among 39 genomes** <a name="tp18"></a>
>&emsp;&emsp;With this functionality, users can search orthologous groups among 39 soybeans genomes by inputting a single gene ID of any of the 39 genomes (**Figure 21**). Then, orthologs of the input gene in other genomes would be displayed in the main panel after clicking the **Submit!** button.
<div align=center><img src="Fig21.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 21. Orthologous genes search</font></div>  

## **19. GO Annotation of a user-input gene list** <a name="tp19"></a>
>&emsp;&emsp;This functionality is used to perform GO annotation of a user-input gene list from any of the 39 soybean genomes. Steps to conduct GO annotation are shown in **Figure 22**. The full annotation result is displayed as a table, which can be downloaded. The top 30 GO terms with the largest number of genes are displayed as a bar plot (**Figure 22**).
<div align=center><img src="Fig22.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 22. GO Annotation of user-input genes</font></div>  

## **20. GO enrichment analysis of a user-input gene list** <a name="tp20"></a>
>&emsp;&emsp;This functionality is used to perform GO enrichment analysis of a user-input gene list from any of the 39 soybean genomes. Steps to conduct GO enrichment analysis are shown in **Figure 23**. Enrichment analysis for each GO category (Molecular Function, Biological Process, Cellular Component) was conducted separately. For each category, the full enrichment result is displayed as a table, and the significant enrichment result is displayed in two figures. The enrichment result can be further filtered by other parameters, including adjusted P value, Q value, etc. (**Figure 23**).  
<div align=center><img src="Fig23.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 23. GO Enrichment analysis of user-input genes</font></div>  

## **21. KEGG Annotation of a user-input gene list** <a name="tp21"></a>
>&emsp;&emsp;This functionality is used to perform KEGG pathway annotation of a user-input gene list from any of the 39 soybean genomes. Steps to conduct KEGG annotation are shown in **Figure 24**. The full annotation result is displayed as a table, which can be downloaded. The top 30 largest KEGG pathways are displayed as a bar plot (**Figure 24**).  
<div align=center><img src="Fig24.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 24. KEGG Annotation of user-input genes</font></div>  

## **22. KEGG enrichment analysis of a user-input gene list** <a name="tp22"></a>
>&emsp;&emsp;This functionality is used to perform KEGG enrichment analysis of a user-input gene list from any of the 39 soybean genomes. Steps to conduct KEGG enrichment analysis are shown in **Figure 25**. The full enrichment result is displayed as a table, and the significant enrichment result is displayed as a figure. The enrichment result can be further filtered by other parameters, including adjusted P value, Q value, etc. (**Figure 25**).  
<div align=center><img src="Fig25.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 25. KEGG Enrichment analysis</font></div>  

## **23. Information of 2898 soybean accessions** <a name="tp23"></a>
>&emsp;&emsp;A total of 2898 soybean accessions were collected in SoybeanGDB. The detailed information of all 2898 accessions is displayed in the "Accessions" submenu under the "Help" menu of SoybeanGDB, which can be downloaded as a csv or excel file. In the sidebar panel of the "Accessions" submenu, a widget is provided for users to select one or multiple accessions to view the detailed information in the main panel (**Figure 26**).  
<div align=center><img src="Fig26.png" width="100%" height="100%" align=center /></div>
<div align=center><font color=blue size=5>Figure 26. Retrieve information of soybean accessions</font></div>  

<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
