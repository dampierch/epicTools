######
######
###### Gene Expression Analysis
###### code for preliminary exploration
######
######


#################
##             ##
##   readme    ##
##             ##
#################


# load section loads libraries and data
# libraries loads packages installed from cran and bioconductor
# samples reads saved output from metaCrcNatHltFilt.R
# txi loads saved output from metaCrcNatHltCntPrp.R

# explore section provides exploratory gene expression analysis
# relies on txi data object and samples dataframe loaded in load section
# uses following packages: limma; edgeR; DESeq2; EDASeq; RUVSeq
# creates following plots: MDS, PCA, heatmap, RLE
# process takes 6h 44m for prelim plots inc 3 models for RUVr, n=847


##############
##          ##
##   load   ##
##          ##
##############


### load libraries ###


#generic reference
#source("https://bioconductor.org/biocLite.R")
BiocManager::install()  # a Yes

#for analysis with limma
#BiocManager::install("limma")  # a Yes
#BiocManager::install("statmod")  # a Yes
library("limma")
library("statmod")
library("ggplot2")
library("ggfortify")

#for analysis with edgeR
#BiocManager::install("edgeR")  # a Yes
#BiocManager::install("org.Hs.eg.db") # a Yes
library("edgeR")
library("AnnotationDbi")
library("org.Hs.eg.db")

#for analysis with DESeq2
#BiocManager::install("DESeq2")  # a Yes
#BiocManager::install("apeglm")
#browseVignettes("DESeq2")
library("DESeq2")
library("apeglm")
library("pheatmap")
library("RColorBrewer")

#for analysis with RUVSeq
#BiocManager::install("RUVSeq")
library("RUVSeq")


### read samples into dataframe ###


setwd("/scratch/chd5n/meta/")
samples <- read.table(file="metaCrcNatHltPhenoFilt.tsv", header=TRUE, sep='\t', stringsAsFactors=TRUE)


### read txi into list ###


setwd("/scratch/chd5n/meta/")
load(file="metaCrcNatHltTxi.Rdata") ## this is my count matrix


### prepare color palettes ###


cbSet1 <- brewer.pal(n=9,"Set1")
cbSet1NoGrn <- cbSet1[-c(3)]
cbPaired <- brewer.pal(n=12,"Paired")
cbPairedNoGrn <- cbPaired[-c(3,4)]
#to load color palettes for base R:
# >palette("x")
#to load color palettes for ggplot2:
# >+ scale_color_manual(values=cbSet1NoGrn)


#################
##             ##
##   explore   ##
##             ##
#################


### create preliminary plots ###


###
###limma and edgeR
###

#get initial counts and offsets from tximport list
counts <- #ENTER YOUR COUNT MATRIX HERE

#load gene list data object
geneSymbols <- mapIds(org.Hs.eg.db, keys = row.names(counts), keytype = "ENSEMBL", column = "SYMBOL", multiVals = "first")
dge <- DGEList(counts = counts, group = samples$sampTypeCln, samples = samples, genes = geneSymbols)
#^^ you don't need to define the group; just leave that parameter out

#filter out rows (genes) w/ low counts in > 2/3 of cases
#keep <- filterByExpr(dge, group = samples$sampTypeCln)
keep <- rowSums(dge$counts > 10) >= nrow(dge$samples) * (1/3)
dge <- dge[keep, ,keep.lib.sizes=FALSE]

# ~~~~~~~~~~ optional ~~~~~~~~~~~
# calc norm factors (should not be used if dge list object has offset)
dge <- calcNormFactors(dge)

palette(cbSet1NoGrn)

#explore raw and preliminarily normalized data
#
#raw counts without norm factor or offset
plotMDS(dge$counts, col = as.numeric(dge$samples$sampTypeCln), main = "MDS plot for raw counts", cex=0.7)
legend("topright", legend = levels(dge$samples$sampTypeCln), col = as.numeric(as.factor(levels(dge$samples$sampTypeCln))), pch = 16)
#
#counts normalized by {unknown} norm factor / norm factor when present (not offset, even if present)
plotMDS(dge, col=as.numeric(dge$samples$sampTypeCln), main="MDS plot for normalized counts", cex=0.7)
legend("topright", legend = levels(dge$samples$sampTypeCln), col=as.numeric(as.factor(levels(dge$samples$sampTypeCln))), pch = 16)
#
#counts normalized by voom transformation
#set design matrix
design <- model.matrix(~ dge$samples$sampTypeCln)  #!! i guess this is what you were trying to avoid
#voom transform (log2CPM) filtered raw counts (note: should not use norm counts prior to batch reduction (although batch effect removal on norm and raw counts gives same MDS plot); also, note on norm vs offset factors: looks like offset NOT used instead of norm factors if norm factors available)
vRaw <- voom(dge$counts, design = design, plot = FALSE)
plotMDS(vRaw, col=as.numeric(dge$samples$sampTypeCln), main="MDS plot for voom transformed raw counts", cex=0.7)
legend("topright", legend = levels(dge$samples$sampTypeCln), col = as.numeric(as.factor(levels(dge$samples$sampTypeCln))), pch = 16)

#explore batch effect reduced data (requires voom transform)
#
#remove batch effect from filtered raw log2CPM values (better result with center AND format as batches)
vRawB <- removeBatchEffect(vRaw, batch = as.numeric(dge$samples$center), batch2 = as.numeric(dge$samples$format), design = design)
#
#explore MDS plots after batch effect removal
plotMDS(vRawB, col=as.numeric(dge$samples$sampTypeCln), main="MDS plot for voom raw counts, batch effect removed", cex=0.7)
legend("topleft", legend = levels(dge$samples$sampTypeCln), col=as.numeric(as.factor(levels(dge$samples$sampTypeCln))), pch = 16)
#explore PCA plot after batch effect reduction
autoplot(prcomp(t(vRawB)), data=dge$samples, colour="sampTypeCln", main="PCA plot for voom raw counts, batch effect removed") + scale_color_manual(values=cbSet1NoGrn) + theme_classic() + theme(plot.title = element_text(hjust=0.5))
#^^ this autoplot command is the ggfortify wrapper for ggplot2 with PCA


###
###DESeq2
###

#browseVignettes("DESeq2")

#prepare gene annotations for anaylsis; DESeq2 treats them as separate matrix
geneSymbols <- mapIds(org.Hs.eg.db, keys = row.names(txi$counts), keytype = "ENSEMBL", column = "SYMBOL", multiVals = "first")

#set up dds data object
dds <- DESeqDataSetFromTximport(txi, samples, ~ sampTypeCln)

#pre-filter lowly expressed genes to speed up downstream analysis
#want 5-10 counts in a library to prove gene is expressed in that library
#want minimum expression in at least the number of libraries in the smallest group of interest or 1/3 of samples
#minimum count threshold for a gene across all samples should be 10*N
#strict filtering performed internally later; this is simply for size reduction and speed
countLimit <- 10
snum <- ncol(dds)
slim <- (1/3) * snum
keep <- rowSums( counts(dds) > countLimit ) >= slim    #sum logical (T/F) output for each row (i.e. gene) to get total number samples with that many counts for that gene
ddsF <- dds[keep, ]

#estimate size factors and dispersions and fit negative binomial model and test it
ddsFND <- DESeq(ddsF)   #*********rate limiting step ************ rate limiting step *************

#transformations for heatmaps and PCA plots
#
#variance stabilizing transformation
#blind=TRUE if exploring data for quality assurance; blind=FALSE when analyzing design
vsd <- vst(ddsFND, blind=TRUE)

palette(cbSet1NoGrn)

#heatmaps
#
#select top 500 genes by mean norm counts
select <- order(rowMeans(counts(ddsFND ,normalized=TRUE)), decreasing=TRUE)[1:500]
#center and sampTypeCln without and with cluster
df <- as.data.frame(colData(ddsFND)[,c("center", "sampTypeCln")])
colors <- colorRampPalette( rev(brewer.pal(9, "Blues")) )(255)
pheatmap(assay(vsd)[select, ], cluster_rows=FALSE, show_rownames=FALSE, cluster_cols=FALSE, annotation_col=df, main="Heatmap, no cluster", col=colors)
pheatmap(assay(vsd)[select, ], cluster_rows=FALSE, show_rownames=FALSE, cluster_cols=TRUE, annotation_col=df, main="Heatmap, cluster by sampType, center", col=colors)
pheatmap(assay(vsd)[select, ], cluster_rows=TRUE, show_rownames=FALSE, cluster_cols=TRUE, annotation_col=df, main="Heatmap, cluster by sampType, center, gene", col=colors)

#PCA plots
#
#center
pcaData <- plotPCA(vsd, intgroup=c("center"), returnData=TRUE)
percentVar <- round(100*attr(pcaData, "percentVar"))
ggplot(pcaData, aes(PC1, PC2, color=center)) + geom_point(size=1) +
  xlab(paste0("PC1: ",percentVar[1],"% variance")) + ylab(paste0("PC2: ",percentVar[2],"% variance")) +
  coord_fixed() + theme_classic() + theme(plot.title = element_text(hjust=0.5)) +
  ggtitle("PCA for meta analysis :: Centers") + scale_color_manual(values=cbSet1NoGrn)
#
#center and sampTypeCln
pcaData <- plotPCA(vsd, intgroup=c("sampTypeCln", "center"), returnData=TRUE)
percentVar <- round(100*attr(pcaData, "percentVar"))
ggplot(pcaData, aes(PC1, PC2, color=center, shape=sampTypeCln)) + geom_point(size=1) +
  xlab(paste0("PC1: ",percentVar[1],"% variance")) + ylab(paste0("PC2: ",percentVar[2],"% variance")) +
  coord_fixed() + theme_classic() + theme(plot.title = element_text(hjust=0.5)) +
  ggtitle("PCA for meta analysis :: Centers and Tissue types") + scale_color_manual(values=cbSet1NoGrn)
#
#center and sampTypeCln
pcaData <- plotPCA(vsd, intgroup=c("sampTypeCln", "center"), returnData=TRUE)
percentVar <- round(100*attr(pcaData, "percentVar"))
ggplot(pcaData, aes(PC1, PC2, color=sampTypeCln)) + geom_point(size=1) +
  xlab(paste0("PC1: ",percentVar[1],"% variance")) + ylab(paste0("PC2: ",percentVar[2],"% variance")) +
  coord_fixed() + theme_classic() + theme(plot.title = element_text(hjust=0.5)) +
  ggtitle("PCA for meta analysis :: Centers and Tissue types") + facet_wrap("center", nrow = 3, ncol = 3) + scale_color_manual(values=cbSet1NoGrn)


###
###EDASeq
###

counts <- round(txi$counts)
keep <- rowSums(counts > 10) >= ncol(counts) * (1/3)
counts <- counts[keep, ]
set <- newSeqExpressionSet(counts, phenoData = samples)

palette(cbSet1NoGrn)

# explore relative log expression and PCA plots
colors <- cbSet1NoGrn
plotRLE(set, outline=FALSE, ylim=c(-4,4), col=colors[pData(set)[ ,"sampTypeCln"]], main="RLE for RUVr counts, raw")
plotPCA(set, col=colors[pData(set)[ ,"sampTypeCln"]], cex=0.7, main = "PCA plot for raw counts")
legend("topright", legend = levels(pData(set)[ ,"sampTypeCln"]), col=colors[as.numeric(as.factor(levels(pData(set)[ ,"sampTypeCln"])))], pch = 16)

# normalize with upper quartile
setN <- betweenLaneNormalization(set, which = "upper")
plotRLE(setN, outline=FALSE, ylim=c(-4,4), col=colors[pData(setN)[ ,"sampTypeCln"]], main="RLE for EDASeq UQ norm counts")
plotPCA(setN, col=colors[pData(setN)[ ,"sampTypeCln"]], cex=0.7, main = "PCA plot for EDASeq UQ norm counts")
legend("topright", legend = levels(pData(setN)[ ,"sampTypeCln"]), col=colors[as.numeric(as.factor(levels(pData(setN)[ ,"sampTypeCln"])))], pch = 16)
