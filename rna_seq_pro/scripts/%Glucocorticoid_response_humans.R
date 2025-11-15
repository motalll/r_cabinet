## ============================================================
## RNA-seq mini-project (human airway dexamethasone) – one file
## QC → DESeq2 → LFC shrink → MA/volcano/heatmap → Hallmark GSEA
## Output: CSVs + PDFs in the working directory
## ============================================================

## (A) Set up user library & Bioconductor repos (avoid /usr write issues)
if (!nzchar(Sys.getenv("R_LIBS_USER"))) {
  Sys.setenv(R_LIBS_USER = file.path(Sys.getenv("HOME"), "R", paste0(R.version$platform, "-library"), paste(R.version$major, R.version$minor, sep=".")))
}
if (!dir.exists(Sys.getenv("R_LIBS_USER"))) dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)
.libPaths(unique(c(Sys.getenv("R_LIBS_USER"), .libPaths())))

if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager", quiet = TRUE)
options(repos = BiocManager::repositories())  # ensure Bioc repos are active
message("R: ", R.version.string, " | Bioc: ", as.character(BiocManager::version()))

## Optional: soften locale noise in minimal Linux installs
try(Sys.setlocale(category = "LC_ALL", locale = "C.UTF-8"), silent = TRUE)

## (B) Helpers to install only what’s missing
.install_if_missing_bioc <- function(pkgs) {
  missing <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing)) BiocManager::install(missing, ask = FALSE, update = TRUE, quiet = TRUE)
}
.install_if_missing_cran <- function(pkgs) {
  missing <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing)) install.packages(missing, quiet = TRUE)
}

## (C) Install required packages
# Core Bioconductor
.install_if_missing_bioc(c("DESeq2","airway","org.Hs.eg.db"))
# Preferred LFC shrinker (fallback to "normal" if absent)
.install_if_missing_bioc("apeglm")
# Enrichment stack (preferred route) + plotting helpers
.install_if_missing_bioc(c("clusterProfiler","enrichplot"))
# Lightweight GSEA fallback
.install_if_missing_bioc("fgsea")
# Optional volcano helper (we’ll fall back to ggplot if missing)
.install_if_missing_bioc("EnhancedVolcano")
# CRAN
.install_if_missing_cran(c("pheatmap","ggplot2","dplyr","readr","msigdbr"))

## (D) Load libraries (tolerate optional ones gracefully)
suppressPackageStartupMessages({
  library(DESeq2)
  library(airway)
  library(pheatmap)
  library(ggplot2)
  library(dplyr)
  library(org.Hs.eg.db)
  library(msigdbr)
})
have_apeglm <- requireNamespace("apeglm", quietly = TRUE)
have_EV     <- requireNamespace("EnhancedVolcano", quietly = TRUE)
have_CP     <- requireNamespace("clusterProfiler", quietly = TRUE)
have_EP     <- requireNamespace("enrichplot", quietly = TRUE)
have_fgsea  <- requireNamespace("fgsea", quietly = TRUE)

## ============================================================
## 1) Load dataset (ships with Bioconductor; no manual download)
## ============================================================
data("airway")   # creates object 'airway'
se <- airway     # SummarizedExperiment: counts + sample metadata

# Quick peek (printed to console)
print(se)
print(colData(se)[, c("dex","cell")])
print(head(assay(se)))

## ============================================================
## 2) Prepare data and run QC
## ============================================================
# Ensure "untrt" is reference so log2FC is trt vs untrt
colData(se)$dex <- relevel(colData(se)$dex, "untrt")

# Light prefilter to drop nearly-all-zero genes (speeds, boosts power)
keep <- rowSums(assay(se) >= 10) >= 2
se_f <- se[keep, ]

# Build DESeq2 object with donor blocking (paired design)
dds <- DESeqDataSet(se_f, design = ~ cell + dex)

# Library size plot
libsize <- colSums(counts(dds))
pdf("fig_library_sizes.pdf", 6, 4)
barplot(libsize/1e6, ylab="Millions of reads", xlab="Sample", las=2,
        main="Library sizes")
dev.off()

# Variance-stabilizing transform for PCA/heatmaps (visualization only)
vsd <- vst(dds, blind = TRUE)

# PCA
p_pca <- plotPCA(vsd, intgroup = c("dex","cell")) + ggtitle("PCA (VST)")
ggsave("fig_PCA.pdf", p_pca, width = 6, height = 5)

# Sample-to-sample distance heatmap
dists <- dist(t(assay(vsd)))
pdf("fig_sample_distance_heatmap.pdf", 6.5, 5.5)
pheatmap(as.matrix(dists),
         annotation_col = as.data.frame(colData(vsd)[, c("dex","cell")]),
         main="Sample distance heatmap")
dev.off()

## ============================================================
## 3) Differential expression (DESeq2) and LFC shrinkage
## ============================================================
dds <- DESeq(dds)

# Extract treatment effect (treated vs untreated)
res <- results(dds, contrast = c("dex","trt","untrt"))
summary(res)

# LFC shrinkage (preferred: apeglm; fallback: normal)
shrink_type <- if (have_apeglm) "apeglm" else "normal"
res_shr <- lfcShrink(dds, coef = "dex_trt_vs_untrt", type = shrink_type)
res_shr <- res_shr[order(res_shr$padj), ]

# Export results
res_df  <- as.data.frame(res_shr)
sig_res <- subset(res_df, !is.na(padj) & padj < 0.05)
write.csv(res_df,  "DE_full_results.csv")
write.csv(sig_res, "DE_significant_padj_lt_0.05.csv")

## ============================================================
## 4) Publication-style figures: MA, Volcano, Heatmap
## ============================================================
# (a) MA plot
pdf("fig_MA.pdf", 6, 5)
plotMA(res_shr, ylim = c(-4,4), main = paste0("DESeq2 MA (shrink=",shrink_type,")"))
dev.off()

# (b) Volcano (EnhancedVolcano if present; else tidy ggplot)
if (have_EV) {
  EV <- getNamespace("EnhancedVolcano")
  pdf("fig_volcano.pdf", 7, 6)
  EV$EnhancedVolcano(res_df,
    lab = rownames(res_df),
    x   = 'log2FoldChange', y = 'padj',
    pCutoff = 0.05, FCcutoff = 1.0,
    title = 'Dex vs Untrt (airway)',
    subtitle = paste0('DESeq2 + ', shrink_type, ' LFC'),
    legendPosition = 'right')
  dev.off()
} else {
  res_plot <- transform(res_df,
                        neglog10padj = -log10(padj),
                        signif = !is.na(padj) & padj < 0.05 & abs(log2FoldChange) >= 1)
  p_volc <- ggplot(res_plot, aes(log2FoldChange, neglog10padj)) +
    geom_point(aes(alpha = signif), size = 1.2) +
    geom_vline(xintercept = c(-1,1), linetype = 2) +
    geom_hline(yintercept = -log10(0.05), linetype = 2) +
    labs(title = 'Dex vs Untrt (airway)',
         subtitle = paste0('DESeq2 + ', shrink_type, ' LFC'),
         x = 'log2 fold-change', y = '-log10(FDR)') +
    theme_minimal()
  ggsave("fig_volcano.pdf", p_volc, width = 7, height = 6)
}

# (c) Heatmap of top 50 DE genes (VST centered)
top50 <- rownames(sig_res)[1:min(50, nrow(sig_res))]
mat   <- assay(vsd)[top50, ]
mat   <- mat - rowMeans(mat)
ann   <- as.data.frame(colData(vsd)[, c("dex","cell")])
pdf("fig_heatmap_top50.pdf", 7, 7)
pheatmap(mat, annotation_col = ann, show_rownames = FALSE,
         main = "Top DE genes (centered VST)")
dev.off()

## ============================================================
## 5) Hallmark GSEA (clusterProfiler preferred; fgsea fallback)
##     - Use Wald statistic from unshrunken results for ranking
##     - Map Ensembl IDs to HGNC symbols for MSigDB gene sets
## ============================================================
# Build ranks from res$stat
ranks <- res$stat
names(ranks) <- rownames(res)   # Ensembl IDs

# Map Ensembl -> SYMBOL using rowData(se_f)
sym_map <- rowData(se_f)$symbol[match(rownames(res), rownames(se_f))]
names(ranks) <- sym_map

# Keep finite, non-missing symbols and sort
ok <- is.finite(ranks) & !is.na(names(ranks))
ranks <- sort(ranks[ok], decreasing = TRUE)

# Hallmark sets (symbol-based)
h_df <- msigdbr(species = "Homo sapiens", category = "H")[, c("gs_name","gene_symbol")]

if (have_CP) {
  library(clusterProfiler)
  if (have_EP) library(enrichplot)
  gsea_res <- GSEA(geneList = ranks, TERM2GENE = h_df, pvalueCutoff = 0.05)
  if (have_EP) {
    pdf("fig_gsea_hallmark_dotplot.pdf", 7, 6)
    print(enrichplot::dotplot(gsea_res, showCategory = 20) + ggtitle("Hallmark GSEA (clusterProfiler)"))
    dev.off()
  }
  write.csv(as.data.frame(gsea_res), "GSEA_hallmark_clusterProfiler_results.csv", row.names = FALSE)
} else if (have_fgsea) {
  library(fgsea)
  pathways <- split(h_df$gene_symbol, h_df$gs_name)
  set.seed(1)
  fg <- fgsea(pathways = pathways, stats = ranks, minSize = 10, maxSize = 500, eps = 1e-10)
  fg <- fg[order(fg$padj), ]
  write.csv(fg, "GSEA_hallmark_fgsea_results.csv", row.names = FALSE)

  # quick dotplot of top 20
  topn <- head(fg, 20)
  # preserve current order on y-axis
  topn$pathway <- factor(topn$pathway, levels = rev(topn$pathway))
  p <- ggplot(topn, aes(x = NES, y = pathway, size = size, color = padj)) +
    geom_point() + scale_color_continuous(trans = "reverse") +
    labs(title = "Hallmark GSEA (fgsea)", x = "NES", y = NULL, color = "FDR", size = "Genes") +
    theme_minimal()
  ggsave("fig_gsea_hallmark_dotplot.pdf", p, width = 7, height = 6)
} else {
  message("Skipping GSEA: neither clusterProfiler nor fgsea available.")
}

## ============================================================
## Done
## ============================================================
message("\nOutputs written:")
message("  CSV:  DE_full_results.csv, DE_significant_padj_lt_0.05.csv")
message("  PDFs: fig_library_sizes.pdf, fig_PCA.pdf, fig_sample_distance_heatmap.pdf,")
message("        fig_MA.pdf, fig_volcano.pdf, fig_heatmap_top50.pdf, fig_gsea_hallmark_dotplot.pdf (if run)")
message("  GSEA: GSEA_hallmark_clusterProfiler_results.csv or GSEA_hallmark_fgsea_results.csv")



