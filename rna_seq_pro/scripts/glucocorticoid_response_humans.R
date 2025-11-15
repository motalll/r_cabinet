
## ============================================================
## RNA-seq mini-project (airway dexamethasone) – final script
## QC → DESeq2 → LFC shrink → MA/Volcano/Heatmap → Hallmark GSEA
## Outputs: CSVs + PDFs in working directory
## ============================================================

## A) Use user library (avoid /usr write issues)
if (!nzchar(Sys.getenv("R_LIBS_USER"))) {
  Sys.setenv(
    R_LIBS_USER = file.path(
      Sys.getenv("HOME"), "R",
      paste0(R.version$platform, "-library"),
      paste(R.version$major, R.version$minor, sep = ".")
    )
  )
}
if (!dir.exists(Sys.getenv("R_LIBS_USER"))) dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)
.libPaths(unique(c(Sys.getenv("R_LIBS_USER"), .libPaths())))

## B) Repos & installers
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager", quiet = TRUE)
options(repos = BiocManager::repositories())   # Bioc 3.22 ↔ R 4.5
message("R: ", R.version.string, " | Bioc: ", as.character(BiocManager::version()))
try(Sys.setlocale("LC_ALL", "C.UTF-8"), silent = TRUE)  # quiet locale warnings on minimal systems

.install_if_missing_bioc <- function(pkgs) {
  missing <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing)) BiocManager::install(missing, ask = FALSE, update = FALSE, quiet = TRUE)
}
.install_if_missing_cran <- function(pkgs) {
  missing <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing)) install.packages(missing, quiet = TRUE)
}

## C) Install needed packages
.install_if_missing_bioc(c("DESeq2","airway","org.Hs.eg.db","apeglm","clusterProfiler","enrichplot","fgsea"))
.install_if_missing_cran(c("pheatmap","ggplot2","dplyr","readr","msigdbr"))

## D) Load libs (optional ones checked later)
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
have_CP     <- requireNamespace("clusterProfiler", quietly = TRUE)
have_EP     <- requireNamespace("enrichplot", quietly = TRUE)
have_fgsea  <- requireNamespace("fgsea", quietly = TRUE)

## ============================================================
## 1) Load dataset (no manual downloads needed)
## ============================================================
data("airway")     # creates 'airway'
se <- airway       # SummarizedExperiment with counts + metadata
print(se); print(colData(se)[, c("dex","cell")]); print(head(assay(se)))

## ============================================================
## 2) Prep & QC
## ============================================================
# Reference level so log2FC = trt vs untrt
colData(se)$dex <- relevel(colData(se)$dex, "untrt")

# Light prefilter: keep genes with >=10 counts in at least 2 samples
keep <- rowSums(assay(se) >= 10) >= 2
se_f <- se[keep, ]

# Paired design (block by donor 'cell')
dds <- DESeqDataSet(se_f, design = ~ cell + dex)

# Library sizes
libsize <- colSums(counts(dds))
pdf("fig_library_sizes.pdf", 6, 4)
barplot(libsize/1e6, ylab="Millions of reads", xlab="Sample", las=2, main="Library sizes")
dev.off()

# VST for PCA/heatmap
vsd <- vst(dds, blind = TRUE)
p_pca <- plotPCA(vsd, intgroup = c("dex","cell")) + ggtitle("PCA (VST)")
ggsave("fig_PCA.pdf", p_pca, width = 6, height = 5)

dists <- dist(t(assay(vsd)))
pdf("fig_sample_distance_heatmap.pdf", 6.5, 5.5)
pheatmap(as.matrix(dists),
         annotation_col = as.data.frame(colData(vsd)[, c("dex","cell")]),
         main="Sample distance heatmap")
dev.off()

## ============================================================
## 3) DESeq2 fit and LFC shrinkage
## ============================================================
dds <- DESeq(dds)
res <- results(dds, contrast = c("dex","trt","untrt"))
summary(res)

shrink_type <- if (have_apeglm) "apeglm" else "normal"
res_shr <- lfcShrink(dds, coef = "dex_trt_vs_untrt", type = shrink_type)
res_shr <- res_shr[order(res_shr$padj), ]

res_df  <- as.data.frame(res_shr)
sig_res <- subset(res_df, !is.na(padj) & padj < 0.05)
write.csv(res_df,  "DE_full_results.csv")
write.csv(sig_res, "DE_significant_padj_lt_0.05.csv")

## ============================================================
## 4) Figures: MA, Volcano (ggplot), Heatmap
## ============================================================
# MA
pdf("fig_MA.pdf", 6, 5)
plotMA(res_shr, ylim = c(-4,4), main = paste0("DESeq2 MA (shrink=",shrink_type,")"))
dev.off()

# Volcano (no alpha-on-discrete warning)
res_plot <- transform(res_df, neglog10padj = -log10(padj),
                      sig = !is.na(padj) & padj < 0.05 & abs(log2FoldChange) >= 1)
p_volc <- ggplot(res_plot, aes(log2FoldChange, neglog10padj)) +
  geom_point(aes(color = sig), size = 1.2) +
  scale_color_manual(values = c("FALSE" = "grey55", "TRUE" = "black")) +
  geom_vline(xintercept = c(-1,1), linetype = 2) +
  geom_hline(yintercept = -log10(0.05), linetype = 2) +
  labs(title = 'Dex vs Untrt (airway)',
       subtitle = paste0('DESeq2 + ', shrink_type, ' LFC'),
       x = 'log2 fold-change', y = '-log10(FDR)', color = "Significant") +
  theme_minimal()
ggsave("fig_volcano.pdf", p_volc, width = 7, height = 6)

# Heatmap of top 50 (VST, centered)
top50 <- rownames(sig_res)[1:min(50, nrow(sig_res))]
mat   <- assay(vsd)[top50, ]
mat   <- mat - rowMeans(mat)
ann   <- as.data.frame(colData(vsd)[, c("dex","cell")])
pdf("fig_heatmap_top50.pdf", 7, 7)
pheatmap(mat, annotation_col = ann, show_rownames = FALSE,
         main = "Top DE genes (centered VST)")
dev.off()

## ============================================================
## 5) Hallmark GSEA (unique, symbol-named ranks REQUIRED)
##    - Use Wald stat from unshrunken results
##    - Map Ensembl → SYMBOL and deduplicate (keep max |stat|)
## ============================================================
# Build ranks (Wald statistic)
stat_vec <- res$stat
ens_ids  <- rownames(res)

# Map Ensembl → SYMBOL using rowData(se_f)
sym_map <- rowData(se_f)$symbol[match(ens_ids, rownames(se_f))]

# Assemble and clean for uniqueness
ranks_df <- data.frame(symbol = sym_map, stat = as.numeric(stat_vec), stringsAsFactors = FALSE)
ranks_df <- ranks_df[!is.na(ranks_df$symbol) & is.finite(ranks_df$stat), ]

# Deduplicate symbols: keep the entry with the largest absolute statistic
ranks_df <- ranks_df |>
  dplyr::group_by(symbol) |>
  dplyr::slice_max(order_by = abs(stat), n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::arrange(dplyr::desc(stat))

ranks <- ranks_df$stat
names(ranks) <- ranks_df$symbol
stopifnot(length(ranks) == length(unique(names(ranks))))  # guarantee uniqueness

# Hallmark gene sets (new API uses 'collection')
h_df <- msigdbr(species = "Homo sapiens", collection = "H")[, c("gs_name","gene_symbol")]

if (have_CP) {
  library(clusterProfiler)
  if (have_EP) library(enrichplot)
  gsea_res <- GSEA(geneList = ranks, TERM2GENE = h_df, pvalueCutoff = 0.05)
  write.csv(as.data.frame(gsea_res), "GSEA_hallmark_clusterProfiler_results.csv", row.names = FALSE)
  if (have_EP) {
    pdf("fig_gsea_hallmark_dotplot.pdf", 7, 6)
    print(enrichplot::dotplot(gsea_res, showCategory = 20) + ggtitle("Hallmark GSEA (clusterProfiler)"))
    dev.off()
  }
} else if (have_fgsea) {
  library(fgsea)
  pathways <- split(h_df$gene_symbol, h_df$gs_name)
  set.seed(1)
  fg <- fgsea(pathways = pathways, stats = ranks, minSize = 10, maxSize = 500, eps = 1e-10)
  fg <- fg[order(fg$padj), ]
  write.csv(fg, "GSEA_hallmark_fgsea_results.csv", row.names = FALSE)
  # quick dotplot
  topn <- head(fg, 20); topn$pathway <- factor(topn$pathway, levels = rev(topn$pathway))
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
message("\nOutputs:")
message("  CSV:  DE_full_results.csv, DE_significant_padj_lt_0.05.csv")
message("        GSEA_hallmark_clusterProfiler_results.csv or GSEA_hallmark_fgsea_results.csv")
message("  PDF:  fig_library_sizes.pdf, fig_PCA.pdf, fig_sample_distance_heatmap.pdf,")
message("        fig_MA.pdf, fig_volcano.pdf, fig_heatmap_top50.pdf, fig_gsea_hallmark_dotplot.pdf (if run)")




