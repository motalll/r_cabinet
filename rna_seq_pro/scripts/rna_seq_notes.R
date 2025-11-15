# high throughput sequencing tells us which genes are active, and how much they are transcribed
# you compaer the normal cell and the mutated cell
# main stages: 1. prepare a sequencing library 2. sequencing 3. data analysis
# quality score
# the draw data: each sequencing read consists of four lines of data:
# 1 the first line which alway stars with @ is a unique ID for the sequence that follows
# 2 bases called for the sequenced fragment
# 3 always a + character
# quality scores for each base in the sequence fragment that is why a typical sequence run with a 400, 000, 000 reads will generate a file containing 1.6 billion lines of data
# now we understand the raw data we need to: 1 filter out the garbage reads 2 align the high quality reads to a genome 3 count the number of reads per gene
#       garbage reads are 1 reads with low quality base calls
#       2 reads tha are clearly artifacts of the chemistry
# we need to normalise the data so that the read counts are not minipulated by the quality of reads
# step one in analysing data is always the same - plot the data
# in plotting because we hav 20000 genes we cannot have an axis for each so we use PCA# NOTE: Principal component analysis
# plotting the data: 1 tells us if we can expect to find interesting differences 2 tells us if we should exclude some sampls from any down stream analysis
# We have edgeR and DESeq2
# we identified intersting genes, now what? 1 if you know what you are looking for, you can see if the experiment validated your hypothesis 2 if you dont kow what you 're looking for, you can see if certain pathway sare enriched in either the normal or mutant gene sets
#  NOTE: Chip-seq stands for chromatin immunoprecipitation combined with high =throughput sequencing
#       it identifies the locations in the genome bound by proteins.

#


# This creates variables that hold the paths we<U+2019>ll reuse.
# Why: hard-coding full paths everywhere is error-prone; one change here updates all reads/writes.


setwd("/home/motall/r_cabinet/rna_seq_pro/scripts/")

proj <- normalizePath("..", mustWork = FALSE) # go one folder "up" from scripts/ (adjust if needed)
data_dir <- file.path(proj, "data") # join folder names the OS-correct way
fig_dir <- file.path(proj, "figures") # where we<U+2019>ll save plots later

# Check they exist <U+2014> returns TRUE/FALSE (good quick sanity check)
dir.exists(data_dir)
dir.exists(fig_dir)





# ------------- fixing view issue and opening plots and view inside the browser ---

#### ---------- WSL browser + plotting bootstrap (with auto-install httpgd) ----------

options(repos = c(CRAN = "https://cloud.r-project.org"), menu.graphics = FALSE)

.ensure_pkgs <- function(pkgs) {
  need <- setdiff(pkgs, rownames(installed.packages()))
  if (length(need)) install.packages(need, quiet = TRUE)
}

# Open URL/path in Windows (supports \\wsl.localhost\…)
.wsl_open <- function(u) {
  if (grepl("^https?://", u)) {
    system2("cmd.exe", c("/C","start","", u), wait = FALSE)
  } else {
    win <- tryCatch(system2("wslpath", c("-w", u), stdout = TRUE),
                    error = function(e) u)
    if (nzchar(Sys.which("explorer.exe"))) {
      system2("explorer.exe", shQuote(win), wait = FALSE)
    } else {
      url <- paste0("file:///", gsub("\\\\", "/", win))
      system2("cmd.exe", c("/C","start","", url), wait = FALSE)
    }
  }
}

# Try to install httpgd if missing: R-universe -> GitHub (source)
.install_httpgd_if_missing <- function() {
  if (requireNamespace("httpgd", quietly = TRUE)) return(invisible(TRUE))

  # prerequisites for GitHub build
  .ensure_pkgs(c("remotes"))

  old_repos <- getOption("repos")
  on.exit(options(repos = old_repos), add = TRUE)

  # 1) R-universe (unigd first, then httpgd)
  options(repos = c(
    nx10      = "https://nx10.r-universe.dev",
    community = "https://community.r-multiverse.org",
    CRAN      = old_repos[["CRAN"]] %||% "https://cloud.r-project.org"
  ))
  try(suppressWarnings(install.packages("unigd")), silent = TRUE)
  try(suppressWarnings(install.packages("httpgd")), silent = TRUE)

  if (!requireNamespace("httpgd", quietly = TRUE)) {
    # 2) GitHub source build
    remotes::install_github("nx10/httpgd", upgrade = "never")
  }

  invisible(requireNamespace("httpgd", quietly = TRUE))
}
`%||%` <- function(a, b) if (is.null(a) || !nzchar(a)) b else a

# Initialize viewers; start httpgd and route all plots to browser
wsl_init <- function(start_httpgd = TRUE, ensure_httpgd = TRUE) {
  options(
    viewer  = function(u) .wsl_open(u),
    browser = "xdg-open"
  )

  if (start_httpgd) {
    if (ensure_httpgd) .install_httpgd_if_missing()
    if (requireNamespace("httpgd", quietly = TRUE)) {
      httpgd::hgd(port = 0)                         # start device/server
      ok <- try(httpgd::hgd_view(), silent = TRUE)  # open a tab if possible
      if (inherits(ok, "try-error")) .wsl_open(httpgd::hgd_url())
      options(device = function(...) httpgd::hgd(port = 0, silent = TRUE))
    }
  }
}

# Browser-based replacement for View() using DT (no pandoc required)
View <- function(x, title = deparse(substitute(x))) {
  if (requireNamespace("DT", quietly = TRUE) &&
      requireNamespace("htmlwidgets", quietly = TRUE)) {
    safe <- gsub("[^A-Za-z0-9_]+", "_", title)
    f <- file.path(tempdir(), paste0("view_", safe, ".html"))
    w <- DT::datatable(x, caption = title,
                       options = list(pageLength = 25, scrollX = TRUE))
    htmlwidgets::saveWidget(w, f, selfcontained = FALSE,
                            libdir = file.path(tempdir(), "view_lib"))
    getOption("viewer")(f)
    invisible(x)
  } else {
    utils::str(utils::head(x, 100))
  }
}

# Ensure lightweight deps for the View() helper
.ensure_pkgs(c("DT","htmlwidgets"))

# Activate everything for this session (auto-installs httpgd if missing)
wsl_init(start_httpgd = TRUE, ensure_httpgd = TRUE)

#### ---------- quick self-test ----------
# Table viewer
test_df <- data.frame(a = 1:5, b = letters[1:5])
View(test_df)
# Plot (should appear in the same browser tab)
plot(1:10, main = "httpgd browser plotting test")
#### ---------- end ----------













































options(repos = c(CRAN = "https://cloud.r-project.org"), menu.graphics = FALSE)


dir.exists(data_dir)
# install.packages("pacman")
library(pacman)
p_load("readxl", "here")


read_tab <- function(fname) {
        read.delim(file.path(data_dir, fname),
                header = TRUE, sep = "\t",
                # ... other arguments
                na.strings = c("", "NA")
        )
}


set_dark_par <- function() {
        par(
                bg = "#1E1E1E", # Dark Grey Background
                col.lab = "gray", # X/Y Axis Labels
                col.axis = "gray", # Tick Labels
                col.main = "gray", # Main Title
                fg = "gray"
        ) # Foreground (Axes/Ticks)
}





#     ------------- XXX: video tuterial follow through -----------------

expr_df <- data.frame(
        Gene = c("Gene1", "Gene2"),
        Mouse1 = c(10, 6),
        Mouse2 = c(11, 4),
        Mouse3 = c(8, 5),
        Mouse4 = c(3, 3),
        Mouse5 = c(1, 2.8),
        Mouse6 = c(2, 1),
        check.names = FALSE
)

View(expr_df)





data.matrix <- matrix(nrow=100, ncol=10)
colnames(data.matrix) <- c(paste("wt", 1:5, sep=""),
        paste("ko", 1:5, sep=""))










# copy and pasted code from the github
## In this example, the data is in a matrix called
## data.matrix
## columns are individual samples (i.e. cells)
## rows are measurements taken for all the samples (i.e. genes)
## Just for the sake of the example, here's some made up data...
data.matrix <- matrix(nrow=100, ncol=10)
colnames(data.matrix) <- c(
  paste("wt", 1:5, sep=""),
  paste("ko", 1:5, sep=""))
rownames(data.matrix) <- paste("gene", 1:100, sep="")
for (i in 1:100) {
  wt.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  ko.values <- rpois(5, lambda=sample(x=10:1000, size=1))
 
  data.matrix[i,] <- c(wt.values, ko.values)
}
head(data.matrix)
dim(data.matrix)
 
pca <- prcomp(t(data.matrix), scale=TRUE) 
 
## plot pc1 and pc2
plot(pca$x[,1], pca$x[,2])
 
## make a scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
 
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
 
## now make a fancy looking plot that shows the PCs and the variation:
library(ggplot2)
 
pca.data <- data.frame(Sample=rownames(pca$x),
  X=pca$x[,1],
  Y=pca$x[,2])
pca.data
 
ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")
 
## get the name of the top 10 measurements (genes) that contribute
## most to pc1.
loading_scores <- pca$rotation[,1]
gene_scores <- abs(loading_scores) ## get the magnitudes
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
top_10_genes <- names(gene_score_ranked[1:10])
 
top_10_genes ## show the names of the top 10 genes
 
pca$rotation[top_10_genes,1] ## show the scores (and +/- sign)
 
#######
##
## NOTE: Everything that follow is just bonus stuff.
## It simply demonstrates how to get the same
## results using "svd()" (Singular Value Decomposition) or using "eigen()"
## (Eigen Decomposition).
##
#######
 
############################################
##
## Now let's do the same thing with svd()
##
## svd() returns three things
## v = the "rotation" that prcomp() returns, this is a matrix of eigenvectors
##     in other words, a matrix of loading scores
## u = this is similar to the "x" that prcomp() returns. In other words,
##     sum(the rotation * the original data), but compressed to the unit vector
##     You can spread it out by multiplying by "d"
## d = this is similar to the "sdev" value that prcomp() returns (and thus
##     related to the eigen values), but not
##     scaled by sample size in an unbiased way (ie. 1/(n-1)).
##     For prcomp(), sdev = sqrt(var) = sqrt(ss(fit)/(n-1))
##     For svd(), d = sqrt(ss(fit))
##
############################################
 
svd.stuff <- svd(scale(t(data.matrix), center=TRUE))
 
## calculate the PCs
svd.data <- data.frame(Sample=colnames(data.matrix),
  X=(svd.stuff$u[,1] * svd.stuff$d[1]),
  Y=(svd.stuff$u[,2] * svd.stuff$d[2]))
svd.data
 
## alternatively, we could compute the PCs with the eigen vectors and the
## original data
svd.pcs <- t(t(svd.stuff$v) %*% t(scale(t(data.matrix), center=TRUE)))
svd.pcs[,1:2] ## the first to principal components
 
svd.df <- ncol(data.matrix) - 1
svd.var <- svd.stuff$d^2 / svd.df
svd.var.per <- round(svd.var/sum(svd.var)*100, 1)
 
ggplot(data=svd.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", svd.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", svd.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("svd(scale(t(data.matrix), center=TRUE)")
 
############################################
##
## Now let's do the same thing with eigen()
##
## eigen() returns two things...
## vectors = eigen vectors (vectors of loading scores)
##           NOTE: pcs = sum(loading scores * values for sample)
## values = eigen values
##
############################################
cov.mat <- cov(scale(t(data.matrix), center=TRUE))
dim(cov.mat)
 
## since the covariance matrix is symmetric, we can tell eigen() to just
## work on the lower triangle with "symmetric=TRUE"
eigen.stuff <- eigen(cov.mat, symmetric=TRUE)
dim(eigen.stuff$vectors)
head(eigen.stuff$vectors[,1:2])
 
eigen.pcs <- t(t(eigen.stuff$vectors) %*% t(scale(t(data.matrix), center=TRUE)))
eigen.pcs[,1:2]
 
eigen.data <- data.frame(Sample=rownames(eigen.pcs),
  X=(-1 * eigen.pcs[,1]), ## eigen() flips the X-axis in this case, so we flip it back
  Y=eigen.pcs[,2]) ## X axis will be PC1, Y axis will be PC2
eigen.data
 
eigen.var.per <- round(eigen.stuff$values/sum(eigen.stuff$values)*100, 1)
 
ggplot(data=eigen.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", eigen.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", eigen.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("eigen on cov(t(data.matrix))")

