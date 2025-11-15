
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
