#### ---------- WSL browser + plotting bootstrap ----------

# 0) CRAN + no GUI menus (safe on WSL)
options(repos = c(CRAN = "https://cloud.r-project.org"), menu.graphics = FALSE)

# 1) Helper: install CRAN packages if missing (no prompts)
.ensure_pkgs <- function(pkgs) {
        need <- setdiff(pkgs, rownames(installed.packages()))
        if (length(need)) install.packages(need, quiet = TRUE)
}

# 2) Open a URL or Linux path in Windows (handles \\wsl.localhost\<U+2026>)
.wsl_open <- function(u) {
        if (grepl("^https?://", u)) {
                system2("cmd.exe", c("/C", "start", "", u), wait = FALSE)
        } else {
                win <- tryCatch(system2("wslpath", c("-w", u), stdout = TRUE),
                        error = function(e) u
                )
                if (nzchar(Sys.which("explorer.exe"))) {
                        system2("explorer.exe", shQuote(win), wait = FALSE)
                } else {
                        url <- paste0("file:///", gsub("\\\\", "/", win))
                        system2("cmd.exe", c("/C", "start", "", url), wait = FALSE)
                }
        }
}

# 3) One-shot initializer: set viewers; start httpgd if present
wsl_init <- function(start_httpgd = TRUE) {
        options(
                viewer  = function(u) .wsl_open(u),
                browser = "xdg-open"
        )
        if (start_httpgd && requireNamespace("httpgd", quietly = TRUE)) {
                httpgd::hgd(port = 0) # start device/server
                ok <- try(httpgd::hgd_view(), silent = TRUE) # open a tab if possible
                if (inherits(ok, "try-error")) .wsl_open(httpgd::hgd_url())
                options(device = function(...) httpgd::hgd(port = 0, silent = TRUE))
        }
}

# 4) Browser-based View() using DT (falls back to head/str if DT missing)
View <- function(x, title = deparse(substitute(x))) {
        if (requireNamespace("DT", quietly = TRUE) &&
                requireNamespace("htmlwidgets", quietly = TRUE)) {
                safe <- gsub("[^A-Za-z0-9_]+", "_", title)
                f <- file.path(tempdir(), paste0("view_", safe, ".html"))
                w <- DT::datatable(x,
                        caption = title,
                        options = list(pageLength = 25, scrollX = TRUE)
                )
                htmlwidgets::saveWidget(w, f,
                        selfcontained = FALSE,
                        libdir = file.path(tempdir(), "view_lib")
                )
                getOption("viewer")(f)
                invisible(x)
        } else {
                utils::str(utils::head(x, 100))
        }
}

# 5) Ensure the lightweight deps for View()
.ensure_pkgs(c("DT", "htmlwidgets"))

# 6) Activate everything for this session
wsl_init()
#### ---------- end bootstrap ----------


#### ---------- Quick self-test (safe to leave in or remove) ----------
# a) Data viewer test (uses your View() replacement)
if (!exists("cranid_df")) {
        cranid_df <- data.frame(
                Gene = c("Gene1", "Gene2"),
                Mouse1 = c(10, 6),
                Mouse2 = c(11, 4),
                Mouse3 = c(8, 5),
                Mouse4 = c(3, 3),
                Mouse5 = c(1, 2.8),
                Mouse6 = c(2, 1),
                check.names = FALSE
        )
}
View(cranid_df) # should open a browser tab with an interactive table

# b) Plot test <U+2014> if httpgd is installed, this appears in the same browser tab
plot(1:10, main = "httpgd browser plotting test")
#### ---------- end self-test ----------
