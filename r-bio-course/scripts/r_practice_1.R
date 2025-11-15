
# This creates variables that hold the paths we’ll reuse.
# Why: hard-coding full paths everywhere is error-prone; one change here updates all reads/writes.

proj     <- normalizePath("..", mustWork = FALSE)   # go one folder "up" from scripts/ (adjust if needed)
data_dir <- file.path(proj, "data")                 # join folder names the OS-correct way
fig_dir  <- file.path(proj, "figures")              # where we’ll save plots later

# Check they exist — returns TRUE/FALSE (good quick sanity check)
dir.exists(data_dir); dir.exists(fig_dir)








# A helper function we’ll reuse for all .txt files.
# Why: your files are tab-separated; this keeps consistent options across all reads.

read_tab <- function(fname) {
  read.delim(
    file.path(data_dir, fname), # build full path like ".../data/fish.txt"
    header = TRUE,              # first row has column names
    sep = "\t",                 # columns separated by TABs
    stringsAsFactors = FALSE,   # keep text as character, not factor (safer)
    check.names = FALSE,        # don't automatically alter column names
    na.strings = c("", "NA")    # treat empty strings or "NA" as missing
  )
}


fish     <- read_tab("fish.txt")      # expects columns like genotype, length
gardens  <- read_tab("gardens.txt")   # garden, birds (counts)
heights  <- read_tab("heights.txt")   # sex, course, height
paired   <- read_tab("paired.txt")    # Location, Upstream, Downstream
reaction <- read_tab("reaction.txt")  # degree, time







str(fish); head(fish)         # structure: column names/types; first rows
str(gardens); head(gardens)
str(heights); head(heights)
str(paired); head(paired)
str(reaction); head(reaction)



# Helper to turn character columns that *should* be numbers into numeric safely.
# Why: sometimes numbers arrive as text (e.g., "12"); we coerce them and keep NAs if not parseable.
numify <- function(x) {
  if (is.character(x)) suppressWarnings(as.numeric(x)) else x
}

# Helper to trim spaces around text (e.g., "  KO " -> "KO")
trim_chr <- function(x) if (is.character(x)) trimws(x) else x

# Apply trimming to all columns in each data frame:
fish[]     <- lapply(fish, trim_chr)
gardens[]  <- lapply(gardens, trim_chr)
heights[]  <- lapply(heights, trim_chr)
paired[]   <- lapply(paired, trim_chr)
reaction[] <- lapply(reaction, trim_chr)

# Coerce specific numeric columns (each dataset differs):
fish$length        <- numify(fish$length)
gardens$birds      <- numify(gardens$birds)
heights$height     <- numify(heights$height)
paired$Upstream    <- numify(paired$Upstream)
paired$Downstream  <- numify(paired$Downstream)
reaction$time      <- numify(reaction$time)





# Ensure required columns exist (stop if not)
stopifnot(all(c("genotype","length") %in% names(fish)))
stopifnot(all(c("garden","birds")   %in% names(gardens)))
stopifnot(all(c("sex","course","height") %in% names(heights)))
stopifnot(all(c("Location","Upstream","Downstream") %in% names(paired)))
stopifnot(all(c("degree","time") %in% names(reaction)))

# Quick NA counts per column
colSums(is.na(fish))
colSums(is.na(gardens))
colSums(is.na(heights))
colSums(is.na(paired))
colSums(is.na(reaction))

# Quick range/summary checks
range(fish$length, na.rm=TRUE)
summary(paired[c("Upstream","Downstream")])
quantile(reaction$time, c(.01,.5,.99), na.rm=TRUE)




