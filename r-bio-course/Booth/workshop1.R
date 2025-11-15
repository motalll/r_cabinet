install.packages("pacman")
library(pacman)
p_load("readxl", "here", "readr")

cranid_df <- read.csv("Downloads/cranid_full.txt")
read.csv2
View(cranid_df)


# I think you need to select a specific colomn rather than trying to mean the whole thing. so now I need to find a way to select a column and a way to read columns
mean(cranid_df)

sd(df)

var(df)

standard_error <- function(x) {
        sd(x) / sqrt(length(x))
}

standard_error(df)




# mean, sd, se,
