proj <- normalizePath("..", mustWork = FALSE) # adjust if your script isn’t under scripts/ data_dir <- file.path(proj, "data") fig_dir <- file.path(proj, "figures") dir.exists(data_dir) dir.exists(fig_dir)

# 1) ELISA-like duplicate readings
set.seed(1)
elisa <- data.frame(
        sample = paste0("S", 1:12),
        group  = rep(c("WT", "KO"), each = 6),
        rep1   = round(rnorm(12, mean = c(0.35, 0.50)[(rep(c("WT", "KO"), each = 6) == "KO") + 1], sd = .05), 3),
        rep2   = round(rnorm(12, mean = c(0.35, 0.50)[(rep(c("WT", "KO"), each = 6) == "KO") + 1], sd = .05), 3)
)
write.csv(elisa, file.path(data_dir, "elisa.csv"), row.names = FALSE)

# 2) Enzyme kinetics (Michaelis–Menten-ish)
S <- rep(c(0.1, 0.2, 0.5, 1, 2, 5, 10), each = 3)
true_Vmax <- 1.2
true_Km <- 1.5
v <- (true_Vmax * S) / (true_Km + S) + rnorm(length(S), 0, 0.03)
kin <- data.frame(S = S, v = round(v, 3))
write.csv(kin, file.path(data_dir, "enzyme_kinetics.csv"), row.names = FALSE)


#-------------------------------------------------------------



x <- 1:219 # integer sequence - and you do not need the concatinate command to make it work
y <- c(2.2, 1.3) # numeric (double)
z <- c(TRUE, FALSE)
s <- c("East", "Wind")

s # Youc an see what an object hold like this

typeof(s) # Self explanatory


# Vector creation helpers

a <- seq(c(0, 1, by = 0.3))
b <- rep(c("East", "Wind"), times = 9)
c <- rep(c("East", "Wind"), each = 9)

a
b
c
