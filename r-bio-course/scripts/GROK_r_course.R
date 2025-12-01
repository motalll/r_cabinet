# This creates variables that hold the paths we’ll reuse.
# Why: hard-coding full paths everywhere is error-prone; one change here updates all reads/writes.


setwd("/home/motall/h_projects/r_cabinet/r-bio-course/scripts") # for the t470s
setwd("/home/motall/r_cabinet/r-bio-course/scripts/")  # for the wsl


proj <- normalizePath("..", mustWork = FALSE) # go one folder "up" from scripts/ (adjust if needed)
data_dir <- file.path(proj, "data") # join folder names the OS-correct way
fig_dir <- file.path(proj, "figures") # where we’ll save plots later

# Check they exist — returns TRUE/FALSE (good quick sanity check)
dir.exists(data_dir)
dir.exists(fig_dir)





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


library(pacman)
p_load("readxl", "here", "plotly", "htmlwidgets", "DT","htmlwidgets","reactable","httpgd","BiocManager")




read_tab <- function(fname) {
  read.delim(file.path(data_dir, fname),
    header = TRUE, sep = "\t",
    # ... other arguments
    na.strings = c("", "NA")
  )
}



set_dark_par <- function() {
  par(bg = "#1E1E1E",         # Dark Grey Background
      col.lab = "gray",      # X/Y Axis Labels
      col.axis = "gray",     # Tick Labels
      col.main = "gray",     # Main Title
      fg = "gray")           # Foreground (Axes/Ticks)
}








# ---------------- TODO: Run the top at start

# you need to clean this text and add it to the rnconfig so it would use the browser eevery time if you want



# ------------------------------------------- XXX: Start of Actual Code ----------------------------


birds_B <- c(5, 5, 6, 7, 4, 4, 3, 5, 6, 5)
birds_A <- c(6, 6, 7, 8, 3, 3, 6, 6, 4, 5)

birds_A
birds_B

birds_A + birds_B

birds_A * 2


birds_team <- birds_B[c(3:7)]
birds_team

sum(birds_B, na.rm = TRUE)

mean(birds_A, na.rm = TRUE)

fish_path <- file.path(data_dir, "fish.txt")
fish_data <- read.table(fish_path, header = TRUE, sep = "\t")

# one line way of doing it
fish_data <- read.table(file.path(data_dir, "fish.txt"), header = TRUE, sep = "\t")

# even better way of doing it is to create a funciton and just give it the name of the file



fish_data <- read_tab("fish.txt")


fish_data_2 <- read_tab("fish.txt")

View(fish_data)
head(fish_data)
attach(fish_data)
genotype

gardens_data <- read_tab("gardens.txt")
View(gardens_data)
str(gardens_data)
summary(gardens_data)

gardens_data$birds

gardens_data[1:5, ]
gardens_data[1:5, "garden"]

# useful technique
nrow(subset(gardens_data, garden == "A"))

subset(gardens_data, garden == "A")



fish_data_2[fish_data$lengrh > 8, ]
subset(fish_data, length > 2 & genotype == "A")


# if median < mean, the data might be skewed to the right
median(fish_data$length, na.rm = TRUE)
summary(fish_data$length)
attach(fish_data)
summary(length)

#---------------------- Variance -----------

# Variance measures how spread out numbers are from their mean. In experimental design, high variance means measurements differ widely (e.g., fish lengths vary a lot), which can affect conclusions.

# In your course, variance helps assess consistency of experimental results (e.g., are fish lengths stable within genotypes?).


var(fish_data$length)
var(subset(fish_data, genotype == "A")$length)
# var(subset(fish_data, genotype == "A"), length) does not work

any(is.na(fish_data$length))


#---------------- standard deviation -----------------------

sd(subset(fish_data, genotype == "B")$length, na.rm = TRUE)


hist(fish_data$length)
hist(fish_data$length, breaks = 50)
hist(fish_data$length, breaks = 50, main = "Fish Length, Dear Sir")
hist(fish_data$length, breaks = 50, main = "Fish Lengths, Dear Sir", xlab = "Length (cm)")
hist(fish_data$length, breaks = 50, main = "Fish Lengths, Dear Sir", xlab = "Length (cm)", col = "lightblue")

# Not working for now because of the image viewer
par(bg = "#1E1E1E", col.lab = "white", col.axis = "white", col.main = "white", fg = "white")
hist(fish_data$length, breaks = 50, main = "Fish Lengths", xlab = "Length (cm)", col = "#00BFFF", border = "white")

# dev.off() resets parameters




hist(subset(fish_data, genotype == "A" | fish_data$length != 8)$length) # Not entirely correct
hist(subset(fish_data, genotype == "A" | length != 8)$length, main = "Fish Lenghts", xlab = "Length (cm)") # better


which(fish_data$length > 5)
hist(which(fish_data$genotype == "A")) # strange response but why? Could it be because it returns indices


# ---------------------- Histograms and saving with png()


png(file.path(fig_dir, "my_plot_name2.png"))


boxplot(length ~ genotype, data = fish_data, main = "Fish Lengths by Genotype", xlab = "Genotype", ylab = "Length (cm)", col = c("lightblue", "lightgreen"))




# dark mode works when the whole code is ran through leader aa for one of the hist plots alone but then the viewer stops functioning for an unknown reason
set_dark_par()
boxplot(length ~ genotype, data = fish_data, main = "Fish Lengths by Genotype", xlab = "Genotype", ylab = "Length (cm)", col = c("lightblue", "lightgreen", border="gray"))




quantile(fish_data$length)
IQR(fish_data$length)

table(fish_data$genotype) #?????
table(fish_data$length)

#---------------------- T-test and P-value -----------------------

t.test(length ~ genotype, data=fish_data) # This way is better even though both give the same output; the one below is less flexible
t.test(subset(fish_data, genotype=="A")$length, subset(fish_data, genotype=="B")$length)

#------------------------ Merge --------------



heights_data <- read_tab("heights.txt")
head(heights_data)

View(heights_data[50:60,  ])

heights_female <- subset(heights_data, sex=="Female")
head(heights_female)    # leader o prints the results here, commented out
#      sex       course height
# 1 Female life science    138
# 2 Female life science    154
# 3 Female life science    163
# 4 Female life science    164
# 5 Female life science    176
# 6 Female life science    174
heights_life_sciences <- subset(heights_data, course=="life science")
# [1] sex    course height
# <0 rows> (or 0-length row.names)
head(heights_life_sciences)
#      sex       course height
# 1 Female life science    138
# 2 Female life science    154
# 3 Female life science    163
# 4 Female life science    164
# 5 Female life science    176
# 6 Female life science    174
merged_heights_data <- merge(heights_female, heights_life_sciences, by="height")
View(merged_heights_data)




# ---------------- Simple linear regression -----------------

reaction_df <- read_tab("reaction.txt")
head(reaction_df)
View(reaction_df)
lm(reaction_df$time)

# The above dataset is not suitable for regression so I added more datasets
brain_body_df <- read_tab("brain_body.txt")
head(brain_body_df)

lm(brain ~ body, data=brain_body_df)
plot(lm(brain ~ body, data=brain_body_df)) # switch to the console and just hit enter as it is generating four plots

brain_body_model <- lm(brain ~ body, data=brain_body_df)
plot(brain_body_model)



# --------------- normality testing

shapiro.test(fish_data$length) # the p value should be more than 0.05 for H0 to be accepted that the data is no different than normal distributaion
hist(fish_data$length)
qqnorm(fish_data$length)
qqline(fish_data$length) # throws an error


# ---------- in class ------------

rnorm(30, 60, 10)
x <- rnorm(300, 60, 10)
set_dark_par()
plot(x, col="yellow")
mean(x)
abline(mean(x), 0, col="red", lwd=2, lty=2)
dev <- x - mean(x)
hist(dev, breaks=30)


#------------- bar plot ------------


barplot(table(gardens_data$garden))
#tage 39: Conditional Statements - If-Else
#If-else for decision-making in code.

#What is if-else? Executes code based on condition (e.g., if p < 0.05, print "significant").
#Command: if() else. Type p <- 0.03; if(p < 0.05) { print("Significant") } else { print("Not") }. Explanation: Checks condition. Why needed? Automates reporting.
#Tip/Trick: Vectorize with ifelse(p < 0.05, "Sig", "Not"). Better way: In functions for checks.


#---------- paired t-test; difference between paired observations ---------

paired_df <- read_tab("paired.txt")
t.test(Upstream ~ Downstream, data=paired_df, paired=TRUE) # this is confusing to the compiler because you are not using the vector method to refer to the paired data insted you are using this x ~ y method which is a hallmark of unpaired data, so instead:

t.test(paired_df$Upstream, paired_df$Downstream, paired=TRUE)


# the reason that you cannot just use the t-test two by two for all the data, apart from convience, is the fact that you are adding the likelyhood of false positive which is 0.05 for every comparison which adds up



# --------- subsetting with factors ---------

fish_data_2$genotype <- factor(fish_data$genotype)
levels(fish_data$genotype)



# --------- scatter plots ---------


plot(heights_data$heights, reaction_df$time) # will not work because they have different length and do not have a common ID


set_dark_par()
plot(brain_body_df$brain, brain_body_df$body, main="Brain vs. Body", xlib="brain maybe???", ylib="body maybe???", col="yellow")  # there was a way where you got rid of the outlier in the data and expanded the important portion - also could use a regression line 

# -------- in class W7 ------- statistical modeling --------


  cricket_df <- read_tab("cricket_ancova.txt")
cor.test(cricket_df$pulses, cricket_df$pulses) # learn what the command does 
cricket_df_model <- lm(cricket_df$pulses ~ cricket_df$temp, data=cricket_df)

set_dark_par()
plot(cricket_df_model)  # what each plot means, leverage, 

abline(-103.4, 2.36, lwd=2, lty=2, col="yellow")

lm(formula = cricket_df$pulses - cricket_df$temp)



cricket_df_model <- lm(cricket_df$pulses, cricket_df$pulses +sex)

# what R does when you have two catagories that one is a string and one is an integer => it associates 0 or 1 to them => 


interaction_df <- read_tab("Interactions.txt")

table(interaction_df$Food, interaction_df$Condiment)

interaction_df_mdl <- lm(interaction_df$Enjoyment~interaction_df$Food+interaction_df$Condiment, data = interaction_df)   # throws an error


boxplot(interaction_df$Enjoyment, interaction_df$Food+)


# parsimony ocams racor look into it


  # if the mean is zero,you can get rid of the interactions altogether


head(interaction_df)

# model checking


# ----------------------- functions -----------


run_baterry_of_tests <- function(x) {
  c(
    median(x),
    mean(x),
    var(x),
    sd(x),
    t.test(x),
    shapiro.test(x),
    summary(aov(x)),
    TukeyHSD(aov(x))

  )} # this would not work as it is supposed to because the c() can only handle simple vectors and some of these tesets return complex objects, so using list much better suited, but even then a few of these tests require a specific syntanx such as x ~ y



run_battery_of_tests_updated <- function(x) {

list(

  mean_holder <- mean(x),
    median_holder <- median(x)
  )


}

run_battery_of_tests_updated(fish_data$length)







# ---------------------------------------- Anova -----------------
garden_df <- read_tab("gardens.txt")
shapiro.test(garden_df$birds)

garden_aov_modl <- aov(birds ~ garden, data=garden_df)
summary(garden_aov_modl)
TukeyHSD(garden_aov_modl)


#id you do not use the summary command and just call the object, you can a lot of data which might be useful
garden_aov_modl
summary.aov(birds ~ garden, data=garden_df) #??? throwa an error

cranid_df <- read_tab("cranid_full.txt")
shapiro.test(cranid_df$PAC)


worms_df <- read_tab("worms.txt")
head(worms_df)
shapiro.test(worms_df$Area)
worms_anova_model <- aov(Worm.density ~ Vegetation, data=worms_df) # a progression of t-test where t-test compares two groups, this can do more than that. It spits out p-value and the F-value. It tells you whether there is a signifanct differenct among the means which it measures throught examining the varience, but what it does not tell you is which two groups exhibit a difference for that you need to run post-hoc-tests like TukeyHSD()
summary(worms_anova_model)

TukeyHSD(worms_anova_model)

#----------------------------------- correlation - measuring linear relationships ------------

brain_body_df <- read_tab("brain_body.txt")

cor(brain_body_df$brain, brain_body_df$body, use="complete.obs")
plot(brain_body_df$brain, brain_body_df$body) # the bare plot() is used for visuilisation



#------------------------------  linear regression diognostics -------------------


regression_df <- read_tab("regression_example.txt")

model <- lm(y ~ x, data=regression_df)
plot(model) # no curve in the residuals plot means linearity


resid(model)
cooks.distance(model)

# ----------- Multiple linear regression ----------------


multi_model <- lm(Worm.density ~ Area + Slope + Soil.pH, data=worms_df)
summary(multi_model)

cor(worms_df[, c("Area", "Slope", "Soil.pH")])
vif(multi_model) # does not exist as a function in base R

summary(step(multi_model))

# ------------- randomisation ----------

# 1. Set the fixed starting point for the random number generator
set.seed(123)

just_some_randomness <- sample(c("head", "tail"), size= 2000, replace=TRUE)
head(just_some_randomness)
View(just_some_randomness)



# ------------- replication --------

replicated_means <- replicate(1000, sample(fish_data$length, 50))
hist(replicated_means)

# -------------------- handling missing data (NA)

titanic_unclean <- read_tab("titanic.txt")

sum(is.na(titanic_unclean$Age))

titanic_df_clean <- na.omit(titanic_unclean$Age)
sum(is.na(titanic_df_clean$Age))

titanic_unclean$Age[is.na(titanic_unclean$Age)] <- mean(titanic_unclean$Age, na.rm=TRUE) # Mean imputation - be careful using it




# ----------- in class W6 -----

cranid_df

head(cranid_df)

cranid_df_aov_model <- aov(GOL ~ PopNum + Sex, data = cranid_df)
summary(cranid_df_aov_model)

TukeyHSD(cranid_df_aov_model)

x <- c(5, 4, 4, 6, 8)
y <- c(8, 9, 10, 13, 15)


set_dark_par()
plot(y ~ x, pch=16, col="firebrick", ylim=c(0, 16), xlim=c(0, 10))
lm(formula = y ~ x)


plot(lm(formula = y ~ x))


summary.aov(model) # which model ????



cov(x,y)
cor(y, x)


brain_body_df

plot(brain_body_df$brain ~ brain_body_df$body, col= "yellow", pch=16)
hist(brain_body_df$body, breaks=20)
hist(log(brain_body_df$brain), breaks=20)
plot(log(brain_body_df$brain), breaks=20)
plot(lm(log(brain_body_df$brain)), breaks=20)



#-------------- releveling -----------
titanic_df <- read_tab("titanic.txt")

titanic_df$PClass <- factor(titanic_df$PClass)
titanic_df$PClass <- relevel(titanic_df$PClass, ref="3rd")


#-------------- Conditional statements -----------


p <- 0.01; if(p < 0.05) { print("The H0 has been rejected.") } else {print("H0 has been accepted.") }


# ---------------- apply() family - efficient looping ---------

cranid_df <- read_tab("cranid_full.txt")
apply(cranid_df[, 3:9], 2, mean, na.rm=TRUE)



# ------------ Chi Square Test


titanic_df <- read_tab("titanic.txt")

 (chisq.test(table(titanic_df$PClass, titanic_df$Survived)))


chi-square_result <- chisq.test(table(titanic_df$PClass, titanic_df$Survived))
str(chi_square_result)





#---------------------------------------- SED Revision ----------

a <- scan()
1
2
3
attach()
detach()
abs(-2.5)
16 %/% 3 # modulo operation; how many 3s goes into it
16 %% 3 # gives you the remainder
#--------
round(2.333, 4) # tellinf it how many significant figures
floor(6.33)
ceiling(6.33)

1:7
seq(1, 7, 1)
rep(c("a", "b"), 5)

x <- c(1, 2, 3)
y <- c(4, 5, 6)
z <- c(x, y)

rnorm(40, mean=10, sd=1)

# -------- W3 workshop ---------

cranid_df <- read_tab("cranid_full.txt")
head(cranid_df)

summary(cranid_df)


cranid_sex_table <- table(cranid_df$pop, cranid_df$sex)
cranid_sex_table

# trouble shooting why table gives 0x0 => the name of the colomns was different => the length command is a good way of seeing whether you have the name right and whether the length of the colomns is correct; if not, you can clean it with clean_vector <- na.omit(vector1)

length(cranid_df$PopNum)
length(cranid_df$Sex)

table(cranid_df$PopNum, cranid_df$Sex)


mean(cranid_df$Gol)
length(cranid_df$Gol)
mean(cranid_df$GOL)

a <- (cranid_df$GOL - mean(cranid_df$GOL))^2 # or you can use (whatever)^2
a
a < sum(a)
a
b <- a / length(cranid_df$GOL)
b <- na.omit(b)
b

clead_gol <- na.omit(cranid_df$GOL)
sqrt( sum((clead_gol - mean(clead_gol))^2)/ length(clead_gol - 1)) # a few things to nore here, first try to separate only the colomn you want to work with into a separate variable,2. try to clean it by getting rid of the null values so that they wont through an error. 3. you are sqrt when you want to calc the standard deviation because standard devation is the square root of variance. but for calcualting the variance you need to take to the power of two ^2 to get rid of the negatives. 4. you take to the power of two before summing so in other words you square inside the paranthesis for sum not squaring the sum

sd(cranid_df$GOL)

# revising

gol <- na.omit(cranid_df$GOL)
gol <- (gol - mean(gol))^2
gol <- sum(gol)
gol <- gol / length(cranid_df$GOL) - 1 # our varience
gol <- gol^1/2
gol


# the above gives you a higher number because you are not dividing by -1, you are substracting the end result by -1


clean_gol <- na.omit(cranid_df$GOL)
gol_su <- (clean_gol - mean(clean_gol))
gol_sq <- gol_su^2
gol_sum_sq <- sum(gol_sq)
gol_var <- gol_sum_sq / (length(clean_gol) - 1)
gol_var
gol_sd <- gol_var^1/2
gol_sd



# the above code is incorrect becase R things you are taking to the power of 1 and dividing by 2, so it should be inside paranthesis


# practice 2
head(cranid_df)

gol_cl <- na.omit(cranid_df$GOL)
gol_sub <- (gol_cl - mean(gol_cl))
gol_sqr <- gol_sub^2
gol_sum <- sum(gol_sqr)
gol_var <- (gol_sum / (length(gol_cl) - 1))
gol_var
gol_sd <- gol_var^(1/2)
gol_sd


# or more neatly
(sqrt( sum((gol_cl - mean(gol_cl))^2 / (length(gol_cl) - 1))))


# ------ standard error of means, margin of error, tscore, confidence interval --------

# to get the confidence interval multiply the standard error by t-score

# standard error of means
gol_se <- gol_sd / sqrt(length(gol_cl))

gol_se <- gol_sd / sqrt(598) # do not square the clean data
# [1] 0.3490629
qt(0.975, 597)

gol_se * qt(0.975, 597)


View(cranid_df)

DT::datatable(cranid_df, options = list(pageLength = 25, scrollX = TRUE))




set_dark_par()
# use names = unique() if you want your data to be labled
boxplot(cranid_df$GOL~cranid_df$PopNum, names= unique(cranid_df$Population), xlab="Populations", ylab="Gol (mm)", col="lightblue")

# if you want to chop it up based on the population use tapply() to use it put the name of the variable then the index you want to break it up by and then the operation

xbar <- tapply(cranid_df$GOL, cranid_df$Population, mean)
#     AINU  ANDAMAN   ANYANG  ARIKARA   ATAYAL 
# 185.0116 164.4857 181.0000 176.2029 173.6170 
# AUSTRALI     BERG   BURIAT  BUSHMAN    DOGON 
# 185.8416 175.5596 176.7798 174.7444 173.5657 
# EASTER I    EGYPT   ESKIMO     GUAM   HAINAN 
# 187.2442 180.8288 184.5556 180.7018 173.7470 
#   MOKAPU  MORIORI  N JAPAN  N MAORI    NORSE 
# 180.9600 183.5556 178.6782 186.6000 184.2273 
#     PERU PHILLIPI  S JAPAN  S MAORI SANTA CR 
# 173.4818 176.9200 177.3297 187.1000 176.0000 
# TASMANIA    TEITA    TOLAI  ZALAVAR     ZULU 
# 181.7126 178.4096 179.1364 181.1429 182.3168 

n <- tapply(cranid_df$GOL, cranid_df$Population, length)

s <- tapply(cranid_df$GOL, cranid_df$Population, sd)

se <- s / sqrt(n)
t <- qt(0.975, n-1)
ci <- se * t

#upper and lower limit
ul <- xbar + ci
ll <- xbar - ci


# if you want to break it down by more than one catagory, for instance, adding sex
xbar_sex <- tapply(cranid_df$GOL, list(cranid_df$Population, cranid_df$Sex), mean)
n_sex <- tapply(cranid_df$GOL, list(cranid_df$Population, cranid_df$Sex), length)
s_sex <- tapply(cranid_df$GOL, list(cranid_df$Population, cranid_df$Sex), sd)

ci_sex <- s_sex / sqrt(n_sex)
t_sex <- qt(0.975, n_sex - 1)

xbar_sex - t_sex

boxplot(cranid_df$GOL~list(cranid_df$Population, cranid_df$Sex), names = unique(cranid_df$Population), xlab="Population", ylab="Gol (cm)", col="yellow")

# better way of doing it
set_dark_par()
boxplot(GOL ~ Sex + Population, data= cranid_df, col=c("lightpink", "lightblue"), xlab= "Population Based On Sex", ylab="Gol (mm)", main="Cranial Length by Population and Sex", las = 3)


# ------------------------------- week 3 exercises ---------------

week3 <- data.frame(patient = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  bp = c(96, 119, 119, 108, 126, 128, 110, 105, 94))
week3 
#   patient  bp
# 1       1  96
# 2       2 119
# 3       3 119
# 4       4 108
# 5       5 126
# 6       6 128
# 7       7 110
# 8       8 105
# 9       9  94

median(week3$bp)
# not straightforward to get the mode of the data

# this is not meaningful
boxplot(bp ~ patient, data= week3, las = 3, col="lightblue")

#niether is this
boxplot(week3, las = 3, col="lightblue")

# Do this instead
boxplot(week3$bp, las = 3, col="lightblue")

IQR(week3$bp) # if you want the interquartile range
quantile(week3$bp, probes = c(0.25, 0.5, 0.75)) #if you want the actual valuees



httpgd::hgd(open = FALSE, port = 0)
system2("cmd.exe", c("/C","start","", httpgd::hgd_url()), wait = FALSE)

plot(1:10) 



# all plots now stream to the browser



# one-time helper
view_df <- function(x, title = deparse(substitute(x))) {
  if (requireNamespace("DT", quietly = TRUE) &&
      requireNamespace("htmlwidgets", quietly = TRUE) &&
      is.function(getOption("viewer"))) {
    w <- DT::datatable(x, caption = title, options = list(pageLength = 25, scrollX = TRUE))
    f <- file.path(tempdir(), paste0("view_", title, ".html"))
    htmlwidgets::saveWidget(w, f, selfcontained = TRUE)
    getOption("viewer")(f)
  } else {
    print(utils::head(x, 100))
  }
}
# use it
view_df(cranid_df)









# one-off viewer that works without pandoc
view_df <- function(x, title = deparse(substitute(x))) {
  stopifnot(requireNamespace("DT", quietly = TRUE),
            requireNamespace("htmlwidgets", quietly = TRUE))

  w <- DT::datatable(x, caption = title,
                     options = list(pageLength = 25, scrollX = TRUE))

  f <- file.path(tempdir(), paste0("view_", title, ".html"))
  htmlwidgets::saveWidget(w, f, selfcontained = FALSE,   # <-- key change
                          libdir = file.path(tempdir(), "view_lib"))
  getOption("viewer")(f)    # uses your cmd.exe/wslpath viewer
}

view_df(cranid_df)








# Put this in the current session (and add it to ~/.Rprofile to keep it)
options(
  viewer = function(u) {
    if (grepl("^https?://", u)) {
      system2("cmd.exe", c("/C","start","", u), wait = FALSE)
    } else {
      # convert Linux path -> Windows UNC path
      win <- tryCatch(system2("wslpath", c("-w", u), stdout = TRUE), error = function(e) u)

      if (nzchar(Sys.which("explorer.exe"))) {
        # explorer handles \\wsl.localhost\... correctly
        system2("explorer.exe", shQuote(win), wait = FALSE)
      } else {
        # fallback: open as file:// URL via cmd
        url <- paste0("file:///", gsub("\\\\", "/", win))
        system2("cmd.exe", c("/C","start","", url), wait = FALSE)
      }
    }
  },
  menu.graphics = FALSE
)


library(DT); library(htmlwidgets)
f <- file.path(tempdir(), "t.html")
htmlwidgets::saveWidget(DT::datatable(head(mtcars)), f, selfcontained = FALSE,
                        libdir = file.path(tempdir(), "tlib"))
getOption("viewer")(f)  # should open in your Windows browser






# ------------ XXX: W4 main and workshop ------------------


pop <- read_tab("pop.txt")
View(pop)

mean(sample(pop$cfu, 20))
# [1] 105.55
mean(sample(pop$cfu, 20))
# [1] 94.5
# NOTE: how the above values differ - showing that mean of samples can differ from one attempt to another

fish <- read_tab("fish.txt")
set_dark_par()
boxplot(fish$length ~ fish$genotype, names = unique(fish$genotype))
View(fish)

t.test(fish$lengths)

tapply(fish$length, fish$genotype, mean)
#        A        B 
# 8.486056 8.101127 
tapply(fish$length, fish$genotype, sd)
tapply(fish$length, fish$genotype, var)

se <- tapply(fish$length, fish$genotype, sd) / sqrt(25)

CI <- se * qt(0.975, 24)
#          A          B 
# 0.12568250 0.09616603 

# NOTE:  I think the CI do not overlap both negative. substracting, and positive, adding, let us see what t test says because if they indeed do not overlap, one could reject the null hypothesis bc there is something here


# BUG: now let us test the assumptions first before hypothesis testing

# first let us check the equality of variance or homoscadicity - two ways

tapply(fish$length, fish$genotype, var)

var.test(fish$length ~ fish$genotype)



# is data normally distributed - p value should be more than 0.05 opposite of you normally operate because you want to accept the null hypothesis
shapiro.test(fish$length)

t.test(fish$length ~ fish$genotype) # XXX: it does not work the other way around genotype ~ length

# PERF: only if you are sure that the means are equal

t.test(fish$length ~ fish$genotype, var.equal = T)

# you see statistical significnce but does the magnitude of the effect really relavent and useful for that you have to do Effect Size calculations - NOTE: Cohen's D or Peterson's R

# PERF: correlation coeffiecient (r)

cor(fish$length ~ fish$genotype)   #BUG: not working


model  <- lm(fish$length ~ fish$genotype) #BUG: not working



#  ------------ paired t-test -------------
p <- read_tab("paired.txt")
t.test(p$Upstream, p$Downstream, paired=T, var.equal=T) # XXX: note that the format you write the paired and the unpaired are different; for one, you do not use the ~ sign and you chose mention one after the other; two, you say that is paired. also it is var.equal and not equal



barwhisker(p$Upstream, p$Downstream) # BUG: command does not exist








##################### CMA #########################

cu <- read_tab("cuckoo_data.txt")
eg <- read_tab("egypt_skulls.txt")

View(cu)


mean(cu$egglength[cu$nest == "Tree Pipit"])
sd <- sd(cu$egglength[cu$nest == "Robin"])
se <- (sd / sqrt(length(cu$egglength[cu$nest == "Robin"])))
se




sd <- sd(cu$egglength[cu$nest == "Meadow Pipit"])
se <- (sd / sqrt(length(cu$egglength[cu$nest == "Meadow Pipit"])))
se


m_cu <- aov(egglength ~ nest, data = cu)
summary(m_cu)

TukeyHSD(m_cu)


#------------  XXX: skull of mummies or something; the new film was not as good ----


View(eg)


mean(eg$MB[eg$ybp == 6000])

sd <- sd(eg$MB[eg$ybp == 6000])
# [1] 5.129249

shapiro.test(eg$MB[eg$ybp == 5300])

shapiro.test(eg$MB)
hist(eg$MB, col = "lightblue")

mean(eg$BL[eg$ybp == 2200])
sd(eg$BL[eg$ybp == 2200])
range(eg$BH)


cor(eg$NH, eg$ybp)

plot(eg$NH ~ eg$ybp)

cov(eg$MB, eg$NH)


cor(eg$MB, eg$ybp)

m_reg <- lm(BL ~ ybp, data = eg)
summary(m_reg)

m_reg2 <- lm(MB ~ ybp, data = eg)
summary(m_reg2)



# ---------- W9 --------------

tb <- read_tab("tb_glm.txt")
tb_df <- table(tb$treatment, tb$disease)
#                 
#                    0   1
#   not vaccinated  99 110
#   vaccinated     464 306

tb_chi <- chisq.test(tb_df)

tb_chi$expected   # PERF: The technique you were looking for



# ----- XXX: fisher Test -------- Lady tasisiting tea experiment
mosaicplot(tb_df, shade=T)
fisher.test(tb_df)

# --------- XXX: general linear model


tb_glm <- glm(tb$desease, tb$treatment, bionomial)




# ---- 

titanic <- read_tab("titanic.txt")
View(titanic)

titanic_glm <- glm()




# ----- curve fitting

puro <- read_tab("puro.txt")  # FIXME: do not have the data


# nls with its two distinct parameters

# the predict function



# -------------- code redo due to loss ---------------
cranid_df <- read_tab("cranid_full.txt")
View(cranid_df)
tapply(cranid_df$GOL, cranid_df$NOL, mean) # one way of running the same operation on multiple colomns can also be used for testing the var between two groups

tapply(cranid_df$GOL, cranid_df$NOL, var)


garden <- read_tab("gardens.txt")



set_dark_par()
boxplot(garden$bird ~ garden$garden)
abline(2, 0.1, col="firebrick", lwd =2) # just to show you can add abline to any existing plot. The first parameter is alpha dn the second is beta

garden_aov_m <- aov(birds ~ garden, data = garden)
summary(garden_aov_m)


# We need to run post hoc after ANOVA as it does not tell us between which group and appreciate that TUSKey is just running t.test but accounting for family wise error

pairwise.t.test() # This is one way to do it but not as robust as the TukeyHSD test


TukeyHSD(garden_aov_m)


# much like the t-test that has a qf command, ANVOA has a qf() command, but it is hardly ever utilised because anova does that automatically for us

qf(0.975, 1, 18)  #It has three parameters
# [1] 5.978052


# NOTE: you can plot the aov model 

plot(garden_aov_m) # NOTE: in residual vs Fitted, what we are looking for is whether the data points are roughly equally spaced up and below the red line. The red line correspnds to the statistical model, and the dots are the residuals............... The normal QQ plot gives you the quantile of the values against the residuals (basically the same thing as the previosu plotts
# NOTE: we want the dots equally up and below the red line which is the expected value of the residuals
#NOTE: Scale-location: it is to see whether we need to do any transformation to the data. If you pay closer look to the Y-axis, you will see that it has taken the square root of residuals to see whether they have an effect
#NOTE: lastly, there is the leverage plot but we do not see anything on this until we do anything robust (?????)

# if you want to see them all at the same time
par(mfrow=c(2,2)) 
plot(garden_aov_m) 
par(mfrow=c(1,1)) 



# BUG: calculating eta squared, the effect size, of aov: the sum of squares of the treatment, as a proportion of the total sum of squares

20 / (20 + 24)
# [1] 0.4545455

# or more formully

anova_table <- summary(garden_aov_m)[[1]]
#             Df Sum Sq Mean Sq F value   Pr(>F)   
# garden       1     20 20.0000      15 0.001115 **
# Residuals   18     24  1.3333                    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

SS_effect <- anova_table["garden", "Sum Sq"]
# [1] 20
SS_error  <- anova_table["Residuals", "Sum Sq"]
SS_total  <- SS_effect + SS_error

eta_sq <- SS_effect / SS_total
eta_sq
# [1] 0.4545455




x <- c(4, 6, 9, 13,17)
y <- c(4, 3, 8, 15, 23)

cor.test(x, y) # one way to check for corrolation between tow samples
plot(y ~ x, col = "yellow")


# -------------- XXX: Regression

reg_m <- lm(y ~ x)
summary(reg_m)
summary(reg_m)$coefficients[,"t value"] # PERF: Advance syntactical technique
# (Intercept)           x 
#   -2.228360    8.088058 

# you can use the coefficients from the results of a regression to get the alpha and beta and use that when you are plotting the abline
abline(2, 2, col="firebrick", lwd = 2 )

# regression is running analysis of varience under the hood that is why you can put the results of its model object inside of an aov comman
summary.aov(reg_m)



# ---- NOTE: w9 workshop - Skript Kiddy


puro <- read_tab("puro.txt")
attach(puro)
model<-nls(rate~(vmax*conc)/(km+conc), start=list(vmax=180,km=0.1)) 
summary(model) 

xpred<-seq(0,1.1,0.01) 
ypredMM <- predict(model,list(conc=xpred)) 
lines(ypredMM~xpred,lty=2,col="blue" ) 

coef(model) 

confint(model)

detach(puro)



 # ---------- Q and A ------

# xxx: Princpal component analysis PCA, also called dimension reduction --- pairs()
cranid <- read_tab("cranid_full.txt")
pairs(cranid[, 3:9])

# -------------------------------- XXX: catch up ---------------

# ----------------- W6 regression ----
set_dark_par()

brain_body_df <- read_tab("brain_body.txt")
plot(brain_body_df$brain ~ brain_body_df$body)

# as you can see heavily influnced by outliers - let us try another data set

cov(brain_body_df$brain, brain_body_df$body)

# covairence is affected by scale, so a less crude way is:

cor()



reg_ex_df <- read_tab("regression_example.txt")
plot(reg_ex_df$y ~ reg_ex_df$x, col="firebrick", lwd=2, pch= 19)

reg_model_of_reg_ex_df <- lm(y ~ x, data = reg_ex_df)
summary(reg_model_of_reg_ex_df)                             # NOTE: gives coefficients table
summary.lm(reg_model_of_reg_ex_df)  # PERF: it seems to give the exact same results

# NOTE: we can use the reg to get the alpha and beta and fit the abline line which is basically the line of best fit - alpha, the first value, is the y intercept and the beta, the second value, is the gradient

abline(5.43467, 0.46189, col= "tomato", lty = 2, lwd = 2) # You can make it dotted, adjust thinkness, or create a box inside the graph

plot(reg_ex_df$y ~ reg_ex_df$x, col="firebrick", pch=19)
abline(5.43467, 0.46189, col="tomato", lty=2, lwd=2)

# more advance way just combine the two functions together
abline(lm(y ~ x, data = reg_ex_df), col="tomato", lty=2, lwd=2) #BUG: the line is not the correction regression line ====> Fixed, you should put the complete formula inside the paranthesis for lm

# or
abline(reg_model_of_reg_ex_df)



# abline and the line function have different use cases - if you want to add a regression line abline is your guy but if you want to fit multiple data lines on the same plot use line




# ---- PERF: DRAW BOX HERE ----
rect(
  xleft = 2,     # left boundary
  ybottom = 5,   # bottom boundary
  xright = 6,    # right boundary
  ytop = 12,     # top boundary
  border = "yellow",
  lwd = 2
)





# "white" "aliceblue" "antiquewhite" "antiquewhite1" "antiquewhite2" "antiquewhite3" "antiquewhite4""aquamarine" "aquamarine1" "aquamarine2" "aquamarine3" "aquamarine4""azure" "azure1" "azure2" "azure3" "azure4""beige" "bisque" "bisque1" "bisque2" "bisque3" "bisque4""black" "blanchedalmond" "blue" "blue1" "blue2" "blue3" "blue4""blueviolet" "brown" "brown1" "brown2" "brown3" "brown4""burlywood" "burlywood1" "burlywood2" "burlywood3" "burlywood4""cadetblue" "cadetblue1" "cadetblue2" "cadetblue3" "cadetblue4""chartreuse" "chartreuse1" "chartreuse2" "chartreuse3" "chartreuse4""chocolate" "chocolate1" "chocolate2" "chocolate3" "chocolate4""coral" "coral1" "coral2" "coral3" "coral4"
#"cornflowerblue" "cornsilk" "cornsilk1" "cornsilk2" "cornsilk3" "cornsilk4""cyan" "cyan1" "cyan2" "cyan3" "cyan4""darkblue" "darkcyan" "darkgoldenrod" "darkgoldenrod1" "darkgoldenrod2" "darkgoldenrod3" "darkgoldenrod4""darkgray" "darkgreen" "darkgrey" "darkkhaki" "darkmagenta""darkolivegreen" "darkolivegreen1" "darkolivegreen2" "darkolivegreen3" "darkolivegreen4""darkorange" "darkorange1" "darkorange2" "darkorange3" "darkorange4""darkorchid" "darkorchid1" "darkorchid2" "darkorchid3" "darkorchid4""darkred" "darksalmon" "darkseagreen" "darkseagreen1" "darkseagreen2" "darkseagreen3" "darkseagreen4""darkslateblue" "darkslategray" "darkslategray1" "darkslategray2" "darkslategray3" "darkslategray4""darkslategrey" "darkturquoise" "darkviolet""deeppink" "deeppink1" "deeppink2" "deeppink3" "deeppink4""deepskyblue" "deepskyblue1" "deepskyblue2" "deepskyblue3" "deepskyblue4""dimgray" "dimgrey" "dodgerblue" "dodgerblue1" "dodgerblue2" "dodgerblue3" "dodgerblue4""firebrick" "firebrick1" "firebrick2" "firebrick3" "firebrick4""floralwhite" "forestgreen""gainsboro" "ghostwhite""gold" "gold1" "gold2" "gold3" "gold4""goldenrod" "goldenrod1" "goldenrod2" "goldenrod3" "goldenrod4""gray" "gray0" "gray1" "gray2" … "gray100""green" "green1" "green2" "green3" "green4""greenyellow" "grey" "grey0" "grey1" … "grey100""honeydew" "honeydew1" "honeydew2" "honeydew3" "honeydew4""hotpink" "hotpink1" "hotpink2" "hotpink3" "hotpink4""indianred" "indianred1" "indianred2" "indianred3" "indianred4""indigo" "ivory" "ivory1" "ivory2" "ivory3" "ivory4""khaki" "khaki1" "khaki2" "khaki3" "khaki4""lavender" "lavenderblush" "lavenderblush1" "lavenderblush2" "lavenderblush3" "lavenderblush4""lawngreen" "lemonchiffon" "lemonchiffon1" "lemonchiffon2" "lemonchiffon3" "lemonchiffon4""lightblue" "lightblue1" "lightblue2" "lightblue3" "lightblue4""lightcoral" "lightcyan" "lightcyan1" "lightcyan2" "lightcyan3" "lightcyan4""lightgoldenrod" "lightgoldenrod1" "lightgoldenrod2" "lightgoldenrod3" "lightgoldenrod4""lightgoldenrodyellow""lightgray" "lightgreen" "lightgrey""lightpink" "lightpink1" "lightpink2" "lightpink3" "lightpink4""lightsalmon" "lightsalmon1" "lightsalmon2" "lightsalmon3" "lightsalmon4""lightseagreen" "lightskyblue" "lightskyblue1" "lightskyblue2" "lightskyblue3" "lightskyblue4""lightslateblue" "lightslategray" "lightslategrey""lightsteelblue" "lightsteelblue1" "lightsteelblue2" "lightsteelblue3" "lightsteelblue4""lightyellow" "lightyellow1" "lightyellow2" "lightyellow3" "lightyellow4""limegreen" "linen""magenta" "magenta1" "magenta2" "magenta3" "magenta4""maroon" "maroon1" "maroon2" "maroon3" "maroon4""mediumaquamarine""mediumblue""mediumorchid" "mediumorchid1" "mediumorchid2" "mediumorchid3" "mediumorchid4""mediumpurple" "mediumpurple1" "mediumpurple2" "mediumpurple3" "mediumpurple4""mediumseagreen" "mediumslateblue" "mediumspringgreen" "mediumturquoise" "mediumvioletred""midnightblue" "mintcream" "mistyrose" "mistyrose1" "mistyrose2" "mistyrose3" "mistyrose4""moccasin""navajowhite" "navajowhite1" "navajowhite2" "navajowhite3" "navajowhite4""navy" "navyblue""oldlace" "olive" "olivedrab" "olivedrab1" "olivedrab2" "olivedrab3" "olivedrab4""orange" "orange1" "orange2" "orange3" "orange4""orangered" "orangered1" "orangered2" "orangered3" "orangered4""orchid" "orchid1" "orchid2" "orchid3" "orchid4""palegoldenrod" "palegreen" "palegreen1" "palegreen2" "palegreen3" "palegreen4""paleturquoise" "paleturquoise1" "paleturquoise2" "paleturquoise3" "paleturquoise4""palevioletred" "palevioletred1" "palevioletred2" "palevioletred3" "palevioletred4""papayawhip" "peachpuff" "peachpuff1" "peachpuff2" "peachpuff3" "peachpuff4""peru" "pink" "pink1" "pink2" "pink3" "pink4"
#"plum" "plum1" "plum2" "plum3" "plum4""powderblue""purple" "purple1" "purple2" "purple3" "purple4""red" "red1" "red2" "red3" "red4""rosybrown" "rosybrown1" "rosybrown2" "rosybrown3" "rosybrown4""royalblue" "royalblue1" "royalblue2" "royalblue3" "royalblue4""saddlebrown""salmon" "salmon1" "salmon2" "salmon3" "salmon4""sandybrown""seagreen" "seagreen1" "seagreen2" "seagreen3" "seagreen4""seashell" "seashell1" "seashell2" "seashell3" "seashell4""sienna" "sienna1" "sienna2" "sienna3" "sienna4""skyblue" "skyblue1" "skyblue2" "skyblue3" "skyblue4""slateblue" "slateblue1" "slateblue2" "slateblue3" "slateblue4""slategray" "slategray1" "slategray2" "slategray3" "slategray4""slategrey""snow" "snow1" "snow2" "snow3" "snow4""springgreen" "springgreen1" "springgreen2" "springgreen3" "springgreen4""steelblue" "steelblue1" "steelblue2" "steelblue3" "steelblue4""tan" "tan1" "tan2" "tan3" "tan4""thistle" "thistle1" "thistle2" "thistle3" "thistle4""tomato" "tomato1" "tomato2" "tomato3" "tomato4""turquoise" "turquoise1" "turquoise2" "turquoise3" "turquoise4""violet" "violetred" "violetred1" "violetred2" "violetred3" "violetred4""wheat" "wheat1" "wheat2" "wheat3" "wheat4""whitesmoke""yellow" "yellow1" "yellow2" "yellow3" "yellow4""yellowgreen"




 # NOTE: What is happening under the hood with Peter's R test, which is cor()

cov(reg_ex_df$y, reg_ex_df$x)
# [1] 34.06405

cov(reg_ex_df$y, reg_ex_df$x) / (sd(reg_ex_df$y) * sd(reg_ex_df$x))
# [1] 0.8976781


# Note how cor will give you the same results - we do this in the first place because covarience is very affected by sclae and cannot be used to compare values from different things, for instance if we wanted to compare the heart rate of a rat with that of an elephant in context

cor(reg_ex_df$y, reg_ex_df$x)
# [1] 0.8976781


# NOTE: you can test Peter's R, whether it is significant or not, using the following function

cor.test(reg_ex_df$y, reg_ex_df$x)


# you can run summary.aov() on model created by regression
summary.aov(reg_model_of_reg_ex_df)
#             Df Sum Sq Mean Sq F value Pr(>F)    
# x            1  912.6   912.6   236.6 <2e-16 ***
# Residuals   57  219.9     3.9                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




# -------------------- W7 Complex Linear Models - ANCOVA - Analysis of coverience

rat <- read_tab("ratweight.txt")
View(rat)

rat_ancova_m <- aov(Gain ~ Protein + Amount, data = rat)
summary(rat_ancova_m)


# NOTE: you can run posthoc method for ANCOVA but not using TuskeyHSD - you need to install a package 



p_load("multcomp")
posthocs<-glht(rat_ancova_m, linfct = mcp(Amount = "Tukey"))
summary(posthocs)

# BUG: news flash you can use two way ANOVA on this data but not ANCOVA - ANCOVA requires at least one continues covarient - If you have only categorical predictors, you cannot perform ANCOVA. - ANalysis of COvariance → you must have a covariate (a continuous one). The thing that makes a model ANCOVA is the presence of a continuous covariate, not the interaction.

cricket <- read_tab("cricket_ancova.txt")

View(cricket)


cricket_ancova_m <- aov(pulses ~ temp * species, data = cricket)
summary(cricket_ancova_m)

cricket_posthocs<-glht(cricket_ancova_m, linfct = mcp(temp = "Tukey")) # FIXME: not working
summary(posthocs)




# ------------------------------------------ W8 catagorical data ----


treated <- c(4,12)
untreated <- c(5,74)
avadex<-rbind(treated,untreated)
avadex


#Chi-square test uses counts in a contingency table. Those counts come from categorical variables. The fact the numbers are written as integers in R doesn’t make the variables numeric in the statistical sense – they’re frequencies of categories, not continuous measurements.


xi <- chisq.test(avadex, correct = F)
xi

xi$residuals # XXX: for some reason it did not give me the residuals when I asked for the entire info above
# numbers to note are smaller or greater than -/+2 or for extremely unsusual -/+3




# NOTE: calculating effect size - here it is calculating odds ratio

4/12
0.3333333/0.06756757


# ---------------------- XXX: Non Linear Regression - Week 9


## -----------------------------
## 1. Create a small non-linear dataset
## -----------------------------
set.seed(123)                          # for reproducibility

x  <- seq(0, 5, length.out = 30)       # predictor
y_true <- 2 * exp(0.5 * x)             # underlying non-linear curve
y  <- y_true + rnorm(length(x), 0, 5)  # add noise

dat <- data.frame(x, y)

## -----------------------------
## 2. Plot the raw data
## -----------------------------
plot(dat$x, dat$y,
     main = "Non-linear data and different fits",
     xlab = "x", ylab = "y",
     pch = 19)

## Optionally, add the true curve (we know it because we simulated it)
curve(2 * exp(0.5 * x), add = TRUE, lwd = 2)   # true model (for reference)

## -----------------------------
## 3. Linear model (wrong but useful as baseline)
##    - Treats relationship as straight line
## -----------------------------
lm_lin <- lm(y ~ x, data = dat)
summary(lm_lin)

abline(lm_lin, lwd = 2, lty = 2)       # dashed line = simple linear fit

## -----------------------------
## 4. Linearise using log-transform (common trick)
##    - If y ≈ a * exp(bx), then log(y) ≈ log(a) + b x
##    - Fit linear model on log(y), then back-transform
## -----------------------------
lm_log <- lm(log(y) ~ x, data = dat)
summary(lm_log)

## Extract coefficients and reconstruct curve on original y-scale
a_hat <- exp(coef(lm_log)[1])         # exp(intercept)
b_hat <- coef(lm_log)[2]

curve(a_hat * exp(b_hat * x),
      add = TRUE, lwd = 2, lty = 3)   # dotted line = log-linear fit

## -----------------------------
## 5. Polynomial regression (flexible but still linear in parameters)
##    - Fits y ≈ β0 + β1 x + β2 x^2
## -----------------------------
lm_poly <- lm(y ~ x + I(x^2), data = dat)
summary(lm_poly)

## Add polynomial fit as a smooth curve
x_grid <- seq(min(x), max(x), length.out = 200)
y_poly <- predict(lm_poly, newdata = data.frame(x = x_grid))
lines(x_grid, y_poly, lwd = 2, lty = 4)  # dash-dot = quadratic fit

## -----------------------------
## 6. TRUE non-linear regression with nls()
##    - Directly fit y = a * exp(b x)
##    - Need reasonable starting values for parameters
## -----------------------------
# Use rough guesses close to the truth (a ~ 2, b ~ 0.5)
nls_fit <- nls(y ~ a * exp(b * x),
               data = dat,
               start = list(a = 1, b = 0.3))  # starting guesses

summary(nls_fit)
coef(nls_fit)

## Predicted curve from nls model
y_nls <- predict(nls_fit, newdata = data.frame(x = x_grid))
lines(x_grid, y_nls, lwd = 3, col = "black")  # thick black curve = nls fit

legend("topleft",
       legend = c("Data",
                  "True curve",
                  "Linear",
                  "Log-linear",
                  "Quadratic",
                  "nls non-linear"),
       pch = c(19, NA, NA, NA, NA, NA),
       lty = c(NA, 1, 2, 3, 4, 1),
       lwd = c(NA, 2, 2, 2, 2, 3),
       bty = "n")


# --------------------------------- W10 Non parametric statistics ----------


###############################################################################
# 1. Create a small example dataset
#    We'll create a dataset with a nonlinear curved pattern
#    plus a categorical variable for non-parametric tests.
###############################################################################

set.seed(1)                     # reproducibility

# Numeric predictor
x <- seq(0, 10, length.out = 60)

# True nonlinear function with noise
y <- sin(x) + rnorm(60, sd = 0.3)

# A categorical grouping variable for Kruskal-Wallis test
group <- gl(3, 20, labels = c("A", "B", "C")) 

dat <- data.frame(x, y, group)

# Plot raw data
plot(x, y, pch = 19,
     main = "Example Data", xlab = "x", ylab = "y")



###############################################################################
# 2. Kruskal-Wallis test
# kruskal.test(y ~ group) compares median y across three groups (A, B, C)
# It uses ranks instead of raw values → non-parametric.
###############################################################################

kruskal_result <- kruskal.test(y ~ group, data = dat)
kruskal_result



###############################################################################
# 3. Non-parametric regression using loess()
# loess(y ~ x) fits a smooth curve that adapts to the data.
# span controls smoothness: smaller = wigglier slope.
###############################################################################

loess_fit <- loess(y ~ x, data = dat, span = 0.5)

# Generate smooth predictions
x_grid <- seq(min(x), max(x), length.out = 200)
y_loess <- predict(loess_fit, newdata = data.frame(x = x_grid))

# Add to the plot
lines(x_grid, y_loess, col = "blue", lwd = 3)
legend("topright", legend = "LOESS fit", col = "blue", lwd = 3, bty = "n")



###############################################################################
# 4. Kernel smoothing using ksmooth()
# ksmooth(x, y) returns smoothed values using kernel-weighted local averaging.
# bandwidth controls how wide the weighting window is.
###############################################################################

kfit <- ksmooth(x, y, kernel = "normal", bandwidth = 0.8)

# Add to plot
lines(kfit$x, kfit$y, col = "red", lwd = 2, lty = 2)
legend("bottomright", legend = "Kernel smooth", col = "red", lwd = 2, lty = 2, bty = "n")



###############################################################################
# 5. Generalized Additive Model (GAM) for smooth curve
# library(mgcv) uses s(x) to fit a smooth spline.
###############################################################################

library(mgcv)

gam_fit <- gam(y ~ s(x), data = dat)     # s(x) = smooth function of x
y_gam <- predict(gam_fit, newdata = data.frame(x = x_grid))

lines(x_grid, y_gam, col = "darkgreen", lwd = 3)
legend("topleft", legend = "GAM spline", col = "darkgreen", lwd = 3, bty = "n")


###############################################################################
# 6. Logistic regression for binary outcomes
# We simulate a binary (0/1) variable where probability increases with x.
###############################################################################

# Simulate binary outcome
p <- plogis(-2 + 0.4 * x)   # logistic function = sigmoid curve
binary_y <- rbinom(60, size = 1, prob = p)

plot(x, binary_y, pch = 19,
     main = "Binary Data for Logistic Regression",
     ylab = "Outcome (0/1)", xlab = "x")

###############################################################################
# Fit logistic regression using glm()
# glm(outcome ~ predictor, family = binomial) fits a sigmoid curve.
###############################################################################

logit_model <- glm(binary_y ~ x, family = binomial)
summary(logit_model)

# Predict smooth probabilities
p_hat <- predict(logit_model, newdata = data.frame(x = x_grid), type = "response")

lines(x_grid, p_hat, col = "purple", lwd = 3)
legend("bottomright", legend = "Logistic regression", col = "purple", lwd = 3, bty = "n")


 # --------- Lecture w10 ------

parasite_df <- read_tab("parasite.txt")


wilcox.test(parsites~stock) # So strict that is more likely to give you a false negative

# you can use anova but it gives you a much lower p value because non parametric varation of the t test is limited, so we can run the non parametric version of anova


kruskal.test()


# the non parametric version of regression
# NOTE: loess(y~x) - not really useful

loess()

# all the approaches above are nothing more than stop gaps and better approaches exists




#   post hoc method for ANCOVA - with the package ?

# emeans package
emeans (model, -snout_vent)

install.packages("emmeans")

require(emmeans)




p_load(emmeans, readxl)

alligator <- read_excel("/home/motall/r_cabinet/r-bio-course/data/alligator.xlsx")





model <- lm(alligator$pelvic_canal~alligator$snout_vent+alligator$sex)

summary(model)


set_dark_par()
plot(alligator$pelvic_canal~alligator$snout_vent, col=as.factor(alligator$sex))

plot(alligator$pelvic_canal~alligator$snout_vent, col=as.factor(alligator$sex), pch=16)

TukeyHSD(model)

emmeans(model , ~sex)

emmeans(model , ~snout_vent)


# NOTE: Look into marginal means





plot(1:10,1:10,xlim=c(-5,5),ylim=c(0,10),type="n",xlab="",ylab="",xaxt="n",yaxt="n")

# Make the branches

rect(-1,0,1,2,col="tan3",border="tan4",lwd=3)

polygon(c(-5,0,5),c(2,4,2),col="palegreen3",border="palegreen4",lwd=3)

polygon(c(-4,0,4),c(3.5,5.5,3.5),col="palegreen4",border="palegreen3",lwd=3)

polygon(c(-3,0,3),c(5,6.5,5),col="palegreen3",border="palegreen4",lwd=3)

polygon(c(-2,0,2),c(6.25,7.5,6.25),col="palegreen4",border="palegreen3",lwd=3)

#Add some ornaments

points(x=runif(4,-5,5),y=rep(2,4),col=sample(c("blue","red"),size=4,replace=T),cex=3,pch=19)

points(x=runif(4,-4,4),y=rep(3.5,4),col=sample(c("blue","red"),size=4,replace=T),cex=3,pch=19)

points(x=runif(4,-3,3),y=rep(5,4),col=sample(c("blue","red"),size=4,replace=T),cex=3,pch=19)

points(x=runif(4,-2,2),y=rep(6.25,4),col=sample(c("blue","red"),size=4,replace=T),cex=3,pch=19)

points(0,7.5,pch=8,cex=5,col="gold",lwd=3)

# Add some presents

xPres = runif(10,-4.5,4.5)

xWidth = runif(10,0.1,0.5)

xHeight=runif(10,0,1)

for(i in 1:10){

 rect(xPres[i]-xWidth[i],0,xPres[i]+xWidth[i],xHeight[i],col=sample(c("blue","red"),size=1))

 rect(xPres[i]-0.2*xWidth[i],0,xPres[i]+0.2*xWidth[i],xHeight[i],col=sample(c("gold","grey87"),size=1))

}
