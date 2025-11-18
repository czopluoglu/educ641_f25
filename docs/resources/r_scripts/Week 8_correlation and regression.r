###############################################################################
# EDUC 641: Relationship between two continuous variables
###############################################################################

# Helpful printing:
options(digits = 4)

###############################################################################
# Part 0. Packages and datasets
###############################################################################

# Hmisc for rcorr()

# Install first if needed, for example:
# install.packages("Hmisc")

require(Hmisc)

# Import datasets used in the slides
# Make sure to adjust the paths below

infant_df <- read.csv("./resources/datasets/infant_mortality.csv",
                      header = TRUE)

lifeexp_df <- read.csv("./resources/datasets/life_expectancy.csv",
                       header = TRUE)

# Quick preview:
head(infant_df)
str(infant_df)

head(lifeexp_df)
str(lifeexp_df)

###############################################################################
# Part 1. Scatterplots in base R
###############################################################################

# Example: 2019 OECD data
# Variables:
#   doctor : physicians per 1,000 inhabitants
#   infmor : under-five mortality rate per 1,000 live births

# Scatterplot for a small subset of countries

subset_idx <- c(1, 2, 24, 26, 17) 
infant_subset <- infant_df[subset_idx, ]

# Basic scatterplot with labels for countries

plot(infant_subset$doctor,
     infant_subset$infmor,
     xlab = "Doctors per 1,000 inhabitants",
     ylab = "Under-five mortality rate\n(per 1,000 live births)",
     xlim = c(0, 6),
     ylim = c(0, 20),
     pch  = 19)

# Add dashed grid lines (horizontal and vertical)
abline(h = seq(0, 20, 1), v = seq(0, 6, 1),
       lty = "dotted", col = "lightgray")

# Re-plot points on top so they are not hidden by grid
points(infant_subset$doctor,
       infant_subset$infmor,
       pch = 19)

# Add text labels next to points
text(infant_subset$doctor,
     infant_subset$infmor,
     labels = infant_subset$country,
     pos = 3, cex = 0.7)

# Scatterplot for ALL countries
plot(infant_df$doctor,
     infant_df$infmor,
     xlab = "Doctors per 1,000 inhabitants",
     ylab = "Under-five mortality rate (per 1,000 live births)",
     xlim = c(0, 6),
     ylim = c(0, 20),
     pch  = 19)

abline(h = seq(0, 20, 1), v = seq(0, 6, 1),
       lty = "dotted", col = "lightgray")

points(infant_df$doctor,
       infant_df$infmor,
       pch = 19)

###############################################################################
# Part 2. Covariance and Correlation
###############################################################################

x <- infant_df$doctor
y <- infant_df$infmor
N <- length(x)

# Compute covariance using cov() in base R

cov(x = x, 
    y = y,
    method = "pearson")

# Compute correlation using cor() in base R

cor(x = x,
    y = y,
    method = "pearson")

###############################################################################
# Inference for correlation: t-test, p-value, Hmisc::rcorr()
###############################################################################

# We test:
#   H0: rho = 0 (no linear association in the population)

# Sampling distribution: t with df = N - 2
#   t = r * sqrt( (N - 2) / (1 - r^2) )

r <- cor(x, y)
N <- length(x)

t_value <- r * sqrt((N - 2) / (1 - r^2))
t_value

df <- N - 2
df

# Two-sided p-value: P(T >= t_value)*2

p_value <- 2 * pt(q = t_value,
                  df = df,
                  lower.tail = TRUE)
p_value

  # Interpretation:
  #   If p < .05, we reject H0 and conclude the correlation
  #   is significantly different from 0.

# Using rcorr() from Hmisc
# rcorr() requires a matrix; it returns correlations (r), N, and p-values.

rcorr_single <- rcorr(x = x,
                      y = y,
                      type = "pearson")
rcorr_single

# rcorr() reports:
#   - r: 2x2 correlation matrix
#   - n: sample size used
#   - P: p-value for test of rho = 0 (always reports two sided)

###############################################################################
# Correlation matrix for multiple variables
###############################################################################

# Example: life expectancy dataset
# Variables:
#   adult_mortality
#   under_five_deaths
#   measles
#   polio
#   diphtheria

life_sub1 <- lifeexp_df[, c("adult_mortality",
                            "under_five_deaths",
                            "measles",
                            "polio",
                            "diphtheria")]

# Correlation matrix with cor()
my_corr1 <- cor(life_sub1,
                method = "pearson")

my_corr1
round(my_corr1, 2)

# Correlation matrix with rcorr()

rcorr_mat1 <- rcorr(as.matrix(life_sub1))

# Correlations:
rcorr_mat1$r

# Sample sizes for each pair:
rcorr_mat1$n

# p-values:
rcorr_mat1$P


###############################################################################
# Missing data in correlations: listwise vs pairwise deletion
###############################################################################

# Example with life expectancy variables

life_sub2 <- lifeexp_df[, c("life_expectancy",
                            "bmi",
                            "percentage_expenditure",
                            "gdp",
                            "hepatitis_b")]

cor(life_sub2)



# Listwise deletion:
corr_listwise_life <- cor(life_sub2,
                          use = "complete.obs")
round(corr_listwise_life, 2)

# How many complete cases were used?
n_complete_life <- nrow(na.omit(life_sub2))
n_complete_life

# Pairwise deletion:
corr_pairwise_life <- cor(life_sub2,
                          use = "pairwise.complete.obs")
round(corr_pairwise_life, 2)

# rcorr() always uses pairwise deletion and reports N for each pair:
rcorr_life <- rcorr(as.matrix(life_sub2))
rcorr_life$r  # correlations
rcorr_life$n  # Ns
rcorr_life$P  # p-values

###############################################################################
# Simple linear regression (brief introduction)
###############################################################################

# Fit the best-fitting line (least squares) with lm()

model1 <- lm(infmor ~ doctor,
             data = infant_df)

model1  # prints intercept and slope

# Plot data + fitted regression line
plot(infant_df$doctor,
     infant_df$infmor,
     xlab = "Number of doctors (per 1,000 inhabitants)",
     ylab = "Under-five mortality rate\n(per 1,000 live births)",
     pch  = 19)

abline(model1, lty = 2)  # adds the fitted regression line

# Inference for regression coefficients: summary()
summary(model1)

# summary(model1) reports:
#   - Estimate: b0 (intercept), b1 (slope)
#   - Std. Error, t value, Pr(>|t|)
#   - Multiple R-squared: proportion of variance in Y explained by X


