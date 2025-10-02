
# Package installation should be done only once
# Once you install the following packages, you can comment them out

install.packages('dplyr')
install.packages('janitor')
install.packages('moments')
install.packages('psych')

# Load the packages

require('dplyr')
require('janitor')
require('moments')
require('psych')

################################################################################
#
#            Import the dataset and do some simple data wrangling
#
################################################################################


life_exp_2015 <- read.table(file   = "./resources/datasets/life_expectancy_2015.csv",
                       sep    = ",",
                       header = TRUE)

head(life_exp_2015)

################################################################################
#
#       Describing data with distributions
#
################################################################################


# Frequency table for life expectancy variable

table(life_exp_2015$life_expectancy)

# Histogram with a separate bin for each unique value

hist(x      = life_exp_2015$life_expectancy,
     breaks = 50:89,
     col    = 'white',
     xlab   = 'Life Expectancy',
     main   = '')

# Histogram with collapsed bins (a bin for every five year)

hist(x      = life_exp_2015$life_expectancy,
     breaks = seq(50, 90, 5),
     col    = 'white',
     xlab   = 'Life Expectancy',
     main   = '')

# Stem-and-leaf plot
stem(life_exp_2015$life_expectancy,scale=0.5)

# Density plot

d <- density(life_exp_2015$life_expectancy)

plot(d,
     main = "Density Plot of Life Expectancy (2015)",
     xlab = "Life Expectancy",
     ylab = "Density")


################################################################################
#
#       Measures of Central Tendency
#
################################################################################

###########      Mode       ##########

# R does not have a built-in mode() function for this purpose
# Instead, you can tabulate frequencies and find the maximum:

tab <- table(life_exp_2015$life_expectancy)

sort(tab)

  # In this case, there are multiple modes
  # Both 75 and 76 would be reported as mode

d <- density(life_exp_2015$life_expectancy)

plot(d,
     main = "Density Plot of Life Expectancy (2015)",
     xlab = "Life Expectancy",
     ylab = "Density")

abline(v = 75.5,lty=2)  # vertical line at x = 75.5, line type = 2 (dashed)

# Add a legend
legend("topright",                   # Position of legend
       legend = c("Mode (75.5)"),    # Legend labels
       lty = c(2),                   # Match line types to labels
       col = c("black"),             # Colors
       bty = "n")                    # No box around the legend


###########      Median       ##########

?median

median(life_exp_2015$life_expectancy)

d <- density(life_exp_2015$life_expectancy)

plot(d,
     main = "Density Plot of Life Expectancy (2015)",
     xlab = "Life Expectancy",
     ylab = "Density")

abline(v = 75.5,lty=2)  # vertical line at x = 75.5, line type = 2 (dashed)
abline(v = 74,lty=3)    # vertical line at x = 74,   line type = 3 (dotted)

# Add a legend
legend("topright",                                  # Position of legend
       legend = c("Mode (75.5)", "Median (74)"),    # Legend labels
       lty = c(2, 3),                               # Match line types to labels
       col = c("black", "black"),                  # Colors (both black here)
       bty = "n")                                   # No box around the legend

###########      Mean       ##########

?mean

mean(life_exp_2015$life_expectancy)

d <- density(life_exp_2015$life_expectancy)

plot(d,
     main = "Density Plot of Life Expectancy (2015)",
     xlab = "Life Expectancy",
     ylab = "Density")

abline(v = 75.5,lty=2)     # vertical line at x = 75.5, line type = 2 (dashed)
abline(v = 74,lty=3)       # vertical line at x = 74,   line type = 3 (dotted)
abline(v = 71.75,lty=4)    # vertical line at x = 71.75,   line type = 3 (dotdash)

# Add a legend
legend("topright",                                  # Position of legend
       legend = c("Mode (75.5)", "Median (74)", "Mean (71.75)"),    # Legend labels
       lty = c(2, 3, 4),                               # Match line types to labels
       col = c("black", "black", "black"),                  # Colors (both black here)
       bty = "n")                                   # No box around the legend

################################################################################
#
#       Measures of Variability
#
################################################################################

###########     Range      ##########

?range

range(life_exp_2015$life_expectancy)

###########     IQR        ##########

?quantile

quantile(life_exp_2015$life_expectancy)


?IQR

IQR(life_exp_2015$life_expectancy)

###########     Variance   ##########

?var
var(life_exp_2015$life_expectancy)

###########     Standard Deviation   ##########

?sd
sd(life_exp_2015$life_expectancy)

################################################################################
#
#       Measures of Shape
#
################################################################################

# Skewness

?moments::skewness
skewness(life_exp_2015$life_expectancy)

?psych::skew
skew(life_exp_2015$life_expectancy)

# Kurtosis

?moments::kurtosis
kurtosis(life_exp_2015$life_expectancy)

?psych::kurtosi
kurtosi(life_exp_2015$life_expectancy)






















