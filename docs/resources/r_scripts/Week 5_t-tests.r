# Helpful printing:
options(digits = 4)

################################################################################
# Part 1. Working with the t distribution: pt() and qt()
################################################################################

# pt(q, df, lower.tail) gives P(T <= q) when lower.tail = TRUE
# pt(q, df, lower.tail = FALSE) gives P(T > q)
# qt(p, df, lower.tail) gives the quantile q such that P(T <= q) = p (if lower.tail=TRUE)

# Example 1: P(T < -1) with df = 10

pt(q = -1, df = 10, lower.tail = TRUE)

# Example 2: Find the t value q such that P(T < q) = 0.3 with df = 25

qt(p = 0.3, df = 25, lower.tail = TRUE)

################################################################################
# Part 2. One-sample t-test using the lsr package
################################################################################

# Import the LBW data (use a unique object name)

lbw_df <- read.csv("./resources/datasets/lbw.csv", header = TRUE)

# Quick structure/preview:
head(lbw_df)
str(lbw_df)

# Load lsr (install first if needed: install.packages("lsr"))
require(lsr)

# lsr::oneSampleTTest()
# Arguments:
#   x         = numeric vector
#   mu        = null-hypothesis mean
#   one.sided = "greater" tests HA: μ > test_value; 
#               "less" tests HA: μ < test_value; 
#               FALSE for a two-sided test, HA: μ is not equal to test_value


oneSampleTTest(x         = lbw_df$weight, 
               mu        = 100, 
               one.sided = "greater")

# Assumption notes (one-sample):
# - Data are (approximately) normally distributed OR N is large (CLT helps).
# - Observations are independent.


# If the alternative hypothesis were two sided:

oneSampleTTest(x         = lbw_df$weight, 
               mu        = 100, 
               one.sided = FALSE)


################################################################################
# Part 3. Independent-samples t-test with the lsr package
################################################################################

# Import data (new object name)
stereo_df <- read.csv("./resources/datasets/stereotype.csv", header = TRUE)

# Data structure:
# - One row per participant
# - Dependent variable: score
# - Grouping variable: condition (must be a factor)
head(stereo_df)
str(stereo_df)

# Ensure grouping variable is a factor; check the level names:
stereo_df$condition <- factor(stereo_df$condition)
levels(stereo_df$condition)
str(stereo_df)


require(lsr)

# lsr::independentSamplesTTest()

# Arguments:
#   formula   = DV ~ Group
#   data      = data frame
#   var.equal = TRUE  -> Student t-test (assume equal variances)
#               FALSE -> Welch t-test (does not assume equal variances)
#   one.sided = name of the group expected to have the higher mean
#               e.g., if you predict group "control" > other group
#               use one.sided = "control"
#

# First, equal-variance version:

independentSamplesTTest(
  formula   = score ~ condition, 
  data      = stereo_df, 
  var.equal = TRUE,
  one.sided = "control"   # <- make sure "control" is actually one of the levels!
)

# Now Welch's t-test (safer default when variances may differ):

independentSamplesTTest(
  formula   = score ~ condition, 
  data      = stereo_df, 
  var.equal = FALSE,
  one.sided = "control"
)


################################################################################
# Part 5. Dependent/Paired-samples t-test
################################################################################

# Import data (new object name)
anorexia_df <- read.csv("./resources/datasets/anorexia.csv", header = TRUE)

# Structure:
# - One row per participant
# - Two columns for repeated measures (e.g., Weight_before, Weight_after)
head(anorexia_df)
str(anorexia_df)

# Create a difference score: after − before
anorexia_df$difference <- anorexia_df$Weight_after - anorexia_df$Weight_before
head(anorexia_df)

require(lsr)

# Approach A: One-sample t-test on the difference scores (μ_diff = 0)
# one.sided = "greater" tests H1: mean(after − before) > 0 (i.e., an increase)

oneSampleTTest(x = anorexia_df$difference, 
               mu = 0, 
               one.sided = "greater")

