require(dplyr)
require(psych)
################################################################################

# Part I


mu0  <- 16.2      # hypothesized mean
xbar <- 15.9     # sample mean
s    <- 0.45        # sample standard deviation
n    <- 25          # sample size

# Step 1: Calculate the test statistic (t-value)

t_stat <- (xbar - mu0) / (s / sqrt(n))

# Step 2: Degrees of freedom

df <- n - 1

# Step 3: Calculate the p-value for a left-tailed test

p_value <- pt(t_stat, df = df)

################################################################################

ecls <- read.csv('./do_not_upload/Assignment1/ecls-k-sub.csv')

describe(ecls$X1HEIGHT)


summary(ecls$X1HEIGHT)
ecls$X1HEIGHT <- recode(ecls$X1HEIGHT, `-9` = NA_real_)
ecls$X1HEIGHT <- recode(ecls$X1HEIGHT, `-8` = NA_real_)
ecls$X1HEIGHT <- recode(ecls$X1HEIGHT, `-7` = NA_real_)
summary(ecls$X1HEIGHT)

oneSampleTTest(x = ecls$X1HEIGHT,
               mu = 45,
               one.sided = "less")

################################################################################

require(lsr)

time <- c(70, 77, 83, 87, 92, 93, 100, 102, 102, 103, 96,
          34, 36, 48, 48, 65, 91, 98, 102)

category <- c(rep("control", 11),
              rep("experiment", 8))

death_df <- data.frame(time, category)

death_df$category <- as.factor(death_df$category)

str(death_df)

independentSamplesTTest(formula   = time ~ category, 
                        data      = death_df,                         
                        var.equal = TRUE,
                        one.sided = "control")


independentSamplesTTest(formula   = time ~ category, 
                        data      = death_df,                         
                        var.equal = FALSE,
                        one.sided = "control")

################################################################################

silent <- c(6, 7, 7, 5, 4, 4, 6, 9, 5, 7)
noisy <-  c(9, 9, 6, 7, 6, 7, 9, 11, 7, 9)
mistakes_df <- data.frame(silent, noisy)

mistakes_df$difference <- mistakes_df$silent - mistakes_df$noisy

oneSampleTTest(x = mistakes_df$difference,
               mu = 0,
               one.sided = "less")


################################################################################

ecls <- read.csv('./do_not_upload/Assignment1/ecls-k-sub.csv')

table(ecls$X1FIRKDG)
ecls$X1FIRKDG <- recode(ecls$X1FIRKDG, `-9` = NA_integer_)
table(ecls$X1FIRKDG)

ecls$X1FIRKDG <- as.factor(ecls$X1FIRKDG)

str(ecls$X1FIRKDG)


describe(ecls$X1RSCALK5)
ecls$X1RSCALK5 <- recode(ecls$X1RSCALK5, `-9` = NA_real_)
describe(ecls$X1RSCALK5)

independentSamplesTTest(formula   = X1RSCALK5 ~ X1FIRKDG, 
                        data      = ecls,                         
                        var.equal = FALSE,
                        one.sided = FALSE)

################################################################################


ecls <- read.csv('./do_not_upload/Assignment1/ecls-k-sub.csv')

summary(ecls$X1ATTNFS)
ecls$X1ATTNFS <- recode(ecls$X1ATTNFS, `-9` = NA_real_)
ecls$X1ATTNFS <- recode(ecls$X1ATTNFS, `-1` = NA_real_)
summary(ecls$X1ATTNFS)

summary(ecls$X2ATTNFS)
ecls$X2ATTNFS <- recode(ecls$X2ATTNFS, `-9` = NA_real_)
ecls$X2ATTNFS <- recode(ecls$X2ATTNFS, `-1` = NA_real_)
summary(ecls$X2ATTNFS)


ecls$growth <- ecls$X2ATTNFS - ecls$X1ATTNFS

oneSampleTTest(x = ecls$growth,
               mu = 0,
               one.sided = "greater")














