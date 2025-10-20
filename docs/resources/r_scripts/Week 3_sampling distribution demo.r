################################################################################
#    Load data
################################################################################


d <- read.csv("./resources/datasets/life_expectancy.csv", stringsAsFactors = FALSE)

# Peek at the data structure (first few rows)

head(d)

################################################################################
# Treat the full datast as the population
################################################################################

# Variable of interest: life_expectancy

# We’ll treat the entire file as the population 
#  (so the “population mean” below is the true mean for our demo).

# Basic population facts

N         <- nrow(d)                                 # population size
N

mu        <- mean(d$life_expectancy, na.rm = TRUE)   # population mean (parameter)
mu

sigma <- sd(d$life_expectancy,  na.rm = TRUE)        # population SD
sigma


# Population distribution

plot(density(d$life_expectancy))
################################################################################
# Take a single random sample
################################################################################

n <- 10             # Sample size

# Sample n indices without replacement from the population

idx_1  <- sample.int(N, n, replace = FALSE)

d_sub1 <- d[idx_1, ]
View(d_sub1)

# Sample distribution

plot(density(d_sub1$life_expectancy))


# The sample mean from this one study

xbar_1 <- mean(d_sub1$life_expectancy, na.rm = TRUE)

xbar_1




# Each time you take a new random sample, the sample mean will wiggle around
# the true population mean.

# A second independent sample of size n

idx_2  <- sample.int(N, n, replace = FALSE)
d_sub2 <- d[idx_2, ]

xbar_2 <- mean(d_sub2$life_expectancy, na.rm = TRUE)

xbar_2

################################################################################
# Build the sampling distribution of the mean by repetition
################################################################################

# Now repeat the “sample-and-compute-mean” process R times.

# This approximates the sampling distribution of the sample mean (x̄) for n = 10.


N <- 180  # Population size

n <- 10   # Sample Size

R <- 10000  # number of repeated samples

sample_means <- replicate(R,
                          {
                            idx   <- sample.int(N, n, replace = TRUE)
                            d_sub <- d[idx, ]
                            mean(d_sub$life_expectancy, na.rm = TRUE)
                            }
                          )

hist(sample_means,
     breaks = 30,
     main = "Sampling Distribution of the Mean (n = 10)",
     xlab  = "Sample Mean (life_expectancy)",
     col   = "gray90", border = "white")



psych::describe(sample_means)



































