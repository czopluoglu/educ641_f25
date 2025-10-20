################################################################################
#    Load data
################################################################################


d <- read.csv("./resources/datasets/life_expectancy.csv", stringsAsFactors = FALSE)

mu        <- mean(d$life_expectancy, na.rm = TRUE)   # population mean (parameter)
mu

sigma <- sd(d$life_expectancy,  na.rm = TRUE)        # population SD
sigma


################################################################################
# Take a single random sample
################################################################################

N <- 180            # Population size
n <- 30             # Sample size

# Sample n indices without replacement from the population

idx_1  <- sample.int(N, n, replace = FALSE)

d_sub1 <- d[idx_1, ]
View(d_sub1)

# The sample mean from this one study

xbar <- mean(d_sub1$life_expectancy, na.rm = TRUE)

xbar

# 95% confidence interval

qnorm(0.025)
qnorm(0.975)

lower_limit <- xbar - 1.96*(sigma/sqrt(n))
upper_limit <- xbar + 1.96*(sigma/sqrt(n))

c(lower_limit,upper_limit)

  # Does this interval include the population mean?

  (mu >= lower_limit) & (mu <= upper_limit)

  # returns "TRUE" if mu lies within the interval, FALSE otherwise.

################################################################################

# Let's repeat this 1000 times

R <- 10000

conf_intervals <- replicate(R,
                          {
                            idx         <- sample.int(N, n, replace = TRUE)
                            d_sub       <- d[idx, ]
                            xbar        <- mean(d_sub$life_expectancy, na.rm = TRUE)
                            lower_limit <- xbar - 1.96*(sigma/sqrt(n))
                            upper_limit <- xbar + 1.96*(sigma/sqrt(n))
                            (mu >= lower_limit) & (mu <= upper_limit)
                            
                          }
)

table(conf_intervals)
