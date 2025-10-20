################################################################################
# EDUC 641 — Week 3–4 Support Script
# Topics: Normal distribution, pnorm / qnorm, z-scores,
#         one-sample z-test, and confidence intervals
################################################################################

# (Optional) make printed numbers friendly for class display
options(digits = 5, scipen = 99)

?pnorm
?qnorm

# pnorm(x, mean, sd, lower.tail):  P(X <= x)

# qnorm(p, mean, sd, lower.tail):  x such that P(X <= x) = p

################################################################################
# PRACTICE QUESTION 1, Week , Slide 21
# Exam scores ~ Normal(μ = 120, σ = 17). A student scored X = 150.
################################################################################

mu    <- 120
sigma <- 17
X     <- 150

# (a) Percentage below 150 using the raw distribution

pnorm(q          = X, 
      mean       = mu, 
      sd         = sigma, 
      lower.tail = TRUE)


# (b) Convert to z-score

z <- (X - mu) / sigma

z

# show the equivalence of (a) via standard normal

pnorm(q          = z, 
      mean       = 0, 
      sd         = 1, 
      lower.tail = TRUE)


################################################################################
# PRACTICE QUESTION 2, Week 3, Slide 22
# Exam scores ~ Normal(μ = 120, σ = 17).
# Cut score so that only 10% PASS (i.e., top 10%).
# If “pass” = X >= cut, then P(X <= cut) = 0.90 (90th percentile).
################################################################################


mu    <- 120
sigma <- 17

qnorm(p          = 0.9, 
      mean       = mu, 
      sd         = sigma, 
      lower.tail = TRUE)


################################################################################
# Section 2. One-sample z-test (mechanics first), then with BSDA::z.test
################################################################################


# Week 4, Slide 16 - 24

#   Reference (typical children):   μ0 = 50, σ = 10
#   Observed sample (n = 166):      Xbar = 55.71  (from your dataset)

d         <- read.csv("./resources/datasets/ysr.csv")

Xbar      <- mean(d$ysr)
Xbar

n         <- length(d$ysr)
n

mu0       <- 50
sigma_pop <- 10

# Standard error under H0 (σ known): σ_Xbar = σ / sqrt(n)

SE <- sigma_pop / sqrt(n)
SE

# One-tailed test (greater): H0: μ = 50 vs. HA: μ > 50

z_stat <- (Xbar - mu0) / SE
z_stat

pnorm(z_stat, 
      mean = 0, 
      sd = 1, 
      lower.tail = FALSE)



# Note that these numbers are slightly different in the lecture slides
# because, Xbar is rounded to 55.71 in class example

# Two-tailed version, H0: μ = 50 vs. HA: μ is not equal to 50

2 * pnorm(z_stat,mean = 0, sd = 1, lower.tail = FALSE)

################################################################################
# Confidence Interval with known σ
#  (1 - α)100% CI:  Xbar ± z_{α/2} * (σ / sqrt(n))
################################################################################

alpha  <- 0.05

z_star <- qnorm(1 - alpha/2)  # 1.96 for 95%
z_star

# 95% Confidence Interval

CI     <- c(Xbar - z_star*SE, Xbar + z_star*SE)
CI


# BONUS ACTIVITY:
# Construct 90% Confidence Interval instead of 95% Confidence Interval


################################################################################
# Optional: Using BSDA::z.test to confirm our manual claculations
################################################################################


# install.packages("BSDA")

library(BSDA)

mu0       <- 50
sigma_pop <- 10


# One-tailed test

fit_one_tailed <- z.test(x           = d$ysr,
                         mu          = mu0,
                         sigma.x     = sigma_pop,
                         alternative = "greater",
                         conf.level  = 0.95)

  # Sample Mean estimate
  
  fit_one_tailed$estimate
  
  # Z-statistic
  
  fit_one_tailed$statistic
  
  # p-value 
  
  fit_one_tailed$p.value


# Two-tailed test

fit_two_tailed <- z.test(x           = d$ysr,
                         mu          = mu0,
                         sigma.x     = sigma_pop,
                         alternative = "two.sided",
                         conf.level  = 0.95)

  # Sample Mean estimate
  
  fit_two_tailed$estimate
  
  # Z-statistic
  
  fit_two_tailed$statistic
  
  # p-value 
  
  fit_two_tailed$p.value