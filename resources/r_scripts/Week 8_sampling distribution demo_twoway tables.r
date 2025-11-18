?r2dtable


# Simulate a single 2x2 table under H0 (fixed margins)
tab <- r2dtable(
  n = 1,
  r = c(2445, 2445),
  c = c(4480, 410)
)[[1]]

tab  # a random 2×2 table with fixed margins

# Expected counts under H0
E11 <- 2240
E12 <- 205
E21 <- 2240
E22 <- 205

# Chi-square components for the single table
r11 <- (tab[1,1] - E11)^2 / E11
r12 <- (tab[1,2] - E12)^2 / E12
r21 <- (tab[2,1] - E21)^2 / E21
r22 <- (tab[2,2] - E22)^2 / E22

# Discrepancy statistic for this table
r11 + r12 + r21 + r22

################################################################################

r <- replicate(10000, {
  
  # Generate a 2×2 table under the null of independence
  tab <- r2dtable(
    n = 1, 
    r = c(2445, 2445),
    c = c(4480, 410)
  )[[1]]
  
  # Expected frequencies under H0
  E11 <- 2240
  E12 <- 205
  E21 <- 2240
  E22 <- 205
  
  # Discrepancy components
  r11 <- (tab[1,1] - E11)^2 / E11
  r12 <- (tab[1,2] - E12)^2 / E12
  r21 <- (tab[2,1] - E21)^2 / E21
  r22 <- (tab[2,2] - E22)^2 / E22
  
  # Return the chi-square statistic
  r11 + r12 + r21 + r22
})

hist(r, main = "Empirical null distribution of discrepancy statistic")













