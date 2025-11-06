
d <- read.csv('./resources/datasets/infant_mortality.csv',
              header=TRUE)

View(d)

################################################################################

plot(x = d$doctor,
     y = d$infmor,
     xlab = "Total Number of Doctors (per 1000 inhabitant)",
     ylab = "Under-five mortality rate (per 1000 live births)",
     xlim = c(0,6),
     ylim = c(0,20))

# Same plot with custom axes

plot(x = d$doctor,
     y = d$infmor,
     xlab = "Total Number of Doctors (per 1000 inhabitant)",
     ylab = "Under-five mortality rate (per 1000 live births)",
     xlim = c(0,6),
     ylim = c(0,20),
     axes = FALSE)

axis(side = 1, at = seq(0, 6, by = 1))     # x-axis ticks every 1 unit
axis(side = 2, at = seq(0, 20, by = 2))    # y-axis ticks every 5 units
