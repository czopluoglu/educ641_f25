require(dplyr)
require(psych)
################################################################################

ecls <- read.csv('./do_not_upload/Assignment1/ecls-k-sub.csv')

summary(ecls$X1RSCALK5)
ecls$X1RSCALK5 <- recode(ecls$X1RSCALK5, `-9` = NA_real_)
summary(ecls$X1RSCALK5)

summary(ecls$X2RSCALK5)
ecls$X2RSCALK5 <- recode(ecls$X2RSCALK5, `-9` = NA_real_)
summary(ecls$X2RSCALK5)

summary(ecls$X1MSCALK5)
ecls$X1MSCALK5 <- recode(ecls$X1MSCALK5, `-9` = NA_real_)
summary(ecls$X1MSCALK5)

summary(ecls$X2MSCALK5)
ecls$X2MSCALK5 <- recode(ecls$X2MSCALK5, `-9` = NA_real_)
summary(ecls$X2MSCALK5)

summary(ecls$X12LANGST)
ecls$X12LANGST <- recode(.x = ecls$X12LANGST, 
                         `-9` = NA_integer_)
summary(ecls$X12LANGST)


describe(ecls$X1RSCALK5)
describe(ecls$X2RSCALK5)
describe(ecls$X1MSCALK5)
describe(ecls$X2MSCALK5)
################################################################################
# Fall Reading histogram
hist(x      = ecls$X1RSCALK5,
     freq   = TRUE,
     col    = 'white',
     border = 'black',
     xlab   = 'Reading Score',
     main   = 'Fall Reading Scores')

# Spring Reading Histogram
hist(x      = ecls$X2RSCALK5,
     freq   = TRUE,
     col    = 'white',
     border = 'black',
     xlab   = 'Reading Score',
     main   = 'Spring Reading Scores')

# Fall Math Histogram
hist(x      = ecls$X1MSCALK5,
     freq   = TRUE,
     col    = 'white',
     border = 'black',
     xlab   = 'Math Scores',
     main   = 'Fall Math Scores')

# Spring Math Histogram
hist(x      = ecls$X2MSCALK5,
     freq   = TRUE,
     col    = 'white',
     border = 'black',
     xlab   = 'Math Scores',
     main   = 'Spring Math Scores')
################################################################################
# Fall Reading Box Plot
boxplot(x       = ecls$X1RSCALK5,
        main    = 'Fall Reading Scores',
        xlab    = 'Reading Score',
        col     = 'white',
        horizontal = TRUE)

# Spring Reading Box Plot
boxplot(x       = ecls$X2RSCALK5,
        main    = 'Spring Reading Scores',
        xlab    = 'Reading Score',
        col     = 'white',
        horizontal = TRUE)

# Fall Math Box Plot
boxplot(x       = ecls$X1MSCALK5,
        main    = 'Fall Math Scores',
        xlab    = 'Math Score',
        col     = 'white',
        horizontal = TRUE)

# Spring Math Box Plot
boxplot(x       = ecls$X2MSCALK5,
        main    = 'Spring Math Scores',
        xlab    = 'Math Score',
        col     = 'white',
        horizontal = TRUE)
################################################################################


describeBy(x = ecls$X1RSCALK5, group = ecls$X12LANGST)


# Create a boxplot of Fall Reading IRT scores by home language status

boxplot(X1RSCALK5 ~ X12LANGST, 
        data = ecls,
        main = "Fall Kindergarten Reading Scores by Home Language Status",
        xlab = "Home Language Status",
        ylab = "Reading IRT Scale Score",
        names = c("Non-English", "English", "Undetermined"),
        col = "lightblue")

