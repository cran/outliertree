## ---- include = FALSE---------------------------------------------------------
### Don't overload CRAN servers
### https://stackoverflow.com/questions/28961431/computationally-heavy-r-vignettes
is_check <- ("CheckExEnv" %in% search()) || any(c("_R_CHECK_TIMINGS_",
             "_R_CHECK_LICENSE_") %in% names(Sys.getenv()))

## ----message=FALSE------------------------------------------------------------
library(magrittr)
library(dplyr)
library(kableExtra)
library(outliertree)
data("titanic")

titanic %>%
	head(5) %>%
	kable() %>%
	kable_styling()

## -----------------------------------------------------------------------------
## Capitalize column names and some values for easier reading
capitalize <- function(x) gsub("^(\\w)", "\\U\\1\\E", x, perl=TRUE)
titanic %<>%
	rename_all(capitalize) %>%
	mutate(Sex = capitalize(Sex)) %>%
	rename(SibSp = Sibsp)

## Convert 'survived' to yes/no for easier reading
titanic %<>%
	mutate(Survived = recode(Survived, `1`="Yes", `0`="No"))

## Some columns are not useful, such as name (an ID), ticket number (another ID),
## or destination (too many values, many non-repeated)
titanic %<>%
	select(-Name, -Ticket, -Home.dest)

## Ordinal columns need to be passed as ordered factors
cols_ord <- c("Pclass", "Parch", "SibSp")
titanic %<>%
	mutate_at(.vars=cols_ord, function(.) factor(., ordered = TRUE))

## A look at the processed data
titanic %>%
	head(5) %>%
	kable() %>%
	kable_styling()

## ---- eval=FALSE--------------------------------------------------------------
#  library(outliertree)
#  
#  ## Fit model with default hyperparameters
#  otree <- outlier.tree(titanic)
#  otree

## ---- echo=FALSE, comment=NA--------------------------------------------------
library(outliertree)

## Fit model with default hyperparameters
otree <- outlier.tree(titanic, nthreads=1)
otree

## -----------------------------------------------------------------------------
## Double-check the data (last 2 outliers)
titanic[c(1147, 1164), ]

## -----------------------------------------------------------------------------
## Distribution of the group from which those two outliers were flagged
titanic %>%
	filter(Pclass == 3,
		   SibSp == 0,
		   Embarked == "Q") %$%
	Fare %>%
	hist(breaks = 100, col = "navy", xlab="Fare",
		 main="Distribution of Fare within cluster")

## ----comment=NA---------------------------------------------------------------
## Get the outliers in a manipulable format
predict(otree, titanic, outliers_print = 0)[[1147]]

## ----comment=NA---------------------------------------------------------------
## To programatically get all the outliers that were flagged
pred <- predict(otree, titanic, outliers_print = 0)
only_flagged <- pred[!is.na(sapply(pred, function(x) x$outlier_score))]

## ----comment=NA---------------------------------------------------------------
## To print selected rows only
print(pred, only_these_rows = 1147)

## ---- eval=FALSE--------------------------------------------------------------
#  ## In order to flag more outliers, one can also experiment
#  ## with lowering the threshold hyperparameters
#  outlier.tree(titanic, z_outlier = 6., outliers_print = 5)

## ---- echo=FALSE, comment=NA--------------------------------------------------
## In order to flag more outliers, one can also experiment
## with lowering the threshold hyperparameters
outlier.tree(titanic, z_outlier = 6., outliers_print = 5, nthreads=1)

## ---- eval=FALSE--------------------------------------------------------------
#  ## One can also lower the gain threshold, but this tends
#  ## to result in more spurious outliers which come from
#  ## not-so-good splits (not recommended)
#  outlier.tree(titanic, z_outlier = 6., min_gain = 1e-6, outliers_print = 5)

## ---- echo=FALSE, comment=NA--------------------------------------------------
## One can also lower the gain threshold, but this tends
## to result in more spurious outliers which come from
## not-so-good splits (not recommended)
outlier.tree(titanic, z_outlier = 6., min_gain = 1e-6, outliers_print = 5, nthreads=1)

