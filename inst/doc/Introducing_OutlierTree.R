## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = ""
)
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(outliertree)
data(hypothyroid)
summary(hypothyroid)

## -----------------------------------------------------------------------------
otree <- outlier.tree(hypothyroid, nthreads=1)

## ---- fig.height=4, fig.width=5-----------------------------------------------
pregnant <- hypothyroid[hypothyroid$pregnant,]
hist(pregnant$age, breaks=50, col="navy",
     main="Age distribution among pregnant patients",
     xlab="Age")

## ---- fig.height=4, fig.width=5-----------------------------------------------
non.hyperthyr <- hypothyroid[!hypothyroid$query.hyperthyroid,]
hist(non.hyperthyr$T3, breaks=50, col="darkred",
     main="T3 hormone levels\n(Non-hyperthyroidal patients)",
     xlab="T3 blood concentration")

## -----------------------------------------------------------------------------
outliers <- predict(otree, hypothyroid, outliers_print=FALSE)
outliers[1:700]

## -----------------------------------------------------------------------------
outliers[1138]

## -----------------------------------------------------------------------------
outliers[[1138]]

