## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## ----minimal-example, eval = TRUE---------------------------------------------
# Minimal executable example
library(gmsp)
library(data.table)
t_vec <- seq(0, 10, by = 0.01)
tsl <- data.table(RSN = "R1", OCID = "H1", ID = "AT",
                  t = t_vec, s = 500 * sin(2 * pi * 2 * t_vec))
im <- TSL2IM(tsl, units.source = "mm", units.target = "mm")
head(im)

## ----wide-output, eval = TRUE-------------------------------------------------
imw <- TSL2IM(tsl, units.source = "mm", output = "IMW")
names(imw)[1:min(6, length(names(imw)))]

