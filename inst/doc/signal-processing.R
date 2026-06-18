## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## ----minimal-example, eval = TRUE---------------------------------------------
# Minimal executable example
library(gmsp)
library(data.table)
dt_acc <- data.table(t = seq(0, 5, by = 0.01), H1 = sin(2 * pi * 2 * seq(0, 5, by = 0.01)))
tsl <- AT2TS(dt_acc, units.source = "mm", Fmax = 10, audit = FALSE, output = "TSL")
head(tsl)

## ----tsl-tsw-converters, eval = TRUE------------------------------------------
tsw <- TSL2TSW(tsl)
roundtrip <- TSW2TSL(tsw)
head(roundtrip)

