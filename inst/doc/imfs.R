## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## ----minimal-example, eval = TRUE---------------------------------------------
# Minimal executable example
library(gmsp)
library(data.table)
t_vec <- seq(0, 5, by = 0.01)
dt_sig <- data.table(t = t_vec, s = sin(2 * pi * 2 * t_vec) + 0.3 * sin(2 * pi * 8 * t_vec))
imf_res <- TS2IMF(dt_sig, method = "vmd", K = 4, output = "IMF")
print(imf_res)

