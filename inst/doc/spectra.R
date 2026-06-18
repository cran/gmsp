## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## ----minimal-example, eval = TRUE---------------------------------------------
# Minimal executable example
library(gmsp)
library(data.table)
t_vec <- seq(0, 20, by = 0.01)
dt_acc <- data.table(
  t = t_vec,
  H1 = 500 * sin(2 * pi * 1 * t_vec) * exp(-0.1 * t_vec),
  H2 = 300 * cos(2 * pi * 1.5 * t_vec) * exp(-0.1 * t_vec),
  UP = 100 * sin(2 * pi * 0.8 * t_vec) * exp(-0.1 * t_vec)
)
tsl <- AT2TS(dt_acc, units.source = "mm", isRaw = FALSE,
             output = "TSL", audit = FALSE)
ps <- TSL2PS(tsl[OCID == "H1"], xi = 0.05,
             Tn = c(0.1, 0.2, 0.5, 1.0, 2.0),
             output = "PSL")
head(ps)

## ----spectra-shape-converters, eval = TRUE------------------------------------
psw <- PSL2PSW(ps)
psl_again <- PSW2PSL(psw)
head(psl_again)

## ----vector-xi-example, eval = TRUE-------------------------------------------
ps_xi <- TSL2PS(tsl[OCID == "H1"], xi = c(0.02, 0.05),
                Tn = c(0.1, 0.2), output = "PSW")
head(ps_xi)

## ----d50-example, eval = TRUE-------------------------------------------------
t_d50 <- seq(0, 1, by = 0.01)
at_wide <- data.table(
  t = t_d50,
  H1 = 10 * sin(2 * pi * 3 * t_d50),
  H2 = 7 * cos(2 * pi * 4 * t_d50),
  UP = 3 * sin(2 * pi * 2 * t_d50)
)

tsl <- AT2TS(at_wide, units.source = "mm", isRaw = FALSE,
             output = "TSL", audit = FALSE)
rot_psl <- TSL2PS(tsl, Tn = c(0.1, 0.2),
                  output = "PSL", D50 = TRUE, D100 = TRUE, nTheta = 12L)
rot_psl[OCID %chin% c("D50", "D100")]

