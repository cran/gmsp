## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4)

## ----libs---------------------------------------------------------------------
library(gmsp)
library(data.table)

## ----make-input---------------------------------------------------------------
NP <- 1000L
dt <- 1 / 200
t  <- seq.int(0, by = dt, length.out = NP)

env <- exp(-((t - 2.5) / 1.5)^2)

DT <- data.table(
  t  = t,
  H1 = env * (3.0 * sin(2 * pi *  5 * t) + 0.6 * sin(2 * pi * 20 * t)),
  H2 = env * (2.0 * sin(2 * pi *  7 * t) + 0.4 * sin(2 * pi * 17 * t))
)

str(DT)

## ----at2ts--------------------------------------------------------------------
TSL <- AT2TS(
  DT,
  units.source = "mm",
  units.target = "mm",
  Fmax        = 25,
  output      = "TSL",
  audit       = FALSE,
  verbose     = FALSE
)

TSL[, .N, by = .(ID, OCID)]
head(TSL[ID == "VT" & OCID == "H1"], 6)

## ----plot-at2ts, fig.alt = "AT, VT and DT for channel H1"---------------------
op <- par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))
plot(TSL[ID == "AT" & OCID == "H1", .(t, s)], type = "l",
     main = "AT (mm/s^2)", xlab = "", ylab = "")
plot(TSL[ID == "VT" & OCID == "H1", .(t, s)], type = "l",
     main = "VT (mm/s)",   xlab = "", ylab = "")
plot(TSL[ID == "DT" & OCID == "H1", .(t, s)], type = "l",
     main = "DT (mm)",     xlab = "t [s]", ylab = "")
par(op)

## ----intensity----------------------------------------------------------------
TSL[, RSN := "demo"]
IM <- getIntensity(TSL, units.source = "mm", units.target = "mm")

dcast(IM[IM %in% c("PGA", "PGV", "PGD", "AI", "D0595", "CAV")],
      OCID + IM ~ ., value.var = "value")

## ----ts2ps, fig.alt = "Pseudo-acceleration response spectrum"-----------------
Tn  <- 10 ^ seq(log10(0.05), log10(3), length.out = 60)

PS <- TSL2PS(TSL[OCID == "H1"], xi = 0.05, Tn = Tn, output = "PSL")

head(PS)

plot(PS[ID == "PSA", .(Tn, S)], log = "x",
     type = "l", lwd = 2,
     xlab = "Tn [s]", ylab = "PSA [mm/s^2]",
     main = "5%-damped pseudo-acceleration spectrum (H1)")

## ----imf, fig.alt = "First two IMFs of the AT H1 signal"----------------------
AT_H1 <- TSL[ID == "AT" & OCID == "H1", .(t, s)]
IMFs  <- TS2IMF(AT_H1, method = "vmd", K = 6, output = "TSW")

names(IMFs)
head(IMFs[, 1:5])

op <- par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(IMFs$t, IMFs$IMF1, type = "l",
     main = "IMF1", xlab = "", ylab = "")
plot(IMFs$t, IMFs$IMF2, type = "l",
     main = "IMF2", xlab = "t [s]", ylab = "")
par(op)

