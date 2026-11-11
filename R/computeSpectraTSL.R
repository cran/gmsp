# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Compute response spectra from canonical time series
#'
#' Computes PSA, PSV and SD from AT, VT and DT as separate excitations.
#' Each ordinate retains the units of its source domain. The zero-period
#' anchor is the peak absolute value of the regularized source signal.
#'
#' @param TSL A table with \code{OCID,ID,t,s} and the metadata selected by
#'   \code{by}. Each record/component requires AT, VT and DT, with more than
#'   two finite samples per domain and unique times. Time is in seconds.
#' @param by Character vector of record metadata columns, or
#'   \code{character()} for one record without metadata.
#' @param xi Nonempty numeric vector of finite damping ratios in \code{[0,1]}.
#' @param Tn Nonempty numeric vector of finite positive periods in seconds.
#'   Zero is reserved for the internally added peak-value anchor.
#' @param includeD50 Logical scalar selecting the median rotated component.
#' @param includeD100 Logical scalar selecting the maximum rotated component.
#' @param nTheta Number of angles in [0,pi), required when a rotated
#'   component is selected; an integer of at least two.
#' @return A new data.table with \code{by,OCID,Tn,ID,S}, ordered by metadata,
#'   component, spectral ID and period. Vector \code{xi} adds an xi column
#'   after the metadata and preserves the requested damping order within
#'   each record. Repeated periods and damping ratios remain repeated.
#' @details Each domain is regularized independently to the minimum
#'   rationalized time step and a zero time origin. The oscillator starts
#'   from zero state and uses the final sample of each interval as input.
#'   PSV and SD use VT and DT excitations, respectively.
#'
#'   Rotations require H1 and H2 on identical input time grids in each domain.
#'   D50 uses the median with quantile type 7; D100 uses the maximum across
#'   the selected angles, including the peak-value anchor. Original
#'   components remain in the result. Vector xi rejects an input column
#'   named xi. Duplicate sample keys and invalid controls are errors.
#'   The input is copied; the operation uses no random numbers or files and
#'   does not change session options. Units are preserved without conversion.
#' @seealso [widenPSL()], [lengthenPSW()]
#' @examples
#' t <- seq(0, 1, by = 0.01)
#' TSL <- data.table::rbindlist(list(
#'   data.table::data.table(t = t, s = sin(2 * pi * t), ID = "AT", OCID = "H1"),
#'   data.table::data.table(t = t, s = cos(2 * pi * t), ID = "VT", OCID = "H1"),
#'   data.table::data.table(t = t, s = sin(pi * t), ID = "DT", OCID = "H1")))
#' computeSpectraTSL(TSL, by = character(), xi = 0.05, Tn = c(0.1, 0.2))
#' @importFrom data.table as.data.table copy data.table dcast rbindlist
#'   setcolorder setorderv :=
#' @export
computeSpectraTSL <- function(TSL, by, xi, Tn,
                              includeD50 = FALSE, includeD100 = FALSE, nTheta) {
  DT <- copy(as.data.table(TSL))
  .checkTableKeys(DT, by = by, columns = c("OCID", "ID", "t", "s"),
                  keys = c(by, "OCID", "ID", "t"), operation = "computeSpectraTSL")
  if (!is.numeric(xi) || !length(xi) || any(!is.finite(xi)) ||
      any(xi < 0 | xi > 1)) {
    .stopUsage("xi must contain finite damping ratios in [0,1].",
               operation = "computeSpectraTSL")
  }
  if (!is.numeric(Tn) || !length(Tn) || any(!is.finite(Tn)) || any(Tn <= 0)) {
    .stopUsage("Tn must contain finite positive periods; zero is added internally.",
               operation = "computeSpectraTSL")
  }
  for (x in list(includeD50, includeD100)) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      .stopUsage("Rotation selectors must be logical scalars.",
                 operation = "computeSpectraTSL")
    }
  }
  if (includeD50 || includeD100) {
    if (missing(nTheta) || !is.numeric(nTheta) || length(nTheta) != 1L ||
        !is.finite(nTheta) || nTheta < 2 || nTheta != floor(nTheta)) {
      .stopUsage("nTheta must be an integer of at least two for rotations.",
                 operation = "computeSpectraTSL")
    }
  }
  if (length(xi) > 1L && "xi" %in% names(DT)) {
    .stopUsage("Vector xi cannot be used with an input column named xi.",
               operation = "computeSpectraTSL")
  }
  if (!nrow(DT)) .stopUsage("TSL has no samples.", operation = "computeSpectraTSL")
  Domains <- c(AT = "PSA", VT = "PSV", DT = "SD")
  OK <- DT[, .(complete = all(names(Domains) %in% ID)), by = c(by, "OCID")]
  if (!all(OK[["complete"]])) {
    .stopUsage("Each record/component requires AT, VT and DT.",
               operation = "computeSpectraTSL")
  }
  DT <- DT[ID %in% names(Domains)]
  for (x in c("t", "s")) {
    if (!is.numeric(DT[[x]]) || any(!is.finite(DT[[x]]))) {
      .stopUsage(x, " must contain finite numeric values.", operation = "computeSpectraTSL")
    }
  }
  OK <- DT[, .N, by = c(by, "OCID", "ID")]
  if (any(OK[["N"]] <= 2L)) {
    .stopUsage("Each domain requires more than two samples.", operation = "computeSpectraTSL")
  }
  setorderv(DT, c(by, "OCID", "ID", "t"), na.last = TRUE)
  DampingRatios <- xi
  Periods <- Tn
  OUT <- DT[, {
    DATA <- copy(.SD)
    Ratios <- ..DampingRatios
    NaturalPeriods <- ..Periods
    rbindlist(lapply(seq_along(Ratios), function(i) {
      PSL <- DATA[, {
        Grid <- .regularizeSeries(t = t, s = s)
        data.table(Tn = c(0, NaturalPeriods), S = c(max(abs(Grid$s)),
          vapply(NaturalPeriods, function(x) .computeOscillator(Grid$s,
            samplingInterval = Grid$samplingInterval, xi = Ratios[i], Tn = x), numeric(1L))))
      }, by = c("OCID", "ID")]
      PSL[, ID := unname(Domains[ID])]
      if (includeD50 || includeD100) {
        AUX <- DATA[OCID %in% c("H1", "H2"), {
          if (!setequal(OCID, c("H1", "H2"))) {
            .stopUsage("Rotations require H1 and H2 in each domain.",
                       operation = "computeSpectraTSL")
          }
          Pair <- dcast(.SD, t ~ OCID, value.var = "s")
          if (anyNA(Pair)) {
            .stopUsage("Rotations require identical H1/H2 input time grids.",
                       operation = "computeSpectraTSL")
          }
          Grid <- .regularizeSeries(t = Pair[["t"]], s = as.matrix(Pair[, .(H1, H2)]))
          Spectra <- .computeRotatedSpectra(Grid$s, samplingInterval = Grid$samplingInterval,
            xi = Ratios[i], Tn = NaturalPeriods, nTheta = nTheta,
            includeD50 = includeD50, includeD100 = includeD100)
          rbindlist(lapply(names(Spectra), function(x) {
            data.table(OCID = x, Tn = c(0, NaturalPeriods), S = Spectra[[x]])
          }), use.names = TRUE)
        }, by = "ID"]
        if (!nrow(AUX)) {
          .stopUsage("Rotations require H1 and H2 in each domain.",
                     operation = "computeSpectraTSL")
        }
        AUX[, ID := unname(Domains[ID])]
        PSL <- rbindlist(list(PSL, AUX), use.names = TRUE)
      }
      setorderv(PSL, c("OCID", "ID", "Tn"), na.last = TRUE)
      if (length(Ratios) > 1L) PSL[, xi := Ratios[i]]
      PSL
    }), use.names = TRUE)
  }, by = by, .SDcols = c("OCID", "ID", "t", "s")]
  setcolorder(OUT, c(by, if (length(xi) > 1L) "xi", "OCID", "Tn", "ID", "S"))
  OUT[]
}

.computeOscillator <- function(s, samplingInterval, xi, Tn) {
  if (is.null(dim(s))) s <- matrix(s, ncol = 1L)
  Frequency <- 2 * pi / Tn
  Damping <- 2 * xi * Frequency
  Stiffness <- Frequency ^ 2
  StateMatrix <- matrix(c(0, 1, -Stiffness, -Damping), 2L, 2L, byrow = TRUE)
  StepMatrix <- expm::expm(StateMatrix * samplingInterval)
  StepInput <- (StepMatrix - diag(2L)) %*% pracma::pinv(StateMatrix) %*%
    matrix(c(0, 1), 2L, 1L)
  State <- matrix(0, 2L, ncol(s))
  MaxAbs <- numeric(ncol(s))
  for (k in 2L:nrow(s)) {
    State <- StepMatrix %*% State + StepInput %*% s[k, , drop = FALSE]
    MaxAbs <- pmax.int(MaxAbs, abs(State[1L, ]))
  }
  MaxAbs * Stiffness
}

.computeRotatedSpectra <- function(s, samplingInterval, xi, Tn, nTheta,
                                    includeD50, includeD100) {
  Angles <- seq(0, pi, length.out = nTheta + 1L)[seq_len(nTheta)]
  DATA <- outer(s[, 1L], cos(Angles)) + outer(s[, 2L], sin(Angles))
  AUX <- rbind(apply(abs(DATA), 2L, max),
    t(vapply(Tn, function(x) .computeOscillator(DATA,
      samplingInterval = samplingInterval, xi = xi, Tn = x), numeric(ncol(DATA)))))
  OUT <- list()
  if (includeD50) {
    OUT[["D50"]] <- apply(AUX, 1L, stats::quantile, probs = 0.5,
                          type = 7L, names = FALSE, na.rm = FALSE)
  }
  if (includeD100) OUT[["D100"]] <- apply(AUX, 1L, max)
  OUT
}
