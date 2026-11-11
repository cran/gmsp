# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Build acceleration, velocity, and displacement histories
#'
#' `buildTSL()` processes one measured quantity into acceleration (`AT`),
#' velocity (`VT`), and displacement (`DT`) on a common time grid per record.
#' Records are identified by the explicit metadata columns in `by` and their
#' channels by `OCID`.
#'
#' @param TSL A data.table with columns `by`, `OCID`, `ID`, `t`, and `s`.
#'   The call contains one `ID`: `"AT"`, `"VT"`, or `"DT"`. Times `t`
#'   are seconds and samples `s` are finite numeric values. Channels of each
#'   record have identical time grids and unique `by,OCID,ID,t` keys.
#' @param by Character vector of metadata columns identifying a record.
#'   `character()` declares one record without metadata. Only these metadata
#'   columns are retained. A retained `Units` column contains the target
#'   length basis; a conflicting value is an error.
#' @param unitsSource Input length basis when `isRaw = TRUE`: `"mm"`,
#'   `"cm"`, or `"m"`; `AT` also accepts `"g"` and `"gal"`.
#'   Matching is case-insensitive. The quantity determines whether the units
#'   are length/s^2, length/s, or length. `g` uses 9806.650 mm/s^2 and `gal`
#'   means cm/s^2. With `isRaw = FALSE`, this argument is not evaluated.
#' @param unitsTarget Required target length basis: `"mm"`, `"cm"`, or
#'   `"m"`, matched case-insensitively, including with `isRaw = FALSE`.
#' @param fmax Required finite positive frequency of interest in Hz.
#' @param windowLength Required positive integer nominal STFT window length
#'   in samples. The strategy can adjust the length; the initial request is
#'   capped for `AT` and `DT` inputs according to record length.
#'   The effective STFT window must be even.
#' @param overlap Required overlap percentage in `[0,100)`.
#' @param astop0,apass0 Required finite positive normalized
#'   amplitude thresholds for the channel edge tapers.
#' @param astopLP,apassLP Required finite positive
#'   magnitudes for the resampling antialias and derivative low-pass filters.
#'   The controls require astopLP < apassLP < 1.
#' @param kNyq `NULL` selects the automatic sampling-frequency grid. A finite
#'   positive scalar requests `round(kNyq * fmax)` Hz, which must be positive.
#' @param isRaw Logical scalar. Raw input is converted from `unitsSource`,
#'   regularized, and tapered. Processed input is already in `unitsTarget`.
#' @param flatZeros Logical scalar enabling edge tapers for processed input.
#'   For `AT` input it also applies an exact amplitude-support mask.
#' @param trimZeros Logical scalar selecting final nonzero window support.
#'   `AT` retains the union of channel supports; `VT` and `DT` retain their
#'   intersection.
#' @param removeMean Logical scalar enabling removal of channel means at the
#'   processing stages of the measured quantity.
#' @param forceRegularization Logical scalar forcing interpolation of
#'   processed input. Irregular or non-rational grids are always regularized.
#' @param includeWarnings Logical scalar enabling strategy warnings.
#'   Diagnostics do not alter the computed amplitudes.
#' @param derivate Derivative method for `VT` and `DT` inputs: `"freq"`
#'   or `"time"`, matched case-insensitively. It is not evaluated for `AT`.
#' @param lowPass Logical scalar enabling the derivative low-pass with
#'   `derivate = "freq"`. It is not evaluated for `AT`.
#'
#' @return A new data.table with columns `by,OCID,ID,t,s`, ordered by record,
#'   channel, quantity (`AT`, `VT`, `DT`), and time. The common grid starts at
#'   zero for each record. Amplitudes use `unitsTarget`/s^2, `unitsTarget`/s,
#'   and `unitsTarget`, respectively. The input is unchanged.
#'
#' @details
#' Each channel requires at least three samples; derivatives require more
#' than four samples on the grid used by the derivative. Frequency-domain
#' integration and differentiation set the zero-frequency term to zero.
#' Tapering, resampling, and removal of means affect the processed histories.
#' The returned quantities are not exact algebraic inverses of the input.
#'
#' Invalid keys, non-finite samples, incompatible channel grids, unsupported
#' units, and invalid controls cause an error. Errors during record processing
#' carry the record context. The function reads or writes no files, uses no
#' random numbers, and changes no working directory, options, or environment.
#'
#' @seealso [normalizeTSL()], [widenTSL()], [lengthenTSW()]
#' @examples
#' Time <- seq(0, 5, by = 0.01)
#' TSL <- data.table::data.table(
#'   OCID = "H1", ID = "AT", t = Time,
#'   s = 120 * sin(pi * Time / max(Time))^2 * sin(2 * pi * 1.5 * Time)
#' )
#' Histories <- buildTSL(
#'   TSL, by = character(), unitsSource = "mm", unitsTarget = "mm", fmax = 5,
#'   windowLength = 128, overlap = 75,
#'   astop0 = 1e-3, apass0 = 1e-3,
#'   astopLP = 1e-3, apassLP = 0.98,
#'   includeWarnings = FALSE
#' )
#' unique(Histories$ID)
#' @export
buildTSL <- function(TSL, by, unitsSource, unitsTarget, fmax,
                     windowLength, overlap, astop0, apass0,
                     astopLP, apassLP,
                     kNyq = NULL, isRaw = TRUE, flatZeros = FALSE,
                     trimZeros = FALSE, removeMean = FALSE,
                     forceRegularization = FALSE, includeWarnings = TRUE,
                     derivate = "freq", lowPass = TRUE) {
  if (!data.table::is.data.table(TSL)) {
    .stopUsage("TSL must be a data.table.", operation = "buildTSL")
  }
  .checkTableKeys(TSL, by = by, columns = c("OCID", "ID", "t", "s"),
                  keys = c(by, "OCID", "ID", "t"), operation = "buildTSL")
  if (!nrow(TSL)) .stopUsage("TSL has no samples.", operation = "buildTSL")
  for (j in c("t", "s")) {
    if (!is.numeric(TSL[[j]]) || any(!is.finite(TSL[[j]]))) {
      .stopUsage(j, " must contain finite numeric values.", operation = "buildTSL")
    }
  }
  for (j in c("ID", "OCID")) {
    if (!is.character(TSL[[j]]) || anyNA(TSL[[j]]) || any(!nzchar(TSL[[j]]))) {
      .stopUsage(j, " must contain nonempty character values.", operation = "buildTSL")
    }
  }
  Quantity <- unique(TSL[["ID"]])
  if (length(Quantity) != 1L || !Quantity %in% c("AT", "VT", "DT")) {
    .stopUsage("ID must contain exactly one of AT, VT, or DT.", operation = "buildTSL")
  }
  Controls <- list(isRaw = isRaw, flatZeros = flatZeros, trimZeros = trimZeros,
                   removeMean = removeMean, forceRegularization = forceRegularization,
                   includeWarnings = includeWarnings)
  if (Quantity != "AT") Controls <- c(Controls, list(lowPass = lowPass))
  for (j in names(Controls)) {
    x <- Controls[[j]]
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      .stopUsage(j, " must be one logical value.", operation = "buildTSL")
    }
  }
  Controls <- list(fmax = fmax, windowLength = windowLength,
                   astop0 = astop0, apass0 = apass0,
                   astopLP = astopLP,
                   apassLP = apassLP)
  if (!is.null(kNyq)) Controls$kNyq <- kNyq
  for (j in names(Controls)) {
    x <- Controls[[j]]
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
      .stopUsage(j, " must be one finite positive number.", operation = "buildTSL")
    }
  }
  if (windowLength != floor(windowLength)) {
    .stopUsage("windowLength must be an integer.", operation = "buildTSL")
  }
  if (apassLP >= 1 || astopLP >= apassLP) {
    .stopUsage("astopLP must be less than apassLP, which must be less than 1.",
               operation = "buildTSL")
  }
  if (!is.numeric(overlap) || length(overlap) != 1L ||
      !is.finite(overlap) || overlap < 0 || overlap >= 100) {
    .stopUsage("overlap must be in [0,100).", operation = "buildTSL")
  }
  if (!is.null(kNyq) && (!is.finite(kNyq * fmax) || round(kNyq * fmax) <= 0)) {
    .stopUsage("round(kNyq * fmax) must be finite and positive.", operation = "buildTSL")
  }
  if (Quantity != "AT") {
    if (!is.character(derivate) || length(derivate) != 1L || is.na(derivate) ||
        !tolower(derivate) %in% c("freq", "time")) {
      .stopUsage("derivate must be freq or time.", operation = "buildTSL")
    }
    derivate <- tolower(derivate)
  }
  SourceUnits <- unitsTarget
  if (isRaw) {
    if (missing(unitsSource)) {
      .stopUsage("unitsSource is required for raw input.", operation = "buildTSL")
    }
    SourceUnits <- unitsSource
  }
  UnitFactor <- .resolveUnitFactor(unitsSource = SourceUnits, unitsTarget = unitsTarget, ID = Quantity)
  unitsTarget <- tolower(unitsTarget)
  if ("Units" %in% by &&
      (!is.character(TSL[["Units"]]) || anyNA(TSL[["Units"]]) ||
       any(tolower(TSL[["Units"]]) != unitsTarget))) {
    .stopUsage("Units metadata must agree with unitsTarget.", operation = "buildTSL")
  }

  DATA <- data.table::copy(TSL[, c(by, "OCID", "ID", "t", "s"), with = FALSE])
  data.table::setorderv(DATA, c(by, "OCID", "t"), na.last = TRUE)
  LIST <- list(DATA)
  if (length(by)) LIST <- split(DATA, by = by, keep.by = TRUE, sorted = TRUE)
  OUT <- vector("list", length(LIST))
  for (i in seq_along(LIST)) {
    DT <- LIST[[i]]
    Context <- "record without metadata"
    if (length(by)) {
      Context <- paste(paste0(by, "=", vapply(DT[1L, by, with = FALSE],
        as.character, character(1L))), collapse = ", ")
    }
    OUT[[i]] <- tryCatch({
      Channels <- unique(DT[["OCID"]])
      Time <- DT[["t"]][DT[["OCID"]] == Channels[1L]]
      SampleCount <- length(Time)
      if (SampleCount < 3L || any(diff(Time) <= 0)) {
        .stopUsage("Each channel requires at least three increasing times.",
                    operation = "buildTSL")
      }
      if (nrow(DT) != SampleCount * length(Channels) ||
          !identical(DT[["t"]], rep(Time, length(Channels)))) {
        .stopUsage("OCID channels must have identical time grids.", operation = "buildTSL")
      }
      Signal <- matrix(DT[["s"]], nrow = SampleCount,
                       dimnames = list(NULL, Channels))
      OriginalTime <- Time
      OriginalSignal <- Signal[, 1L]
      SamplingInterval <- unique(diff(Time))
      if (isRaw || forceRegularization || length(SamplingInterval) > 1L ||
          !isTRUE(all.equal(SamplingInterval, .rationalizeInterval(SamplingInterval)))) {
        Grid <- .regularizeSeries(t = Time, s = Signal)
        Signal <- Grid$s
        SamplingInterval <- Grid$samplingInterval
      }
      if (!isTRUE(all.equal(1 / SamplingInterval, round(1 / SamplingInterval)))) {
        .stopUsage("The effective sampling frequency must be an integer.",
                    operation = "buildTSL")
      }
      if (UnitFactor != 1) Signal <- Signal * UnitFactor
      if (removeMean) {
        for (j in seq_along(Channels)) Signal[, j] <- Signal[, j] - mean(Signal[, j])
      }
      WindowLength <- windowLength
      if (Quantity != "VT" && WindowLength > SampleCount / 2) {
        LowerLength <- 2^floor(log2(SampleCount / 2))
        UpperLength <- 2^ceiling(log2(SampleCount / 2))
        WindowLength <- min(as.integer(UpperLength), SampleCount)
        if (abs(LowerLength - SampleCount / 2) < abs(UpperLength - SampleCount / 2)) {
          WindowLength <- min(as.integer(LowerLength), SampleCount)
        }
      }
      Strategy <- .assessSTFT(t = OriginalTime, s = OriginalSignal,
        samplingInterval = SamplingInterval, fmax = fmax, kNyq = kNyq,
        windowLength = WindowLength, overlap = overlap,
        astopLP = astopLP,
        apassLP = apassLP, includeWarnings = includeWarnings)
      WindowLength <- Strategy$windowLength
      Overlap <- Strategy$overlap
      if (WindowLength %% 2 != 0) {
        .stopUsage("The effective windowLength must be even.", operation = "buildTSL")
      }
      if (includeWarnings && length(Strategy$warnings)) {
        .warnSignal(Context, ": ", paste(Strategy$warnings, collapse = "\n"),
                    operation = "buildTSL")
      }
      if (Strategy$resample) {
        Signal <- .resampleSeries(s = Signal, samplingInterval = SamplingInterval,
          frequency = as.integer(round(Strategy$frequency)), fmax = fmax,
          overlap = Overlap, astop0 = astopLP,
          apass0 = apassLP)
        SamplingInterval <- 1 / as.integer(round(Strategy$frequency))
      }
      if (Quantity != "AT" && nrow(Signal) <= 4L) {
        .stopUsage("Derivatives require more than four samples.", operation = "buildTSL")
      }
      Histories <- list(AT = Signal, VT = Signal, DT = Signal)
      Weights <- matrix(1, nrow = nrow(Signal), ncol = length(Channels))
      for (j in seq_along(Channels)) {
        x <- Signal[, j]
        w <- rep(1, length(x))
        if (isRaw || flatZeros) {
          Taper <- .buildTaper(s = x, astop0 = astop0,
            apass0 = apass0, flatZeros = flatZeros && Quantity == "AT")
          w <- Taper$weights * Taper$mask
        }
        if (Quantity == "AT") {
          a <- x * w
          v <- .integrateSeries(s = a, samplingInterval = SamplingInterval,
            windowLength = WindowLength, overlap = Overlap) * w
          if (removeMean) v <- v - mean(v)
          d <- .integrateSeries(s = v, samplingInterval = SamplingInterval,
            windowLength = WindowLength, overlap = Overlap) * w
          if (removeMean) {
            d <- d - mean(d)
            a <- a - mean(a)
            v <- v - mean(v)
            d <- d - mean(d)
          }
        }
        if (Quantity == "VT") {
          v <- x * w
          a <- .differentiateSeries(s = v, samplingInterval = SamplingInterval,
            method = derivate, windowLength = WindowLength, overlap = Overlap,
            fmax = fmax, lowPass = lowPass, astop0 = astopLP,
            apass0 = apassLP)
          if (removeMean) a <- a - mean(a)
          d <- .integrateSeries(s = v, samplingInterval = SamplingInterval,
            windowLength = WindowLength, overlap = Overlap) * w
          if (removeMean) d <- d - mean(d)
        }
        if (Quantity == "DT") {
          d <- x * w
          if (removeMean) d <- d - mean(d)
          v <- .differentiateSeries(s = d, samplingInterval = SamplingInterval,
            method = derivate, windowLength = WindowLength, overlap = Overlap,
            fmax = fmax, lowPass = lowPass, astop0 = astopLP,
            apass0 = apassLP)
          if (removeMean) v <- v - mean(v)
          a <- .differentiateSeries(s = v, samplingInterval = SamplingInterval,
            method = derivate, windowLength = WindowLength, overlap = Overlap,
            fmax = fmax, lowPass = lowPass, astop0 = astopLP,
            apass0 = apassLP)
          if (removeMean) a <- a - mean(a)
        }
        if (isRaw || flatZeros) {
          Taper <- .buildTaper(s = a, astop0 = astop0,
            apass0 = apass0, flatZeros = flatZeros && Quantity == "AT")
          w <- Taper$weights * Taper$mask
        }
        Histories$AT[, j] <- a * w
        Histories$VT[, j] <- v * w
        Histories$DT[, j] <- d * w
        Weights[, j] <- w
      }
      MASK <- rep(TRUE, nrow(Signal))
      if (trimZeros && Quantity == "AT") MASK <- rowSums(Weights != 0) > 0L
      if (trimZeros && Quantity != "AT") MASK <- rowSums(Weights != 0) == length(Channels)
      if (!any(MASK)) .stopUsage("Trimming removed every sample.", operation = "buildTSL")
      Histories <- lapply(Histories, function(x) x[MASK, , drop = FALSE])
      if (removeMean && Quantity == "DT") {
        for (k in names(Histories)) {
          for (j in seq_along(Channels)) {
            Histories[[k]][, j] <- Histories[[k]][, j] - mean(Histories[[k]][, j])
          }
        }
      }
      if (any(!vapply(Histories, function(x) all(is.finite(x)), logical(1L)))) {
        stop("The derived histories contain non-finite samples.", call. = FALSE)
      }
      Time <- seq(0, SamplingInterval * (nrow(Histories$AT) - 1L), SamplingInterval)
      AUX <- data.table::rbindlist(lapply(seq_along(Channels), function(j) {
        data.table::data.table(OCID = Channels[j],
          ID = rep(c("AT", "VT", "DT"), each = length(Time)), t = rep(Time, 3L),
          s = c(Histories$AT[, j], Histories$VT[, j], Histories$DT[, j]))
      }), use.names = TRUE)
      for (j in by) data.table::set(AUX, j = j, value = DT[[j]][1L])
      data.table::setcolorder(AUX, c(by, "OCID", "ID", "t", "s"))
      AUX
    }, error = function(e) {
      if (!inherits(e, "moduleFailure")) {
        Cause <- e
        class(e) <- unique(c("gmspFailure", "moduleFailure", class(e)))
        if (is.null(e$cause)) e$cause <- Cause
      }
      e$message <- paste0(Context, ": ", conditionMessage(e))
      e$unit <- Context
      e$operation <- "buildTSL"
      stop(e)
    })
  }
  data.table::rbindlist(OUT, use.names = TRUE)
}
