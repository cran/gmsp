# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.crossTime <- function(t, s, fraction) {
  if (fraction <= 0) return(min(t))
  if (fraction >= 1) return(max(t))
  IDX <- which(is.finite(s) & s >= fraction)
  if (!length(IDX)) return(NA_real_)
  t[IDX[1L]]
}

.buildTrimWindow <- function(t, s, range, taper, astop, apass) {
  if (!is.numeric(t) || length(t) < 2L || any(!is.finite(t)) || any(diff(t) <= 0)) {
    .stopUsage("Window times must be finite and increasing.", operation = ".trimTSL")
  }
  if (!is.matrix(s) || !is.numeric(s) || nrow(s) != length(t) || ncol(s) == 0L) {
    .stopUsage("Cumulative histories must form an aligned matrix.", operation = ".trimTSL")
  }
  if (!is.numeric(range) || length(range) != 2L || any(!is.finite(range)) ||
      range[1L] < 0 || range[2L] > 1 || range[1L] >= range[2L]) {
    .stopUsage("range must contain two increasing values in [0,1].", operation = ".trimTSL")
  }
  if (!is.numeric(taper) || length(taper) != 1L || !is.finite(taper) || taper < 0) {
    .stopUsage("taper must be a finite nonnegative number of seconds.", operation = ".trimTSL")
  }
  for (x in list(astop, apass)) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
      .stopUsage("Ramp amplitudes must be finite scalars.", operation = ".trimTSL")
    }
  }
  if (astop <= 0 || astop >= apass || apass >= 1) {
    .stopUsage("Ramp amplitudes must satisfy 0 < astop < apass < 1.", operation = ".trimTSL")
  }
  Limits <- vapply(seq_len(ncol(s)), function(j) {
    a <- .crossTime(t, s[, j], fraction = range[1L])
    b <- .crossTime(t, s[, j], fraction = range[2L])
    if (!is.finite(a)) a <- min(t)
    if (!is.finite(b)) b <- max(t)
    c(a, b)
  }, numeric(2L))
  Start <- min(Limits[1L, ])
  End <- max(Limits[2L, ])
  if (Start >= End) .stopUsage("Invalid trim window.", operation = ".trimTSL")
  StartIndex <- which(t >= Start)[1L]
  EndIndex <- utils::tail(which(t <= End), 1L)
  if (StartIndex >= EndIndex) .stopUsage("Invalid trim indices.", operation = ".trimTSL")
  n <- length(t)
  w <- rep(1, n)
  if (range[1L] > 0) {
    IDX <- which(t <= Start + taper)
    PassIndex <- if (length(IDX)) utils::tail(IDX, 1L) else StartIndex
    AUX <- rep(1, n)
    if (PassIndex > StartIndex && PassIndex <= n) {
      AUX <- .computeButterworth(seq_len(n), stopFrequency = StartIndex,
        passFrequency = PassIndex, astop0 = astop, apass0 = apass,
        highPass = TRUE)
      AUX[seq.int(PassIndex, n)] <- 1
    }
    AUX[seq_len(StartIndex)] <- 0
    w <- w * AUX
  }
  if (range[2L] < 1) {
    IDX <- which(t <= End - taper)
    PassIndex <- if (length(IDX)) utils::tail(IDX, 1L) else StartIndex
    AUX <- rep(1, n)
    if (PassIndex >= 1L && EndIndex > PassIndex) {
      AUX <- .computeButterworth(seq_len(n), stopFrequency = EndIndex,
        passFrequency = PassIndex, astop0 = astop, apass0 = apass,
        highPass = FALSE)
      AUX[seq_len(PassIndex)] <- 1
    }
    AUX[seq.int(EndIndex, n)] <- 0
    w <- w * AUX
  }
  w[t < Start | t > End] <- 0
  list(weights = w, t.start = Start, t.end = End,
       i.start = StartIndex, i.end = EndIndex)
}

.trimTSL <- function(TSL, by, ID, OCID, range, taper, astop, apass,
                     fmax, kNyq, unitsTarget, windowLength, overlap,
                     astop0, apass0, astopLP,
                     apassLP) {
  DT <- data.table::copy(data.table::as.data.table(TSL))
  .checkTableKeys(DT, by = by, columns = c("OCID", "ID", "t", "s"),
    keys = c(by, "OCID", "ID", "t"), operation = ".trimTSL")
  if (!is.character(ID) || length(ID) != 1L || is.na(ID) ||
      !ID %in% c("AI", "CAV", "CAV5")) {
    .stopUsage("ID must be AI, CAV or CAV5.", operation = ".trimTSL")
  }
  if (!is.character(OCID) || !length(OCID) || anyNA(OCID) ||
      any(!nzchar(OCID)) || anyDuplicated(OCID)) {
    .stopUsage("OCID must name distinct components.", operation = ".trimTSL")
  }
  data.table::setorderv(DT, c(by, "OCID", "t"), na.last = TRUE)
  LIST <- list(DT)
  if (length(by)) LIST <- split(DT, by = by, keep.by = TRUE, sorted = TRUE)
  Histories <- Windows <- vector("list", length(LIST))
  for (i in seq_along(LIST)) {
    DT <- LIST[[i]]
    Context <- "record without metadata"
    if (length(by)) Context <- paste(paste0(by, "=", vapply(
      DT[1L, by, with = FALSE], as.character, character(1L))), collapse = ", ")
    OUT <- tryCatch({
      IDX <- DT[["ID"]] == "AT"
      AT <- DT[IDX]
      Channels <- unique(AT[["OCID"]])
      if (!length(Channels) || length(setdiff(OCID, Channels))) {
        .stopUsage("AT is missing a requested component.", operation = ".trimTSL")
      }
      if (!is.numeric(AT[["t"]]) || !is.numeric(AT[["s"]]) ||
          any(!is.finite(AT[["t"]])) || any(!is.finite(AT[["s"]]))) {
        .stopUsage("AT times and amplitudes must be finite numeric values.", operation = ".trimTSL")
      }
      Time <- AT[["t"]][AT[["OCID"]] == Channels[1L]]
      if (length(Time) < 3L || any(diff(Time) <= 0) ||
          !identical(AT[["t"]], rep(Time, length(Channels)))) {
        .stopUsage("AT components must share an increasing time grid.", operation = ".trimTSL")
      }
      IDX <- DT[["ID"]] == ID
      Cumulative <- DT[IDX]
      if (!nrow(Cumulative)) {
        Cumulative <- accumulateIntensityTSL(AT, by = character(), ID = ID,
          unitsSource = unitsTarget, unitsTarget = unitsTarget)
      }
      s <- vapply(OCID, function(x) {
        IDX <- Cumulative[["OCID"]] == x
        AUX <- Cumulative[IDX]
        if (!identical(AUX[["t"]], Time) || !is.numeric(AUX[["s"]])) {
          .stopUsage("Cumulative components must align with AT.", operation = ".trimTSL")
        }
        AUX[["s"]]
      }, numeric(length(Time)))
      Window <- .buildTrimWindow(Time, s, range = range, taper = taper,
                                astop = astop, apass = apass)
      data.table::set(AT, j = "s", value = AT[["s"]] * rep(Window$weights, length(Channels)))
      IDX <- AT[["t"]] >= Window$t.start & AT[["t"]] <= Window$t.end
      AT <- AT[IDX]
      TSL <- buildTSL(AT, by = by, unitsTarget = unitsTarget, fmax = fmax,
        kNyq = kNyq, windowLength = windowLength, overlap = overlap,
        astop0 = astop0, apass0 = apass0,
        astopLP = astopLP,
        apassLP = apassLP, isRaw = FALSE,
        flatZeros = FALSE, trimZeros = FALSE, removeMean = FALSE,
        forceRegularization = FALSE, includeWarnings = FALSE)
      if (Window$t.start != 0) data.table::set(TSL, j = "t", value = TSL[["t"]] + Window$t.start)
      WINDOW <- data.table::data.table(ID = ID, range.start = range[1L], range.end = range[2L],
        t.start = Window$t.start, t.end = Window$t.end,
        i.start = Window$i.start, i.end = Window$i.end)
      for (j in by) data.table::set(WINDOW, j = j, value = DT[[j]][1L])
      data.table::setcolorder(WINDOW, c(by, "ID", "range.start", "range.end",
        "t.start", "t.end", "i.start", "i.end"))
      list(TSL = TSL, WINDOW = WINDOW)
    }, error = function(e) {
      e$message <- paste0(Context, ": ", conditionMessage(e))
      e$unit <- Context
      e$operation <- ".trimTSL"
      stop(e)
    })
    Histories[[i]] <- OUT$TSL
    Windows[[i]] <- OUT$WINDOW
  }
  list(TSL = data.table::rbindlist(Histories, use.names = TRUE),
       WINDOW = data.table::rbindlist(Windows, use.names = TRUE))
}
