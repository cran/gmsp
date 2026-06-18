#' Compute intensity measures from a canonical long time-series table.
#'
#' @description
#' Given a long time-series table with columns for the record identifier,
#' `OCID`, `ID`, `t`, and `s`, converts amplitudes to `units.target` and
#' computes standard intensity measures grouped by record x OCID x ID.
#' Valid `ID` content is either `AT` only, or a complete time-series set with
#' `AT`, `VT`, and `DT`.
#'
#' @param .x `data.table`. Long-format TSL with at least `OCID`, `ID`, `t`,
#'   `s` columns plus optional record-identifier metadata columns.
#' @param units.source character. Source units of the `s` column
#'   (`"mm"`, `"cm"`, `"m"`, `"gal"`, `"g"`).
#' @param units.target character. Target units for the returned intensities.
#'   Default `"mm"`.
#' @param output character. `"IML"` returns long intensity rows; `"IMW"`
#'   returns one row per metadata and `OCID`, with intensity measures as
#'   columns.
#'
#' @return A `data.table`. `output = "IML"` returns long rows with columns
#'   `<metadata cols>, OCID, ID, IM, value, units`; `output = "IMW"` returns
#'   wide intensity columns.
#' @examples
#' t <- seq(0, 1, by = 0.01)
#' tsl <- data.table::rbindlist(list(
#'   data.table::data.table(t = t, s = sin(2 * pi * t),
#'                          ID = "AT", OCID = "H1"),
#'   data.table::data.table(t = t, s = cos(2 * pi * t),
#'                          ID = "VT", OCID = "H1"),
#'   data.table::data.table(t = t, s = sin(pi * t),
#'                          ID = "DT", OCID = "H1")
#' ))
#' im <- TSL2IM(tsl, units.source = "mm")
#' head(im)
#'
#' @export
#' @import data.table
#' @importFrom data.table := data.table is.data.table rbindlist copy setcolorder
TSL2IM <- function(.x, units.source, units.target = "mm",
                   output = c("IML", "IMW")) {
  .tsl2im(.x = .x, units.source = units.source, units.target = units.target,
          output = output, label = "TSL2IM")
}

#' Compute intensity measures from a long time-series table.
#'
#' `getIntensity()` is a compatibility wrapper around [TSL2IM()]. It accepts
#' canonical long `TSL` input only; convert wide `TSW` input with [TSW2TSL()]
#' first.
#'
#' @inheritParams TSL2IM
#' @return See [TSL2IM()].
#' @examples
#' t <- seq(0, 1, by = 0.01)
#' tsl <- data.table::data.table(t = t, s = sin(2 * pi * t),
#'                               ID = "AT", OCID = "H1")
#' getIntensity(tsl, units.source = "mm")
#'
#' @export
getIntensity <- function(.x, units.source, units.target = "mm",
                         output = c("IML", "IMW")) {
  .tsl2im(.x = .x, units.source = units.source, units.target = units.target,
          output = output, label = "getIntensity")
}

.tsl2im <- function(.x, units.source, units.target = "mm",
                    output = c("IML", "IMW"), label = "TSL2IM") {

  # Canonical length units only (see R/validateUnits.R). TSL carries
  # AT/VT/DT rows sharing one length base; passing "g"/"gal" or verbose
  # forms (e.g. "mm/s") would silently misapply the scale to non-AT rows.
  units.source <- .validateUnits(units.source, kind = "DT")
  units.target <- .validateUnits(units.target, kind = "DT")
  output <- match.arg(output)
  Label <- paste0(label, "()")

  DT <- copy(as.data.table(.x))
  REQ <- c("OCID", "ID", "t", "s")
  MISS <- setdiff(REQ, names(DT))
  if (length(MISS)) {
    stop(sprintf("%s missing required TSL columns: %s",
                 Label,
                 paste(MISS, collapse = ", ")), call. = FALSE)
  }
  IDS <- sort(unique(as.character(DT$ID)))
  if (!(identical(IDS, "AT") || identical(IDS, c("AT", "DT", "VT")))) {
    stop(sprintf(
      "%s expects ID to contain either AT only or complete AT/VT/DT; got: %s",
      Label, paste(IDS, collapse = ", ")
    ), call. = FALSE)
  }

  Sf <- .getSF(SourceUnits = units.source, TargetUnits = units.target)
  stopifnot(is.numeric(Sf), length(Sf) == 1L)
  DT[, s := s * Sf]

  g <- .getG(units.target)

  CANDS <- setdiff(names(DT), c("t", "s", "ID", "OCID"))
  TYPES <- vapply(CANDS, function(nm) class(DT[[nm]])[1L], character(1L))
  COLS <- c(CANDS[TYPES %chin% c("character", "factor", "integer")],
            "OCID", "ID")
  emptyIM <- function() {
    OUT <- DT[0, COLS, with = FALSE]
    OUT[, `:=`(IM = character(), value = numeric(), units = character())]
    OUT[]
  }

  # ── Acceleration ─────────────────────────────────────────────────────────
  AT    <- DT[ID == "AT"]
  IA    <- AT[, .(IM = "AI",    value = .getAI(s, t, g),                               units = paste(units.target, "/s")),            by = COLS]
  IAu   <- AT[, .(IM = "AIu",   value = .getAI(pmax(s, 0), t, g),                      units = paste(units.target, "/s")),            by = COLS]
  IAd   <- AT[, .(IM = "AId",   value = .getAI(pmin(s, 0), t, g),                      units = paste(units.target, "/s")),            by = COLS]
  PGA   <- AT[, .(IM = "PGA",   value = .getPeak(s),                                    units = paste(units.target, "/s2")),           by = COLS]
  ARMS  <- AT[, .(IM = "ARMS",  value = .getRMS(s),                                     units = paste(units.target, "/s2")),           by = COLS]
  AZC   <- AT[, .(IM = "AZC",   value = .getZC(s),                                      units = "-"),                                by = COLS]
  ATo   <- AT[, .(IM = "ATo",   value = first(s),                                       units = paste(units.target, "/s2")),           by = COLS]
  ATn   <- AT[, .(IM = "ATn",   value = last(s),                                        units = paste(units.target, "/s2")),           by = COLS]
  D0595 <- AT[, .(IM = "D0595", value = .getHBD(s, t, a = 0.05, b = 0.95, g = g),     units = "s"),                                by = COLS]
  D2080 <- AT[, .(IM = "D2080", value = .getHBD(s, t, a = 0.20, b = 0.80, g = g),     units = "s"),                                by = COLS]
  D0575 <- AT[, .(IM = "D0575", value = .getHBD(s, t, a = 0.05, b = 0.75, g = g),     units = "s"),                                by = COLS]
  TmA   <- AT[, .(IM = "TmA",   value = .getTm(s, t, fmin = 0.1, fmax = 25),           units = "s"),                                by = COLS]
  NP    <- AT[, .(IM = "NP",    value = .N,                                              units = "samples"),                          by = COLS]
  dt    <- AT[, .(IM = "dt",    value = mean(t - shift(t), na.rm = TRUE),               units = "s"),                                by = COLS]
  Fs    <- dt[, c(.SD, .(IM = "Fs", value = 1 / value, units = "Hz")), .SDcols = COLS]
  Dmax  <- AT[, .(IM = "Dmax",  value = last(t),                                        units = "s"),                                by = COLS]

  thr  <- 0.05 * g
  CAV  <- AT[, .(IM = "CAV",  value = sum(abs(s)) * mean(t - shift(t), na.rm = TRUE),                   units = paste(units.target, "/s")), by = COLS]
  CAV5 <- AT[, .(IM = "CAV5", value = sum(abs(s) * (abs(s) >= thr)) * mean(t - shift(t), na.rm = TRUE), units = paste(units.target, "/s")), by = COLS]

  AUX  <- IA[, c(.SD, .(IA = value)),    .SDcols = COLS][D0595[, c(.SD, .(D0595 = value)), .SDcols = COLS], on = COLS]
  EPI  <- AUX[, c(.SD, .(IM = "EPI",  value = 0.9 / pi * IA * 2 * g * D0595, units = paste0(units.target, "^2/s^2"))), .SDcols = COLS]

  AUX  <- AZC[, c(.SD, .(AZC = value)),  .SDcols = COLS][
              Dmax[, c(.SD, .(Dmax = value)), .SDcols = COLS], on = COLS][
              IA[,   c(.SD, .(IA   = value)), .SDcols = COLS], on = COLS]
  PDI  <- AUX[, c(.SD, .(IM = "PDI", value = IA * (Dmax / AZC)^2, units = paste0(units.target, ".s"))), .SDcols = COLS]

  IM.AT <- rbindlist(list(IA, IAu, IAd, PGA, ARMS, AZC, ATo, ATn, D0595, D2080, D0575, TmA, CAV, CAV5, NP, dt, Fs, Dmax, EPI, PDI), use.names = TRUE)

  # ── Velocity ─────────────────────────────────────────────────────────────
  VT    <- DT[ID == "VT"]
  IM.VT <- emptyIM()
  if (nrow(VT)) {
    PGV   <- VT[, .(IM = "PGV",  value = .getPeak(s), units = paste(units.target, "/s")),  by = COLS]
    VRMS  <- VT[, .(IM = "VRMS", value = .getRMS(s),  units = paste(units.target, "/s")),  by = COLS]
    VZC   <- VT[, .(IM = "VZC",  value = .getZC(s),   units = "-"),                        by = COLS]
    VTo   <- VT[, .(IM = "VTo",  value = first(s),    units = paste(units.target, "/s")),  by = COLS]
    VTn   <- VT[, .(IM = "VTn",  value = last(s),     units = paste(units.target, "/s")),  by = COLS]
    TmV   <- VT[, .(IM = "TmV",  value = .getTm(s, t, fmin = 0.1, fmax = 25), units = "s"), by = COLS]
    IM.VT <- rbindlist(list(PGV, VRMS, VZC, VTo, VTn, TmV), use.names = TRUE)
  }

  # ── Displacement ─────────────────────────────────────────────────────────
  Dt    <- DT[ID == "DT"]
  IM.DT <- emptyIM()
  if (nrow(Dt)) {
    PGD   <- Dt[, .(IM = "PGD",  value = .getPeak(s), units = units.target), by = COLS]
    DRMS  <- Dt[, .(IM = "DRMS", value = .getRMS(s),  units = units.target), by = COLS]
    DZC   <- Dt[, .(IM = "DZC",  value = .getZC(s),   units = "-"),          by = COLS]
    DTo   <- Dt[, .(IM = "DTo",  value = first(s),    units = units.target), by = COLS]
    DTn   <- Dt[, .(IM = "DTn",  value = last(s),     units = units.target), by = COLS]
    TmD   <- Dt[, .(IM = "TmD",  value = .getTm(s, t, fmin = 0.1, fmax = 25), units = "s"), by = COLS]
    IM.DT <- rbindlist(list(PGD, DRMS, DZC, DTo, DTn, TmD), use.names = TRUE)
  }

  IML <- list(IM.AT = IM.AT, IM.VT = IM.VT, IM.DT = IM.DT)
  SCHEMA <- lapply(IML, function(D) {
    paste(names(D),
          vapply(D, function(col) paste(class(col), collapse = "/"), character(1L)),
          sep = ":")
  })
  SAME <- vapply(SCHEMA, identical, logical(1L), SCHEMA[[1L]])
  if (!all(SAME)) {
    stop(sprintf("%s internal IM schema mismatch before binding: %s",
                 Label, paste(names(SAME)[!SAME], collapse = ", ")),
         call. = FALSE)
  }

  OUT <- rbindlist(IML, use.names = TRUE)
  setcolorder(OUT, c(COLS, "IM", "value", "units"))
  if (identical(output, "IML")) return(OUT[])
  IML2IMW(OUT)
}
