#' Arias-intensity ratio history from canonical acceleration time series.
#'
#' `TSL2AI()` computes the cumulative Arias-intensity history from
#' acceleration rows in a canonical `TSL` table and returns the relative
#' value `AI(t) / max(AI)`.
#'
#' @param .x `data.table`. Long-format `TSL` with at least `OCID`, `ID`, `t`,
#'   and `s` columns plus optional metadata columns. Only rows with
#'   `ID == "AT"` are used.
#' @param units.source character. Source length units of the `s` column
#'   (`"mm"`, `"cm"`, or `"m"`). Canonical `TSL` acceleration is interpreted
#'   as length units per second squared.
#' @param units.target character. Target length units used for intermediate
#'   calculations. Default `"mm"`.
#' @param output character. `"wide"` returns one row per metadata and `t`, with
#'   one ratio column per `OCID`; `"long"` returns columns
#'   `<metadata>, OCID, t, ratio`.
#'
#' @return A `data.table`. Values are ratios in `[0, 1]` when the final
#'   cumulative value is positive; otherwise ratios are `NA_real_`.
#' @examples
#' t <- seq(0, 1, by = 0.01)
#' tsl <- data.table::data.table(RecordID = "R1", OCID = "H1",
#'                               ID = "AT", t = t,
#'                               s = 1000 * sin(2 * pi * t))
#' TSL2AI(tsl, units.source = "mm")
#'
#' @export
#' @import data.table
TSL2AI <- function(.x, units.source, units.target = "mm",
                   output = c("wide", "long")) {
  .tsl2Ratio(
    .x = .x,
    units.source = units.source,
    units.target = units.target,
    output = output,
    label = "TSL2AI",
    fun = function(a, t, g) .getAIc(a = a, t = t, g = g)
  )
}

#' CAV ratio history from canonical acceleration time series.
#'
#' `TSL2CAV()` computes the cumulative absolute velocity history from
#' acceleration rows in a canonical `TSL` table and returns the relative
#' value `CAV(t) / max(CAV)`.
#'
#' @inheritParams TSL2AI
#' @return See [TSL2AI()].
#' @examples
#' t <- seq(0, 1, by = 0.01)
#' tsl <- data.table::data.table(RecordID = "R1", OCID = "H1",
#'                               ID = "AT", t = t,
#'                               s = 1000 * sin(2 * pi * t))
#' TSL2CAV(tsl, units.source = "mm")
#'
#' @export
TSL2CAV <- function(.x, units.source, units.target = "mm",
                    output = c("wide", "long")) {
  .tsl2Ratio(
    .x = .x,
    units.source = units.source,
    units.target = units.target,
    output = output,
    label = "TSL2CAV",
    fun = function(a, t, g) .getCAVc(a = a, t = t)
  )
}

#' CAV5 ratio history from canonical acceleration time series.
#'
#' `TSL2CAV5()` computes the cumulative absolute velocity ratio using only
#' acceleration samples with absolute value at least `0.05 g`.
#'
#' @inheritParams TSL2AI
#' @return See [TSL2AI()].
#' @examples
#' t <- seq(0, 1, by = 0.01)
#' tsl <- data.table::data.table(RecordID = "R1", OCID = "H1",
#'                               ID = "AT", t = t,
#'                               s = 1000 * sin(2 * pi * t))
#' TSL2CAV5(tsl, units.source = "mm")
#'
#' @export
TSL2CAV5 <- function(.x, units.source, units.target = "mm",
                     output = c("wide", "long")) {
  .tsl2Ratio(
    .x = .x,
    units.source = units.source,
    units.target = units.target,
    output = output,
    label = "TSL2CAV5",
    fun = function(a, t, g) .getCAV5c(a = a, t = t, g = g)
  )
}

.tsl2Ratio <- function(.x, units.source, units.target, output, label, fun) {
  units.source <- .validateUnits(units.source, kind = "DT")
  units.target <- .validateUnits(units.target, kind = "DT")
  output <- match.arg(output, choices = c("wide", "long"))
  Label <- paste0(label, "()")

  DT <- copy(as.data.table(.x))
  REQ <- c("OCID", "ID", "t", "s")
  .requireTableColumns(DT, REQ, label)

  AT <- DT[ID == "AT"]
  if (!nrow(AT)) {
    stop(sprintf("%s requires at least one row with ID == \"AT\".", Label),
         call. = FALSE)
  }

  CANDS <- setdiff(names(AT), c("t", "s", "ID", "OCID"))
  TYPES <- vapply(CANDS, function(nm) class(AT[[nm]])[1L], character(1L))
  META <- CANDS[TYPES %chin% c("character", "factor", "integer")]
  GROUP <- c(META, "OCID")
  .checkUniqueKeys(AT, c(GROUP, "t"), label)

  Sf <- .getSF(SourceUnits = units.source, TargetUnits = units.target)
  stopifnot(is.numeric(Sf), length(Sf) == 1L)
  AT[, s := s * Sf]
  setorderv(AT, c(GROUP, "t"), na.last = TRUE)

  g <- .getG(units.target)
  LONG <- AT[, {
    Time <- .SD[["t"]]
    Signal <- .SD[["s"]]
    .checkRatioSignal(t = Time, s = Signal, label = Label)
    Value <- fun(a = Signal, t = Time, g = g)
    Ratio <- .relativeByMax(Value)
    data.table(t = Time, ratio = Ratio)
  }, by = GROUP, .SDcols = c("t", "s")]

  if (identical(output, "long")) {
    COLS <- c(META, "OCID", "t", "ratio")
    setcolorder(LONG, COLS)
    setorderv(LONG, COLS[COLS != "ratio"], na.last = TRUE)
    return(LONG[])
  }

  ROW <- c(META, "t")
  FORMULA <- stats::as.formula(paste(paste(ROW, collapse = " + "), "~ OCID"))
  WIDE <- dcast(LONG, FORMULA, value.var = "ratio")
  OCIDS <- intersect(unique(as.character(LONG[["OCID"]])), names(WIDE))
  setcolorder(WIDE, c(ROW, OCIDS, setdiff(names(WIDE), c(ROW, OCIDS))))
  setorderv(WIDE, ROW, na.last = TRUE)
  WIDE[]
}

.relativeByMax <- function(x) {
  Max <- max(x, na.rm = TRUE)
  if (is.finite(Max) && Max != 0) return(x / Max)
  rep(NA_real_, length(x))
}

.checkRatioSignal <- function(t, s, label) {
  if (length(t) < 2L) {
    stop(sprintf("%s requires at least two samples per group.", label),
         call. = FALSE)
  }
  if (any(!is.finite(t)) || any(!is.finite(s))) {
    stop(sprintf("%s requires finite t and s values.", label),
         call. = FALSE)
  }
  if (any(diff(t) <= 0)) {
    stop(sprintf("%s requires strictly increasing t values per group.",
                 label), call. = FALSE)
  }
  TRUE
}

.getAIc <- function(a, t, g) {
  dt <- mean(diff(t))
  cumsum(a^2) * dt * pi / (2 * g)
}

.getCAVc <- function(a, t) {
  dt <- mean(diff(t))
  cumsum(abs(a)) * dt
}

.getCAV5c <- function(a, t, g) {
  dt <- mean(diff(t))
  cumsum(abs(a) * (abs(a) >= 0.05 * g)) * dt
}
