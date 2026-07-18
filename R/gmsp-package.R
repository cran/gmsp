#' gmsp: Ground Motion Signal Processing
#'
#' Tools for processing strong-motion records: STFT-based pipeline for
#' consistent acceleration/velocity/displacement time series, IMF
#' decomposition (EMD/EEMD/VMD), elastic SDOF response spectra, intensity
#' measures (PGA, PGV, AI, CAV, duration metrics), and parsers for common
#' strong-motion record formats (PEER NGA-W2 AT2, CESMD V2/V2c, two-column
#' ASCII) and helpers to index parsed records into per-record CSV/JSON pairs
#' and a master record catalog.
#'
#' @keywords internal
#' @name gmsp-package
#' @aliases gmsp
#' @importFrom stats setNames
#' @importFrom utils tail tar untar
"_PACKAGE"

## ---- Package-wide constants -------------------------------------------------

#' Canonical base unit written by [extractRecord()] into `raw/AT.<RID>.csv`.
#' All signal samples and PGA values reported in the sidecar JSON are in
#' this unit. Records whose provider unit cannot be parsed or scaled are
#' skipped by [extractRecord()].
#'
#' @noRd
TARGET_UNITS <- "mm"

## ---- globalVariables --------------------------------------------------------
## All NSE bindings (data.table columns, parser fields, index-table column
## names) collected here so that R CMD check does not flag them as undefined.
utils::globalVariables(c(
  ## data.table internals
  ".", ".N", ".SD", ":=", "i", "N",
  "..COLS.event", "..COLS.station", "..KEEP",

  ## time-series core
  "t", "s", "ts", "value", "variable", "DATA", "offset",
  "AT", "ATo", "ATn", "AZC", "ARMS",
  "VT", "VTo", "VTn", "VZC", "VRMS",
  "DT", "DTo", "DTn", "DZC", "DRMS", "Dmax",
  "X", "Y",

  ## channels / identifiers
  "OCID", "RSN", "ID", "DIR", "isUP", "H1", "H2", "UP",

  ## intensity measures
  "PGA", "PGV", "PGD", "IA", "IAu", "IAd",
  "CAV5", "D0575", "D0595", "D2080",
  "EPI", "PDI", "PPI", "TmA", "Tm", "fm", "IM", "units",

  ## spectra tables
  "Tn", "S", "xi",

  ## IMF
  "IMF", "NIMF", "NIT", "namp", "residue", "signal.R",

  ## misc signal-processing
  "Scaled", "RecordSN", "CRC32",
  "Wo", "WoA", "WoI", "var", "Verbose",

  ## DB indexing -- identifiers
  "ComponentID", "FileID", "Fs", "NP",
  "OwnerID", "EventID", "StationID", "RecordID",
  "NetworkID", "Reason", "MagnitudeType",
  "Repi", "Rhyp",

  ## DB indexing -- event/station scalars
  "EventDepth", "EventLatitude", "EventLongitude",
  "EventMagnitude", "EventMagnitudeType", "EventMechanism",
  "EventTectonicRegion", "EventTimeUTC", "EventMagnitude.source",
  "EventDepth.ISC", "EventDepth.USGS", "EventDepth.owner",
  "EventLatitude.ISC", "EventLatitude.USGS", "EventLatitude.owner",
  "EventLongitude.ISC", "EventLongitude.USGS", "EventLongitude.owner",
  "EventMagnitude.ISC", "EventMagnitude.USGS", "EventMagnitude.owner",
  "EventMagnitudeType.ISC", "EventMagnitudeType.USGS",
  "EventMagnitudeType.owner",
  "EventMechanism.STREC", "EventMechanism.owner",
  "EventTectonicRegion.STREC", "EventTectonicRegion.owner",
  "EventTimeUTC.ISC", "EventTimeUTC.USGS", "EventTimeUTC.owner",
  "StationLatitude", "StationLongitude", "StationVs30",

  ## DB indexing -- helpers
  "V1", "as.data.table", "fifelse", "idx", "isArray", "key", "RowCount",
  "set", "uniqueN", "XiOrder"
))
