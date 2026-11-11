library(gmsp)
library(data.table)

.checkProcessModalInterval <- function() {
  Namespace <- getOption("gmsp.testNamespace", asNamespace("gmsp"))
  t <- (0:999) * 0.001
  Grid <- Namespace$.regularizeSeries(t, cos(2 * pi * 2 * t))
  Signal <- data.table(ProcessID = "TEST", RecordID = "R1", OwnerID = "A", EventID = "E1",
    StationID = "S1", OCID = "H1", ID = "AT", t = Grid$t, s = as.vector(Grid$s))
  Params <- Namespace$.processIMFParams(list(method = "vmd", k = 2, alpha = 2000,
    tau = 0, DC = TRUE, init = 0, tolerance = 1e-6, maxIterations = 98))
  Used <- Namespace$.translateControl("vmd", Params$control, Grid$samplingInterval, NULL)
  Saved <- Namespace$.processEffectiveControls(list(signal = Signal), Params)[[1L]]
  stopifnot(identical(Saved$samplingInterval, Grid$samplingInterval),
            identical(Saved$control$alpha, Used$control$alpha))
}
.checkProcessModalInterval()
