#' STFT strategy selection (window, overlap, resampling)
#'
#' @description
#' Helper that chooses STFT parameters, and whether to resample, given `NP`, `Fs`
#' and optionally `Fmax`/`kNyq`, aiming at good bin alignment and sufficient
#' windows.
#'
#' @param NP integer. Number of samples.
#' @param Fs numeric. Sampling frequency (Hz).
#' @param Fmax numeric or `NULL`. Max frequency of interest (Hz).
#' @param kNyq numeric. Target Nyquist multiplier
#'   (`Fs_target ~= kNyq * Fmax`).
#' @param NW.min integer. Minimum window size. Default: `128`.
#' @param MW.min integer. Minimum number of windows. Default: `16`.
#' @param OVLP numeric. Window overlap percent. Default: `75`.
#' @param UPF.max integer. Maximum upsampling factor. Default: `16`.
#'
#' @return A list with nine fields, all populated on every branch:
#'   `NW` (integer, chosen window length), `MW` (integer, number of
#'   STFT windows the input supports), `OVLP` (numeric, overlap
#'   percent passed through), `Fs` (numeric, target sampling rate
#'   after the proposed resample), `UPF` (integer, upsampling factor
#'   relative to the input rate; `1` for no upsampling), three logical
#'   flags (`Resample`, `Upsample`, `Downsample`) summarising the
#'   transform direction, and `strategy` (character label describing
#'   which branch produced the result, e.g. `"no_resample"`,
#'   `"upsample"`, `"downsample"`).
#' @noRd
.setSTFT <- function(NP, Fs, Fmax = NULL, kNyq = 3.125,
                     NW.min = 128, MW.min = 16, OVLP = 75, UPF.max = 16) {

  if (is.null(Fmax)) {
    # ========================================================================
    # MODO 1: OPTIMIZACIÓN PARA NP DADO (SIN RESAMPLE)
    # ========================================================================
    # Encuentra los parámetros STFT óptimos para el NP actual sin cambiar la señal

    # Calcular el NW máximo posible que permita al menos MW.min ventanas
    hop <- NW.min * (100 - OVLP) / 100

    if (NP >= NW.min + (MW.min - 1) * hop) {
      # Señal suficiente para parámetros mínimos deseados
      NW <- NW.min
      MW <- floor((NP - NW) / hop) + 1

      return(list(
        NW = NW,
        MW = MW,
        OVLP = OVLP,
        Fs = Fs,
        UPF = 1,
        downsample_factor = 1,
        Resample = FALSE,
        Upsample = FALSE,
        Downsample = FALSE,
        strategy = "optimal_for_given_NP"
      ))

    } else {
      # Señal insuficiente: optimizar NW para maximizar ventanas
      # Resolver: NP = NW + (MW-1) * NW * (100-OVLP)/100 
      # NP = NW * (1 + (MW-1) * (100-OVLP)/100)

      # Para MW.min ventanas, NW máximo sería:
      NW <- floor(NP / (1 + (MW.min - 1) * (100 - OVLP) / 100))

      # Si aún no alcanza mínimos, reducir MW
      if (NW < 64) {  # 64 = mínimo absoluto para resolución frecuencial
        # Buscar combinación NW=64, MW ajustado
        NW <- 64
        hop <- NW * (100 - OVLP) / 100
        MW <- max(3, floor((NP - NW) / hop) + 1)  # Mínimo 3 ventanas en modo sin Fmax

        return(list(
          NW = NW,
          MW = MW,
          OVLP = OVLP,
          Fs = Fs,
          UPF = 1,
          downsample_factor = 1,
          Resample = FALSE,
          Upsample = FALSE,
          Downsample = FALSE,
          strategy = "minimal_parameters_for_NP"
        ))

      } else {
        # Usar NW máximo posible con MW.min
        NW <- max(64, 2^floor(log2(NW)))  # Potencia de 2 más cercana
        hop <- NW * (100 - OVLP) / 100
        MW <- floor((NP - NW) / hop) + 1

        return(list(
          NW = NW,
          MW = MW,
          OVLP = OVLP,
          Fs = Fs,
          UPF = 1,
          downsample_factor = 1,
          Resample = FALSE,
          Upsample = FALSE,
          Downsample = FALSE,
          strategy = "optimized_for_short_signal"
        ))
      }
    }

  } else {
    # ========================================================================
    # MODO 2: Optimización STFT para Fmax (elige FsTarget y NW para minimizar leakage)
    # ========================================================================
    # Objetivo: escoger (FsC, NWc) que minimicen L = |frac(Fmax/df)| con df = FsC/NWc
    # Sujeto a: MW(FsC,NWc) >= MW.min (ventanas suficientes) y límites prácticos
    # Nota: sin introducir filtros externos; todo dentro del framework STFT

    # Helpers locales
    fracpart <- function(x) abs(x - round(x))
    # estima NP after resample
    estNP <- function(NP, F1, F2) max(8L, floor(NP * (F2 / F1)))
    # calcula MW dado NP y NW
    getMW <- function(NP, NW, OV) {
      HOP <- NW * (100 - OV) / 100
      if (HOP <= 0) return(0L)
      as.integer(floor((NP - NW) / HOP) + 1)
    }

    # Candidatos de Fs
    # Si el usuario pasó kNyq explícitamente, forzar Fs_target = round(kNyq * Fmax).
    # En caso contrario, modo automático (candidatos múltiples).
    if (!missing(kNyq)) {
      Fs.grid <- unique(as.integer(round(kNyq * Fmax)))
    } else {
      Fs.grid <- unique(as.integer(round(c(
        Fs,
        c(max(2.5, kNyq), 4, 5, 6, 8, 10) * Fmax
      ))))
    }
    # Limitar upsample según UPF.max
    Fs.grid <- Fs.grid[Fs.grid <= as.integer(Fs * max(1, UPF.max))]
    Fs.grid <- Fs.grid[Fs.grid > 0]

    # Candidatos de NW: potencias de 2 alrededor de NW.min
    NW.grid <- sort(unique(pmax(64L, c(NW.min, 2^floor(log2(NW.min)), 2^ceiling(log2(NW.min)),
      2L * NW.min, 4L * NW.min))))

    BEST <- NULL
    Jbest <- Inf

    # Opcional: registro de candidatos cuando Verbose=TRUE
    CAND <- list()
    ci <- 0L
    OK <- .verbose()

    if (OK) {
      cat("=== setSTFT candidates (pre-selection) ===\n")
      cat(sprintf("Mode: %s | Fs_in=%.1f Hz | Fmax=%.1f Hz\n", if (!missing(kNyq)) "forced_kNyq" else "auto", Fs, Fmax))
      cat(sprintf("Fs.grid: %s\n", paste(Fs.grid, collapse = ", ")))
      cat(sprintf("NW.grid: %s\n", paste(NW.grid, collapse = ", ")))
    }

    for (Fs.try in Fs.grid) {
      NPe <- estNP(NP, Fs, Fs.try)
      # límite superior NW para al menos 4 ventanas
      NW <- max(64L, as.integer(floor(NPe / 4)))
      for (NW.try in NW.grid) {
        if (NW.try > NW) next
        MW <- getMW(NPe, NW.try, OVLP)
        if (MW < MW.min) next
        df <- Fs.try / NW.try
        L <- fracpart(Fmax / df)
        # métrica compuesta: leakage + penalización por cambiar Fs (favorecer downsample)
        if (missing(kNyq)) {
          penalty <- if (Fs.try > Fs) 0.08 else 0.02
        } else {
          penalty <- 0.0
        }
        J <- L + penalty * abs(Fs.try - Fs) / max(Fs, 1)

        if (OK) {
          ci <- ci + 1L
          CAND[[ci]] <- list(Fs.try = as.integer(Fs.try), NW.try = as.integer(NW.try), MW = as.integer(MW), df = df, leakage = L, J = J)
        }

        if (J < Jbest - 1e-12) {
          Jbest <- J
          BEST <- list(Fs = as.integer(Fs.try), NW = as.integer(NW.try), MW = as.integer(MW), leakage = L)
        }
      }
    }

    if (OK && length(CAND) > 0) {
      DF <- data.table::rbindlist(CAND)
      ord <- order(DF$J, DF$leakage)
      DF <- DF[ord, , drop = FALSE]
      nshow <- min(20L, nrow(DF))
      cat(sprintf("Top %d candidates by J (leakage then penalty):\n", nshow))
      # Print header
      cat(sprintf("%8s %8s %6s %8s %12s %12s\n", "Fs.try", "NW", "MW", "df", "leakage", "J"))
      for (i in seq_len(nshow)) {
        cat(sprintf("%8d %8d %6d %8.3f %12.5e %12.5e\n", DF$Fs.try[i], DF$NW.try[i], DF$MW[i], DF$df[i], DF$leakage[i], DF$J[i]))
      }
    }

    # Fallback si no se encontró candidato
    if (is.null(BEST)) {
      # Si el usuario pasó kNyq explícitamente, forzar Fs_target y elegir NW/MW viables
      if (!missing(kNyq)) {
        Fs.forced <- unique(as.integer(round(kNyq * Fmax)))
        NPe <- estNP(NP, Fs, Fs.forced)
        # Elegir NW que no exceda NPe y permita algunas ventanas (>= 3 si es posible)
        NW <- max(16L, as.integer(floor(NPe / 3)))
        if (NW < 16L) NW <- max(8L, as.integer(floor(NPe / 2)))
        NW.try <- max(16L, min(NW.min, NW))
        # Ajustar a potencia de 2 razonable
        NW.try <- as.integer(2^floor(log2(max(16L, min(NW.try, max(16L, NPe - 1))))))
        MW <- getMW(NPe, NW.try, OVLP)
        ResampleFlag <- (Fs.forced != Fs)
        DownFlag <- (Fs.forced < Fs)
        UpFlag <- (Fs.forced > Fs)
if (.verbose()) {
        cat("=== setSTFT (forced kNyq fallback) ===\n")
        cat(sprintf("Fs_in=%.1f Fs_out=%.1f NW=%d MW=%d OVLP=%.0f df=%.3f leakage~%.3e\n",
          Fs, Fs.forced, NW.try, MW, OVLP, Fs.forced / max(1L, NW.try), abs((Fmax / (Fs.forced / max(1L, NW.try))) - round(Fmax / (Fs.forced / max(1L, NW.try))))) )
          cat(sprintf("Resample=%s Upsample=%s Downsample=%s\n",
            ResampleFlag, UpFlag, DownFlag))
          cat("strategy=forced_knyq_fallback\n")
        }
        return(list(
          NW = as.integer(max(16L, NW.try)),
          MW = as.integer(max(1L, MW)),
          OVLP = OVLP,
          Fs = as.integer(Fs.forced),
          UPF = if (UpFlag) ceiling(Fs.forced / Fs) else 1,
          downsample_factor = if (DownFlag) (Fs / max(1, Fs.forced)) else 1,
          Resample = ResampleFlag,
          Upsample = UpFlag,
          Downsample = DownFlag,
          strategy = "forced_knyq_fallback"
        ))
      } else {
        # Modo automático: mantener configuración actual
        return(list(
          NW = NW.min,
          MW = max(3L, getMW(NP, NW.min, OVLP)),
          OVLP = OVLP,
          Fs = Fs,
          UPF = 1,
          downsample_factor = 1,
          Resample = FALSE,
          Upsample = FALSE,
          Downsample = FALSE,
          strategy = "fallback_no_candidate"
        ))
      }
    }

    # Decide using original input Fs and optimized Fs
    Fs.in <- as.integer(Fs)
    Fs.out <- as.integer(BEST$Fs)
    NW <- as.integer(BEST$NW)
    MW <- as.integer(BEST$MW)

    if (Fs.out == Fs.in) {
if (.verbose()) {
        cat("=== setSTFT (optimized) ===\n")
        cat(sprintf("Fs_in=%.1f Fs_out=%.1f NW=%d MW=%d OVLP=%.0f df=%.3f leakage=%.3e\n",
          Fs.in, Fs.out, NW, MW, OVLP, Fs.out / NW, abs((Fmax / (Fs.out / NW)) - round(Fmax / (Fs.out / NW)))))
        cat("Resample=FALSE Upsample=FALSE Downsample=FALSE\n")
        cat("strategy=optimized_min_leakage_no_resample\n")
      }
      return(list(
        NW = NW,
        MW = MW,
        OVLP = OVLP,
        Fs = Fs.out,
        UPF = 1,
        downsample_factor = 1,
        Resample = FALSE,
        Upsample = FALSE,
        Downsample = FALSE,
        strategy = "optimized_min_leakage_no_resample"
      ))
    }

    if (Fs.out < Fs.in) {
if (.verbose()) {
        cat("=== setSTFT (optimized) ===\n")
        cat(sprintf("Fs_in=%.1f Fs_out=%.1f NW=%d MW=%d OVLP=%.0f df=%.3f leakage=%.3e\n",
          Fs.in, Fs.out, NW, MW, OVLP, Fs.out / NW, abs((Fmax / (Fs.out / NW)) - round(Fmax / (Fs.out / NW)))))
        cat(sprintf("Resample=TRUE Upsample=FALSE Downsample=TRUE factor=%.3f\n", Fs.in / Fs.out))
        cat("strategy=optimized_min_leakage_downsample\n")
      }
      return(list(
        NW = NW,
        MW = MW,
        OVLP = OVLP,
        Fs = Fs.out,
        UPF = 1,
        downsample_factor = Fs.in / Fs.out,
        Resample = TRUE,
        Upsample = FALSE,
        Downsample = TRUE,
        strategy = "optimized_min_leakage_downsample"
      ))
    } else {
      UPF <- ceiling(Fs.out / max(1, Fs.in))
      if (UPF > UPF.max) UPF <- UPF.max
if (.verbose()) {
        cat("=== setSTFT (optimized) ===\n")
        cat(sprintf("Fs_in=%.1f Fs_out=%.1f NW=%d MW=%d OVLP=%.0f df=%.3f leakage=%.3e\n",
          Fs.in, Fs.out, NW, MW, OVLP, Fs.out / NW, abs((Fmax / (Fs.out / NW)) - round(Fmax / (Fs.out / NW)))))
        cat(sprintf("Resample=TRUE Upsample=TRUE Downsample=FALSE UPF=%d\n", UPF))
        cat("strategy=optimized_min_leakage_upsample\n")
      }
      return(list(
        NW = NW,
        MW = MW,
        OVLP = OVLP,
        Fs = Fs.out,
        UPF = UPF,
        downsample_factor = 1,
        Resample = TRUE,
        Upsample = TRUE,
        Downsample = FALSE,
        strategy = "optimized_min_leakage_upsample"
      ))
    }
  }
}
