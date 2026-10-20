#' @details
#'   Actual hardware-adjusted waveform parameters are not returned. Requested
#'   values are stored in the pending Analog Out configuration, and the
#'   corresponding \code{GetAnalogOutCarrier*()} functions report updated values
#'   only after the configuration is applied. To inspect the actual values, call
#'   \code{ApplyAnalogOutSettings()} or \code{StartAnalogOut()} first, then use
#'   the corresponding getter functions.
