#' @param func
#'   A string specifying the carrier waveform function. Available functions
#'   depend on the selected device and can be queried with
#'   \code{GetAnalogOutNodeFunctionTypes()}. The list of functions from the
#'   manual:
#'   \itemize{
#'     \item{\code{"dc"} - DC value set as offset;}
#'     \item{\code{"sine"} - sine waveform;}
#'     \item{\code{"square"} - square waveform (offset +/- amplitude);}
#'     \item{\code{"triangle"} - triangle waveform;}
#'     \item{\code{"ramp_up"} - waveform with a ramp-up voltage at the
#'       beginning;}
#'     \item{\code{"ramp_down"} - a waveform with a ramp-down voltage at the
#'       end;}
#'     \item{\code{"noise"} - noise waveform from random samples;}
#'     \item{\code{"pulse"} - pulse waveform (offset + amplitude);}
#'     \item{\code{"trapezium"} - trapezium;}
#'     \item{\code{"sine_power"} - sine with symmetry used as power function;}
#'     \item{\code{"sine_na"} - sine waveform for Network Analyzer, dynamic
#'       frequency adjustment without glitch}
#'     \item{\code{"dual_custom"} - double buffering of the waveform in the
#'       device for smooth switching.}
#'     \item{\code{"dual_pattern"} - double buffering of the waveform in the
#'       device for smooth switching.}
#'     \item{\code{"custom_pattern"} - waveform from custom repeated data with
#'       fixed sample rate;}
#'     \item{\code{"play_pattern"} - waveform in stream play style (it provides
#'       constant sample rate);}
#'     \item{\code{"custom"} - waveform from custom samples (optimizes for
#'       average requested frequency, sample output lengths may vary by one
#'       system frequency period);}
#'     \item{\code{"play"} - waveform in stream play style (optimizes for
#'       average requested frequency).}
#'   }
