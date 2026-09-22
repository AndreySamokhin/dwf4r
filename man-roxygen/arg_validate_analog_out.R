#' @param .validate_analog_out
#'   A logical scalar indicating whether \code{AssertAnalogOut()} should be
#'   called. This argument is intended for advanced use to avoid repeated
#'   validation in controlled sequences of operations. Set to \code{FALSE} only
#'   when the device and relevant Analog Out context, such as the channel and
#'   node, have already been validated. The caller is responsible for ensuring
#'   that the device remains open and the validated context remains valid. Other
#'   argument validation and SDK error handling remain active.
