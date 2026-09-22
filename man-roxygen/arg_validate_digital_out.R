#' @param .validate_digital_out
#'   A logical scalar indicating whether \code{AssertDigitalOut()} should be
#'   called. This argument is intended for advanced use to avoid repeated
#'   validation in controlled sequences of operations. Set to \code{FALSE} only
#'   when the device and relevant Digital Out context, such as the channel and
#'   supported configuration choices, have already been validated. The caller is
#'   responsible for ensuring that the device remains open and the validated
#'   context remains valid. Other argument validation and SDK error handling
#'   remain active.
