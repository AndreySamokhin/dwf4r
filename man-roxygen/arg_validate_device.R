#' @param .validate_device
#'   A logical scalar indicating whether \code{AssertDevice()} should be called.
#'   This argument is intended for advanced use to avoid repeated validation in
#'   controlled sequences of operations. Set to \code{FALSE} only when the
#'   device has already been validated and is known to remain open. Other
#'   argument validation and SDK error handling remain active.
