#' @param .validate_device
#'   A logical scalar indicating whether the device should be validated before
#'   the operation. This argument is intended for advanced use to avoid repeated
#'   validation in controlled sequences of operations. Set to \code{FALSE} only
#'   when the device has already been validated. The caller is responsible for
#'   ensuring that the device remains open. Other argument validation and SDK
#'   error handling remain active.
