#==============================================================================#
#' Convert an SDK bitmask to user-facing names
#'
#' @description
#'   Convert a WaveForms SDK capability bitmask to user-facing names using a
#'   mapping of names to zero-based SDK codes.
#'
#' @param mask
#'   An integer scalar containing an SDK capability bitmask.
#' @param code_map
#'   A named list mapping user-facing names to zero-based SDK codes.
#'
#' @return
#'   A character vector containing the names corresponding to bits set in
#'   \code{mask}, in the order defined by \code{code_map}.
#'
#' @noRd
#==============================================================================#
.ConvertMaskToNames <- function(mask, code_map) {
  codes <- unlist(code_map, use.names = FALSE)
  mask_codes <- which(as.logical(intToBits(mask))) - 1L
  is_supported <- codes %in% mask_codes
  return(names(code_map[is_supported]))
}



#==============================================================================#
#' Convert unsigned-integer values to a natural R storage type
#'
#' @description
#'   Convert non-negative whole-number values returned from an unsigned SDK
#'   integer type to R integers when all values can be represented exactly as R
#'   integers. Otherwise preserve the complete vector as numeric.
#'
#' @param x
#'   A numeric vector containing non-negative whole-number values.
#'
#' @return
#'   An integer vector if all values are no greater than
#'   \code{.Machine$integer.max}; otherwise a numeric vector.
#'
#' @noRd
#==============================================================================#
.ConvertUnsignedValues <- function(x) {
  if (all(x <= .Machine$integer.max)) {
    return(as.integer(x))
  }
  return(as.numeric(x))
}



#==============================================================================#
#' Convert an SDK code to a user-facing name
#'
#' @param code
#'   An integer SDK code.
#' @param code_map
#'   A named list mapping user-facing names to SDK codes (e.g.,
#'   \code{.dwf_constants$analog_out$function_code} or
#'   \code{.dwf_constants$analog_out$idle_code}).
#' @param value_type
#'   A string describing the type of code for error reporting.
#'
#' @return
#'   A string containing the corresponding user-facing name.
#'
#' @noRd
#==============================================================================#
.ConvertCodeToName <- function(code, code_map, value_type) {
  code_values <- unlist(code_map, use.names = FALSE)
  code_idx <- match(as.integer(code), code_values)
  if (is.na(code_idx)) {
    stop_msg <- sprintf(
      "WaveForms SDK returned an unknown %s code (%d).",
      value_type,
      as.integer(code)
    )
    stop(stop_msg, call. = FALSE)
  }
  code_names <- names(code_map)
  return(code_names[[code_idx]])
}



#==============================================================================#
#' Encode a custom Digital Out pattern
#'
#' @description
#'   Convert a user-facing custom Digital Out pattern to the packed bit
#'   representation expected by the WaveForms SDK.
#'
#' @param pattern
#'   A non-empty numeric or character vector specifying the custom pattern.
#'   Numeric patterns must contain only \code{0} and \code{1}. Character
#'   patterns must contain only \code{"l"}, \code{"h"}, and \code{"z"}.
#' @param is_three_state
#'   A logical scalar indicating whether the pattern should be encoded for
#'   three-state output.
#'
#' @return
#'   A named list with elements \code{data}, containing the packed raw data,
#'   and \code{n_bits}, containing the number of valid SDK data bits.
#'
#' @importFrom checkmate assertCharacter
#' @importFrom checkmate assertIntegerish
#'
#' @noRd
#==============================================================================#
.EncodeDigitalOutCustomPattern <- function(pattern, is_three_state) {

  #--[ Check input arguments ]--------------------------------------------------

  if (is.numeric(pattern)) {
    checkmate::assertIntegerish(
      pattern,
      lower = 0L,
      upper = 1L,
      tol = 0,
      any.missing = FALSE,
      min.len = 1L
    )
    io_bits <- as.integer(pattern)
    is_z <- rep(FALSE, length(pattern))

  } else if (is.character(pattern)) {
    checkmate::assertCharacter(
      pattern,
      any.missing = FALSE,
      min.len = 1L
    )
    if (!all(pattern %in% c("l", "h", "z"))) {
      stop(
        "'pattern' must contain only \"l\", \"h\", and \"z\".",
        call. = FALSE
      )
    }
    io_bits <- as.integer(pattern == "h")
    is_z <- (pattern == "z")

  } else {
    stop("'pattern' must be a numeric or character vector.", call. = FALSE)
  }

  if (!is_three_state && any(is_z)) {
    stop("'z' pattern values require 'is_three_state = TRUE'.", call. = FALSE)
  }


  #--[ Encode SDK bits ]--------------------------------------------------------

  if (is_three_state) {
    n_bits <- 2L * length(io_bits)
    bits <- integer(n_bits)
    io_idxs <- seq.int(1L, n_bits, by = 2L)
    oe_idxs <- seq.int(2L, n_bits, by = 2L)
    bits[io_idxs] <- io_bits
    bits[oe_idxs] <- as.integer(!is_z)

  } else {
    bits <- io_bits
    n_bits <- length(bits)
  }

  # The SDK expects custom-data bits in least-significant-bit-first order.
  n_padding <- (8L - (n_bits %% 8L)) %% 8L
  if (n_padding > 0L) {
    bits <- c(bits, rep(0L, n_padding))
  }

  data <- packBits(as.raw(bits), type = "raw")

  return(list(
    data = data,
    n_bits = n_bits
  ))
}


