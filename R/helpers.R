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


