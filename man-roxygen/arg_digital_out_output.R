#' @param output
#'   A string specifying the electrical output mode. Possible values are
#'   \code{"push_pull"} (default), \code{"open_drain"}, \code{"open_source"},
#'   and \code{"three_state"}. The selected mode also determines the custom
#'   pattern encoding: \code{"three_state"} uses two bits per logical pattern
#'   element, whereas the other modes use one bit. Supported modes can be
#'   queried with \code{GetDigitalOutOutputModes()}.
