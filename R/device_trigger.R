#==============================================================================#
#' Get device trigger capabilities
#'
#' @description
#'   Retrieve trigger capabilities of a WaveForms device.
#'
#' @template arg_device
#' @template arg_validate_device
#'
#' @return
#'   A character vector containing supported trigger source or slope names,
#'   depending on the function.
#'
#' @name GetDeviceTriggerInfo
#==============================================================================#
NULL


#' @rdname GetDeviceTriggerInfo
#' @description
#'   \code{GetDeviceTriggerSources()} returns the trigger sources supported by
#'   the device and available through the current \pkg{dwf4r} interface.
#'
#'   Possible values include \code{"none"}, \code{"pc"},
#'   \code{"detector_analog_in"}, \code{"detector_digital_in"},
#'   \code{"analog_in"}, \code{"digital_in"}, \code{"digital_out"},
#'   \code{"analog_out_1"}, \code{"analog_out_2"}, \code{"external_1"}, and
#'   \code{"external_2"}.
#' @export
GetDeviceTriggerSources <- function(device, .validate_device = TRUE) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }
  source_mask <- .QueryDeviceTriggerSourceMaskC(device$device_handle)
  return(.ConvertMaskToNames(
    mask = source_mask,
    code_map = .dwf_constants$trigger$source_code
  ))
}


#' @rdname GetDeviceTriggerInfo
#' @description
#'   \code{GetDeviceTriggerSlopes()} returns the trigger slopes supported by
#'   the device. Possible values are \code{"rising"}, \code{"falling"}, and
#'   \code{"either"}.
#' @export
GetDeviceTriggerSlopes <- function(device, .validate_device = TRUE) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }
  slope_mask <- .QueryDeviceTriggerSlopeMaskC(device$device_handle)
  return(.ConvertMaskToNames(
    mask = slope_mask,
    code_map = .dwf_constants$trigger$slope_code
  ))
}



#==============================================================================#
#' Trigger a WaveForms device
#'
#' @description
#'   Generate a PC trigger pulse for a WaveForms device.
#'
#'   Instruments configured to use the \code{"pc"} trigger source can respond
#'   to this pulse.
#'
#' @template arg_device
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @export
#==============================================================================#
TriggerDevice <- function(device) {
  .AssertDevice(device)
  .DeviceTriggerPcC(device$device_handle)
  return(invisible(NULL))
}


