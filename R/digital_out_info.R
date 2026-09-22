#==============================================================================#
#' Get Digital Out instrument characteristics
#'
#' @description
#'   Retrieve Digital Out characteristics of a device.
#'
#' @template arg_device
#' @template arg_validate_device
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   An integer or numeric vector describing the requested Digital Out
#'   capability. Range functions return two elements containing the minimum and
#'   maximum supported values. Capability values originating from unsigned SDK
#'   integers are returned as integers when all values can be represented by R
#'   integers; otherwise they are returned as numeric whole-number values.
#'
#' @importFrom checkmate assertFlag
#'
#' @name GetDigitalOutInfo
#==============================================================================#
NULL


#' @rdname GetDigitalOutInfo
#' @description
#'   \code{GetDigitalOutChannelCount()} returns the number of Digital Out
#'   channels.
#' @export
GetDigitalOutChannelCount <- function(device, .validate_device = TRUE) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }
  return(.QueryDigitalOutChannelCountC(device$device_handle))
}


#' @rdname GetDigitalOutInfo
#' @description
#'   \code{GetDigitalOutInternalClockFrequency()} returns the internal Digital
#'   Out clock frequency in Hz.
#' @export
GetDigitalOutInternalClockFrequency <- function(
    device,
    .validate_device = TRUE
) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }
  return(.QueryDigitalOutInternalClockFrequencyC(device$device_handle))
}


#' @rdname GetDigitalOutInfo
#' @description
#'   \code{GetDigitalOutRunRange()} returns the supported finite run-time range,
#'   in seconds. A run time of zero is a special SDK value specifying continuous
#'   generation.
#' @export
GetDigitalOutRunRange <- function(device, .validate_device = TRUE) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }
  return(.QueryDigitalOutRunRangeC(device$device_handle))
}


#' @rdname GetDigitalOutInfo
#' @description
#'   \code{GetDigitalOutWaitRange()} returns the supported nonzero wait-time
#'   range, in seconds. A wait time of zero specifies no wait and is the SDK
#'   default.
#' @export
GetDigitalOutWaitRange <- function(device, .validate_device = TRUE) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }
  return(.QueryDigitalOutWaitRangeC(device$device_handle))
}


#' @rdname GetDigitalOutInfo
#' @description
#'   \code{GetDigitalOutRepeatRange()} returns the supported repeat-count range.
#'   A repeat count of zero is a special SDK value specifying infinite repeats.
#' @export
GetDigitalOutRepeatRange <- function(device, .validate_device = TRUE) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }
  value_range <- .QueryDigitalOutRepeatRangeC(device$device_handle)
  return(.ConvertUnsignedValues(value_range))
}



#==============================================================================#
#' Get Digital Out channel characteristics
#'
#' @description
#'   Retrieve Digital Out characteristics of a selected channel.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @template arg_validate_digital_out
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   An integer, numeric, or character vector describing the requested Digital
#'   Out capability. Range functions return two elements containing the minimum
#'   and maximum supported values. Capability values originating from unsigned
#'   SDK integers are returned as integers when all values can be represented by
#'   R integers; otherwise they are returned as numeric whole-number values.
#'
#' @importFrom checkmate assertFlag
#'
#' @name GetDigitalOutChannelInfo
#==============================================================================#
NULL


#' @rdname GetDigitalOutChannelInfo
#' @description
#'   \code{GetDigitalOutOutputModes()} returns the output modes supported by a
#'   Digital Out channel.
#' @export
GetDigitalOutOutputModes <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }
  output_mask <- .QueryDigitalOutOutputMaskC(
    device$device_handle,
    as.integer(channel)
  )
  return(.ConvertMaskToNames(
    mask = output_mask,
    code_map = .dwf_constants$digital_out$output_code
  ))
}


#' @rdname GetDigitalOutChannelInfo
#' @description
#'   \code{GetDigitalOutFunctionTypes()} returns the signal-generation types
#'   supported by a Digital Out channel.
#' @export
GetDigitalOutFunctionTypes <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }
  type_mask <- .QueryDigitalOutTypeMaskC(
    device$device_handle,
    as.integer(channel)
  )
  return(.ConvertMaskToNames(
    mask = type_mask,
    code_map = .dwf_constants$digital_out$type_code
  ))
}


#' @rdname GetDigitalOutChannelInfo
#' @description
#'   \code{GetDigitalOutIdleModes()} returns the idle output modes supported by
#'   a Digital Out channel.
#' @export
GetDigitalOutIdleModes <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }
  idle_mask <- .QueryDigitalOutIdleMaskC(
    device$device_handle,
    as.integer(channel)
  )
  return(.ConvertMaskToNames(
    mask = idle_mask,
    code_map = .dwf_constants$digital_out$idle_code
  ))
}


#' @rdname GetDigitalOutChannelInfo
#' @description
#'   \code{GetDigitalOutDividerRange()} returns the supported divider range for
#'   a Digital Out channel.
#' @export
GetDigitalOutDividerRange <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }
  value_range <- .QueryDigitalOutDividerRangeC(
    device$device_handle,
    as.integer(channel)
  )
  return(.ConvertUnsignedValues(value_range))
}


#' @rdname GetDigitalOutChannelInfo
#' @description
#'   \code{GetDigitalOutCounterRange()} returns the supported counter range for
#'   a Digital Out channel.
#' @export
GetDigitalOutCounterRange <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }
  value_range <- .QueryDigitalOutCounterRangeC(
    device$device_handle,
    as.integer(channel)
  )
  return(.ConvertUnsignedValues(value_range))
}


#' @rdname GetDigitalOutChannelInfo
#' @description
#'   \code{GetDigitalOutMaxDataBits()} returns the maximum number of custom-data
#'   bits supported by a Digital Out channel.
#' @export
GetDigitalOutMaxDataBits <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }
  max_bits <- .QueryDigitalOutMaxDataBitsC(
    device$device_handle,
    as.integer(channel)
  )
  return(.ConvertUnsignedValues(max_bits))
}


