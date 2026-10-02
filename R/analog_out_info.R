#==============================================================================#
#' Get Analog Out characteristics
#'
#' @description
#'   Retrieve Analog Out characteristics of a device.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @template arg_node
#' @template arg_validate_device
#' @template arg_validate_analog_out
#'
#' @return
#'   An integer, numeric, or character vector describing the requested Analog
#'   Out capability. Range functions return two elements containing the minimum
#'   and maximum supported values.
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @name GetAnalogOutInfo
#==============================================================================#
NULL


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutChannelCount()} returns the number of Analog Out
#'   channels.
#' @importFrom checkmate assertFlag
#' @export
GetAnalogOutChannelCount <- function(device, .validate_device = TRUE) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }
  return(.QueryAnalogOutChannelCountC(device$device_handle))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutChannelNodes()} returns the nodes supported by an Analog
#'   Out channel.
#' @importFrom checkmate assertFlag
#' @export
GetAnalogOutChannelNodes <- function(
    device,
    channel,
    .validate_analog_out = TRUE
) {
  checkmate::assertFlag(.validate_analog_out)
  if (.validate_analog_out) {
    .AssertAnalogOut(device, channel = channel)
  }
  node_mask <- .QueryAnalogOutNodeMaskC(
    device$device_handle,
    as.integer(channel)
  )
  return(.ConvertMaskToNames(node_mask, .dwf_constants$analog_out$node_code))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutRunRange()} returns the supported run-time range, in
#'   seconds, for an Analog Out channel.
#' @importFrom checkmate assertFlag
#' @export
GetAnalogOutRunRange <- function(
    device,
    channel,
    .validate_analog_out = TRUE
    ) {
  checkmate::assertFlag(.validate_analog_out)
  if (.validate_analog_out) {
    .AssertAnalogOut(device, channel = channel)
  }
  return(.QueryAnalogOutRunRangeC(
    device$device_handle,
    as.integer(channel)
  ))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutWaitRange()} returns the supported wait-time range, in
#'   seconds, for an Analog Out channel.
#' @importFrom checkmate assertFlag
#' @export
GetAnalogOutWaitRange <- function(
    device,
    channel,
    .validate_analog_out = TRUE
) {
  checkmate::assertFlag(.validate_analog_out)
  if (.validate_analog_out) {
    .AssertAnalogOut(device, channel = channel)
  }
  return(.QueryAnalogOutWaitRangeC(
    device$device_handle,
    as.integer(channel)
  ))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutRepeatRange()} returns the supported repeat-count range
#'   for an Analog Out channel.
#' @importFrom checkmate assertFlag
#' @export
GetAnalogOutRepeatRange <- function(
    device,
    channel,
    .validate_analog_out = TRUE
) {
  checkmate::assertFlag(.validate_analog_out)
  if (.validate_analog_out) {
    .AssertAnalogOut(device, channel = channel)
  }
  return(.QueryAnalogOutRepeatRangeC(
    device$device_handle,
    as.integer(channel)
  ))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutIdleModes()} returns the idle output modes supported by
#'   an Analog Out channel.
#' @importFrom checkmate assertFlag
#' @export
GetAnalogOutIdleModes <- function(
    device,
    channel,
    .validate_analog_out = TRUE
) {
  checkmate::assertFlag(.validate_analog_out)
  if (.validate_analog_out) {
    .AssertAnalogOut(device, channel = channel)
  }
  idle_mask <- .QueryAnalogOutIdleMaskC(
    device$device_handle,
    as.integer(channel)
  )
  return(.ConvertMaskToNames(idle_mask, .dwf_constants$analog_out$idle_code))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutNodeFunctionTypes()} returns the waveform functions
#'   supported by an Analog Out node.
#' @importFrom checkmate assertFlag
#' @export
GetAnalogOutNodeFunctionTypes <- function(
    device,
    channel,
    node,
    .validate_analog_out = TRUE
) {
  checkmate::assertFlag(.validate_analog_out)
  if (.validate_analog_out) {
    .AssertAnalogOut(device, channel = channel, node = node)
  }
  function_mask <- .QueryAnalogOutNodeFunctionMaskC(
    device$device_handle,
    as.integer(channel),
    .dwf_constants$analog_out$node_code[[node]]
  )
  return(.ConvertMaskToNames(
    function_mask,
    .dwf_constants$analog_out$function_code
  ))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutNodeFrequencyRange()} returns the supported frequency
#'   range for an Analog Out node.
#' @importFrom checkmate assertFlag
#' @export
GetAnalogOutNodeFrequencyRange <- function(
    device,
    channel,
    node,
    .validate_analog_out = TRUE
) {
  checkmate::assertFlag(.validate_analog_out)
  if (.validate_analog_out) {
    .AssertAnalogOut(device, channel = channel, node = node)
  }
  return(.QueryAnalogOutNodeFrequencyRangeC(
    device$device_handle,
    as.integer(channel),
    .dwf_constants$analog_out$node_code[[node]]
  ))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutNodeAmplitudeRange()} returns the supported amplitude
#'   range for an Analog Out node.
#' @importFrom checkmate assertFlag
#' @export
GetAnalogOutNodeAmplitudeRange <- function(
    device,
    channel,
    node,
    .validate_analog_out = TRUE
) {
  checkmate::assertFlag(.validate_analog_out)
  if (.validate_analog_out) {
    .AssertAnalogOut(device, channel = channel, node = node)
  }
  return(.QueryAnalogOutNodeAmplitudeRangeC(
    device$device_handle,
    as.integer(channel),
    .dwf_constants$analog_out$node_code[[node]]
  ))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutNodeOffsetRange()} returns the supported offset range for
#'   an Analog Out node.
#' @importFrom checkmate assertFlag
#' @export
GetAnalogOutNodeOffsetRange <- function(
    device,
    channel,
    node,
    .validate_analog_out = TRUE
) {
  checkmate::assertFlag(.validate_analog_out)
  if (.validate_analog_out) {
    .AssertAnalogOut(device, channel = channel, node = node)
  }
  return(.QueryAnalogOutNodeOffsetRangeC(
    device$device_handle,
    as.integer(channel),
    .dwf_constants$analog_out$node_code[[node]]
  ))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutNodeSymmetryRange()} returns the supported symmetry range
#'   for an Analog Out node.
#' @importFrom checkmate assertFlag
#' @export
GetAnalogOutNodeSymmetryRange <- function(
    device,
    channel,
    node,
    .validate_analog_out = TRUE
) {
  checkmate::assertFlag(.validate_analog_out)
  if (.validate_analog_out) {
    .AssertAnalogOut(device, channel = channel, node = node)
  }
  return(.QueryAnalogOutNodeSymmetryRangeC(
    device$device_handle,
    as.integer(channel),
    .dwf_constants$analog_out$node_code[[node]]
  ))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutNodePhaseRange()} returns the supported phase range, in
#'   degrees, for an Analog Out node.
#' @importFrom checkmate assertFlag
#' @export
GetAnalogOutNodePhaseRange <- function(
    device,
    channel,
    node,
    .validate_analog_out = TRUE
) {
  checkmate::assertFlag(.validate_analog_out)
  if (.validate_analog_out) {
    .AssertAnalogOut(device, channel = channel, node = node)
  }
  return(.QueryAnalogOutNodePhaseRangeC(
    device$device_handle,
    as.integer(channel),
    .dwf_constants$analog_out$node_code[[node]]
  ))
}

#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutNodeSampleCountRange()} returns the supported range of
#'   sample counts for custom waveform data.
#' @importFrom checkmate assertFlag
#' @export
GetAnalogOutNodeSampleCountRange <- function(
    device,
    channel,
    node,
    .validate_analog_out = TRUE
) {
  checkmate::assertFlag(.validate_analog_out)
  if (.validate_analog_out) {
    .AssertAnalogOut(device, channel = channel, node = node)
  }
  return(.QueryAnalogOutNodeSampleCountRangeC(
    device$device_handle,
    as.integer(channel),
    .dwf_constants$analog_out$node_code[[node]]
  ))
}


