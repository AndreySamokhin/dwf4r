#==============================================================================#
#' Get Analog Out characteristics
#'
#' @description
#'   Retrieve Analog Out characteristics of a device.
#'
#' @template arg_device
#' @template arg_channel
#' @template arg_node
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
#' @export
GetAnalogOutChannelCount <- function(device) {
  .AssertDevice(device)
  return(.QueryAnalogOutChannelCountC(device$device_handle))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutChannelNodes()} returns the nodes supported by an Analog
#'   Out channel.
#' @export
GetAnalogOutChannelNodes <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel)
  return(.QueryAnalogOutChannelNodesC(
    device$device_handle,
    as.integer(channel)
  ))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutRunRange()} returns the supported run-time range, in
#'   seconds, for an Analog Out channel.
#' @export
GetAnalogOutRunRange <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel)
  return(.QueryAnalogOutRunRangeC(
    device$device_handle,
    as.integer(channel)
  ))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutWaitRange()} returns the supported wait-time range, in
#'   seconds, for an Analog Out channel.
#' @export
GetAnalogOutWaitRange <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel)
  return(.QueryAnalogOutWaitRangeC(
    device$device_handle,
    as.integer(channel)
  ))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutRepeatRange()} returns the supported repeat-count range
#'   for an Analog Out channel.
#' @export
GetAnalogOutRepeatRange <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel)
  return(.QueryAnalogOutRepeatRangeC(
    device$device_handle,
    as.integer(channel)
  ))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutIdleModes()} returns the idle output modes supported by
#'   an Analog Out channel.
#' @export
GetAnalogOutIdleModes <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel)
  return(.QueryAnalogOutIdleModesC(
    device$device_handle,
    as.integer(channel)
  ))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutNodeFunctionTypes()} returns the waveform functions
#'   supported by an Analog Out node.
#' @export
GetAnalogOutNodeFunctionTypes <- function(device, channel, node) {
  .AssertAnalogOut(device, channel = channel, node = node)
  return(.QueryAnalogOutNodeFunctionTypesC(
    device$device_handle,
    as.integer(channel),
    .dwf_constants$analog_out$node_code[[node]]
  ))
}


#' @rdname GetAnalogOutInfo
#' @description
#'   \code{GetAnalogOutNodeFrequencyRange()} returns the supported frequency
#'   range for an Analog Out node.
#' @export
GetAnalogOutNodeFrequencyRange <- function(device, channel, node) {
  .AssertAnalogOut(device, channel = channel, node = node)
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
#' @export
GetAnalogOutNodeAmplitudeRange <- function(device, channel, node) {
  .AssertAnalogOut(device, channel = channel, node = node)
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
#' @export
GetAnalogOutNodeOffsetRange <- function(device, channel, node) {
  .AssertAnalogOut(device, channel = channel, node = node)
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
#' @export
GetAnalogOutNodeSymmetryRange <- function(device, channel, node) {
  .AssertAnalogOut(device, channel = channel, node = node)
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
#' @export
GetAnalogOutNodePhaseRange <- function(device, channel, node) {
  .AssertAnalogOut(device, channel = channel, node = node)
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
#' @export
GetAnalogOutNodeSampleCountRange <- function(device, channel, node) {
  .AssertAnalogOut(device, channel = channel, node = node)
  return(.QueryAnalogOutNodeSampleCountRangeC(
    device$device_handle,
    as.integer(channel),
    .dwf_constants$analog_out$node_code[[node]]
  ))
}


