#==============================================================================#
#' Check a device object
#'
#' @param device Object to check.
#'
#' @return Invisibly returns NULL.
#'
#' @importFrom checkmate testInt
#'
#' @noRd
#==============================================================================#
.AssertDevice <- function(device) {
  if (!inherits(device, "dwf4r_device")) {
    stop("'device' must be a 'dwf4r_device' object.", call. = FALSE)
  }
  if (!is.list(device) ||
      !checkmate::testInt(device$device_handle, lower = 1L) ||
      !checkmate::testInt(device$session_id, lower = 1L)) {
    stop("'device' is not a valid 'dwf4r_device' object.", call. = FALSE)
  }
  if (device$session_id > length(the$devices)) {
    stop("'device' refers to a session that does not exist.", call. = FALSE)
  }

  registered_device <- the$devices[[device$session_id]]
  if (is.null(registered_device)) {
    stop_msg <- "'device' refers to a session that has already been closed."
    stop(stop_msg, call. = FALSE)
  }
  if (device$device_handle != registered_device$device_handle) {
    stop("'device' does not match the registered device.", call. = FALSE)
  }
  return(invisible(NULL))
}



#==============================================================================#
#' Check a device object for Analog Out use
#'
#' @param device Object to check.
#' @param channel (optional) Zero-based Analog Out channel index.
#' @param node (optional) Node.
#' @param func (optional) Analog out function.
#'
#' @return Invisibly returns NULL.
#'
#' @importFrom checkmate assertInt
#' @importFrom checkmate testChoice
#'
#' @noRd
#==============================================================================#
.AssertAnalogOut <- function(device, channel = NULL, node = NULL, func = NULL) {

  # 'device'
  .AssertDevice(device)

  # 'channel'
  if (!is.null(channel)) {
    checkmate::assertInt(channel, lower = 0L)
    n_channels <- .QueryAnalogOutChannelCountC(device$device_handle)
    if (as.integer(channel) >= n_channels) {
      stop_msg <- sprintf(
        "'channel' is out of range; the device has %d channel(s).", n_channels
      )
      stop(stop_msg, call. = FALSE)
    }
  }

  # 'node'
  if (!is.null(node)) {
    if (is.null(channel)) {
      stop("'node' cannot be specified without 'channel'.", call. = FALSE)
    }
    supported_nodes <-
      .QueryAnalogOutChannelNodesC(device$device_handle, as.integer(channel))
    if (!checkmate::testChoice(node, supported_nodes)) {
      stop_msg <-
        sprintf("Node '%s' is not supported for channel %d.", node, channel)
      stop(stop_msg, call. = FALSE)
    }
  }

  # 'func'
  if (!is.null(func)) {
    if (is.null(channel)) {
      stop("'func' cannot be specified without 'channel'.", call. = FALSE)
    }
    if (is.null(node)) {
      stop("'func' cannot be specified without 'node'.", call. = FALSE)
    }
    supported_functions <- .QueryAnalogOutNodeFunctionTypesC(
      device$device_handle,
      channel = as.integer(channel),
      node = .dwf_constants$analog_out$node_code[[node]]
    )
    if (!checkmate::testChoice(func, supported_functions)) {
      stop_msg <-
        sprintf("Function '%s' is not supported for node '%s' on channel %d.",
                func, node, channel)
      stop(stop_msg, call. = FALSE)
    }
  }

  return(invisible(NULL))
}



#==============================================================================#
#' Check a device object for Digital Out use
#'
#' @description
#'   The function does not validate relationships between \code{output},
#'   \code{type}, and \code{idle}. A combination of individually supported
#'   values is therefore not necessarily a valid Digital Out configuration.
#'   Combination-specific constraints are not currently validated by
#'   \pkg{dwf4r}; the WaveForms SDK may reject an incompatible configuration.
#'
#' @param device Object to check.
#' @param channel (optional) Zero-based Digital Out channel index.
#' @param output (optional) Digital Out output mode.
#' @param func (optional) Digital Out func.
#' @param idle (optional) Digital Out idle mode.
#'
#' @return Invisibly returns NULL.
#'
#' @importFrom checkmate assertInt
#' @importFrom checkmate testChoice
#'
#' @noRd
#==============================================================================#
.AssertDigitalOut <- function(
    device,
    channel = NULL,
    output = NULL,
    func = NULL,
    idle = NULL
) {

  # 'device'
  .AssertDevice(device)

  # 'channel'
  if (!is.null(channel)) {
    checkmate::assertInt(channel, lower = 0L)
    n_channels <- .QueryDigitalOutChannelCountC(device$device_handle)
    if (as.integer(channel) >= n_channels) {
      stop_msg <- sprintf(
        "'channel' is out of range; the device has %d Digital Out channel(s).",
        n_channels
      )
      stop(stop_msg, call. = FALSE)
    }
  }

  # 'output'
  if (!is.null(output)) {
    if (is.null(channel)) {
      stop("'output' cannot be specified without 'channel'.", call. = FALSE)
    }
    output_mask <- .QueryDigitalOutOutputMaskC(
      device$device_handle,
      as.integer(channel)
    )
    supported_outputs <- .ConvertMaskToNames(
      mask = output_mask,
      code_map = .dwf_constants$digital_out$output_code
    )
    if (!checkmate::testChoice(output, supported_outputs)) {
      stop_msg <- sprintf(
        "Output mode '%s' is not supported for channel %d.",
        output,
        channel
      )
      stop(stop_msg, call. = FALSE)
    }
  }

  # 'func'
  if (!is.null(func)) {
    if (is.null(channel)) {
      stop("'func' cannot be specified without 'channel'.", call. = FALSE)
    }
    type_mask <- .QueryDigitalOutTypeMaskC(
      device$device_handle,
      as.integer(channel)
    )
    supported_types <- .ConvertMaskToNames(
      mask = type_mask,
      code_map = .dwf_constants$digital_out$type_code
    )
    if (!checkmate::testChoice(func, supported_types)) {
      stop_msg <- sprintf(
        "Function '%s' is not supported for channel %d.",
        func,
        channel
      )
      stop(stop_msg, call. = FALSE)
    }
  }

  # 'idle'
  if (!is.null(idle)) {
    if (is.null(channel)) {
      stop("'idle' cannot be specified without 'channel'.", call. = FALSE)
    }
    idle_mask <- .QueryDigitalOutIdleMaskC(
      device$device_handle,
      as.integer(channel)
    )
    supported_idle_modes <- .ConvertMaskToNames(
      mask = idle_mask,
      code_map = .dwf_constants$digital_out$idle_code
    )
    if (!checkmate::testChoice(idle, supported_idle_modes)) {
      stop_msg <- sprintf(
        "Idle mode '%s' is not supported for channel %d.",
        idle,
        channel
      )
      stop(stop_msg, call. = FALSE)
    }
  }

  return(invisible(NULL))
}


