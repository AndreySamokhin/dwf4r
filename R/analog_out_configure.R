# TODO:
# Low-level 'FDwfAnalogOut*()' functions often allow 'idxChannel = -1' to
# apply a setting to all Analog Out channels. This is not currently supported
# in this package; settings can be applied only to one channel at a time.



#==============================================================================#
#' Reset Analog Out channel
#'
#' @description
#'   Reset the Analog Out settings of a selected channel to their SDK defaults.
#'   Because automatic configuration is disabled by \pkg{dwf4r}, call
#'   \code{ApplyAnalogOutSettings()} or \code{StartAnalogOut()} to apply the
#'   reset configuration to the device.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @export
#==============================================================================#
ResetAnalogOut <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel)
  .AnalogOutResetC(device$device_handle, as.integer(channel))
  return(invisible(NULL))
}



#==============================================================================#
#' Configure Analog Out channel
#'
#' @description
#'   Apply settings, start, or stop Analog Out generation for a selected
#'   channel.
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param channel
#'   A zero-based Analog Out channel index.
#' @param action
#'   A string specifying the configuration action.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertChoice
#'
#' @noRd
#==============================================================================#
.ConfigureAnalogOut <- function(device, channel, action) {
  .AssertAnalogOut(device, channel = channel)
  checkmate::assertChoice(action, c("apply", "start", "stop"))

  action_code <- c(stop = 0L, start = 1L, apply = 3L)
  .AnalogOutConfigureC(
    device$device_handle,
    as.integer(channel),
    unname(action_code[[action]])
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Configure Analog Out generation
#'
#' @description
#'   Apply settings, start, or stop Analog Out generation for a selected
#'   channel. Applying settings sends updated configuration to the device
#'   without changing the current channel state.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @name ConfigureAnalogOut
#==============================================================================#
NULL


#' @rdname ConfigureAnalogOut
#' @description
#'   \code{ApplyAnalogOutSettings()} applies updated Analog Out settings.
#' @export
ApplyAnalogOutSettings <- function(device, channel) {
  .ConfigureAnalogOut(device = device, channel = channel, action = "apply")
}


#' @rdname ConfigureAnalogOut
#' @description
#'   \code{StartAnalogOut()} starts Analog Out generation.
#' @export
StartAnalogOut <- function(device, channel) {
  .ConfigureAnalogOut(device = device, channel = channel, action = "start")
}


#' @rdname ConfigureAnalogOut
#' @description
#'   \code{StopAnalogOut()} stops Analog Out generation.
#' @export
StopAnalogOut <- function(device, channel) {
  .ConfigureAnalogOut(device = device, channel = channel, action = "stop")
}



#==============================================================================#
#' Get the state of an Analog Out channel
#'
#' @description
#'   Retrieve the current state of a selected Analog Out channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   A string containing \code{"ready"}, \code{"armed"}, \code{"wait"},
#'   \code{"running"}, \code{"done"}, \code{"config"}, \code{"prefill"}, or
#'   \code{"not_done"}. The value \code{"unknown"} is returned for an
#'   unrecognized SDK state.
#'
#' @export
#==============================================================================#
GetAnalogOutStatus <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel)
  return(.AnalogOutStatusC(device$device_handle, as.integer(channel)))
}



#==============================================================================#
#' Set an Analog Out node mode
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param channel
#'   A zero-based Analog Out channel index.
#' @param node
#'   A string specifying the Analog Out node.
#' @param mode
#'   A string specifying the node mode (see local variable \code{mode_map}).
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertChoice
#'
#' @noRd
#==============================================================================#
.SetAnalogOutNodeMode <- function(
    device,
    channel,
    node,
    mode
) {

  #--[ Check input arguments ]--------------------------------------------------

  .AssertAnalogOut(device, channel = channel, node = node)

  mode_map <- switch(
    node,
    carrier = c(disable = 0L, enable = 1L),
    fm = c(fm = 1L, pm = 2L, pmd = 3L),
    am = c(am = 1L, sum = 2L, sumv = 3L)
  )
  checkmate::assertChoice(mode, names(mode_map))
  mode_code <- unname(mode_map[[mode]])


  #--[ Call low-level function ]------------------------------------------------

  .AnalogOutNodeEnableSetC(
    device$device_handle,
    as.integer(channel),
    .dwf_constants$analog_out$node_code[[node]],
    mode_code
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Enable or disable carrier output
#'
#' @description
#'   Enable or disable the carrier node for an Analog Out channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#'
#' @details
#'   These functions change the carrier node configuration. To apply the
#'   changes, call \code{ApplyAnalogOutSettings()} or \code{StartAnalogOut()}.
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @name CarrierEnableDisable
#==============================================================================#
NULL


#' @rdname CarrierEnableDisable
#' @description
#'   \code{EnableCarrier()} enables the carrier node.
#' @export
EnableCarrier <- function(device, channel) {
  .SetAnalogOutNodeMode(
    device = device,
    channel = channel,
    node = "carrier",
    mode = "enable"
  )
}


#' @rdname CarrierEnableDisable
#' @description
#'   \code{DisableCarrier()} disables the carrier node.
#' @export
DisableCarrier <- function(device, channel) {
  .SetAnalogOutNodeMode(
    device = device,
    channel = channel,
    node = "carrier",
    mode = "disable"
  )
}



#==============================================================================#
#' Check whether carrier output is enabled
#'
#' @description
#'   Retrieve the configured enable state of the carrier node for an Analog Out
#'   channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   A logical scalar, \code{TRUE} if the carrier node is enabled and
#'   \code{FALSE} if it is disabled.
#'
#' @export
#==============================================================================#
IsCarrierEnabled <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel)
  mode_code <- .AnalogOutNodeEnableGetC(
    handle = device$device_handle,
    channel = as.integer(channel),
    node = .dwf_constants$analog_out$node_code[["carrier"]]
  )
  if (!(mode_code %in% c(0L, 1L))) {
    stop_msg <- sprintf(
      paste0(
        "WaveForms SDK returned an unexpected carrier mode code (%d) ",
        "for channel %d."
      ),
      mode_code,
      channel
    )
    stop(stop_msg, call. = FALSE)
  }
  return(mode_code == 1L)
}



#==============================================================================#
#' Set an Analog Out waveform function
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param channel
#'   A zero-based Analog Out channel index.
#' @param node
#'   A string specifying the Analog Out node.
#' @param func
#'   A string specifying the waveform function.
#' @param validate
#'   A logical scalar indicating whether \code{device}, \code{channel}, and
#'   \code{node} should be validated. Set to \code{FALSE} only when these
#'   arguments have already been validated by the calling function.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#'
#' @noRd
#==============================================================================#
.SetAnalogOutFunction <- function(
    device,
    channel,
    node,
    func,
    validate = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(validate)
  if (validate) {
    .AssertAnalogOut(device, channel = channel, node = node, func = func)
  }

  #--[ Call low-level function ]------------------------------------------------

  .AnalogOutNodeFunctionSetC(
    device$device_handle,
    as.integer(channel),
    .dwf_constants$analog_out$node_code[[node]],
    .dwf_constants$analog_out$function_code[[func]]
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Set a numeric Analog Out parameter
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param channel
#'   A zero-based Analog Out channel index.
#' @param node
#'   A string specifying the Analog Out node.
#' @param param
#'   A string specifying the Analog Out parameter (\code{"frequency"},
#'   \code{"amplitude"}, \code{"offset"}, \code{"symmetry"}, or \code{"phase"}).
#' @param value
#'   A numeric scalar to set.
#' @param validate
#'   A logical scalar indicating whether \code{device}, \code{channel}, and
#'   \code{node} should be validated. Set to \code{FALSE} only when these
#'   arguments have already been validated by the calling function.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertChoice
#' @importFrom checkmate assertNumber
#'
#' @noRd
#==============================================================================#
.SetAnalogOutValue <- function(
    device,
    channel,
    node,
    param,
    value,
    validate = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(validate)
  if (validate) {
    .AssertAnalogOut(device, channel = channel, node = node)
  }
  checkmate::assertChoice(
    param,
    c("frequency", "amplitude", "offset", "symmetry", "phase")
  )
  checkmate::assertNumber(value, finite = TRUE)

  range_function <- switch(
    param,
    frequency = .QueryAnalogOutNodeFrequencyRangeC,
    amplitude = .QueryAnalogOutNodeAmplitudeRangeC,
    offset    = .QueryAnalogOutNodeOffsetRangeC,
    symmetry  = .QueryAnalogOutNodeSymmetryRangeC,
    phase     = .QueryAnalogOutNodePhaseRangeC
  )
  value_range <- range_function(
    handle = device$device_handle,
    channel = as.integer(channel),
    node = .dwf_constants$analog_out$node_code[[node]]
  )

  if (!is.numeric(value_range) || length(value_range) != 2L) {
    stop_msg <- sprintf(
      "Analog Out '%s' range is invalid for node '%s' on channel %d.",
      param, node, channel
    )
    stop(stop_msg, call. = FALSE)
  }
  if (value < value_range[[1L]] || value > value_range[[2L]]) {
    stop_msg <- sprintf(
      "'%s' (%.3g) is out of supported range [%.3g, %.3g].",
      param, value, value_range[[1L]], value_range[[2L]]
    )
    stop(stop_msg, call. = FALSE)
  }


  #--[ Call low-level function ]------------------------------------------------

  setter_function <- switch(
    param,
    frequency = .AnalogOutNodeFrequencySetC,
    amplitude = .AnalogOutNodeAmplitudeSetC,
    offset    = .AnalogOutNodeOffsetSetC,
    symmetry  = .AnalogOutNodeSymmetrySetC,
    phase     = .AnalogOutNodePhaseSetC
  )
  setter_function(
    handle = device$device_handle,
    channel = as.integer(channel),
    node = .dwf_constants$analog_out$node_code[[node]],
    value = value
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Set carrier waveform parameters
#'
#' @description
#'   Set parameters of the carrier node for an Analog Out channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @template arg_func
#' @template arg_frequency
#' @template arg_amplitude
#' @template arg_offset
#' @template arg_symmetry
#' @template arg_phase
#'
#' @details
#'   These functions set carrier parameters only. Use
#'   \code{ApplyAnalogOutSettings()} to send updated settings to the device
#'   without changing the current channel state, or \code{StartAnalogOut()} to
#'   apply settings and start generation.
#'
#'   The requested value may differ from the value actually configured by the
#'   device; use the corresponding \code{GetCarrier*()} function to retrieve the
#'   actual value.
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @name SetCarrier
#==============================================================================#
NULL


#' @rdname SetCarrier
#' @description
#'   \code{SetCarrierFunction()} sets the waveform function of the carrier node.
#' @export
SetCarrierFunction <- function(device, channel, func) {
  .SetAnalogOutFunction(
    device  = device,
    channel = channel,
    node    = "carrier",
    func    = func
  )
}


#' @rdname SetCarrier
#' @description
#'   \code{SetCarrierFrequency()} sets the frequency of the carrier node.
#' @export
SetCarrierFrequency <- function(device, channel, frequency) {
  .SetAnalogOutValue(
    device  = device,
    channel = channel,
    node    = "carrier",
    param   = "frequency",
    value   = frequency
  )
}


#' @rdname SetCarrier
#' @description
#'   \code{SetCarrierAmplitude()} sets the amplitude of the carrier node.
#' @export
SetCarrierAmplitude <- function(device, channel, amplitude) {
  .SetAnalogOutValue(
    device  = device,
    channel = channel,
    node    = "carrier",
    param   = "amplitude",
    value   = amplitude
  )
}


#' @rdname SetCarrier
#' @description
#'   \code{SetCarrierOffset()} sets the offset of the carrier node.
#' @export
SetCarrierOffset <- function(device, channel, offset) {
  .SetAnalogOutValue(
    device  = device,
    channel = channel,
    node    = "carrier",
    param   = "offset",
    value   = offset
  )
}


#' @rdname SetCarrier
#' @description
#'   \code{SetCarrierSymmetry()} sets the symmetry of the carrier node.
#' @export
SetCarrierSymmetry <- function(device, channel, symmetry) {
  .SetAnalogOutValue(
    device  = device,
    channel = channel,
    node    = "carrier",
    param   = "symmetry",
    value   = symmetry
  )
}


#' @rdname SetCarrier
#' @description
#'   \code{SetCarrierPhase()} sets the phase (in degrees) of the carrier node.
#' @export
SetCarrierPhase <- function(device, channel, phase) {
  .SetAnalogOutValue(
    device  = device,
    channel = channel,
    node    = "carrier",
    param   = "phase",
    value   = phase
  )
}


#' @rdname SetCarrier
#' @description
#'   \code{SetCarrier()} sets one or more parameters of the carrier node for an
#'   Analog Out channel. Carrier parameters are updated sequentially. If an
#'   error occurs, parameters set before the error remain in the pending
#'   configuration but are not applied until \code{ApplyAnalogOutSettings()} or
#'   \code{StartAnalogOut()} is called.
#' @export
SetCarrier <- function(
    device,
    channel,
    func = NULL,
    frequency = NULL,
    amplitude = NULL,
    offset = NULL,
    symmetry = NULL,
    phase = NULL
) {
  if (all(
    is.null(func),
    is.null(frequency),
    is.null(amplitude),
    is.null(offset),
    is.null(symmetry),
    is.null(phase)
  )) {
    stop("At least one carrier parameter must be specified.", call. = FALSE)
  }
  if (!is.null(func)) {
    .AssertAnalogOut(device, channel = channel, node = "carrier", func = func)
  } else {
    .AssertAnalogOut(device, channel = channel, node = "carrier")
  }

  if (!is.null(func)) {
    .SetAnalogOutFunction(
      device   = device,
      channel  = channel,
      node     = "carrier",
      func     = func,
      validate = FALSE
    )
  }
  if (!is.null(frequency)) {
    .SetAnalogOutValue(
      device   = device,
      channel  = channel,
      node     = "carrier",
      param    = "frequency",
      value    = frequency,
      validate = FALSE
    )
  }
  if (!is.null(amplitude)) {
    .SetAnalogOutValue(
      device   = device,
      channel  = channel,
      node     = "carrier",
      param    = "amplitude",
      value    = amplitude,
      validate = FALSE
    )
  }
  if (!is.null(offset)) {
    .SetAnalogOutValue(
      device   = device,
      channel  = channel,
      node     = "carrier",
      param    = "offset",
      value    = offset,
      validate = FALSE
    )
  }
  if (!is.null(symmetry)) {
    .SetAnalogOutValue(
      device   = device,
      channel  = channel,
      node     = "carrier",
      param    = "symmetry",
      value    = symmetry,
      validate = FALSE
    )
  }
  if (!is.null(phase)) {
    .SetAnalogOutValue(
      device   = device,
      channel  = channel,
      node     = "carrier",
      param    = "phase",
      value    = phase,
      validate = FALSE
    )
  }
  return(invisible(NULL))
}



#==============================================================================#
#' Get an Analog Out waveform function
#'
#' @description
#'   Retrieve the waveform function currently configured for a selected Analog
#'   Out node.
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param channel
#'   A zero-based Analog Out channel index.
#' @param node
#'   A string specifying the Analog Out node.
#' @param validate
#'   A logical scalar indicating whether \code{device}, \code{channel}, and
#'   \code{node} should be validated. Set to \code{FALSE} only when these
#'   arguments have already been validated by the calling function.
#'
#' @return
#'   A string identifying the configured waveform function.
#'
#' @importFrom checkmate assertFlag
#'
#' @noRd
#==============================================================================#
.GetAnalogOutFunction <- function(device, channel, node, validate = TRUE) {
  checkmate::assertFlag(validate)
  if (validate) {
    .AssertAnalogOut(device, channel = channel, node = node)
  }
  function_code <- .AnalogOutNodeFunctionGetC(
    handle = device$device_handle,
    channel = as.integer(channel),
    node = .dwf_constants$analog_out$node_code[[node]]
  )
  return(.ConvertCodeToName(
    code = function_code,
    code_map = .dwf_constants$analog_out$function_code,
    value_type = sprintf("Analog Out %s function", node)
  ))
}


#==============================================================================#
#' Get the carrier waveform function
#'
#' @description
#'   Retrieve the waveform function currently configured for the carrier node
#'   of an Analog Out channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   A string identifying the configured waveform function. Possible values can
#'   be queried with \code{GetAnalogOutNodeFunctionTypes()}.
#'
#' @export
#==============================================================================#
GetCarrierFunction <- function(device, channel) {
  return(.GetAnalogOutFunction(
    device = device,
    channel = channel,
    node = "carrier"
  ))
}



#==============================================================================#
#' Get a numeric Analog Out parameter
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param channel
#'   A zero-based Analog Out channel index.
#' @param node
#'   A string specifying the Analog Out node.
#' @param param
#'   A string specifying the Analog Out parameter: \code{"frequency"},
#'   \code{"amplitude"}, \code{"offset"}, \code{"symmetry"}, or \code{"phase"}.
#' @param validate
#'   A logical scalar indicating whether \code{device}, \code{channel}, and
#'   \code{node} should be validated. Set to \code{FALSE} only when these
#'   arguments have already been validated by the calling function.
#'
#' @return
#'   A numeric scalar containing the configured parameter value.
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertChoice
#'
#' @noRd
#==============================================================================#
.GetAnalogOutValue <- function(device, channel, node, param, validate = TRUE) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(validate)
  if (validate) {
    .AssertAnalogOut(device, channel = channel, node = node)
  }
  checkmate::assertChoice(
    param,
    c("frequency", "amplitude", "offset", "symmetry", "phase")
  )


  #--[ Call low-level function ]------------------------------------------------

  getter_function <- switch(
    param,
    frequency = .AnalogOutNodeFrequencyGetC,
    amplitude = .AnalogOutNodeAmplitudeGetC,
    offset    = .AnalogOutNodeOffsetGetC,
    symmetry  = .AnalogOutNodeSymmetryGetC,
    phase     = .AnalogOutNodePhaseGetC
  )

  return(getter_function(
    handle = device$device_handle,
    channel = as.integer(channel),
    node = .dwf_constants$analog_out$node_code[[node]]
  ))
}



#==============================================================================#
#' Get numeric carrier waveform parameters
#'
#' @description
#'   Retrieve numeric parameters currently configured for the carrier node of
#'   an Analog Out channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#'
#' @details
#'   The functions return values reported by the corresponding WaveForms SDK
#'   \code{*Get} functions. A returned value may differ slightly from the value
#'   supplied to a setter if the SDK adjusted it to a value supported by the
#'   device.
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   A numeric scalar containing the requested carrier parameter.
#'
#' @name GetCarrierValues
#==============================================================================#
NULL


#' @rdname GetCarrierValues
#' @description
#'   \code{GetCarrierFrequency()} returns the configured carrier frequency in
#'   Hz.
#' @export
GetCarrierFrequency <- function(device, channel) {
  return(.GetAnalogOutValue(
    device = device,
    channel = channel,
    node = "carrier",
    param = "frequency"
  ))
}


#' @rdname GetCarrierValues
#' @description
#'   \code{GetCarrierAmplitude()} returns the configured carrier peak amplitude
#'   in volts relative to the configured offset.
#' @export
GetCarrierAmplitude <- function(device, channel) {
  return(.GetAnalogOutValue(
    device = device,
    channel = channel,
    node = "carrier",
    param = "amplitude"
  ))
}


#' @rdname GetCarrierValues
#' @description
#'   \code{GetCarrierOffset()} returns the configured carrier offset in volts.
#' @export
GetCarrierOffset <- function(device, channel) {
  return(.GetAnalogOutValue(
    device = device,
    channel = channel,
    node = "carrier",
    param = "offset"
  ))
}


#' @rdname GetCarrierValues
#' @description
#'   \code{GetCarrierSymmetry()} returns the configured carrier symmetry in
#'   percent.
#' @export
GetCarrierSymmetry <- function(device, channel) {
  return(.GetAnalogOutValue(
    device = device,
    channel = channel,
    node = "carrier",
    param = "symmetry"
  ))
}


#' @rdname GetCarrierValues
#' @description
#'   \code{GetCarrierPhase()} returns the configured carrier phase in degrees.
#' @export
GetCarrierPhase <- function(device, channel) {
  return(.GetAnalogOutValue(
    device = device,
    channel = channel,
    node = "carrier",
    param = "phase"
  ))
}



#==============================================================================#
#' Get carrier waveform settings
#'
#' @description
#'   Retrieve the current configuration of the carrier node for an Analog Out
#'   channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   A named list with the following elements:
#'   \describe{
#'     \item{\code{enabled}}{
#'       Logical value indicating whether the carrier node is enabled.
#'     }
#'     \item{\code{func}}{
#'       String identifying the configured waveform function.
#'     }
#'     \item{\code{frequency}}{
#'       Numeric carrier frequency in Hz.
#'     }
#'     \item{\code{amplitude}}{
#'       Numeric carrier peak amplitude in volts relative to the configured
#'       offset.
#'     }
#'     \item{\code{offset}}{
#'       Numeric carrier offset in volts.
#'     }
#'     \item{\code{symmetry}}{
#'       Numeric carrier symmetry in percent.
#'     }
#'     \item{\code{phase}}{
#'       Numeric carrier phase in degrees.
#'     }
#'   }
#'
#' @export
#==============================================================================#
GetCarrierSettings <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel, node = "carrier")

  # TODO: '.AssertAnalogOut()' is additionally called in 'IsCarrierEnabled()'
  out <- list(
    enabled = IsCarrierEnabled(
      device = device,
      channel = channel
    ),
    func = .GetAnalogOutFunction(
      device = device,
      channel = channel,
      node = "carrier",
      validate = FALSE
    ),
    frequency = .GetAnalogOutValue(
      device = device,
      channel = channel,
      node = "carrier",
      param = "frequency",
      validate = FALSE
    ),
    amplitude = .GetAnalogOutValue(
      device = device,
      channel = channel,
      node = "carrier",
      param = "amplitude",
      validate = FALSE
    ),
    offset = .GetAnalogOutValue(
      device = device,
      channel = channel,
      node = "carrier",
      param = "offset",
      validate = FALSE
    ),
    symmetry = .GetAnalogOutValue(
      device = device,
      channel = channel,
      node = "carrier",
      param = "symmetry",
      validate = FALSE
    ),
    phase = .GetAnalogOutValue(
      device = device,
      channel = channel,
      node = "carrier",
      param = "phase",
      validate = FALSE
    )
  )
  return(out)
}



#==============================================================================#
#' Upload custom Analog Out data
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @template arg_node
#' @param data
#'   A numeric vector of waveform samples normalized to the range
#'   \code{[-1, 1]}.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertNumeric
#'
#' @noRd
#==============================================================================#
.SetAnalogOutData <- function(device, channel, node, data) {

  #--[ Check input arguments ]--------------------------------------------------

  .AssertAnalogOut(device, channel = channel, node = node)
  checkmate::assertNumeric(
    data,
    any.missing = FALSE,
    lower = -1,
    upper = 1,
    min.len = 1L
  )

  value_range <-
    GetAnalogOutNodeSampleCountRange(device, channel = channel, node = node)
  if (length(data) < value_range[[1L]] || length(data) > value_range[[2L]]) {
    stop_msg <- sprintf(
      "Length of 'data' (%d) is out of supported range [%d, %d].",
      length(data), value_range[[1L]], value_range[[2L]]
    )
    stop(stop_msg, call. = FALSE)
  }


  #--[ Call low-level function ]------------------------------------------------#

  .AnalogOutNodeDataSetC(
    handle = device$device_handle,
    channel = as.integer(channel),
    node = .dwf_constants$analog_out$node_code[[node]],
    data = as.numeric(data)
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Set carrier custom waveform samples
#'
#' @description
#'   Upload a custom sample vector for the carrier node of an Analog Out
#'   channel. Samples must be normalized to the range \code{[-1, 1]}.
#'
#'   This function uploads sample data but does not select the custom waveform
#'   function or start generation. Use \code{SetCarrierFunction(..., func =
#'   "custom")} and then call \code{ApplyAnalogOutSettings()} or
#'   \code{StartAnalogOut()}.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @param data
#'   A numeric vector of waveform samples normalized to the range
#'   \code{[-1, 1]}.
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @export
#==============================================================================#
SetCarrierData <- function(device, channel, data) {
  .SetAnalogOutData(
    device = device,
    channel = channel,
    node = "carrier",
    data = data
  )
}



#==============================================================================#
#' Set Analog Out idle output
#'
#' @description
#'   Set the idle output mode for a selected Analog Out channel.
#'
#'   The idle output mode controls the Analog Out output while the generator is
#'   not actively running, including \code{Ready}, \code{Stopped}, \code{Done},
#'   and \code{Wait} states.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @param idle
#'   A string specifying the idle output mode. Supported values are
#'   \code{"disable"}, \code{"offset"}, \code{"initial"}, and \code{"hold"}.
#'
#' @details
#'   The \code{"offset"} mode uses the configured offset level. Use
#'   \code{SetCarrierOffset()} to set that voltage level. If device
#'   auto-configuration is disabled, call \code{ApplyAnalogOutSettings()} or
#'   \code{StartAnalogOut()} after changing Analog Out settings.
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate testChoice
#'
#' @export
#==============================================================================#
SetAnalogOutIdle <- function(device, channel, idle) {

  #--[ Check input arguments ]--------------------------------------------------

  .AssertAnalogOut(device, channel = channel)
  supported_idle_modes <- GetAnalogOutIdleModes(device, channel)
  if (!checkmate::testChoice(idle, supported_idle_modes)) {
    stop_msg <- sprintf("Idle mode '%s' is not supported for channel %d.",
                        idle, channel)
    stop(stop_msg, call. = FALSE)
  }


  #--[ Call low-level function ]------------------------------------------------

  .AnalogOutIdleSetC(
    handle = device$device_handle,
    channel = as.integer(channel),
    idle_mode_code = unname(.dwf_constants$analog_out$idle_code[[idle]])
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Get the Analog Out idle output mode
#'
#' @description
#'   Retrieve the idle output mode currently configured for a selected Analog
#'   Out channel.
#'
#'   The idle mode controls the output while the generator is not actively
#'   running, including the Ready, Stopped, Done, and Wait states.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   A string containing \code{"disable"}, \code{"offset"},
#'   \code{"initial"}, or \code{"hold"}.
#'
#' @export
#==============================================================================#
GetAnalogOutIdle <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel)
  idle_code <- .AnalogOutIdleGetC(
    handle = device$device_handle,
    channel = as.integer(channel)
  )
  return(.ConvertCodeToName(
    code = idle_code,
    code_map = .dwf_constants$analog_out$idle_code,
    value_type = "Analog Out idle mode"
  ))
}



#==============================================================================#
#' Set an Analog Out timing parameter
#'
#' @description
#'   Validate and set a selected Analog Out timing parameter.
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param channel
#'   A zero-based Analog Out channel index.
#' @param param
#'   A string specifying the timing parameter: \code{"run"}, \code{"wait"}, or
#'   \code{"repeat"}.
#' @param value
#'   A numeric or integer scalar containing the value to set.
#' @param validate
#'   A logical scalar indicating whether \code{device} and \code{channel}
#'   should be validated. Set to \code{FALSE} only when these arguments have
#'   already been validated by the calling function.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertChoice
#' @importFrom checkmate assertInt
#' @importFrom checkmate assertNumber
#'
#' @noRd
#==============================================================================#
.SetAnalogOutTimingValue <- function(
    device,
    channel,
    param,
    value,
    validate = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(validate)
  if (validate) {
    .AssertAnalogOut(device, channel = channel)
  }

  checkmate::assertChoice(param, c("run_time", "wait_time", "repeat_count"))

  if (identical(param, "repeat_count")) {
    checkmate::assertInt(value, .var.name = param)
  } else { # i.e., "run_time" or "wait_time"
    checkmate::assertNumber(value, finite = TRUE, .var.name = param)
  }

  value_range <- switch(
    param,
    run_time = .QueryAnalogOutRunRangeC(
      handle = device$device_handle,
      channel = as.integer(channel)
    ),
    wait_time = .QueryAnalogOutWaitRangeC(
      handle = device$device_handle,
      channel = as.integer(channel)
    ),
    repeat_count = .QueryAnalogOutRepeatRangeC(
      handle = device$device_handle,
      channel = as.integer(channel)
    )
  )

  # Zero is valid for continuous run, no wait, or infinite repetition.
  is_special_zero <- (value == 0)

  if (!is_special_zero &&
      (value < value_range[[1L]] || value > value_range[[2L]])) {
    if (identical(param, "repeat_count")) {
      stop_msg <- sprintf(
        "'%s' (%d) is out of supported range [%d, %d].",
        param,
        as.integer(value),
        value_range[[1L]],
        value_range[[2L]]
      )
    } else { # i.e., "run_time" or "wait_time"
      stop_msg <- sprintf(
        "'%s' (%f) is out of supported range [%f, %f].",
        param,
        value,
        value_range[[1L]],
        value_range[[2L]]
      )
    }
    stop(stop_msg, call. = FALSE)
  }


  #--[ Call low-level function ]------------------------------------------------

  switch(
    param,
    run_time = .AnalogOutRunSetC(
      handle = device$device_handle,
      channel = as.integer(channel),
      time_s = value
    ),
    wait_time = .AnalogOutWaitSetC(
      handle = device$device_handle,
      channel = as.integer(channel),
      time_s = value
    ),
    repeat_count = .AnalogOutRepeatSetC(
      handle = device$device_handle,
      channel = as.integer(channel),
      repeat_count = as.integer(value)
    )
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Set Analog Out timing
#'
#' @description
#'   Set the run time, post-trigger wait time, or repeat count for a selected
#'   Analog Out channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @param run_time
#'   A numeric scalar specifying the run length in seconds. A value of
#'   \code{0} requests continuous generation.
#' @param wait_time
#'   A numeric scalar specifying the delay, in seconds, between receiving a
#'   trigger and beginning signal generation. A value of \code{0} disables
#'   this delay.
#' @param repeat_count
#'   An integer scalar specifying the repeat count. A value of \code{0}
#'   requests infinite repetition.
#'
#' @details
#'   These functions update the pending Analog Out configuration. Because
#'   automatic configuration is disabled by \pkg{dwf4r}, call
#'   \code{ApplyAnalogOutSettings()} to apply the settings without changing the
#'   current channel state, or \code{StartAnalogOut()} to apply the settings
#'   and start generation.
#'
#'   In \code{SetAnalogOutTiming()}, a \code{NULL} argument leaves the
#'   corresponding setting unchanged. Selected parameters are updated in the
#'   order \code{run_time}, \code{wait_time}, and \code{repeat_count}. If an
#'   error occurs, parameters set before the error remain in the pending
#'   configuration.
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @name SetAnalogOutTiming
#==============================================================================#
NULL


#' @rdname SetAnalogOutTiming
#' @description
#'   \code{SetAnalogOutRun()} sets the run length of an Analog Out channel.
#'
#' @export
SetAnalogOutRun <- function(device, channel, run_time) {
  .SetAnalogOutTimingValue(
    device = device,
    channel = channel,
    param = "run_time",
    value = run_time
  )
  return(invisible(NULL))
}


#' @rdname SetAnalogOutTiming
#' @description
#'   \code{SetAnalogOutWait()} sets the delay between receiving a trigger and
#'   beginning signal generation.
#'
#' @export
SetAnalogOutWait <- function(device, channel, wait_time) {
  .SetAnalogOutTimingValue(
    device = device,
    channel = channel,
    param = "wait_time",
    value = wait_time
  )
  return(invisible(NULL))
}


#' @rdname SetAnalogOutTiming
#' @description
#'   \code{SetAnalogOutRepeat()} sets the repeat count of an Analog Out
#'   channel.
#'
#' @export
SetAnalogOutRepeat <- function(device, channel, repeat_count) {
  .SetAnalogOutTimingValue(
    device = device,
    channel = channel,
    param = "repeat_count",
    value = repeat_count
  )
  return(invisible(NULL))
}


#' @rdname SetAnalogOutTiming
#' @description
#'   \code{SetAnalogOutTiming()} sets one or more timing parameters of an
#'   Analog Out channel.
#'
#' @export
SetAnalogOutTiming <- function(
    device,
    channel,
    run_time = NULL,
    wait_time = NULL,
    repeat_count = NULL
) {
  if (all(
    is.null(run_time),
    is.null(wait_time),
    is.null(repeat_count)
  )) {
    stop(
      "At least one Analog Out timing parameter must be specified.",
      call. = FALSE
    )
  }

  .AssertAnalogOut(device, channel = channel)

  if (!is.null(run_time)) {
    .SetAnalogOutTimingValue(
      device = device,
      channel = channel,
      param = "run_time",
      value = run_time,
      validate = FALSE
    )
  }

  if (!is.null(wait_time)) {
    .SetAnalogOutTimingValue(
      device = device,
      channel = channel,
      param = "wait_time",
      value = wait_time,
      validate = FALSE
    )
  }

  if (!is.null(repeat_count)) {
    .SetAnalogOutTimingValue(
      device = device,
      channel = channel,
      param = "repeat_count",
      value = repeat_count,
      validate = FALSE
    )
  }

  return(invisible(NULL))
}



#==============================================================================#
#' Get an Analog Out timing parameter
#'
#' @description
#'   Retrieve a selected configured Analog Out timing parameter.
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param channel
#'   A zero-based Analog Out channel index.
#' @param param
#'   A string specifying the timing parameter: \code{"run_time"},
#'   \code{"wait_time"}, or \code{"repeat_count"}.
#' @param validate
#'   A logical scalar indicating whether \code{device} and \code{channel} should
#'   be validated. Set to \code{FALSE} only when these arguments have already
#'   been validated by the calling function.
#'
#' @return
#'   A numeric or integer scalar containing the configured timing parameter.
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertChoice
#'
#' @noRd
#==============================================================================#
.GetAnalogOutTimingValue <- function(
    device,
    channel,
    param,
    validate = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(validate)
  if (validate) {
    .AssertAnalogOut(device, channel = channel)
  }

  checkmate::assertChoice(param, c("run_time", "wait_time", "repeat_count"))


  #--[ Call low-level function ]------------------------------------------------

  getter_function <- switch(
    param,
    run_time     = .AnalogOutRunGetC,
    wait_time    = .AnalogOutWaitGetC,
    repeat_count = .AnalogOutRepeatGetC
  )

  return(getter_function(
    handle = device$device_handle,
    channel = as.integer(channel)
  ))
}



#==============================================================================#
#' Get Analog Out timing
#'
#' @description
#'   Retrieve the configured run time, post-trigger wait time, or repeat count
#'   for a selected Analog Out channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#'
#' @details
#'   These functions retrieve the configured timing values reported by the
#'   corresponding WaveForms SDK \code{*Get} functions. A run time of \code{0}
#'   represents continuous generation, a wait time of \code{0} represents no
#'   post-trigger delay, and a repeat count of \code{0} represents infinite
#'   repetition.
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   \code{GetAnalogOutRun()} and \code{GetAnalogOutWait()} return a numeric
#'   scalar in seconds. \code{GetAnalogOutRepeat()} returns an integer scalar.
#'   \code{GetAnalogOutTiming()} returns a named list with the following
#'   elements:
#'   \describe{
#'     \item{\code{run_time}}{
#'       Numeric configured run time in seconds.
#'     }
#'     \item{\code{wait_time}}{
#'       Numeric configured post-trigger wait time in seconds.
#'     }
#'     \item{\code{repeat_count}}{
#'       Integer configured repeat count.
#'     }
#'   }
#'
#' @name GetAnalogOutTiming
#==============================================================================#
NULL


#' @rdname GetAnalogOutTiming
#' @description
#'   \code{GetAnalogOutRun()} returns the configured run length of an Analog Out
#'   channel.
#'
#' @export
GetAnalogOutRun <- function(device, channel) {
  return(.GetAnalogOutTimingValue(
    device = device,
    channel = channel,
    param = "run_time"
  ))
}


#' @rdname GetAnalogOutTiming
#' @description
#'   \code{GetAnalogOutWait()} returns the configured delay between receiving a
#'   trigger and beginning signal generation.
#'
#' @export
GetAnalogOutWait <- function(device, channel) {
  return(.GetAnalogOutTimingValue(
    device = device,
    channel = channel,
    param = "wait_time"
  ))
}


#' @rdname GetAnalogOutTiming
#' @description
#'   \code{GetAnalogOutRepeat()} returns the configured repeat count of an
#'   Analog Out channel.
#'
#' @export
GetAnalogOutRepeat <- function(device, channel) {
  return(.GetAnalogOutTimingValue(
    device = device,
    channel = channel,
    param = "repeat_count"
  ))
}


#' @rdname GetAnalogOutTiming
#' @description
#'   \code{GetAnalogOutTiming()} returns all configured timing parameters of an
#'   Analog Out channel.
#'
#' @export
GetAnalogOutTiming <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel)

  out <- list(
    run_time = .GetAnalogOutTimingValue(
      device = device,
      channel = channel,
      param = "run_time",
      validate = FALSE
    ),
    wait_time = .GetAnalogOutTimingValue(
      device = device,
      channel = channel,
      param = "wait_time",
      validate = FALSE
    ),
    repeat_count = .GetAnalogOutTimingValue(
      device = device,
      channel = channel,
      param = "repeat_count",
      validate = FALSE
    )
  )

  return(out)
}



#==============================================================================#
#' Get a remaining Analog Out timing value
#'
#' @description
#'   Retrieve a remaining Analog Out timing value from the current or most
#'   recent status update.
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param channel
#'   A zero-based Analog Out channel index.
#' @param param
#'   A string specifying the remaining value to retrieve: \code{"run_time"} or
#'   \code{"repeat_count"}.  The remaining wait time cannot be retrieved because
#'   the WaveForms SDK does not provide a corresponding Analog Out wait-status
#'   function.
#' @param update
#'   A logical scalar indicating whether the Analog Out status should be updated
#'   before retrieving the remaining value.
#' @param validate
#'   A logical scalar indicating whether \code{device} and \code{channel} should
#'   be validated. Set to \code{FALSE} only when these arguments have already
#'   been validated by the calling function.
#'
#' @return
#'   A numeric or integer scalar containing the requested remaining value.
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertChoice
#'
#' @noRd
#==============================================================================#
.GetAnalogOutRemainingValue <- function(
    device,
    channel,
    param,
    update = TRUE,
    validate = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(validate)
  if (validate) {
    .AssertAnalogOut(device, channel = channel)
  }

  checkmate::assertChoice(param, c("run_time", "repeat_count"))
  checkmate::assertFlag(update)


  #--[ Update status ]----------------------------------------------------------

  if (update) {
    .AnalogOutStatusC(
      handle = device$device_handle,
      channel = as.integer(channel)
    )
  }


  #--[ Get remaining value ]----------------------------------------------------

  status_function <- switch(
    param,
    run_time     = .AnalogOutRunStatusC,
    repeat_count = .AnalogOutRepeatStatusC
  )

  return(status_function(
    handle = device$device_handle,
    channel = as.integer(channel)
  ))
}



#==============================================================================#
#' Get remaining Analog Out timing values
#'
#' @description
#'   Retrieve the remaining run time or repeat count for a selected Analog Out
#'   channel. The remaining wait time cannot be retrieved because the WaveForms
#'   SDK does not provide a corresponding Analog Out wait-status function.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @param update
#'   A logical scalar indicating whether the Analog Out status should be updated
#'   before retrieving the remaining value. The default is \code{TRUE}. If
#'   \code{FALSE}, the value from the most recent Analog Out status update
#'   (performed with \code{GetAnalogOutStatus()}) is returned.
#'
#' @details
#'   The WaveForms SDK functions used to retrieve the remaining run time and
#'   repeat count do not themselves read the device. They return information
#'   obtained by the most recent \code{GetAnalogOutStatus()} (i.e.
#'   \code{FDwfAnalogOutStatus()}) call.
#'
#'   With the default \code{update = TRUE}, these functions update the Analog
#'   Out status before retrieving the remaining value. With \code{update =
#'   FALSE}, no status update is performed and the returned value may therefore
#'   be stale.
#'
#'   Setting \code{update = FALSE} can also be used to retrieve several values
#'   from the same status update. For example, first call
#'   \code{GetAnalogOutStatus()}, then call both remaining-value functions with
#'   \code{update = FALSE}.
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   \code{GetAnalogOutRemainingRun()} returns a numeric scalar containing the
#'   remaining run time in seconds. \code{GetAnalogOutRemainingRepeat()} returns
#'   an integer scalar containing the remaining repeat count.
#'
#' @name GetAnalogOutRemaining
#==============================================================================#
NULL


#' @rdname GetAnalogOutRemaining
#' @description
#'   \code{GetAnalogOutRemainingRun()} returns the remaining run time of an
#'   Analog Out channel.
#'
#' @export
GetAnalogOutRemainingRun <- function(device, channel, update = TRUE) {
  return(.GetAnalogOutRemainingValue(
    device = device,
    channel = channel,
    param = "run_time",
    update = update
  ))
}


#' @rdname GetAnalogOutRemaining
#' @description
#'   \code{GetAnalogOutRemainingRepeat()} returns the remaining repeat count of
#'   an Analog Out channel.
#'
#' @export
GetAnalogOutRemainingRepeat <- function(device, channel, update = TRUE) {
  return(.GetAnalogOutRemainingValue(
    device = device,
    channel = channel,
    param = "repeat_count",
    update = update
  ))
}



#==============================================================================#
#' Set the Analog Out trigger source
#'
#' @description
#'   Set the trigger source for a selected Analog Out channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @param source
#'   A string specifying the trigger source. Recognized values include
#'   \code{"none"}, \code{"pc"}, \code{"detector_analog_in"},
#'   \code{"detector_digital_in"}, \code{"analog_in"}, \code{"digital_in"},
#'   \code{"digital_out"}, \code{"analog_out_1"}, \code{"analog_out_2"},
#'   \code{"external_1"}, and \code{"external_2"}. This list is not exhaustive;
#'   available sources depend on the selected device and can be queried with
#'   \code{GetDeviceTriggerSources()}.
#'
#' @details
#'   This function updates the pending Analog Out configuration. Because
#'   automatic configuration is disabled by \pkg{dwf4r}, call
#'   \code{ApplyAnalogOutSettings()} to apply the setting without changing the
#'   current channel state, or \code{StartAnalogOut()} to apply the setting and
#'   start generation.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate testChoice
#'
#' @export
#==============================================================================#
SetAnalogOutTriggerSource <- function(device, channel, source) {

  #--[ Check input arguments ]--------------------------------------------------

  .AssertAnalogOut(device, channel = channel)
  supported_sources <- GetDeviceTriggerSources(device)
  if (!checkmate::testChoice(source, supported_sources)) {
    stop_msg <- sprintf(
      "Trigger source '%s' is not supported by the device.",
      source
    )
    stop(stop_msg, call. = FALSE)
  }


  #--[ Call low-level function ]------------------------------------------------

  .AnalogOutTriggerSourceSetC(
    handle = device$device_handle,
    channel = as.integer(channel),
    source_code = unname(.dwf_constants$trigger$source_code[[source]])
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Get the Analog Out trigger source
#'
#' @description
#'   Retrieve the trigger source currently configured for a selected Analog Out
#'   channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#'
#' @return
#'   A string identifying the configured trigger source.
#'
#' @export
#==============================================================================#
GetAnalogOutTriggerSource <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel)

  source_code <- .AnalogOutTriggerSourceGetC(
    handle = device$device_handle,
    channel = as.integer(channel)
  )

  return(.ConvertCodeToName(
    code = source_code,
    code_map = .dwf_constants$trigger$source_code,
    value_type = "Analog Out trigger source"
  ))
}



#==============================================================================#
#' Set the Analog Out trigger slope
#'
#' @description
#'   Set the trigger slope for a selected Analog Out channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @param slope
#'   A string specifying the trigger slope. Recognized values are
#'   \code{"rising"}, \code{"falling"}, and \code{"either"}.
#'
#' @details
#'   This function updates the pending Analog Out configuration. Because
#'   automatic configuration is disabled by \pkg{dwf4r}, call
#'   \code{ApplyAnalogOutSettings()} to apply the setting without changing the
#'   current channel state, or \code{StartAnalogOut()} to apply the setting and
#'   start generation.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate testChoice
#'
#' @export
#==============================================================================#
SetAnalogOutTriggerSlope <- function(device, channel, slope) {

  #--[ Check input arguments ]--------------------------------------------------

  .AssertAnalogOut(device, channel = channel)
  supported_slopes <- GetDeviceTriggerSlopes(device)
  if (!checkmate::testChoice(slope, supported_slopes)) {
    stop_msg <- sprintf(
      "Trigger slope '%s' is not supported by the device.",
      slope
    )
    stop(stop_msg, call. = FALSE)
  }


  #--[ Call low-level function ]------------------------------------------------

  .AnalogOutTriggerSlopeSetC(
    handle = device$device_handle,
    channel = as.integer(channel),
    slope_code = unname(.dwf_constants$trigger$slope_code[[slope]])
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Get the Analog Out trigger slope
#'
#' @description
#'   Retrieve the trigger slope currently configured for a selected Analog Out
#'   channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#'
#' @return
#'   A string identifying the configured trigger slope.
#'
#' @export
#==============================================================================#
GetAnalogOutTriggerSlope <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel)

  slope_code <- .AnalogOutTriggerSlopeGetC(
    handle = device$device_handle,
    channel = as.integer(channel)
  )

  return(.ConvertCodeToName(
    code = slope_code,
    code_map = .dwf_constants$trigger$slope_code,
    value_type = "Analog Out trigger slope"
  ))
}



#==============================================================================#
#' Set Analog Out repeat triggering
#'
#' @description
#'   Set whether a new trigger is included in each repeat cycle of a selected
#'   Analog Out channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @param repeat_trigger
#'   A logical scalar. If \code{TRUE}, a new trigger is included in each
#'   wait-run repeat cycle. If \code{FALSE}, repeated cycles proceed without
#'   waiting for another trigger.
#'
#' @details
#'   This setting controls trigger behavior during repetition and does not
#'   specify the number of repetitions. Use \code{SetAnalogOutRepeat()} to set
#'   the repeat count.
#'
#'   This function updates the pending Analog Out configuration. Because
#'   automatic configuration is disabled by \pkg{dwf4r}, call
#'   \code{ApplyAnalogOutSettings()} to apply the setting without changing the
#'   current channel state, or \code{StartAnalogOut()} to apply the setting and
#'   start generation.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#'
#' @export
#==============================================================================#
SetAnalogOutRepeatTrigger <- function(
    device,
    channel,
    repeat_trigger
) {

  #--[ Check input arguments ]--------------------------------------------------

  .AssertAnalogOut(device, channel = channel)
  checkmate::assertFlag(repeat_trigger)


  #--[ Call low-level function ]------------------------------------------------

  .AnalogOutRepeatTriggerSetC(
    handle = device$device_handle,
    channel = as.integer(channel),
    repeat_trigger = repeat_trigger
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Get Analog Out repeat triggering
#'
#' @description
#'   Retrieve whether a new trigger is included in each repeat cycle of a
#'   selected Analog Out channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#'
#' @return
#'   A logical scalar. \code{TRUE} indicates that a new trigger is included in
#'   each wait-run repeat cycle; \code{FALSE} indicates that repeated cycles
#'   proceed without waiting for another trigger.
#'
#' @export
#==============================================================================#
GetAnalogOutRepeatTrigger <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel)

  return(.AnalogOutRepeatTriggerGetC(
    handle = device$device_handle,
    channel = as.integer(channel)
  ))
}



#==============================================================================#
#' Set the Analog Out master channel
#'
#' @description
#'   Set the state-machine master for a selected Analog Out channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @param master_channel
#'   An integer scalar representing the zero-based Analog Out channel index to
#'   use as the state-machine master.
#'
#' @details
#'   A channel whose master differs from itself is controlled by the state
#'   machine of the master channel. Trigger, wait, run, and repeat settings
#'   therefore normally need to be configured only for the master channel.
#'
#'   Setting \code{master_channel} equal to \code{channel} appears to correspond
#'   to independent channel operation. This behavior is not explicitly described
#'   in the WaveForms SDK reference manual, but was observed experimentally with
#'   Analog Discovery 2.
#'
#'   This function updates the pending Analog Out configuration. Because
#'   automatic configuration is disabled by \pkg{dwf4r}, call
#'   \code{ApplyAnalogOutSettings()} to apply the setting without changing the
#'   current channel state, or \code{StartAnalogOut()} to apply the setting.
#'
#'   When starting synchronized channels, initialize the slave channel before
#'   starting the master channel.
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertInt
#'
#' @export
#==============================================================================#
SetAnalogOutMaster <- function(device, channel, master_channel) {

  #--[ Check input arguments ]--------------------------------------------------

  .AssertAnalogOut(device, channel = channel)
  checkmate::assertInt(master_channel, lower = 0L)

  channel_count <- .QueryAnalogOutChannelCountC(device$device_handle)
  if (as.integer(master_channel) >= channel_count) {
    stop_msg <- sprintf(
      "'master_channel' is out of range; the device has %d channel(s).",
      channel_count
    )
    stop(stop_msg, call. = FALSE)
  }


  #--[ Call low-level function ]------------------------------------------------

  .AnalogOutMasterSetC(
    handle = device$device_handle,
    channel = as.integer(channel),
    master_channel = as.integer(master_channel)
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Get the Analog Out master channel
#'
#' @description
#'   Retrieve the state-machine master currently configured for a selected
#'   Analog Out channel.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#'
#' @seealso
#'   See the vignette \emph{Basic Analog Out Functionality} for a complete
#'   Analog Out workflow.
#'
#' @return
#'   An integer scalar containing the zero-based index of the configured master
#'   channel. If the returned index is equal to \code{channel}, the channel
#'   reports itself as its state-machine master. On Analog Discovery 2, this
#'   configuration was observed experimentally to correspond to independent
#'   channel operation, although this behavior is not explicitly described in
#'   the WaveForms SDK reference manual.
#'
#' @export
#==============================================================================#
GetAnalogOutMaster <- function(device, channel) {
  .AssertAnalogOut(device, channel = channel)

  return(.AnalogOutMasterGetC(
    handle = device$device_handle,
    channel = as.integer(channel)
  ))
}


