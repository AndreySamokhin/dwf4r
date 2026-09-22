#==============================================================================#
#' Reset Digital Out instrument
#'
#' @description
#'   Reset all Digital Out instrument parameters to their SDK defaults. Because
#'   automatic configuration is disabled by \pkg{dwf4r}, call
#'   \code{StartDigitalOut()} or \code{StopDigitalOut()} after resetting,
#'   depending on the desired instrument state.
#'
#' @template arg_device
#' @template arg_validate_device
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#'
#' @export
#==============================================================================#
ResetDigitalOut <- function(device, .validate_device = TRUE) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }
  .DigitalOutResetC(device$device_handle)
  return(invisible(NULL))
}



#==============================================================================#
#' Configure Digital Out instrument
#'
#' @description
#'   Start or stop the Digital Out instrument.
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param action
#'   A string specifying the configuration action.
#' @param .validate_device
#'   Whether to validate the device using \code{.AssertDevice()}.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertChoice
#'
#' @noRd
#==============================================================================#
.ConfigureDigitalOut <- function(device, action, .validate_device = TRUE) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }
  checkmate::assertChoice(action, c("start", "stop"))

  .DigitalOutConfigureC(
    handle = device$device_handle,
    start = identical(action, "start")
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Start or stop Digital Out
#'
#' @description
#'   Start or stop the Digital Out instrument.
#'
#' @details
#'   Unlike Analog Out, Digital Out does not provide a separate operation for
#'   applying modified settings while preserving the current instrument state.
#'   After changing Digital Out settings, use \code{StartDigitalOut()} to start
#'   or restart the instrument with the new configuration.
#'
#' @template arg_device
#' @template arg_validate_device
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @name ConfigureDigitalOut
#==============================================================================#
NULL


#' @rdname ConfigureDigitalOut
#' @description
#'   \code{StartDigitalOut()} starts (or restarts) the Digital Out instrument
#'   using the configured settings.
#' @export
StartDigitalOut <- function(device, .validate_device = TRUE) {
  .ConfigureDigitalOut(
    device = device,
    action = "start",
    .validate_device = .validate_device
  )
  return(invisible(NULL))
}


#' @rdname ConfigureDigitalOut
#' @description
#'   \code{StopDigitalOut()} stops the Digital Out instrument.
#' @export
StopDigitalOut <- function(device, .validate_device = TRUE) {
  .ConfigureDigitalOut(
    device = device,
    action = "stop",
    .validate_device = .validate_device
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Get the state of the Digital Out instrument
#'
#' @description
#'   Retrieve the current state of the Digital Out instrument.
#'
#' @template arg_device
#' @template arg_validate_device
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   A string identifying the current Digital Out state. The WaveForms SDK
#'   documents \code{"ready"}, \code{"armed"}, \code{"wait"}, \code{"running"},
#'   and \code{"done"} as Digital Out states. The additional SDK states
#'   \code{"config"}, \code{"prefill"}, and \code{"not_done"} are also
#'   recognized if returned. The value \code{"unknown"} is returned for any
#'   unrecognized SDK state.
#'
#' @importFrom checkmate assertFlag
#'
#' @export
#==============================================================================#
GetDigitalOutStatus <- function(device, .validate_device = TRUE) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }
  return(.DigitalOutStatusC(device$device_handle))
}



#==============================================================================#
#' Set the enable state of a Digital Out channel
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param channel
#'   A zero-based Digital Out channel index.
#' @param enabled
#'   A logical scalar specifying whether the channel should be enabled.
#' @param .validate_digital_out
#'   Whether to validate the device using \code{.AssertDigitalOut()}.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#'
#' @noRd
#==============================================================================#
.SetDigitalOutChannelEnabled <- function(
    device,
    channel,
    enabled,
    .validate_digital_out = TRUE
) {
  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }
  checkmate::assertFlag(enabled)

  .DigitalOutEnableSetC(
    handle = device$device_handle,
    channel = as.integer(channel),
    enable = enabled
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Enable or disable a Digital Out channel
#'
#' @description
#'   Enable or disable a selected Digital Out channel.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @template arg_validate_digital_out
#'
#' @details
#'   This function updates the pending Digital Out configuration. Because
#'   automatic configuration is disabled by \pkg{dwf4r}, call
#'   \code{StartDigitalOut()} to start or restart generation with the updated
#'   settings. To stop generation, call \code{StopDigitalOut()} explicitly.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @name DigitalOutChannelEnableDisable
#==============================================================================#
NULL


#' @rdname DigitalOutChannelEnableDisable
#' @description
#'   \code{EnableDigitalOutChannel()} enables a Digital Out channel.
#' @export
EnableDigitalOutChannel <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  .SetDigitalOutChannelEnabled(
    device = device,
    channel = channel,
    enabled = TRUE,
    .validate_digital_out = .validate_digital_out
  )
  return(invisible(NULL))
}


#' @rdname DigitalOutChannelEnableDisable
#' @description
#'   \code{DisableDigitalOutChannel()} disables a Digital Out channel.
#' @export
DisableDigitalOutChannel <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  .SetDigitalOutChannelEnabled(
    device = device,
    channel = channel,
    enabled = FALSE,
    .validate_digital_out = .validate_digital_out
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Check whether a Digital Out channel is enabled
#'
#' @description
#'   Retrieve the configured enable state of a selected Digital Out channel.
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
#'   A logical scalar, \code{TRUE} if the channel is enabled and \code{FALSE}
#'   if it is disabled.
#'
#' @importFrom checkmate assertFlag
#'
#' @export
#==============================================================================#
IsDigitalOutChannelEnabled <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }
  return(.DigitalOutEnableGetC(
    handle = device$device_handle,
    channel = as.integer(channel)
  ))
}



#==============================================================================#
#' Set a Digital Out channel setting
#'
#' @description
#'   Validate and set a selected Digital Out channel setting.
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param channel
#'   A zero-based Digital Out channel index.
#' @param param
#'   A string specifying the setting: \code{"output"}, \code{"func"}, or
#'   \code{"idle"}.
#' @param value
#'   A string specifying the value to set.
#' @param .validate_digital_out
#'   Whether to validate the device using \code{.AssertDigitalOut()}.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertChoice
#' @importFrom checkmate assertFlag
#'
#' @noRd
#==============================================================================#
.SetDigitalOutChannelSetting <- function(
    device,
    channel,
    param,
    value,
    .validate_digital_out = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertChoice(param, c("output", "func", "idle"))
  code_map <- switch(
    param,
    output = .dwf_constants$digital_out$output_code,
    func   = .dwf_constants$digital_out$type_code,
    idle   = .dwf_constants$digital_out$idle_code
  )
  checkmate::assertChoice(value, names(code_map))
  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    switch(
      param,
      output = .AssertDigitalOut(
        device,
        channel = channel,
        output = value
      ),
      func = .AssertDigitalOut(
        device,
        channel = channel,
        func = value
      ),
      idle = .AssertDigitalOut(
        device,
        channel = channel,
        idle = value
      )
    )
  }


  #--[ Call low-level function ]------------------------------------------------

  setter_function <- switch(
    param,
    output = .DigitalOutOutputSetC,
    func   = .DigitalOutTypeSetC,
    idle   = .DigitalOutIdleSetC
  )

  setter_function(
    device$device_handle,
    as.integer(channel),
    unname(code_map[[value]])
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Set Digital Out channel settings
#'
#' @description
#'   Set the output mode, signal-generation function, or idle output mode of a
#'   selected Digital Out channel.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @template arg_validate_digital_out
#' @param output
#'   A string specifying the output mode. Possible values are
#'   \code{"push_pull"}, \code{"open_drain"}, \code{"open_source"}, and
#'   \code{"three_state"}. Supported values for a selected channel can be
#'   queried with \code{GetDigitalOutOutputModes()}.
#' @param func
#'   A string specifying the signal-generation function. Possible values are
#'   \code{"pulse"}, \code{"custom"}, \code{"random"}, \code{"rom"},
#'   \code{"state"}, and \code{"play"}. Supported values for a selected channel
#'   can be queried with \code{GetDigitalOutFunctionTypes()}.
#' @param idle
#'   A string specifying the idle output mode. Possible values are
#'   \code{"initial"}, \code{"low"}, \code{"high"}, and \code{"three_state"}.
#'   Supported values for a selected channel can be queried with
#'   \code{GetDigitalOutIdleModes()}.
#'
#' @details
#'   The WaveForms SDK and the public \pkg{dwf4r} API use different terms for
#'   selecting the type of generated signal. The SDK uses \emph{type}, whereas
#'   the R API uses \emph{function}. The latter is used primarily to maintain
#'   consistent naming between Analog Out and Digital Out functionality.
#'
#'   Each requested value is checked against the capabilities reported for the
#'   selected channel. Relationships between the output mode, function, and idle
#'   mode are not validated; a combination of individually supported values is
#'   therefore not necessarily a valid Digital Out configuration.
#'
#'   These functions update the pending Digital Out configuration. With
#'   automatic configuration disabled by \pkg{dwf4r}, changing a setting does
#'   not stop current Digital Out generation. Call \code{StartDigitalOut()} to
#'   start or restart the instrument with the updated configuration. To stop
#'   generation, call \code{StopDigitalOut()} explicitly.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @name SetDigitalOutChannelSettings
#==============================================================================#
NULL


#' @rdname SetDigitalOutChannelSettings
#' @description
#'   \code{SetDigitalOutOutput()} sets the output mode of a Digital Out channel.
#' @export
SetDigitalOutOutput <- function(
    device,
    channel,
    output,
    .validate_digital_out = TRUE) {
  .SetDigitalOutChannelSetting(
    device = device,
    channel = channel,
    param = "output",
    value = output,
    .validate_digital_out = .validate_digital_out
  )
  return(invisible(NULL))
}


#' @rdname SetDigitalOutChannelSettings
#' @description
#'   \code{SetDigitalOutFunction()} sets the signal-generation function of a
#'   Digital Out channel.
#' @export
SetDigitalOutFunction <- function(
    device,
    channel,
    func,
    .validate_digital_out = TRUE) {
  .SetDigitalOutChannelSetting(
    device = device,
    channel = channel,
    param = "func",
    value = func,
    .validate_digital_out = .validate_digital_out
  )
  return(invisible(NULL))
}


#' @rdname SetDigitalOutChannelSettings
#' @description
#'   \code{SetDigitalOutIdle()} sets the idle output mode of a Digital Out
#'   channel.
#' @export
SetDigitalOutIdle <- function(
    device,
    channel,
    idle,
    .validate_digital_out = TRUE) {
  .SetDigitalOutChannelSetting(
    device = device,
    channel = channel,
    param = "idle",
    value = idle,
    .validate_digital_out = .validate_digital_out
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Get a Digital Out channel setting
#'
#' @description
#'   Retrieve a selected setting currently configured for a Digital Out
#'   channel.
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param channel
#'   A zero-based Digital Out channel index.
#' @param param
#'   A string specifying the setting: \code{"output"}, \code{"func"}, or
#'   \code{"idle"}.
#' @param .validate_digital_out
#'   Whether to validate the device using \code{.AssertDigitalOut()}.
#'
#' @return
#'   A string identifying the configured setting.
#'
#' @importFrom checkmate assertChoice
#' @importFrom checkmate assertFlag
#'
#' @noRd
#==============================================================================#
.GetDigitalOutChannelSetting <- function(
    device,
    channel,
    param,
    .validate_digital_out = TRUE) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }
  checkmate::assertChoice(param, c("output", "func", "idle"))


  #--[ Call low-level function ]------------------------------------------------

  getter_function <- switch(
    param,
    output = .DigitalOutOutputGetC,
    func   = .DigitalOutTypeGetC,
    idle   = .DigitalOutIdleGetC
  )

  code_map <- switch(
    param,
    output = .dwf_constants$digital_out$output_code,
    func   = .dwf_constants$digital_out$type_code,
    idle   = .dwf_constants$digital_out$idle_code
  )

  value_type <- switch(
    param,
    output = "Digital Out output mode",
    func   = "Digital Out function",
    idle   = "Digital Out idle mode"
  )

  value_code <- getter_function(
    handle = device$device_handle,
    channel = as.integer(channel)
  )

  return(.ConvertCodeToName(
    code = value_code,
    code_map = code_map,
    value_type = value_type
  ))
}



#==============================================================================#
#' Get Digital Out channel settings
#'
#' @description
#'   Retrieve the output mode, signal-generation function, or idle output mode
#'   currently configured for a selected Digital Out channel.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @template arg_validate_digital_out
#'
#' @details
#'   The WaveForms SDK and the public \pkg{dwf4r} API use different terms for
#'   selecting the type of generated signal. The SDK uses \emph{type}, whereas
#'   the R API uses \emph{function}. The latter is used primarily to maintain
#'   consistent naming between Analog Out and Digital Out functionality.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   A string identifying the configured Digital Out channel setting. Possible
#'   values for each setting can be queried with the corresponding capability
#'   function.
#'
#' @name GetDigitalOutChannelSettings
#==============================================================================#
NULL


#' @rdname GetDigitalOutChannelSettings
#' @description
#'   \code{GetDigitalOutOutput()} returns the configured output mode of a
#'   Digital Out channel.
#' @export
GetDigitalOutOutput <- function(device, channel, .validate_digital_out = TRUE) {
  return(.GetDigitalOutChannelSetting(
    device = device,
    channel = channel,
    param = "output",
    .validate_digital_out = .validate_digital_out
  ))
}


#' @rdname GetDigitalOutChannelSettings
#' @description
#'   \code{GetDigitalOutFunction()} returns the configured signal-generation
#'   function of a Digital Out channel.
#' @export
GetDigitalOutFunction <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  return(.GetDigitalOutChannelSetting(
    device = device,
    channel = channel,
    param = "func",
    .validate_digital_out = .validate_digital_out
  ))
}


#' @rdname GetDigitalOutChannelSettings
#' @description
#'   \code{GetDigitalOutIdle()} returns the configured idle output mode of a
#'   Digital Out channel.
#' @export
GetDigitalOutIdle <- function(device, channel, .validate_digital_out = TRUE) {
  return(.GetDigitalOutChannelSetting(
    device = device,
    channel = channel,
    param = "idle",
    .validate_digital_out = .validate_digital_out
  ))
}



#==============================================================================#
#' Set a Digital Out divider value
#'
#' @description
#'   Set the ordinary or initial divider of a selected Digital Out channel.
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param channel
#'   A zero-based Digital Out channel index.
#' @param divider
#'   A numeric scalar specifying the divider value.
#' @param is_init
#'   A logical scalar indicating whether to set the initial divider.
#' @param .validate_digital_out
#'   Whether to validate the device using \code{.AssertDigitalOut()}.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertNumber
#'
#' @noRd
#==============================================================================#
.SetDigitalOutDividerValue <- function(
    device,
    channel,
    divider,
    is_init,
    .validate_digital_out = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }

  checkmate::assertFlag(is_init)

  if (is_init) {
    argument_name <- "initial_divider"
  } else {
    argument_name <- "divider"
  }
  checkmate::assertNumber(
    divider,
    lower = 0,
    upper = 2^32 - 1,
    finite = TRUE,
    .var.name = argument_name
  )

  if (divider != floor(divider)) {
    stop_msg <- sprintf("'%s' must be a whole number.", argument_name)
    stop(stop_msg, call. = FALSE)
  }

  value_range <- GetDigitalOutDividerRange(
    device = device,
    channel = channel,
    .validate_digital_out = FALSE
  )

  if (divider < value_range[[1L]] || divider > value_range[[2L]]) {
    stop_msg <- sprintf(
      "'%s' (%.3g) is out of supported range [%.3g, %.3g].",
      argument_name, divider, value_range[[1L]], value_range[[2L]]
    )
    stop(stop_msg, call. = FALSE)
  }


  #--[ Call low-level function ]------------------------------------------------

  if (is_init) {
    .DigitalOutDividerInitSetC(
      handle = device$device_handle,
      channel = as.integer(channel),
      initial_divider = divider
    )
  } else {
    .DigitalOutDividerSetC(
      handle = device$device_handle,
      channel = as.integer(channel),
      divider = divider
    )
  }

  return(invisible(NULL))
}



#==============================================================================#
#' Set the Digital Out divider
#'
#' @description
#'   Set the divider used by a selected Digital Out channel after the initial
#'   divider expires. The internal clock frequency and divider value determine
#'   the duration of one counter step.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @param divider
#'   A non-negative whole-number scalar specifying the divider value. The
#'   supported range depends on the selected channel and can be queried with
#'   \code{GetDigitalOutDividerRange()}.
#' @template arg_validate_digital_out
#'
#' @details
#'   This function updates the pending Digital Out configuration. With automatic
#'   configuration disabled by \pkg{dwf4r}, changing the divider does not stop
#'   current Digital Out generation. Call \code{StartDigitalOut()} to start or
#'   restart the instrument with the updated configuration. To stop generation,
#'   call \code{StopDigitalOut()} explicitly.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @export
#==============================================================================#
SetDigitalOutDivider <- function(
    device,
    channel,
    divider,
    .validate_digital_out = TRUE
) {
  .SetDigitalOutDividerValue(
    device = device,
    channel = channel,
    divider = divider,
    is_init = FALSE,
    .validate_digital_out = .validate_digital_out
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Set the initial Digital Out divider
#'
#' @description
#'   Set the divider value initially loaded when the Digital Out instrument
#'   enters the Running state. The internal clock frequency and initial divider
#'   value determine the duration of the first counter step.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @param initial_divider
#'   A non-negative whole-number scalar specifying the initial divider value.
#'   The supported range depends on the selected channel and can be queried
#'   with \code{GetDigitalOutDividerRange()}.
#' @template arg_validate_digital_out
#'
#' @details
#'   This function updates the pending Digital Out configuration. With automatic
#'   configuration disabled by \pkg{dwf4r}, changing the initial divider does
#'   not stop current Digital Out generation. Call \code{StartDigitalOut()} to
#'   start or restart the instrument with the updated configuration. To stop
#'   generation, call \code{StopDigitalOut()} explicitly.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @export
#==============================================================================#
SetDigitalOutInitialDivider <- function(
    device,
    channel,
    initial_divider,
    .validate_digital_out = TRUE
) {
  .SetDigitalOutDividerValue(
    device = device,
    channel = channel,
    divider = initial_divider,
    is_init = TRUE,
    .validate_digital_out = .validate_digital_out
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Get Digital Out divider values
#'
#' @description
#'   Retrieve the ordinary or initial divider configured for a selected Digital
#'   Out channel. The internal clock frequency and divider value determine the
#'   duration of one count. The initial divider is loaded when the instrument
#'   enters the Running state; after it expires, the ordinary divider is loaded
#'   on subsequent expirations.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @template arg_validate_digital_out
#'
#' @details
#'   The functions return the values reported by the corresponding WaveForms
#'   SDK \code{*Get} functions.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   An integer scalar containing the configured divider value, or a numeric
#'   whole-number scalar if the value exceeds R's integer range.
#'
#' @name GetDigitalOutDividerValues
#==============================================================================#
NULL


#' @rdname GetDigitalOutDividerValues
#' @description
#'   \code{GetDigitalOutDivider()} returns the configured ordinary divider.
#' @importFrom checkmate assertFlag
#' @export
GetDigitalOutDivider <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }

  divider <- .DigitalOutDividerGetC(
    handle = device$device_handle,
    channel = as.integer(channel)
  )
  return(.ConvertUnsignedValues(divider))
}



#' @rdname GetDigitalOutDividerValues
#' @description
#'   \code{GetDigitalOutInitialDivider()} returns the configured initial
#'   divider.
#' @export
GetDigitalOutInitialDivider <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }

  initial_divider <- .DigitalOutDividerInitGetC(
    handle = device$device_handle,
    channel = as.integer(channel)
  )
  return(.ConvertUnsignedValues(initial_divider))
}



#==============================================================================#
#' Set Digital Out low and high counters
#'
#' @description
#'   Set the low and high counter values of a selected Digital Out channel.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @param low_count,high_count
#'   Non-negative whole-number scalars specifying the low and high counter
#'   values respectively. The supported range can be queried with
#'   \code{GetDigitalOutCounterRange()}.
#' @template arg_validate_digital_out
#'
#' @details
#'   For pulse generation, the low and high counters determine how many counts
#'   the output remains low and high, respectively. The internal clock
#'   frequency and divider value determine the duration of one count.
#'
#'   The WaveForms SDK specifies that a zero low or high counter value prevents
#'   the output level from toggling.
#'
#'   This function updates the pending Digital Out configuration. With automatic
#'   configuration disabled by \pkg{dwf4r}, changing the counter values does not
#'   stop current Digital Out generation. Call \code{StartDigitalOut()} to start
#'   or restart the instrument with the updated configuration. To stop
#'   generation, call \code{StopDigitalOut()} explicitly.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertNumber
#'
#' @export
#==============================================================================#
SetDigitalOutCounter <- function(
    device,
    channel,
    low_count,
    high_count,
    .validate_digital_out = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }

  value_range <- GetDigitalOutCounterRange(
    device = device,
    channel = channel,
    .validate_digital_out = FALSE
  )

  counter_values <- list(
    low_count = low_count,
    high_count = high_count
  )

  for (argument_name in names(counter_values)) {
    value <- counter_values[[argument_name]]

    checkmate::assertNumber(
      value,
      lower = 0,
      upper = 2^32 - 1,
      finite = TRUE,
      .var.name = argument_name
    )

    if (value != floor(value)) {
      stop_msg <- sprintf(
        "'%s' must be a whole number.",
        argument_name
      )
      stop(stop_msg, call. = FALSE)
    }

    if (value != 0 &&
        (value < value_range[[1L]] || value > value_range[[2L]])) {
      stop_msg <- sprintf(
        "'%s' (%.3g) is out of supported range [%.3g, %.3g].",
        argument_name, value, value_range[[1L]], value_range[[2L]]
      )
      stop(stop_msg, call. = FALSE)
    }
  }


  #--[ Call low-level function ]------------------------------------------------

  .DigitalOutCounterSetC(
    handle = device$device_handle,
    channel = as.integer(channel),
    low_count = low_count,
    high_count = high_count
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Set the initial Digital Out counter
#'
#' @description
#'   Set the initial output state and counter value of a selected Digital Out
#'   channel.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @param initial_state
#'   A string specifying the initial output state, either \code{"low"} or
#'   \code{"high"}.
#' @param initial_count
#'   A non-negative whole-number scalar specifying the initial counter value.
#'   The supported range can be queried with \code{GetDigitalOutCounterRange()}.
#'   An initial count of zero is accepted.
#' @template arg_validate_digital_out
#'
#' @details
#'   The initial state and counter value are loaded when the Digital Out
#'   instrument enters the Running state. The initial counter determines the
#'   number of counts before the ordinary low and high counter sequence begins.
#'   The internal clock frequency and divider values determine the duration of
#'   each count.
#'
#'   This function updates the pending Digital Out configuration. With automatic
#'   configuration disabled by \pkg{dwf4r}, changing the initial counter does
#'   not stop current Digital Out generation. Call \code{StartDigitalOut()} to
#'   start or restart the instrument with the updated configuration. To stop
#'   generation, call \code{StopDigitalOut()} explicitly.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertChoice
#' @importFrom checkmate assertNumber
#'
#' @export
#==============================================================================#
SetDigitalOutInitialCounter <- function(
    device,
    channel,
    initial_state,
    initial_count,
    .validate_digital_out = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }

  checkmate::assertChoice(initial_state, c("low", "high"))

  checkmate::assertNumber(
    initial_count,
    lower = 0,
    upper = 2^32 - 1,
    finite = TRUE
  )

  if (initial_count != floor(initial_count)) {
    stop("'initial_count' must be a whole number.", call. = FALSE)
  }

  value_range <- GetDigitalOutCounterRange(
    device = device,
    channel = channel,
    .validate_digital_out = FALSE
  )

  if (initial_count != 0 &&
      (initial_count < value_range[[1L]] ||
       initial_count > value_range[[2L]])) {
    stop_msg <- sprintf(
      "'initial_count' (%.3g) is out of supported range [%.3g, %.3g].",
      initial_count, value_range[[1L]], value_range[[2L]]
    )
    stop(stop_msg, call. = FALSE)
  }


  #--[ Call low-level function ]------------------------------------------------

  .DigitalOutCounterInitSetC(
    handle = device$device_handle,
    channel = as.integer(channel),
    initial_high = identical(initial_state, "high"),
    initial_count = initial_count
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Get Digital Out low and high counters
#'
#' @description
#'   Retrieve the low and high counter values configured for a selected
#'   Digital Out channel.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @template arg_validate_digital_out
#'
#' @details
#'   The low and high counters determine how many counts the output remains
#'   low and high, respectively. The internal clock frequency and divider
#'   value determine the duration of one count.
#'
#'   The function returns the values reported by the corresponding WaveForms
#'   SDK \code{*Get} function.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   A named vector containing \code{low_count} and \code{high_count}. Values
#'   are returned as integers when both can be represented by R integers;
#'   otherwise they are returned as numeric whole-number values.
#'
#' @importFrom checkmate assertFlag
#'
#' @export
#==============================================================================#
GetDigitalOutCounter <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }

  counter_values <- .DigitalOutCounterGetC(
    handle = device$device_handle,
    channel = as.integer(channel)
  )

  counter_values <- .ConvertUnsignedValues(counter_values)
  names(counter_values) <- c("low_count", "high_count")

  return(counter_values)
}



#==============================================================================#
#' Get the initial Digital Out counter
#'
#' @description
#'   Retrieve the initial output state and counter value configured for a
#'   selected Digital Out channel.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @template arg_validate_digital_out
#'
#' @details
#'   The initial state and counter value are loaded when the Digital Out
#'   instrument enters the Running state. The initial counter determines the
#'   number of counts before the ordinary low and high counter sequence begins.
#'
#'   The function returns the values reported by the corresponding WaveForms SDK
#'   \code{*Get} function.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   A named list with the following elements:
#'   \describe{
#'     \item{\code{initial_state}}{
#'       A string, either \code{"low"} or \code{"high"}, identifying the
#'       configured initial output state.
#'     }
#'     \item{\code{initial_count}}{
#'       The configured initial counter value, returned as an integer or a
#'       numeric whole-number scalar if it exceeds R's integer range.
#'     }
#'   }
#'
#' @importFrom checkmate assertFlag
#'
#' @export
#==============================================================================#
GetDigitalOutInitialCounter <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(device, channel = channel)
  }

  counter_values <- .DigitalOutCounterInitGetC(
    handle = device$device_handle,
    channel = as.integer(channel)
  )

  return(list(
    initial_state = if (counter_values$initial_high) "high" else "low",
    initial_count = .ConvertUnsignedValues(counter_values$initial_count)
  ))
}



#==============================================================================#
#' Set a custom Digital Out pattern
#'
#' @description
#'   Set the custom pattern of a selected Digital Out channel.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @param pattern
#'   A non-empty numeric or character vector specifying the custom pattern. A
#'   numeric pattern must contain only \code{0} and \code{1}. A character
#'   pattern must contain only \code{"l"}, \code{"h"}, and \code{"z"}, which
#'   represent logical low, logical high, and high-impedance samples,
#'   respectively. The \code{"z"} value can only be used when
#'   \code{is_three_state = TRUE}.
#' @param is_three_state
#'   A logical scalar indicating whether the pattern should be encoded for
#'   three-state output. This argument controls only pattern encoding; it does
#'   not configure the Digital Out output mode.
#' @template arg_validate_digital_out
#'
#' @details
#'   This function uploads custom data but does not change the signal-generation
#'   function. Before starting generation, configure the channel with
#'   \code{SetDigitalOutFunction(..., func = "custom")}.
#'
#'   With \code{is_three_state = FALSE}, each pattern element consumes one
#'   custom-data bit. With \code{is_three_state = TRUE}, each pattern element
#'   consumes two custom-data bits: an output-value (\code{IO}) bit followed by
#'   an output-enable (\code{OE}) bit. For \code{"l"} and \code{"h"}, the
#'   output is enabled; for \code{"z"}, it is disabled.
#'
#'   When \code{is_three_state = TRUE}, configure the corresponding channel
#'   output mode separately with \code{SetDigitalOutOutput(..., output =
#'   "three_state")}.
#'
#'   The maximum number of custom-data bits depends on the selected channel and
#'   can be queried with \code{GetDigitalOutMaxDataBits()}. Three-state patterns
#'   consume two custom-data bits per pattern element.
#'
#'   Setting a custom pattern automatically configures the Digital Out counters
#'   used internally to step through the pattern. These counters normally do not
#'   need to be configured manually for custom-pattern generation. Changing them
#'   with \code{SetDigitalOutCounter()} or \code{SetDigitalOutInitialCounter()}
#'   after setting the pattern may change how the pattern is generated.
#'
#'   This function updates the Digital Out configuration without stopping
#'   current generation. Call \code{StartDigitalOut()} to start or restart the
#'   instrument with the updated custom pattern. To stop generation, call
#'   \code{StopDigitalOut()} explicitly.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#'
#' @export
#==============================================================================#
SetDigitalOutCustomPattern <- function(
    device,
    channel,
    pattern,
    is_three_state = FALSE,
    .validate_digital_out = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(is_three_state)
  checkmate::assertFlag(.validate_digital_out)

  if (.validate_digital_out) {
    .AssertDigitalOut(
      device,
      channel = channel,
      output = if (is_three_state) "three_state" else NULL,
      func = "custom"
    )
  }

  encoded_pattern <- .EncodeDigitalOutCustomPattern(
    pattern = pattern,
    is_three_state = is_three_state
  )

  max_bits <- GetDigitalOutMaxDataBits(
    device = device,
    channel = channel,
    .validate_digital_out = FALSE
  )

  if (encoded_pattern$n_bits > max_bits) {
    stop_msg <- sprintf(
      "'pattern' requires %d bits, exceeding the supported maximum of %.0f.",
      encoded_pattern$n_bits,
      max_bits
    )
    stop(stop_msg, call. = FALSE)
  }


  #--[ Call low-level function ]------------------------------------------------

  .DigitalOutDataSetC(
    handle = device$device_handle,
    channel = as.integer(channel),
    data = encoded_pattern$data,
    n_bits = encoded_pattern$n_bits
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Set the Digital Out custom-pattern sample rate
#'
#' @description
#'   Set the sample rate used for custom-pattern generation on a selected
#'   Digital Out channel.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @param sample_rate
#'   A positive numeric scalar specifying the custom-pattern sample rate in Hz.
#'   The sample rate is the rate at which individual pattern elements are
#'   generated, not the repetition frequency of the complete pattern. For
#'   example, at a sample rate of 1 kHz, each pattern element lasts 1 ms.
#' @template arg_validate_digital_out
#'
#' @details
#'   The custom-pattern sample rate is determined by the Digital Out internal
#'   clock frequency divided by the ordinary channel divider. Because the
#'   divider is a whole number, the requested sample rate is converted to the
#'   nearest supported divider value. The resulting sample rate may therefore
#'   differ slightly from the requested value.
#'
#'   The initial timing interval may differ from subsequent intervals when the
#'   initial divider differs from the ordinary channel divider.
#'
#'   This function changes the ordinary divider but does not modify the initial
#'   divider. Use \code{GetDigitalOutCustomSampleRate()} to retrieve the sample
#'   rate corresponding to the configured divider.
#'
#'   This function updates the pending Digital Out configuration. With automatic
#'   configuration disabled by \pkg{dwf4r}, changing the sample rate does not
#'   stop current Digital Out generation. Call \code{StartDigitalOut()} to start
#'   or restart the instrument with the updated configuration. To stop
#'   generation, call \code{StopDigitalOut()} explicitly.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertNumber
#'
#' @export
#==============================================================================#
SetDigitalOutCustomSampleRate <- function(
    device,
    channel,
    sample_rate,
    .validate_digital_out = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(
      device,
      channel = channel,
      func = "custom"
    )
  }

  checkmate::assertNumber(
    sample_rate,
    lower = 0,
    finite = TRUE
  )
  if (sample_rate == 0) {
    stop("'sample_rate' must be greater than zero.", call. = FALSE)
  }

  internal_clock <- GetDigitalOutInternalClockFrequency(
    device,
    .validate_device = FALSE
  )

  divider_range <- GetDigitalOutDividerRange(
    device = device,
    channel = channel,
    .validate_digital_out = FALSE
  )

  sample_rate_range <- internal_clock / rev(divider_range)

  if (sample_rate < sample_rate_range[[1L]] ||
      sample_rate > sample_rate_range[[2L]]) {
    stop_msg <- sprintf(
      "'sample_rate' (%.3g Hz) is out of supported range [%.3g, %.3g] Hz.",
      sample_rate,
      sample_rate_range[[1L]],
      sample_rate_range[[2L]]
    )
    stop(stop_msg, call. = FALSE)
  }

  divider <- round(internal_clock / sample_rate)


  #--[ Call low-level function ]------------------------------------------------

  .DigitalOutDividerSetC(
    handle = device$device_handle,
    channel = as.integer(channel),
    divider = divider
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Get the Digital Out custom-pattern sample rate
#'
#' @description
#'   Retrieve the custom-pattern sample rate corresponding to the ordinary
#'   divider configured for a selected Digital Out channel.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @template arg_validate_digital_out
#'
#' @details
#'   The sample rate is calculated as the Digital Out internal clock frequency
#'   divided by the configured ordinary divider.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   A numeric scalar containing the custom-pattern sample rate in Hz. The
#'   sample rate is the rate at which individual pattern elements are generated,
#'   not the repetition frequency of the complete pattern. For example, at a
#'   sample rate of 1 kHz, each pattern element lasts 1 ms.
#'
#' @importFrom checkmate assertFlag
#'
#' @export
#==============================================================================#
GetDigitalOutCustomSampleRate <- function(
    device,
    channel,
    .validate_digital_out = TRUE
) {
  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(
      device,
      channel = channel,
      func = "custom"
    )
  }

  internal_clock <- GetDigitalOutInternalClockFrequency(
    device,
    .validate_device = FALSE
  )

  divider <- GetDigitalOutDivider(
    device = device,
    channel = channel,
    .validate_digital_out = FALSE
  )

  return(internal_clock / divider)
}



#==============================================================================#
#' Set a Digital Out timing parameter
#'
#' @description
#'   Validate and set a selected Digital Out timing parameter.
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param param
#'   A string specifying the timing parameter: \code{"run_time"},
#'   \code{"wait_time"}, or \code{"repeat_count"}.
#' @param value
#'   A numeric scalar specifying the requested value.
#' @param .validate_device
#'   Whether to validate the device using \code{.AssertDevice()}.
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
.SetDigitalOutTimingValue <- function(
    device,
    param,
    value,
    .validate_device = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }

  checkmate::assertChoice(param, c("run_time", "wait_time", "repeat_count"))

  if (identical(param, "repeat_count")) {
    checkmate::assertNumber(
      value,
      lower = 0,
      upper = 2^32 - 1,
      finite = TRUE,
      .var.name = param
    )

    if (value != floor(value)) {
      stop_msg <- sprintf("'%s' must be a whole number.", param)
      stop(stop_msg, call. = FALSE)
    }
  } else { # i.e., "run_time" or "wait_time"
    checkmate::assertNumber(
      value,
      lower = 0,
      finite = TRUE,
      .var.name = param
    )
  }

  range_function <- switch(
    param,
    run_time     = GetDigitalOutRunRange,
    wait_time    = GetDigitalOutWaitRange,
    repeat_count = GetDigitalOutRepeatRange
  )

  value_range <- range_function(
    device = device,
    .validate_device = FALSE
  )

  # Zero is valid for continuous run, no wait, or infinite repetition.
  is_special_zero <- (value == 0)

  if (!is_special_zero &&
      (value < value_range[[1L]] || value > value_range[[2L]])) {
    stop_msg <- sprintf(
      "'%s' (%.3g) is out of supported range [%.3g, %.3g].",
      param, value, value_range[[1L]], value_range[[2L]]
    )
    stop(stop_msg, call. = FALSE)
  }


  #--[ Call low-level function ]------------------------------------------------

  switch(
    param,
    run_time = .DigitalOutRunSetC(
      handle = device$device_handle,
      time_s = value
    ),
    wait_time = .DigitalOutWaitSetC(
      handle = device$device_handle,
      time_s = value
    ),
    repeat_count = .DigitalOutRepeatSetC(
      handle = device$device_handle,
      repeat_count = value
    )
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Set Digital Out timing
#'
#' @description
#'   Set the run time, post-trigger wait time, or repeat count for the Digital
#'   Out instrument. These settings are shared by all Digital Out channels.
#'
#' @template arg_device
#' @template arg_validate_device
#' @param run_time
#'   A finite, non-negative numeric scalar specifying the run length in seconds.
#'   A value of \code{0} requests continuous generation.
#' @param wait_time
#'   A finite, non-negative numeric scalar specifying the delay, in seconds,
#'   between receiving a trigger and beginning signal generation. A value of
#'   \code{0} disables this delay.
#' @param repeat_count
#'   A non-negative whole-number numeric scalar specifying the number of
#'   wait-run cycles. A value of \code{0} requests infinite repetition.
#'
#' @details
#'   Nonzero values are checked against the ranges reported by
#'   \code{GetDigitalOutRunRange()}, \code{GetDigitalOutWaitRange()}, and
#'   \code{GetDigitalOutRepeatRange()}. Zero is accepted as a special value even
#'   when the corresponding reported range has a positive lower bound.
#'
#'   The repeat count controls the number of wait-run cycles, not the number of
#'   individual pulses or copies of a custom pattern. Whether each cycle
#'   requires another trigger depends on the SDK repeat-trigger setting, which
#'   is disabled by default. These functions do not change that setting. With a
#'   run time of \code{0}, generation continues indefinitely and the repeat
#'   count does not impose a finite stopping point.
#'
#'   These functions update the pending Digital Out configuration. Because
#'   automatic configuration is disabled by \pkg{dwf4r}, call
#'   \code{StartDigitalOut()} to start or restart the instrument with the
#'   updated settings.
#'
#'   In \code{SetDigitalOutTiming()}, a \code{NULL} argument leaves the
#'   corresponding setting unchanged. At least one timing parameter must be
#'   supplied. Selected parameters are validated and set in the order
#'   \code{run_time}, \code{wait_time}, and \code{repeat_count}. If an error
#'   occurs, parameters set before the error remain in the pending
#'   configuration.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @name SetDigitalOutTiming
#==============================================================================#
NULL


#' @rdname SetDigitalOutTiming
#' @description
#'   \code{SetDigitalOutRun()} sets the run length of the Digital Out
#'   instrument.
#' @export
SetDigitalOutRun <- function(device, run_time, .validate_device = TRUE) {
  .SetDigitalOutTimingValue(
    device = device,
    param = "run_time",
    value = run_time,
    .validate_device = .validate_device
  )
  return(invisible(NULL))
}


#' @rdname SetDigitalOutTiming
#' @description
#'   \code{SetDigitalOutWait()} sets the delay between receiving a trigger and
#'   beginning signal generation.
#' @export
SetDigitalOutWait <- function(device, wait_time, .validate_device = TRUE) {
  .SetDigitalOutTimingValue(
    device = device,
    param = "wait_time",
    value = wait_time,
    .validate_device = .validate_device
  )
  return(invisible(NULL))
}


#' @rdname SetDigitalOutTiming
#' @description
#'   \code{SetDigitalOutRepeat()} sets the repeat count of the Digital Out
#'   instrument.
#' @export
SetDigitalOutRepeat <- function(device, repeat_count, .validate_device = TRUE) {
  .SetDigitalOutTimingValue(
    device = device,
    param = "repeat_count",
    value = repeat_count,
    .validate_device = .validate_device
  )
  return(invisible(NULL))
}


#' @rdname SetDigitalOutTiming
#' @description
#'   \code{SetDigitalOutTiming()} sets one or more timing parameters of the
#'   Digital Out instrument.
#' @importFrom checkmate assertFlag
#' @export
SetDigitalOutTiming <- function(
    device,
    run_time = NULL,
    wait_time = NULL,
    repeat_count = NULL,
    .validate_device = TRUE
) {
  checkmate::assertFlag(.validate_device)

  if (all(
    is.null(run_time),
    is.null(wait_time),
    is.null(repeat_count)
  )) {
    stop(
      "At least one Digital Out timing parameter must be specified.",
      call. = FALSE
    )
  }

  if (.validate_device) {
    .AssertDevice(device)
  }

  if (!is.null(run_time)) {
    .SetDigitalOutTimingValue(
      device = device,
      param = "run_time",
      value = run_time,
      .validate_device = FALSE
    )
  }

  if (!is.null(wait_time)) {
    .SetDigitalOutTimingValue(
      device = device,
      param = "wait_time",
      value = wait_time,
      .validate_device = FALSE
    )
  }

  if (!is.null(repeat_count)) {
    .SetDigitalOutTimingValue(
      device = device,
      param = "repeat_count",
      value = repeat_count,
      .validate_device = FALSE
    )
  }

  return(invisible(NULL))
}



#==============================================================================#
#' Get a Digital Out timing parameter
#'
#' @description
#'   Retrieve a selected configured Digital Out timing parameter.
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param param
#'   A string specifying the timing parameter: \code{"run_time"},
#'   \code{"wait_time"}, or \code{"repeat_count"}.
#' @param .validate_device
#'   Whether to validate the device using \code{.AssertDevice()}.
#'
#' @return
#'   A numeric scalar containing the configured value. Repeat counts are
#'   returned as integers when representable as R integers, otherwise as
#'   numeric values.
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertChoice
#'
#' @noRd
#==============================================================================#
.GetDigitalOutTimingValue <- function(
    device,
    param,
    .validate_device = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }

  checkmate::assertChoice(param, c("run_time", "wait_time", "repeat_count"))


  #--[ Call low-level function ]------------------------------------------------

  getter_function <- switch(
    param,
    run_time     = .DigitalOutRunGetC,
    wait_time    = .DigitalOutWaitGetC,
    repeat_count = .DigitalOutRepeatGetC
  )

  value <- getter_function(handle = device$device_handle)

  if (identical(param, "repeat_count")) {
    return(.ConvertUnsignedValues(value))
  }
  return(value)
}



#==============================================================================#
#' Get Digital Out timing
#'
#' @description
#'   Retrieve the configured run time, post-trigger wait time, or repeat count
#'   for the Digital Out instrument. These settings are shared by all Digital
#'   Out channels.
#'
#' @template arg_device
#' @template arg_validate_device
#'
#' @details
#'   These functions retrieve the configured timing values reported by the
#'   corresponding WaveForms SDK \code{*Get} functions. A run time of \code{0}
#'   represents continuous generation, a wait time of \code{0} represents no
#'   post-trigger delay, and a repeat count of \code{0} represents infinite
#'   repetition.
#'
#'   Configured values are distinct from remaining run time and repeat count.
#'   Use \code{GetDigitalOutRemainingRun()} and
#'   \code{GetDigitalOutRemainingRepeat()} to retrieve the remaining values.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   \code{GetDigitalOutRun()} and \code{GetDigitalOutWait()} return numeric
#'   scalars in seconds. \code{GetDigitalOutRepeat()} returns an integer scalar
#'   when the count is no greater than \code{.Machine$integer.max}; otherwise it
#'   returns a numeric scalar to preserve the full unsigned SDK range.
#'
#'   \code{GetDigitalOutTiming()} returns a named list with the following
#'   elements:
#'   \describe{
#'     \item{\code{run_time}}{
#'       Numeric configured run time in seconds.
#'     }
#'     \item{\code{wait_time}}{
#'       Numeric configured post-trigger wait time in seconds.
#'     }
#'     \item{\code{repeat_count}}{
#'       Integer or numeric configured repeat count, using the same conversion
#'       as \code{GetDigitalOutRepeat()}.
#'     }
#'   }
#'
#' @name GetDigitalOutTiming
#==============================================================================#
NULL


#' @rdname GetDigitalOutTiming
#' @description
#'   \code{GetDigitalOutRun()} returns the configured run length of the Digital
#'   Out instrument.
#' @export
GetDigitalOutRun <- function(device, .validate_device = TRUE) {
  return(.GetDigitalOutTimingValue(
    device = device,
    param = "run_time",
    .validate_device = .validate_device
  ))
}


#' @rdname GetDigitalOutTiming
#' @description
#'   \code{GetDigitalOutWait()} returns the configured delay between receiving a
#'   trigger and beginning signal generation.
#' @export
GetDigitalOutWait <- function(device, .validate_device = TRUE) {
  return(.GetDigitalOutTimingValue(
    device = device,
    param = "wait_time",
    .validate_device = .validate_device
  ))
}


#' @rdname GetDigitalOutTiming
#' @description
#'   \code{GetDigitalOutRepeat()} returns the configured repeat count of the
#'   Digital Out instrument.
#' @export
GetDigitalOutRepeat <- function(device, .validate_device = TRUE) {
  return(.GetDigitalOutTimingValue(
    device = device,
    param = "repeat_count",
    .validate_device = .validate_device
  ))
}


#' @rdname GetDigitalOutTiming
#' @description
#'   \code{GetDigitalOutTiming()} returns all configured timing parameters of
#'   the Digital Out instrument.
#' @importFrom checkmate assertFlag
#' @export
GetDigitalOutTiming <- function(device, .validate_device = TRUE) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }

  out <- list(
    run_time = .GetDigitalOutTimingValue(
      device = device,
      param = "run_time",
      .validate_device = FALSE
    ),
    wait_time = .GetDigitalOutTimingValue(
      device = device,
      param = "wait_time",
      .validate_device = FALSE
    ),
    repeat_count = .GetDigitalOutTimingValue(
      device = device,
      param = "repeat_count",
      .validate_device = FALSE
    )
  )

  return(out)
}



#==============================================================================#
#' Get a remaining Digital Out timing value
#'
#' @description
#'   Retrieve a remaining Digital Out timing value from the current or most
#'   recent status update.
#'
#' @param device
#'   A \code{"dwf4r_device"} object.
#' @param param
#'   A string specifying the remaining value: \code{"run_time"} or
#'   \code{"repeat_count"}.
#' @param update
#'   A logical scalar indicating whether the Digital Out status should be
#'   updated before retrieving the remaining value.
#' @param .validate_device
#'   Whether to validate the device using \code{.AssertDevice()}.
#'
#' @return
#'   A numeric scalar containing the requested remaining value. Repeat counts
#'   are returned as integers when representable as R integers, otherwise as
#'   numeric values.
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertChoice
#'
#' @noRd
#==============================================================================#
.GetDigitalOutRemainingValue <- function(
    device,
    param,
    update = TRUE,
    .validate_device = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }

  checkmate::assertChoice(param, c("run_time", "repeat_count"))
  checkmate::assertFlag(update)


  #--[ Update status ]----------------------------------------------------------

  if (update) {
    .DigitalOutStatusC(handle = device$device_handle)
  }


  #--[ Get remaining value ]----------------------------------------------------

  status_function <- switch(
    param,
    run_time     = .DigitalOutRunStatusC,
    repeat_count = .DigitalOutRepeatStatusC
  )

  value <- status_function(handle = device$device_handle)

  if (identical(param, "repeat_count")) {
    return(.ConvertUnsignedValues(value))
  }
  return(value)
}



#==============================================================================#
#' Get remaining Digital Out timing values
#'
#' @description
#'   Retrieve the remaining run value or repeat count for the Digital Out
#'   instrument. The remaining wait time cannot be retrieved because the
#'   WaveForms SDK does not provide a corresponding Digital Out wait-status
#'   function.
#'
#' @template arg_device
#' @template arg_validate_device
#' @param update
#'   A logical scalar indicating whether the Digital Out status should be
#'   updated before retrieving the remaining value. The default is \code{TRUE}.
#'   If \code{FALSE}, the value from the most recent Digital Out status update
#'   is returned.
#'
#' @details
#'   The WaveForms SDK functions used to retrieve the remaining run time and
#'   repeat count do not themselves read the device. They return information
#'   obtained by the most recent \code{FDwfDigitalOutStatus()} call, performed
#'   by \code{GetDigitalOutStatus()} or by either remaining-value function
#'   with \code{update = TRUE}.
#'
#'   With the default \code{update = TRUE}, these functions update the Digital
#'   Out status before retrieving the remaining value. With
#'   \code{update = FALSE}, no status update is performed and the returned
#'   value may therefore be stale.
#'
#'   To retrieve both remaining values from the same status update, first call
#'   \code{GetDigitalOutStatus()}, then call both remaining-value functions
#'   with \code{update = FALSE}.
#'
#'   These functions preserve the values reported by the SDK. They do not
#'   convert values to \code{Inf} or infer completion from a remaining value
#'   of zero. Use \code{GetDigitalOutStatus()} to determine the instrument
#'   state and the configured timing getters to identify continuous generation
#'   or infinite repetition.
#'
#'   The WaveForms SDK does not document the units returned by
#'   \code{FDwfDigitalOutRunStatus()}. On Analog Discovery 2, the returned value
#'   was found to correspond to Digital Out internal-clock ticks. This behavior
#'   has not been verified for other devices.
#'
#' @seealso
#'   See the vignette \emph{Basic Digital Out Functionality} for a complete
#'   Digital Out workflow.
#'
#' @return
#'   \code{GetDigitalOutRemainingRun()} returns a numeric scalar containing the
#'   remaining run value reported by the SDK. For Analog Discovery 2, this value
#'   is reported in Digital Out internal-clock ticks; the units have not been
#'   verified for other devices.
#'
#'   \code{GetDigitalOutRemainingRepeat()} returns the remaining repeat count
#'   as an integer scalar when it is no greater than
#'   \code{.Machine$integer.max}; otherwise it returns a numeric scalar to
#'   preserve the full unsigned SDK range.
#'
#' @name GetDigitalOutRemaining
#==============================================================================#
NULL


#' @rdname GetDigitalOutRemaining
#' @description
#'   \code{GetDigitalOutRemainingRun()} returns the remaining run value reported
#'   by the Digital Out instrument.
#' @export
GetDigitalOutRemainingRun <- function(
    device,
    update = TRUE,
    .validate_device = TRUE
) {
  return(.GetDigitalOutRemainingValue(
    device = device,
    param = "run_time",
    update = update,
    .validate_device = .validate_device
  ))
}


#' @rdname GetDigitalOutRemaining
#' @description
#'   \code{GetDigitalOutRemainingRepeat()} returns the remaining repeat count
#'   of the Digital Out instrument.
#' @export
GetDigitalOutRemainingRepeat <- function(
    device,
    update = TRUE,
    .validate_device = TRUE
) {
  return(.GetDigitalOutRemainingValue(
    device = device,
    param = "repeat_count",
    update = update,
    .validate_device = .validate_device
  ))
}



#==============================================================================#
#' Set the Digital Out trigger source
#'
#' @description
#'   Set the trigger source for the Digital Out instrument. This setting is
#'   shared by all Digital Out channels.
#'
#' @template arg_device
#' @param source
#'   A string specifying the trigger source. Available sources depend on the
#'   selected device and can be queried with \code{GetDeviceTriggerSources()}.
#'   Names use the same convention as \code{SetAnalogOutTriggerSource()}. The
#'   SDK default is \code{"none"}.
#' @template arg_validate_device
#'
#' @details
#'   This function updates the pending Digital Out configuration. Because
#'   automatic configuration is disabled by \pkg{dwf4r}, call
#'   \code{StartDigitalOut()} to start or restart the instrument with the
#'   updated setting. To stop generation, call \code{StopDigitalOut()}.
#'
#'   For PC triggering, select \code{source = "pc"}, start the instrument with
#'   \code{StartDigitalOut()}, and use \code{TriggerDevice()} to issue a PC
#'   trigger pulse while the instrument is armed. Other instruments configured
#'   to use the PC trigger can also respond to this pulse.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertString
#' @importFrom checkmate testChoice
#'
#' @export
#==============================================================================#
SetDigitalOutTriggerSource <- function(
    device,
    source,
    .validate_device = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }
  checkmate::assertString(source)

  supported_sources <- GetDeviceTriggerSources(
    device,
    .validate_device = FALSE
  )
  if (!checkmate::testChoice(source, supported_sources)) {
    stop_msg <- sprintf(
      "Trigger source '%s' is not supported by the device.",
      source
    )
    stop(stop_msg, call. = FALSE)
  }


  #--[ Call low-level function ]------------------------------------------------

  .DigitalOutTriggerSourceSetC(
    handle = device$device_handle,
    source_code = unname(.dwf_constants$trigger$source_code[[source]])
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Get the Digital Out trigger source
#'
#' @description
#'   Retrieve the trigger source currently configured for the Digital Out
#'   instrument. This setting is shared by all Digital Out channels.
#'
#' @template arg_device
#' @template arg_validate_device
#'
#' @return
#'   A string identifying the configured trigger source. An error is raised if
#'   the SDK returns a source code not recognized by \pkg{dwf4r}.
#'
#' @importFrom checkmate assertFlag
#'
#' @export
#==============================================================================#
GetDigitalOutTriggerSource <- function(device, .validate_device = TRUE) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }

  source_code <- .DigitalOutTriggerSourceGetC(
    handle = device$device_handle
  )

  return(.ConvertCodeToName(
    code = source_code,
    code_map = .dwf_constants$trigger$source_code,
    value_type = "Digital Out trigger source"
  ))
}



#==============================================================================#
#' Set the Digital Out trigger slope
#'
#' @description
#'   Set the trigger slope for the Digital Out instrument. This setting is
#'   shared by all Digital Out channels.
#'
#' @template arg_device
#' @param slope
#'   A string specifying the trigger slope. Recognized values are
#'   \code{"rising"}, \code{"falling"}, and \code{"either"}. Supported values
#'   can be queried with \code{GetDeviceTriggerSlopes()}.
#' @template arg_validate_device
#'
#' @details
#'   This function updates the pending Digital Out configuration. Because
#'   automatic configuration is disabled by \pkg{dwf4r}, call
#'   \code{StartDigitalOut()} to start or restart the instrument with the
#'   updated setting. To stop generation, call \code{StopDigitalOut()}.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertString
#' @importFrom checkmate testChoice
#'
#' @export
#==============================================================================#
SetDigitalOutTriggerSlope <- function(
    device,
    slope,
    .validate_device = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }
  checkmate::assertString(slope)

  supported_slopes <- GetDeviceTriggerSlopes(device, .validate_device = FALSE)
  if (!checkmate::testChoice(slope, supported_slopes)) {
    stop_msg <- sprintf(
      "Trigger slope '%s' is not supported by the device.",
      slope
    )
    stop(stop_msg, call. = FALSE)
  }


  #--[ Call low-level function ]------------------------------------------------

  .DigitalOutTriggerSlopeSetC(
    handle = device$device_handle,
    slope_code = unname(.dwf_constants$trigger$slope_code[[slope]])
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Get the Digital Out trigger slope
#'
#' @description
#'   Retrieve the trigger slope currently configured for the Digital Out
#'   instrument. This setting is shared by all Digital Out channels.
#'
#' @template arg_device
#' @template arg_validate_device
#'
#' @return
#'   A string identifying the configured trigger slope: \code{"rising"},
#'   \code{"falling"}, or \code{"either"}. An error is raised if the SDK returns
#'   a slope code not recognized by \pkg{dwf4r}.
#'
#' @importFrom checkmate assertFlag
#'
#' @export
#==============================================================================#
GetDigitalOutTriggerSlope <- function(device, .validate_device = TRUE) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }

  slope_code <- .DigitalOutTriggerSlopeGetC(handle = device$device_handle)
  return(.ConvertCodeToName(
    code = slope_code,
    code_map = .dwf_constants$trigger$slope_code,
    value_type = "Digital Out trigger slope"
  ))
}



#==============================================================================#
#' Set Digital Out repeat triggering
#'
#' @description
#'   Set whether a trigger is included in each wait-run repeat cycle of the
#'   Digital Out instrument. This setting is shared by all Digital Out channels.
#'
#' @template arg_device
#' @param repeat_trigger
#'   A logical scalar. If \code{TRUE}, each subsequent repeat cycle returns to
#'   the Armed state before proceeding through Wait and Running. If
#'   \code{FALSE}, subsequent cycles proceed without waiting for another
#'   trigger. The SDK default is \code{FALSE}.
#' @template arg_validate_device
#'
#' @details
#'   This setting controls trigger behavior during repetition and does not
#'   specify the number of repetitions. Use \code{SetDigitalOutRepeat()} to set
#'   the repeat count. The selected trigger source determines the trigger
#'   condition; enabling repeat triggering does not itself select a source.
#'
#'   A run time of \code{0} requests continuous generation, so the instrument
#'   does not reach another wait-run repeat cycle.
#'
#'   This function updates the pending Digital Out configuration. Because
#'   automatic configuration is disabled by \pkg{dwf4r}, call
#'   \code{StartDigitalOut()} to start or restart the instrument with the
#'   updated setting. To stop generation, call \code{StopDigitalOut()}.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @importFrom checkmate assertFlag
#'
#' @export
#==============================================================================#
SetDigitalOutRepeatTrigger <- function(
    device,
    repeat_trigger,
    .validate_device = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }
  checkmate::assertFlag(repeat_trigger)


  #--[ Call low-level function ]------------------------------------------------

  .DigitalOutRepeatTriggerSetC(
    handle = device$device_handle,
    repeat_trigger = repeat_trigger
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Get Digital Out repeat triggering
#'
#' @description
#'   Retrieve whether a trigger is included in each wait-run repeat cycle of the
#'   Digital Out instrument. This setting is shared by all Digital Out channels.
#'
#' @template arg_device
#' @template arg_validate_device
#'
#' @return
#'   A logical scalar. \code{TRUE} indicates that subsequent repeat cycles
#'   return to the Armed state before proceeding through Wait and Running;
#'   \code{FALSE} indicates that they proceed without waiting for another
#'   trigger. This is the configured setting, not the current instrument state.
#'
#' @importFrom checkmate assertFlag
#'
#' @export
#==============================================================================#
GetDigitalOutRepeatTrigger <- function(device, .validate_device = TRUE) {
  checkmate::assertFlag(.validate_device)
  if (.validate_device) {
    .AssertDevice(device)
  }

  return(.DigitalOutRepeatTriggerGetC(
    handle = device$device_handle
  ))
}


