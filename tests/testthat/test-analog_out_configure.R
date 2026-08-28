#==[ Mock-based tests ]=========================================================

test_that("Analog Out: control and carrier configuration", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(
    list(device_handle = 123L)
  )

  mock_state <- new.env(parent = emptyenv())
  mock_state$last_call <- list()
  mock_state$calls <- list()

  RegisterNewCall <- function(call) {
    call_index <- length(mock_state$calls) + 1L
    mock_state$calls[[call_index]] <- call
    mock_state$last_call <- call
    return(invisible(NULL))
  }

  local_mocked_bindings(
    .QueryAnalogOutChannelCountC = function(handle) {
      return(2L)
    },
    .QueryAnalogOutChannelNodesC = function(handle, channel) {
      return(c("carrier", "fm", "am"))
    },
    .QueryAnalogOutNodeFunctionTypesC = function(handle, channel, node) {
      return(c("dc", "sine", "square", "custom"))
    },
    .QueryAnalogOutNodeFrequencyRangeC = function(handle, channel, node) {
      return(c(1e-06, 1e+08))
    },
    .QueryAnalogOutNodeAmplitudeRangeC = function(handle, channel, node) {
      return(c(0.01, 5))
    },
    .QueryAnalogOutNodeOffsetRangeC = function(handle, channel, node) {
      return(c(-5, 5))
    },
    .QueryAnalogOutNodeSymmetryRangeC = function(handle, channel, node) {
      return(c(0, 100))
    },
    .QueryAnalogOutNodePhaseRangeC = function(handle, channel, node) {
      return(c(0, 360))
    },
    .QueryAnalogOutNodeSampleCountRangeC = function(handle, channel, node) {
      return(c(1L, 4096L))
    },
    .QueryAnalogOutIdleModesC = function(handle, channel) {
      return(c("offset", "initial", "hold"))
    },

    .AnalogOutResetC = function(handle, channel) {
      call <- list(call_type = "reset", handle = handle, channel = channel)
      RegisterNewCall(call)
      return(invisible(NULL))
    },
    .AnalogOutConfigureC = function(handle, channel, action) {
      call <- list(
        call_type = "configure",
        handle = handle,
        channel = channel,
        action = action
      )
      RegisterNewCall(call)
      return(invisible(NULL))
    },
    .AnalogOutNodeEnableSetC = function(handle, channel, node, mode) {
      call <- list(
        call_type = "set_node_mode",
        handle = handle,
        channel = channel,
        node = node,
        mode = mode
      )
      RegisterNewCall(call)
      return(invisible(NULL))
    },
    .AnalogOutNodeFunctionSetC = function(handle,
                                          channel,
                                          node,
                                          function_code) {
      call <- list(
        call_type = "set_function",
        handle = handle,
        channel = channel,
        node = node,
        function_code = function_code
      )
      RegisterNewCall(call)
      return(invisible(NULL))
    },
    .AnalogOutNodeFrequencySetC = function(handle, channel, node, value) {
      call <- list(
        call_type = "set_frequency",
        handle = handle,
        channel = channel,
        node = node,
        value = value
      )
      RegisterNewCall(call)
      return(invisible(NULL))
    },
    .AnalogOutNodeAmplitudeSetC = function(handle, channel, node, value) {
      call <- list(
        call_type = "set_amplitude",
        handle = handle,
        channel = channel,
        node = node,
        value = value
      )
      RegisterNewCall(call)
      return(invisible(NULL))
    },
    .AnalogOutNodeOffsetSetC = function(handle, channel, node, value) {
      call <- list(
        call_type = "set_offset",
        handle = handle,
        channel = channel,
        node = node,
        value = value
      )
      RegisterNewCall(call)
      return(invisible(NULL))
    },
    .AnalogOutNodeSymmetrySetC = function(handle, channel, node, value) {
      call <- list(
        call_type = "set_symmetry",
        handle = handle,
        channel = channel,
        node = node,
        value = value
      )
      RegisterNewCall(call)
      return(invisible(NULL))
    },
    .AnalogOutNodePhaseSetC = function(handle, channel, node, value) {
      call <- list(
        call_type = "set_phase",
        handle = handle,
        channel = channel,
        node = node,
        value = value
      )
      RegisterNewCall(call)
      return(invisible(NULL))
    },
    .AnalogOutNodeDataSetC = function(handle, channel, node, data) {
      call <- list(
        call_type = "set_data",
        handle = handle,
        channel = channel,
        node = node,
        data = data
      )
      RegisterNewCall(call)
      return(invisible(NULL))
    },
    .AnalogOutIdleSetC = function(handle, channel, idle_mode_code) {
      call <- list(
        call_type = "set_idle_mode",
        handle = handle,
        channel = channel,
        idle_mode_code = idle_mode_code
      )
      RegisterNewCall(call)
      return(invisible(NULL))
    },
    .AnalogOutMasterSetC = function(handle, channel, master_channel) {
      call <- list(
        call_type = "set_master",
        handle = handle,
        channel = channel,
        master_channel = master_channel
      )
      RegisterNewCall(call)
      return(invisible(NULL))
    },
    .AnalogOutMasterGetC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_master",
        handle = handle,
        channel = channel
      )
      return(1L)
    },

    .package = "dwf4r"
  )


  #.. ResetAnalogOut() .........................................................

  expect_null(ResetAnalogOut(device, channel = 1L))
  expect_identical(
    mock_state$last_call,
    list(call_type = "reset", handle = 123L, channel = 1L)
  )


  #.. .ConfigureAnalogOut(), *AnalogOutSettings() ..............................

  expect_null(ApplyAnalogOutSettings(device, channel = 1L))
  expect_identical(
    mock_state$last_call,
    list(call_type = "configure", handle = 123L, channel = 1L, action = 3L)
  )

  expect_null(StartAnalogOut(device, channel = 1L))
  expect_identical(
    mock_state$last_call,
    list(call_type = "configure", handle = 123L, channel = 1L, action = 1L)
  )

  expect_null(StopAnalogOut(device, channel = 1L))
  expect_identical(
    mock_state$last_call,
    list(call_type = "configure", handle = 123L, channel = 1L, action = 0L)
  )

  expect_error(
    .ConfigureAnalogOut(device, channel = 0L, action = "invalid_action"),
    'action' # "Assertion on 'action' failed"
  )


  #.. .SetAnalogOutNodeMode(), EnableCarrier(), DisableCarrier .................

  expect_null(EnableCarrier(device, channel = 0L))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_node_mode",
      handle = 123L,
      channel = 0L,
      node = 0L,
      mode = 1L
    )
  )

  expect_null(DisableCarrier(device, channel = 0L))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_node_mode",
      handle = 123L,
      channel = 0L,
      node = 0L,
      mode = 0L
    )
  )

  expect_error(
    .SetAnalogOutNodeMode(
      device,
      channel = 0L,
      node = "carrier",
      mode = "invalid_mode"
    ),
    'mode' #"Assertion on 'mode' failed"
  )


  #.. SetCarrierFunction() .....................................................

  expect_null(SetCarrierFunction(device, channel = 1L, func = "sine"))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_function",
      handle = 123L,
      channel = 1L,
      node = 0L,
      function_code = 1L
    )
  )
  expect_error(
    SetCarrierFunction(device, channel = 1L, func = "triangle"),
    "Function 'triangle' is not supported"
  )


  #.. .SetAnalogOutValue(), SetCarrier*() ......................................

  expect_null(SetCarrierFrequency(device, channel = 1L, frequency = 1000))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_frequency",
      handle = 123L,
      channel = 1L,
      node = 0L,
      value = 1000
    )
  )
  expect_error(
    SetCarrierFrequency(device, channel = 1L, frequency = 1e10),
    "out of supported range"
  )

  expect_null(SetCarrierAmplitude(device, channel = 1L, amplitude = 2))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_amplitude",
      handle = 123L,
      channel = 1L,
      node = 0L,
      value = 2
    )
  )
  expect_error(
    SetCarrierAmplitude(device, channel = 1L, amplitude = 99),
    "out of supported range"
  )

  expect_null(SetCarrierOffset(device, channel = 1L, offset = -1))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_offset",
      handle = 123L,
      channel = 1L,
      node = 0L,
      value = -1
    )
  )
  expect_error(
    SetCarrierOffset(device, channel = 1L, offset = 99),
    "out of supported range"
  )

  expect_null(SetCarrierSymmetry(device, channel = 1L, symmetry = 25))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_symmetry",
      handle = 123L,
      channel = 1L,
      node = 0L,
      value = 25
    )
  )
  expect_error(
    SetCarrierSymmetry(device, channel = 1L, symmetry = 999),
    "out of supported range"
  )

  expect_null(SetCarrierPhase(device, channel = 1L, phase = 90))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_phase",
      handle = 123L,
      channel = 1L,
      node = 0L,
      value = 90
    )
  )
  expect_error(
    SetCarrierPhase(device, channel = 1L, phase = 999),
    "out of supported range"
  )

  expect_error(
    .SetAnalogOutValue(
      device,
      channel = 0L,
      node = "carrier",
      param = "invalid_param",
      value = 1
    ),
    'param' # "Assertion on 'param' failed"
  )


  #.. SetCarrier() .............................................................

  expect_null(
    SetCarrier(
      device,
      channel = 1L,
      func = "sine",
      frequency = 1000,
      amplitude = 2,
      offset = -1,
      symmetry = 25,
      phase = 90
    )
  )
  expect_identical(
    tail(mock_state$calls, 6L),
    list(
      list(
        call_type = "set_function",
        handle = 123L,
        channel = 1L,
        node = 0L,
        function_code = 1L
      ),
      list(
        call_type = "set_frequency",
        handle = 123L,
        channel = 1L,
        node = 0L,
        value = 1000
      ),
      list(
        call_type = "set_amplitude",
        handle = 123L,
        channel = 1L,
        node = 0L,
        value = 2
      ),
      list(
        call_type = "set_offset",
        handle = 123L,
        channel = 1L,
        node = 0L,
        value = -1
      ),
      list(
        call_type = "set_symmetry",
        handle = 123L,
        channel = 1L,
        node = 0L,
        value = 25
      ),
      list(
        call_type = "set_phase",
        handle = 123L,
        channel = 1L,
        node = 0L,
        value = 90
      )
    )
  )

  expect_null(SetCarrier(device, channel = 1L, func = "square", amplitude = 3))
  expect_identical(
    tail(mock_state$calls, 2L),
    list(
      list(
        call_type = "set_function",
        handle = 123L,
        channel = 1L,
        node = 0L,
        function_code = 2L
      ),
      list(
        call_type = "set_amplitude",
        handle = 123L,
        channel = 1L,
        node = 0L,
        value = 3
      )
    )
  )

  expect_null(SetCarrier(device, channel = 1L, phase = 45))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_phase",
      handle = 123L,
      channel = 1L,
      node = 0L,
      value = 45
    )
  )

  #.. SetCarrierData() .........................................................

  expect_null(SetCarrierData(device, channel = 1L, data = c(-0.5, 0.5, -0.5)))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_data",
      handle = 123L,
      channel = 1L,
      node = 0L,
      data = c(-0.5, 0.5, -0.5)
    )
  )
  expect_error(
    SetCarrierData(device, channel = 1L, data = rep(0, 9999L)),
    "out of supported range"
  )
  expect_error(
    SetCarrierData(device, channel = 1L, data = c(-0.5, 1.1, -0.3)),
    'data' # "Assertion on 'data' failed"
  )


  #.. SetAnalogOutIdle() .......................................................

  expect_null(SetAnalogOutIdle(device, channel = 1L, idle = "offset"))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_idle_mode",
      handle = 123L,
      channel = 1L,
      idle_mode_code = 1L
    )
  )
  expect_error(
    SetAnalogOutIdle(device, channel = 1L, idle = "invalide_idle_mode"),
    'idle' # "Assertion on 'idle' failed"
  )


  #.. SetAnalogOutMaster() ......................................................

  expect_null(
    SetAnalogOutMaster(
      device,
      channel = 1L,
      master_channel = 0L
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_master",
      handle = 123L,
      channel = 1L,
      master_channel = 0L
    )
  )

  # A channel can be its own master for independent operation.
  expect_null(
    SetAnalogOutMaster(
      device,
      channel = 1L,
      master_channel = 1L
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_master",
      handle = 123L,
      channel = 1L,
      master_channel = 1L
    )
  )

  expect_error(
    SetAnalogOutMaster(
      device,
      channel = 1L,
      master_channel = 2L
    ),
    "'master_channel'" # 'master_channel' is out of range
  )

  expect_error(
    SetAnalogOutMaster(
      device,
      channel = 1L,
      master_channel = -1L
    ),
    "master_channel" # Assertion on 'master_channel' failed
  )


  #.. GetAnalogOutMaster() ......................................................

  expect_identical(
    GetAnalogOutMaster(device, channel = 0L),
    1L
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_master",
      handle = 123L,
      channel = 0L
    )
  )
})


test_that("Analog Out: configuration getters", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(
    list(device_handle = 123L)
  )

  mock_state <- new.env(parent = emptyenv())
  mock_state$last_call <- list()
  mock_state$carrier_mode <- 1L

  local_mocked_bindings(
    .QueryAnalogOutChannelCountC = function(handle) {
      return(2L)
    },
    .QueryAnalogOutChannelNodesC = function(handle, channel) {
      return(c("carrier", "fm", "am"))
    },

    .AnalogOutStatusC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_status",
        handle = handle,
        channel = channel
      )
      return("running")
    },
    .AnalogOutNodeEnableGetC = function(handle, channel, node) {
      mock_state$last_call <- list(
        call_type = "get_node_mode",
        handle = handle,
        channel = channel,
        node = node
      )
      return(mock_state$carrier_mode)
    },
    .AnalogOutNodeFunctionGetC = function(handle, channel, node) {
      mock_state$last_call <- list(
        call_type = "get_function",
        handle = handle,
        channel = channel,
        node = node
      )
      return(1L)
    },
    .AnalogOutNodeFrequencyGetC = function(handle, channel, node) {
      mock_state$last_call <- list(
        call_type = "get_frequency",
        handle = handle,
        channel = channel,
        node = node
      )
      return(1000)
    },
    .AnalogOutNodeAmplitudeGetC = function(handle, channel, node) {
      mock_state$last_call <- list(
        call_type = "get_amplitude",
        handle = handle,
        channel = channel,
        node = node
      )
      return(2)
    },
    .AnalogOutNodeOffsetGetC = function(handle, channel, node) {
      mock_state$last_call <- list(
        call_type = "get_offset",
        handle = handle,
        channel = channel,
        node = node
      )
      return(-1)
    },
    .AnalogOutNodeSymmetryGetC = function(handle, channel, node) {
      mock_state$last_call <- list(
        call_type = "get_symmetry",
        handle = handle,
        channel = channel,
        node = node
      )
      return(25)
    },
    .AnalogOutNodePhaseGetC = function(handle, channel, node) {
      mock_state$last_call <- list(
        call_type = "get_phase",
        handle = handle,
        channel = channel,
        node = node
      )
      return(90)
    },
    .AnalogOutIdleGetC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_idle",
        handle = handle,
        channel = channel
      )
      return(1L)
    },

    .package = "dwf4r"
  )


  #.. GetAnalogOutStatus() .....................................................

  expect_identical(
    GetAnalogOutStatus(device, channel = 1L),
    "running"
  )
  expect_identical(
    mock_state$last_call,
    list(call_type = "get_status", handle = 123L, channel = 1L)
  )


  #.. IsCarrierEnabled() .......................................................

  expect_true(IsCarrierEnabled(device, channel = 1L))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_node_mode",
      handle = 123L,
      channel = 1L,
      node = 0L
    )
  )

  mock_state$carrier_mode <- 0L
  expect_false(IsCarrierEnabled(device, channel = 1L))
  mock_state$carrier_mode <- 1L


  #.. GetCarrierFunction() .....................................................

  expect_identical(
    GetCarrierFunction(device, channel = 1L),
    "sine"
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_function",
      handle = 123L,
      channel = 1L,
      node = 0L
    )
  )


  #.. GetCarrier*() ............................................................

  expect_identical(
    GetCarrierFrequency(device, channel = 1L),
    1000
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_frequency",
      handle = 123L,
      channel = 1L,
      node = 0L
    )
  )

  expect_identical(
    GetCarrierAmplitude(device, channel = 1L),
    2
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_amplitude",
      handle = 123L,
      channel = 1L,
      node = 0L
    )
  )

  expect_identical(
    GetCarrierOffset(device, channel = 1L),
    -1
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_offset",
      handle = 123L,
      channel = 1L,
      node = 0L
    )
  )

  expect_identical(
    GetCarrierSymmetry(device, channel = 1L),
    25
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_symmetry",
      handle = 123L,
      channel = 1L,
      node = 0L
    )
  )

  expect_identical(
    GetCarrierPhase(device, channel = 1L),
    90
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_phase",
      handle = 123L,
      channel = 1L,
      node = 0L
    )
  )


  #.. GetCarrierSettings() .....................................................

  expect_identical(
    GetCarrierSettings(device, channel = 1L),
    list(
      enabled = TRUE,
      func = "sine",
      frequency = 1000,
      amplitude = 2,
      offset = -1,
      symmetry = 25,
      phase = 90
    )
  )


  #.. GetAnalogOutIdle() .......................................................

  expect_identical(
    GetAnalogOutIdle(device, channel = 1L),
    "offset"
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_idle",
      handle = 123L,
      channel = 1L
    )
  )
})



test_that("Analog Out: timing configuration and status", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(
    list(device_handle = 123L)
  )

  mock_state <- new.env(parent = emptyenv())
  mock_state$calls <- list()
  mock_state$channel_count_queries <- 0L

  RegisterNewCall <- function(call) {
    call_index <- length(mock_state$calls) + 1L
    mock_state$calls[[call_index]] <- call
    mock_state$last_call <- call
    return(invisible(NULL))
  }

  local_mocked_bindings(
    .QueryAnalogOutChannelCountC = function(handle) {
      mock_state$channel_count_queries <-
        mock_state$channel_count_queries + 1L
      return(2L)
    },
    .QueryAnalogOutRunRangeC = function(handle, channel) {
      # The lower bound is intentionally set to 1e-6 (not 0)
      return(c(1e-6, 86400))
    },
    .QueryAnalogOutWaitRangeC = function(handle, channel) {
      # The lower bound is intentionally set to 1e-6 (not 0)
      return(c(1e-6, 86400))
    },
    .QueryAnalogOutRepeatRangeC = function(handle, channel) {
      # The lower bound is intentionally set to 1L (not 0L)
      return(c(1L, 32000L))
    },
    .AnalogOutRunSetC = function(handle, channel, time_s) {
      RegisterNewCall(list(
        call_type = "set_run",
        handle = handle,
        channel = channel,
        time_s = time_s
      ))
      return(invisible(NULL))
    },
    .AnalogOutWaitSetC = function(handle, channel, time_s) {
      RegisterNewCall(list(
        call_type = "set_wait",
        handle = handle,
        channel = channel,
        time_s = time_s
      ))
      return(invisible(NULL))
    },
    .AnalogOutRepeatSetC = function(handle, channel, repeat_count) {
      RegisterNewCall(list(
        call_type = "set_repeat",
        handle = handle,
        channel = channel,
        repeat_count = repeat_count
      ))
      return(invisible(NULL))
    },
    .AnalogOutRunGetC = function(handle, channel) {
      RegisterNewCall(list(
        call_type = "get_run",
        handle = handle,
        channel = channel
      ))
      return(0.25)
    },
    .AnalogOutWaitGetC = function(handle, channel) {
      RegisterNewCall(list(
        call_type = "get_wait",
        handle = handle,
        channel = channel
      ))
      return(0.1)
    },
    .AnalogOutRepeatGetC = function(handle, channel) {
      RegisterNewCall(list(
        call_type = "get_repeat",
        handle = handle,
        channel = channel
      ))
      return(10L)
    },
    .AnalogOutStatusC = function(handle, channel) {
      RegisterNewCall(list(
        call_type = "get_status",
        handle = handle,
        channel = channel
      ))
      return("running")
    },
    .AnalogOutRunStatusC = function(handle, channel) {
      RegisterNewCall(list(
        call_type = "get_remaining_run",
        handle = handle,
        channel = channel
      ))
      return(0.125)
    },
    .AnalogOutRepeatStatusC = function(handle, channel) {
      RegisterNewCall(list(
        call_type = "get_remaining_repeat",
        handle = handle,
        channel = channel
      ))
      return(4L)
    },

    .package = "dwf4r"
  )


  #.. SetAnalogOutRun() ........................................................

  expect_null(
    SetAnalogOutRun(device, channel = 1L, run_time = 0.25)
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_run",
      handle = 123L,
      channel = 1L,
      time_s = 0.25
    )
  )

  # Zero requests continuous generation and is valid even when it is below the
  # minimum finite run time reported by the SDK.
  expect_null(
    SetAnalogOutRun(device, channel = 1L, run_time = 0)
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_run",
      handle = 123L,
      channel = 1L,
      time_s = 0
    )
  )

  expect_error(
    SetAnalogOutRun(device, channel = 1L, run_time = 1e10),
    "run_time" # 'run_time' (...) is out of supported range
  )


  #.. SetAnalogOutWait() .......................................................

  expect_null(
    SetAnalogOutWait(device, channel = 1L, wait_time = 0.1)
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_wait",
      handle = 123L,
      channel = 1L,
      time_s = 0.1
    )
  )

  expect_null(
    SetAnalogOutWait(device, channel = 1L, wait_time = 0)
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_wait",
      handle = 123L,
      channel = 1L,
      time_s = 0
    )
  )

  expect_error(
    SetAnalogOutWait(device, channel = 1L, wait_time = -0.1),
    "wait_time" # 'wait_time' (...) is out of supported range
  )


  #.. SetAnalogOutRepeat() .....................................................

  expect_null(
    SetAnalogOutRepeat(device, channel = 1L, repeat_count = 10L)
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_repeat",
      handle = 123L,
      channel = 1L,
      repeat_count = 10L
    )
  )

  # Zero requests infinite repetition and is valid even when it is below the
  # minimum finite repeat count reported by the SDK.
  expect_null(
    SetAnalogOutRepeat(device, channel = 1L, repeat_count = 0L)
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_repeat",
      handle = 123L,
      channel = 1L,
      repeat_count = 0L
    )
  )

  expect_error(
    SetAnalogOutRepeat(device, channel = 1L, repeat_count = 1.5),
    "repeat_count" # Assertion on 'repeat_count' failed
  )

  expect_error(
    SetAnalogOutRepeat(device, channel = 1L, repeat_count = 9999999L),
    "repeat_count" # 'repeat_count' (...) is out of supported range
  )


  #.. SetAnalogOutTiming() .....................................................

  mock_state$channel_count_queries <- 0L

  expect_null(
    SetAnalogOutTiming(
      device,
      channel = 1L,
      run_time = 0.25,
      wait_time = 0.1,
      repeat_count = 10L
    )
  )

  # The combined wrapper validates the device and channel once.
  expect_identical(mock_state$channel_count_queries, 1L)

  expect_identical(
    tail(mock_state$calls, 3L),
    list(
      list(
        call_type = "set_run",
        handle = 123L,
        channel = 1L,
        time_s = 0.25
      ),
      list(
        call_type = "set_wait",
        handle = 123L,
        channel = 1L,
        time_s = 0.1
      ),
      list(
        call_type = "set_repeat",
        handle = 123L,
        channel = 1L,
        repeat_count = 10L
      )
    )
  )

  expect_null(
    SetAnalogOutTiming(
      device,
      channel = 1L,
      wait_time = 0.2
    )
  )
  expect_identical(
    tail(mock_state$calls, 1L),
    list(
      list(
        call_type = "set_wait",
        handle = 123L,
        channel = 1L,
        time_s = 0.2
      )
    )
  )

  expect_error(
    SetAnalogOutTiming(device, channel = 1L),
    "At least one Analog Out timing parameter must be specified"
  )

  expect_error(
    SetAnalogOutRun(device, channel = 1L, run_time = 1e10),
    "run_time" # 'run_time' (...) is out of supported range
  )

  #.. GetAnalogOutRun() ........................................................

  expect_identical(
    GetAnalogOutRun(device, channel = 1L),
    0.25
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_run",
      handle = 123L,
      channel = 1L
    )
  )


  #.. GetAnalogOutWait() .......................................................

  expect_identical(
    GetAnalogOutWait(device, channel = 1L),
    0.1
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_wait",
      handle = 123L,
      channel = 1L
    )
  )


  #.. GetAnalogOutRepeat() .....................................................

  expect_identical(
    GetAnalogOutRepeat(device, channel = 1L),
    10L
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_repeat",
      handle = 123L,
      channel = 1L
    )
  )


  #.. GetAnalogOutTiming() .....................................................

  mock_state$channel_count_queries <- 0L

  expect_identical(
    GetAnalogOutTiming(device, channel = 1L),
    list(
      run_time = 0.25,
      wait_time = 0.1,
      repeat_count = 10L
    )
  )

  # The combined wrapper validates the device and channel once.
  expect_identical(mock_state$channel_count_queries, 1L)

  expect_identical(
    tail(mock_state$calls, 3L),
    list(
      list(
        call_type = "get_run",
        handle = 123L,
        channel = 1L
      ),
      list(
        call_type = "get_wait",
        handle = 123L,
        channel = 1L
      ),
      list(
        call_type = "get_repeat",
        handle = 123L,
        channel = 1L
      )
    )
  )


  #.. GetAnalogOutRemainingRun() ...............................................

  calls_before <- length(mock_state$calls)
  expect_identical(
    GetAnalogOutRemainingRun(device, channel = 1L),
    0.125
  )
  expect_identical(
    length(mock_state$calls) - calls_before,
    2L
  )
  expect_identical(
    tail(mock_state$calls, 2L),
    list(
      list(
        call_type = "get_status",
        handle = 123L,
        channel = 1L
      ),
      list(
        call_type = "get_remaining_run",
        handle = 123L,
        channel = 1L
      )
    )
  )

  calls_before <- length(mock_state$calls)
  expect_identical(
    GetAnalogOutRemainingRun(
      device,
      channel = 1L,
      update = FALSE
    ),
    0.125
  )
  expect_identical(
    length(mock_state$calls) - calls_before,
    1L
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_remaining_run",
      handle = 123L,
      channel = 1L
    )
  )

  expect_error(
    GetAnalogOutRemainingRun(
      device,
      channel = 1L,
      update = "invalid"
    ),
    "update" # Assertion on 'update' failed ...
  )


  #.. GetAnalogOutRemainingRepeat() ............................................

  calls_before <- length(mock_state$calls)
  expect_identical(
    GetAnalogOutRemainingRepeat(device, channel = 1L),
    4L
  )
  expect_identical(
    length(mock_state$calls) - calls_before,
    2L
  )
  expect_identical(
    tail(mock_state$calls, 2L),
    list(
      list(
        call_type = "get_status",
        handle = 123L,
        channel = 1L
      ),
      list(
        call_type = "get_remaining_repeat",
        handle = 123L,
        channel = 1L
      )
    )
  )

  calls_before <- length(mock_state$calls)
  expect_identical(
    GetAnalogOutRemainingRepeat(
      device,
      channel = 1L,
      update = FALSE
    ),
    4L
  )
  expect_identical(
    length(mock_state$calls) - calls_before,
    1L
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_remaining_repeat",
      handle = 123L,
      channel = 1L
    )
  )

  expect_error(
    GetAnalogOutRemainingRepeat(
      device,
      channel = 1L,
      update = "invalid"
    ),
    "update" # Assertion on 'update' failed ...
  )


  #.. .GetAnalogOutRemainingValue() ............................................

  expect_error(
    .GetAnalogOutRemainingValue(
      device,
      channel = 1L,
      param = "wait_time",
      update = FALSE
    ),
    "param" # Assertion on 'param' failed
  )
})



test_that("Analog Out: trigger configuration", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(
    list(device_handle = 123L)
  )

  mock_state <- new.env(parent = emptyenv())
  mock_state$last_call <- list()
  mock_state$repeat_trigger <- TRUE

  local_mocked_bindings(
    .QueryAnalogOutChannelCountC = function(handle) {
      return(2L)
    },
    .QueryDeviceTriggerSourceMaskC = function(handle) {
      # Bits 0 ("none"), 1 ("pc"), 7 ("analog_out_1"), and 11 ("external_1") are
      # set.
      return(2179L)
    },
    .QueryDeviceTriggerSlopeMaskC = function(handle) {
      # Bits 0 ("rising") and 2 ("either") are set.
      return(5L)
    },

    .AnalogOutTriggerSourceSetC = function(handle, channel, source_code) {
      mock_state$last_call <- list(
        call_type = "set_trigger_source",
        handle = handle,
        channel = channel,
        source_code = source_code
      )
      return(invisible(NULL))
    },
    .AnalogOutTriggerSourceGetC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_trigger_source",
        handle = handle,
        channel = channel
      )
      return(11L) # "external_1"
    },
    .AnalogOutTriggerSlopeSetC = function(handle, channel, slope_code) {
      mock_state$last_call <- list(
        call_type = "set_trigger_slope",
        handle = handle,
        channel = channel,
        slope_code = slope_code
      )
      return(invisible(NULL))
    },
    .AnalogOutTriggerSlopeGetC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_trigger_slope",
        handle = handle,
        channel = channel
      )
      return(2L) # "either"
    },
    .AnalogOutRepeatTriggerSetC = function(
    handle,
    channel,
    repeat_trigger
    ) {
      mock_state$last_call <- list(
        call_type = "set_repeat_trigger",
        handle = handle,
        channel = channel,
        repeat_trigger = repeat_trigger
      )
      return(invisible(NULL))
    },
    .AnalogOutRepeatTriggerGetC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_repeat_trigger",
        handle = handle,
        channel = channel
      )
      return(mock_state$repeat_trigger)
    },

    .package = "dwf4r"
  )


  #.. SetAnalogOutTriggerSource() ..............................................

  expect_null(
    SetAnalogOutTriggerSource(
      device,
      channel = 1L,
      source = "external_1"
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_trigger_source",
      handle = 123L,
      channel = 1L,
      source_code = 11L
    )
  )

  expect_error(
    SetAnalogOutTriggerSource(
      device,
      channel = 1L,
      source = "external_2"
    ),
    "external_2" # Trigger source 'external_2' is not supported by the device.
  )


  #.. GetAnalogOutTriggerSource() ..............................................

  expect_identical(
    GetAnalogOutTriggerSource(device, channel = 1L),
    "external_1"
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_trigger_source",
      handle = 123L,
      channel = 1L
    )
  )


  #.. SetAnalogOutTriggerSlope() ...............................................

  expect_null(
    SetAnalogOutTriggerSlope(
      device,
      channel = 1L,
      slope = "either"
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_trigger_slope",
      handle = 123L,
      channel = 1L,
      slope_code = 2L
    )
  )

  expect_error(
    SetAnalogOutTriggerSlope(
      device,
      channel = 1L,
      slope = "falling"
    ),
    "falling" # Trigger slope 'falling' is not supported by the device.
  )


  #.. GetAnalogOutTriggerSlope() ...............................................

  expect_identical(
    GetAnalogOutTriggerSlope(device, channel = 1L),
    "either"
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_trigger_slope",
      handle = 123L,
      channel = 1L
    )
  )


  #.. SetAnalogOutRepeatTrigger() ..............................................

  expect_null(
    SetAnalogOutRepeatTrigger(
      device,
      channel = 1L,
      repeat_trigger = TRUE
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_repeat_trigger",
      handle = 123L,
      channel = 1L,
      repeat_trigger = TRUE
    )
  )

  expect_null(
    SetAnalogOutRepeatTrigger(
      device,
      channel = 1L,
      repeat_trigger = FALSE
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_repeat_trigger",
      handle = 123L,
      channel = 1L,
      repeat_trigger = FALSE
    )
  )

  expect_error(
    SetAnalogOutRepeatTrigger(
      device,
      channel = 1L,
      repeat_trigger = 1L
    ),
    "repeat_trigger" # Assertion on 'repeat_trigger' failed
  )


  #.. GetAnalogOutRepeatTrigger() ..............................................

  mock_state$repeat_trigger <- TRUE
  expect_true(
    GetAnalogOutRepeatTrigger(device, channel = 1L)
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_repeat_trigger",
      handle = 123L,
      channel = 1L
    )
  )

  mock_state$repeat_trigger <- FALSE
  expect_false(
    GetAnalogOutRepeatTrigger(device, channel = 1L)
  )
})



test_that("Analog Out: native-error propagation", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(
    list(device_handle = 123L)
  )

  local_mocked_bindings(
    .AssertAnalogOut = function(...) {
      return(invisible(NULL))
    },
    .QueryAnalogOutRunRangeC = function(handle, channel) {
      return(c(0, 86400))
    },
    .QueryAnalogOutWaitRangeC = function(handle, channel) {
      return(c(0, 86400))
    },
    .QueryAnalogOutRepeatRangeC = function(handle, channel) {
      return(c(0L, 32000L))
    },
    .QueryAnalogOutChannelCountC = function(handle) {
      return(2L)
    },
    .QueryAnalogOutChannelNodesC = function(handle, channel) {
      return(c("carrier"))
    },
    .QueryAnalogOutNodeFrequencyRangeC = function(handle, channel, node) {
      return(c(1, 1e6))
    },
    .AnalogOutConfigureC = function(handle, channel, action) {
      stop("DWF configuration error.", call. = FALSE)
    },
    .AnalogOutNodeFrequencySetC = function(handle, channel, node, value) {
      stop("DWF frequency error.", call. = FALSE)
    },
    .AnalogOutNodeFrequencyGetC = function(handle, channel, node) {
      stop("DWF frequency query error.", call. = FALSE)
    },
    .AnalogOutRunSetC = function(handle, channel, time_s) {
      stop("DWF run error.", call. = FALSE)
    },
    .AnalogOutWaitSetC = function(handle, channel, time_s) {
      stop("DWF wait error.", call. = FALSE)
    },
    .AnalogOutRepeatSetC = function(handle, channel, repeat_count) {
      stop("DWF repeat error.", call. = FALSE)
    },
    .AnalogOutRunGetC = function(handle, channel) {
      stop("DWF run query error.", call. = FALSE)
    },
    .AnalogOutWaitGetC = function(handle, channel) {
      stop("DWF wait query error.", call. = FALSE)
    },
    .AnalogOutRepeatGetC = function(handle, channel) {
      stop("DWF repeat query error.", call. = FALSE)
    },
    .AnalogOutStatusC = function(handle, channel) {
      stop("DWF status update error.", call. = FALSE)
    },
    .AnalogOutRunStatusC = function(handle, channel) {
      stop("DWF remaining run error.", call. = FALSE)
    },
    .AnalogOutRepeatStatusC = function(handle, channel) {
      stop("DWF remaining repeat error.", call. = FALSE)
    },

    .QueryDeviceTriggerSourceMaskC = function(handle) {
      return(2179L)
    },
    .QueryDeviceTriggerSlopeMaskC = function(handle) {
      return(5L)
    },
    .AnalogOutTriggerSourceSetC = function(handle, channel, source_code) {
      stop("DWF trigger source error.", call. = FALSE)
    },
    .AnalogOutTriggerSourceGetC = function(handle, channel) {
      stop("DWF trigger source query error.", call. = FALSE)
    },
    .AnalogOutTriggerSlopeSetC = function(handle, channel, slope_code) {
      stop("DWF trigger slope error.", call. = FALSE)
    },
    .AnalogOutTriggerSlopeGetC = function(handle, channel) {
      stop("DWF trigger slope query error.", call. = FALSE)
    },
    .AnalogOutRepeatTriggerSetC = function(handle, channel, repeat_trigger) {
      stop("DWF repeat trigger error.", call. = FALSE)
    },
    .AnalogOutRepeatTriggerGetC = function(handle, channel) {
      stop("DWF repeat trigger query error.", call. = FALSE)
    },
    .AnalogOutMasterSetC = function(handle, channel, master_channel) {
      stop("DWF master set error.", call. = FALSE)
    },
    .AnalogOutMasterGetC = function(handle, channel) {
      stop("DWF master query error.", call. = FALSE)
    },

    .package = "dwf4r"
  )

  expect_error(
    StartAnalogOut(device, channel = 0L),
    "DWF configuration error"
  )

  expect_error(
    SetCarrierFrequency(device, channel = 0L, frequency = 1000),
    "DWF frequency error"
  )

  expect_error(
    GetCarrierFrequency(device, channel = 0L),
    "DWF frequency query error"
  )

  expect_error(
    SetAnalogOutRun(device, channel = 1L, run_time = 1),
    "DWF run error"
  )

  expect_error(
    SetAnalogOutWait(device, channel = 1L, wait_time = 1),
    "DWF wait error"
  )

  expect_error(
    SetAnalogOutRepeat(device, channel = 1L, repeat_count = 1),
    "DWF repeat error"
  )

  expect_error(
    GetAnalogOutRun(device, channel = 1L),
    "DWF run query error"
  )

  expect_error(
    GetAnalogOutWait(device, channel = 1L),
    "DWF wait query error"
  )

  expect_error(
    GetAnalogOutRepeat(device, channel = 1L),
    "DWF repeat query error"
  )

  # With the default 'update = TRUE', failure of the status update propagates
  # before the remaining-value function is called.
  expect_error(
    GetAnalogOutRemainingRun(device, channel = 1L),
    "DWF status update error"
  )

  # With 'update = FALSE', the status update is skipped and errors from the
  # corresponding SDK status getter propagate directly.
  expect_error(
    GetAnalogOutRemainingRun(
      device,
      channel = 1L,
      update = FALSE
    ),
    "DWF remaining run error"
  )

  expect_error(
    GetAnalogOutRemainingRepeat(
      device,
      channel = 1L,
      update = FALSE
    ),
    "DWF remaining repeat error"
  )

  expect_error(
    SetAnalogOutTriggerSource(
      device,
      channel = 1L,
      source = "external_1"
    ),
    "DWF trigger source error"
  )

  expect_error(
    GetAnalogOutTriggerSource(device, channel = 1L),
    "DWF trigger source query error"
  )

  expect_error(
    SetAnalogOutTriggerSlope(
      device,
      channel = 1L,
      slope = "either"
    ),
    "DWF trigger slope error"
  )

  expect_error(
    GetAnalogOutTriggerSlope(device, channel = 1L),
    "DWF trigger slope query error"
  )

  expect_error(
    SetAnalogOutRepeatTrigger(
      device,
      channel = 1L,
      repeat_trigger = TRUE
    ),
    "DWF repeat trigger error"
  )

  expect_error(
    GetAnalogOutRepeatTrigger(device, channel = 1L),
    "DWF repeat trigger query error"
  )

  expect_error(
    SetAnalogOutMaster(
      device,
      channel = 1L,
      master_channel = 0L
    ),
    "DWF master set error"
  )

  expect_error(
    GetAnalogOutMaster(device, channel = 1L),
    "DWF master query error"
  )
})



#==[ Hardware tests ]===========================================================

test_that("Hardware: Analog Out control and carrier configuration", {
  testthat::skip_if(
    !identical(Sys.getenv("DWF4R_TEST_ANALOG_OUT"), "true"),
    paste(
      "Set 'DWF4R_TEST_ANALOG_OUT=true' to run tests that may activate",
      "Analog Out channels."
    )
  )

  device <- .OpenHardwareTestDevice()
  channel <- 0L
  on.exit({
    try(StopAnalogOut(device, channel), silent = TRUE)
    try(DisableCarrier(device, channel), silent = TRUE)
    try(ApplyAnalogOutSettings(device, channel), silent = TRUE)
    try(ResetAnalogOut(device, channel), silent = TRUE)
    try(CloseDevice(device), silent = TRUE)
  }, add = TRUE)

  func <- "sine"
  frequency <- 30
  amplitude <- 0.5
  offset <- 0
  symmetry <- 50
  phase <- 0

  skip_if(
    GetAnalogOutChannelCount(device) == 0L,
    "Configured device has no Analog Out channels."
  )
  skip_if(
    !("carrier" %in% GetAnalogOutChannelNodes(device, channel)),
    "Carrier node is not supported."
  )
  skip_if(
    !(func %in% GetAnalogOutNodeFunctionTypes(device, channel, "carrier")),
    sprintf("Function '%s' is not supported.", func)
  )
  frequency_range <- GetAnalogOutNodeFrequencyRange(device, channel, "carrier")
  skip_if(
    frequency < frequency_range[[1L]] || frequency > frequency_range[[2L]],
    sprintf("Frequency %f Hz is out of range.", frequency)
  )
  amplitude_range <- GetAnalogOutNodeAmplitudeRange(device, channel, "carrier")
  skip_if(
    amplitude < amplitude_range[[1L]] || amplitude > amplitude_range[[2L]],
    sprintf("Amplitude %f V is out of range.", amplitude)
  )
  offset_range <- GetAnalogOutNodeOffsetRange(device, channel, "carrier")
  skip_if(
    offset < offset_range[[1L]] || offset > offset_range[[2L]],
    sprintf("Offset %f V is out of range.", offset)
  )
  symmetry_range <- GetAnalogOutNodeSymmetryRange(device, channel, "carrier")
  skip_if(
    symmetry < symmetry_range[[1L]] || symmetry > symmetry_range[[2L]],
    sprintf("Symmetry %f%% is out of range.", symmetry)
  )
  phase_range <- GetAnalogOutNodePhaseRange(device, channel, "carrier")
  skip_if(
    phase < phase_range[[1L]] || phase > phase_range[[2L]],
    sprintf("Phase %f degrees is out of range.", phase)
  )

  #.. Reset device .............................................................

  expect_null(ResetAnalogOut(device, channel))


  #.. Enable/disable carrier ...................................................

  expect_null(DisableCarrier(device, channel))
  ApplyAnalogOutSettings(device, channel)
  expect_false(IsCarrierEnabled(device, channel))

  EnableCarrier(device, channel)
  ApplyAnalogOutSettings(device, channel)
  expect_true(IsCarrierEnabled(device, channel))


  #.. Set/get carrier settings .................................................

  expect_null(
    SetCarrier(
      device,
      channel,
      func = func,
      frequency = frequency,
      amplitude = amplitude,
      offset = offset,
      symmetry = symmetry,
      phase = phase
    )
  )
  expect_null(ApplyAnalogOutSettings(device, channel))

  expect_identical(
    GetCarrierFunction(device, channel),
    func
  )

  actual_frequency <- GetCarrierFrequency(device, channel)
  actual_amplitude <- GetCarrierAmplitude(device, channel)
  actual_offset <- GetCarrierOffset(device, channel)
  actual_symmetry <- GetCarrierSymmetry(device, channel)
  actual_phase <- GetCarrierPhase(device, channel)
  expect_true(all(is.finite(c(
    actual_frequency,
    actual_amplitude,
    actual_offset,
    actual_symmetry,
    actual_phase
  ))))
  uses_strict_tolerance <-
    device$info$device_type %in% c("Analog Discovery 2", "Analog Discovery 3")
  if (uses_strict_tolerance) {
    # The absolute tolerances of 1e-3 were verified experimentally with Analog
    # Discovery 2 and are provisionally applied to Analog Discovery 3.
    expect_equal(actual_frequency, frequency, tolerance = 1e-3)
    expect_equal(actual_amplitude, amplitude, tolerance = 1e-3)
    expect_equal(actual_offset, offset, tolerance = 1e-3)
    expect_equal(actual_symmetry, symmetry, tolerance = 1e-3)
    expect_equal(actual_phase, phase, tolerance = 1e-3)
  }

  settings <- GetCarrierSettings(device, channel)
  expect_identical(settings$enabled, IsCarrierEnabled(device, channel))
  expect_identical(settings$func, GetCarrierFunction(device, channel))
  expect_equal(settings$frequency, actual_frequency)
  expect_equal(settings$amplitude, actual_amplitude)
  expect_equal(settings$offset, actual_offset)
  expect_equal(settings$symmetry, actual_symmetry)
  expect_equal(settings$phase, actual_phase)


  #.. Start/stop analog out ....................................................

  expect_null(StartAnalogOut(device, channel))

  status <- GetAnalogOutStatus(device, channel)
  expect_true(
    status %in% c(
      "ready",
      "armed",
      "wait",
      "running",
      "done",
      "config",
      "prefill",
      "not_done"
    )
  )

  expect_null(StopAnalogOut(device, channel))


  #.. Set/get idle mode ........................................................

  idle_modes <- GetAnalogOutIdleModes(device, channel)
  expect_gt(length(idle_modes), 0L)

  idle <- idle_modes[[1L]]
  SetAnalogOutIdle(device, channel, idle)
  ApplyAnalogOutSettings(device, channel)

  expect_identical(
    GetAnalogOutIdle(device, channel),
    idle
  )


  #.. Timing configuration .....................................................

  run_range <- GetAnalogOutRunRange(device, channel)
  wait_range <- GetAnalogOutWaitRange(device, channel)
  repeat_range <- GetAnalogOutRepeatRange(device, channel)
  expect_null(
    SetAnalogOutTiming(
      device,
      channel,
      run_time = run_range[[1L]],
      wait_time = wait_range[[1L]],
      repeat_count = repeat_range[[1L]]
    )
  )

  timing <- GetAnalogOutTiming(device, channel)
  expect_type(timing$run_time, "double")
  expect_type(timing$wait_time, "double")
  expect_type(timing$repeat_count, "integer")

  remaining_run <- GetAnalogOutRemainingRun(device, channel)
  expect_type(remaining_run, "double")

  remaining_repeat <- GetAnalogOutRemainingRepeat(device, channel)
  expect_type(remaining_repeat, "integer")
})



test_that("Hardware: Analog Out master setting", {
  skip_if(
    !identical(Sys.getenv("DWF4R_TEST_ANALOG_OUT"), "true"),
    paste(
      "Set 'DWF4R_TEST_ANALOG_OUT=true' to run tests that may activate",
      "Analog Out channels."
    )
  )

  device <- .OpenHardwareTestDevice()
  slave_channel <- 1L
  master_channel <- 0L
  on.exit({
    try(ResetAnalogOut(device, slave_channel), silent = TRUE)
    try(CloseDevice(device), silent = TRUE)
  }, add = TRUE)

  skip_if(
    GetAnalogOutChannelCount(device) < 2L,
    "Configured device has fewer than two Analog Out channels."
  )

  expect_null(
    SetAnalogOutMaster(
      device,
      channel = slave_channel,
      master_channel = master_channel
    )
  )
  expect_null(ApplyAnalogOutSettings(device, slave_channel))
  expect_identical(
    GetAnalogOutMaster(device, slave_channel),
    master_channel
  )

  expect_null(
    SetAnalogOutMaster(
      device,
      channel = slave_channel,
      master_channel = slave_channel
    )
  )
  expect_null(ApplyAnalogOutSettings(device, slave_channel))
  expect_identical(
    GetAnalogOutMaster(device, slave_channel),
    slave_channel
  )
})



test_that("Hardware: Analog Out trigger configuration", {
  skip_if(
    !identical(Sys.getenv("DWF4R_TEST_ANALOG_OUT"), "true"),
    paste(
      "Set 'DWF4R_TEST_ANALOG_OUT=true' to run tests that may activate",
      "Analog Out channels."
    )
  )

  skip_if(
    !identical(Sys.getenv("DWF4R_TEST_TRIGGER"), "true"),
    "Set 'DWF4R_TEST_TRIGGER=true' to run trigger hardware tests."
  )

  device <- .OpenHardwareTestDevice()
  channel <- 0L
  on.exit({
    try(ResetAnalogOut(device, channel), silent = TRUE)
    try(CloseDevice(device), silent = TRUE)
  }, add = TRUE)

  skip_if(
    GetAnalogOutChannelCount(device) == 0L,
    "Configured device has no Analog Out channels."
  )

  sources <- GetDeviceTriggerSources(device)
  slopes <- GetDeviceTriggerSlopes(device)

  skip_if(
    !("pc" %in% sources),
    "PC trigger source is not supported."
  )
  skip_if(
    !("rising" %in% slopes),
    "Rising trigger slope is not supported."
  )

  expect_null(
    SetAnalogOutTriggerSource(device, channel, "pc")
  )
  expect_null(
    SetAnalogOutTriggerSlope(device, channel, "rising")
  )
  expect_null(
    SetAnalogOutRepeatTrigger(device, channel, TRUE)
  )
  expect_null(
    ApplyAnalogOutSettings(device, channel)
  )

  expect_identical(
    GetAnalogOutTriggerSource(device, channel),
    "pc"
  )
  expect_identical(
    GetAnalogOutTriggerSlope(device, channel),
    "rising"
  )
  expect_true(
    GetAnalogOutRepeatTrigger(device, channel)
  )
})


