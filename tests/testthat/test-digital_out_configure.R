#==[ Mock-based tests ]=========================================================

test_that("Digital Out: control and channel configuration", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(
    list(device_handle = 123L)
  )

  mock_state <- new.env(parent = emptyenv())
  mock_state$last_call <- list()

  local_mocked_bindings(
    .QueryDigitalOutChannelCountC = function(handle) {
      return(16L)
    },
    .QueryDigitalOutOutputMaskC = function(handle, channel) {
      # push_pull, open_drain, three_state
      return(11L)
    },
    .QueryDigitalOutTypeMaskC = function(handle, channel) {
      # pulse, custom, random, play
      return(39L)
    },
    .QueryDigitalOutIdleMaskC = function(handle, channel) {
      # initial, high, three_state
      return(13L)
    },

    .DigitalOutResetC = function(handle) {
      mock_state$last_call <- list(
        call_type = "reset",
        handle = handle
      )
      return(invisible(NULL))
    },
    .DigitalOutConfigureC = function(handle, start) {
      mock_state$last_call <- list(
        call_type = "configure",
        handle = handle,
        start = start
      )
      return(invisible(NULL))
    },
    .DigitalOutStatusC = function(handle) {
      mock_state$last_call <- list(
        call_type = "status",
        handle = handle
      )
      return("running")
    },
    .DigitalOutEnableSetC = function(handle, channel, enable) {
      mock_state$last_call <- list(
        call_type = "set_enable",
        handle = handle,
        channel = channel,
        enable = enable
      )
      return(invisible(NULL))
    },
    .DigitalOutEnableGetC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_enable",
        handle = handle,
        channel = channel
      )
      return(TRUE)
    },
    .DigitalOutOutputSetC = function(handle, channel, output_code) {
      mock_state$last_call <- list(
        call_type = "set_output",
        handle = handle,
        channel = channel,
        output_code = output_code
      )
      return(invisible(NULL))
    },
    .DigitalOutOutputGetC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_output",
        handle = handle,
        channel = channel
      )
      return(1L) # open_drain
    },
    .DigitalOutTypeSetC = function(handle, channel, type_code) {
      mock_state$last_call <- list(
        call_type = "set_function",
        handle = handle,
        channel = channel,
        type_code = type_code
      )
      return(invisible(NULL))
    },
    .DigitalOutTypeGetC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_function",
        handle = handle,
        channel = channel
      )
      return(1L) # custom
    },
    .DigitalOutIdleSetC = function(handle, channel, idle_code) {
      mock_state$last_call <- list(
        call_type = "set_idle",
        handle = handle,
        channel = channel,
        idle_code = idle_code
      )
      return(invisible(NULL))
    },
    .DigitalOutIdleGetC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_idle",
        handle = handle,
        channel = channel
      )
      return(2L) # high
    },
    .package = "dwf4r"
  )


  #.. ResetDigitalOut() ........................................................

  expect_null(ResetDigitalOut(device))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "reset",
      handle = 123L
    )
  )


  #.. .ConfigureDigitalOut(), StartDigitalOut(), StopDigitalOut() ..............

  expect_null(StartDigitalOut(device))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "configure",
      handle = 123L,
      start = TRUE
    )
  )

  expect_null(StopDigitalOut(device))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "configure",
      handle = 123L,
      start = FALSE
    )
  )


  #.. GetDigitalOutStatus() ....................................................

  expect_identical(GetDigitalOutStatus(device), "running")
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "status",
      handle = 123L
    )
  )


  #.. EnableDigitalOutChannel(), DisableDigitalOutChannel() ....................

  expect_null(EnableDigitalOutChannel(device, channel = 1L))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_enable",
      handle = 123L,
      channel = 1L,
      enable = TRUE
    )
  )

  expect_null(DisableDigitalOutChannel(device, channel = 1L))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_enable",
      handle = 123L,
      channel = 1L,
      enable = FALSE
    )
  )


  #.. IsDigitalOutChannelEnabled() .............................................

  expect_true(IsDigitalOutChannelEnabled(device, channel = 1L))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_enable",
      handle = 123L,
      channel = 1L
    )
  )


  #.. SetDigitalOutOutput(), GetDigitalOutOutput() .............................

  expect_null(
    SetDigitalOutOutput(
      device,
      channel = 1L,
      output = "push_pull"
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_output",
      handle = 123L,
      channel = 1L,
      output_code = 0L
    )
  )

  expect_identical(
    GetDigitalOutOutput(device, channel = 1L),
    "open_drain"
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_output",
      handle = 123L,
      channel = 1L
    )
  )


  #.. SetDigitalOutFunction(), GetDigitalOutFunction() .........................

  expect_null(
    SetDigitalOutFunction(
      device,
      channel = 1L,
      func = "random"
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_function",
      handle = 123L,
      channel = 1L,
      type_code = 2L
    )
  )

  expect_identical(
    GetDigitalOutFunction(device, channel = 1L),
    "custom"
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_function",
      handle = 123L,
      channel = 1L
    )
  )


  #.. SetDigitalOutIdle(), GetDigitalOutIdle() .................................

  expect_null(
    SetDigitalOutIdle(
      device,
      channel = 1L,
      idle = "high"
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_idle",
      handle = 123L,
      channel = 1L,
      idle_code = 2L
    )
  )

  expect_identical(
    GetDigitalOutIdle(device, channel = 1L),
    "high"
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



test_that("Digital Out: control and channel configuration, SDK failure", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(
    list(device_handle = 123L)
  )

  local_mocked_bindings(
    .QueryDigitalOutChannelCountC = function(handle) {
      return(16L)
    },
    .QueryDigitalOutTypeMaskC = function(handle, channel) {
      # pulse, custom, random, play
      return(39L)
    },
    .DigitalOutConfigureC = function(handle, start) {
      stop("DWF configuration error.", call. = FALSE)
    },
    .DigitalOutTypeSetC = function(handle, channel, type_code) {
      stop("DWF type error.", call. = FALSE)
    },
    .package = "dwf4r"
  )

  expect_error(
    StartDigitalOut(device),
    "DWF configuration error"
  )

  expect_error(
    SetDigitalOutFunction(
      device,
      channel = 1L,
      func = "random"
    ),
    "DWF type error"
  )
})


test_that("Digital Out: native unsigned-integer validation", {

  # Dummy device-handle and channel values are used below. Invalid values are
  # rejected locally by 'AsUnsignedInt()' in the C++ wrappers before the
  # corresponding WaveForms SDK functions are called.

  expect_error(
    .DigitalOutDividerSetC(
      handle = 1L,
      channel = 0L,
      divider = 2^32
    ),
    "must be a whole number"
  )

  expect_error(
    .DigitalOutCounterSetC(
      handle = 1L,
      channel = 0L,
      low_count = 1.5,
      high_count = 1
    ),
    "must be a whole number"
  )
})


test_that("Digital Out: divider and counter configuration", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(
    list(device_handle = 123L)
  )

  mock_state <- new.env(parent = emptyenv())
  mock_state$last_call <- list()

  local_mocked_bindings(
    .QueryDigitalOutChannelCountC = function(handle) {
      return(16L)
    },
    .QueryDigitalOutDividerRangeC = function(handle, channel) {
      return(c(1, 2147483649))
    },
    .QueryDigitalOutCounterRangeC = function(handle, channel) {
      # The lower bound is intentionally set to 1 (not 0).
      return(c(1, 32768))
    },

    .DigitalOutDividerSetC = function(handle, channel, divider) {
      mock_state$last_call <- list(
        call_type = "set_divider",
        handle = handle,
        channel = channel,
        divider = divider
      )
      return(invisible(NULL))
    },
    .DigitalOutDividerInitSetC = function(
    handle,
    channel,
    initial_divider
    ) {
      mock_state$last_call <- list(
        call_type = "set_initial_divider",
        handle = handle,
        channel = channel,
        initial_divider = initial_divider
      )
      return(invisible(NULL))
    },
    .DigitalOutDividerGetC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_divider",
        handle = handle,
        channel = channel
      )
      return(2147483649)
    },
    .DigitalOutDividerInitGetC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_initial_divider",
        handle = handle,
        channel = channel
      )
      return(25)
    },
    .DigitalOutCounterSetC = function(
    handle,
    channel,
    low_count,
    high_count
    ) {
      mock_state$last_call <- list(
        call_type = "set_counter",
        handle = handle,
        channel = channel,
        low_count = low_count,
        high_count = high_count
      )
      return(invisible(NULL))
    },
    .DigitalOutCounterInitSetC = function(
    handle,
    channel,
    initial_high,
    initial_count
    ) {
      mock_state$last_call <- list(
        call_type = "set_initial_counter",
        handle = handle,
        channel = channel,
        initial_high = initial_high,
        initial_count = initial_count
      )
      return(invisible(NULL))
    },
    .DigitalOutCounterGetC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_counter",
        handle = handle,
        channel = channel
      )
      return(c(3, 7))
    },
    .DigitalOutCounterInitGetC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_initial_counter",
        handle = handle,
        channel = channel
      )
      return(list(
        initial_high = FALSE,
        initial_count = 5
      ))
    },

    .package = "dwf4r"
  )


  #.. SetDigitalOutDivider() ...................................................

  expect_null(
    SetDigitalOutDivider(
      device,
      channel = 1L,
      divider = 100L
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_divider",
      handle = 123L,
      channel = 1L,
      divider = 100L
    )
  )

  expect_error(
    SetDigitalOutDivider(
      device,
      channel = 1L,
      divider = 1.5
    ),
    "must be a whole number"
  )

  expect_error(
    SetDigitalOutDivider(
      device,
      channel = 1L,
      divider = 2147483649 + 1
    ),
    "out of supported range"
  )


  #.. SetDigitalOutInitialDivider() ............................................

  expect_null(
    SetDigitalOutInitialDivider(
      device,
      channel = 1L,
      initial_divider = 25L
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_initial_divider",
      handle = 123L,
      channel = 1L,
      initial_divider = 25L
    )
  )


  #.. GetDigitalOutDivider() ...................................................

  expect_identical(
    GetDigitalOutDivider(device, channel = 1L),
    2147483649
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_divider",
      handle = 123L,
      channel = 1L
    )
  )


  #.. GetDigitalOutInitialDivider() ............................................

  expect_identical(
    GetDigitalOutInitialDivider(device, channel = 1L),
    25L
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_initial_divider",
      handle = 123L,
      channel = 1L
    )
  )


  #.. SetDigitalOutCounter() ...................................................

  expect_null(
    SetDigitalOutCounter(
      device,
      channel = 1L,
      low_count = 3L,
      high_count = 7L
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_counter",
      handle = 123L,
      channel = 1L,
      low_count = 3L,
      high_count = 7L
    )
  )

  # Zero is valid even when it is below the reported minimum.
  expect_null(
    SetDigitalOutCounter(
      device,
      channel = 1L,
      low_count = 0L,
      high_count = 7L
    )
  )

  expect_error(
    SetDigitalOutCounter(
      device,
      channel = 1L,
      low_count = 1.5,
      high_count = 7L
    ),
    "must be a whole number"
  )

  expect_error(
    SetDigitalOutCounter(
      device,
      channel = 1L,
      low_count = 3L,
      high_count = 99999L
    ),
    "out of supported range"
  )


  #.. SetDigitalOutInitialCounter() ............................................

  expect_null(
    SetDigitalOutInitialCounter(
      device,
      channel = 1L,
      initial_state = "high",
      initial_count = 5L
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_initial_counter",
      handle = 123L,
      channel = 1L,
      initial_high = TRUE,
      initial_count = 5L
    )
  )

  expect_null(
    SetDigitalOutInitialCounter(
      device,
      channel = 1L,
      initial_state = "low",
      initial_count = 0L
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_initial_counter",
      handle = 123L,
      channel = 1L,
      initial_high = FALSE,
      initial_count = 0L
    )
  )

  expect_error(
    SetDigitalOutInitialCounter(
      device,
      channel = 1L,
      initial_state = "invalid",
      initial_count = 5L
    ),
    "initial_state" # Assertion on 'initial_state' failed
  )

  expect_error(
    SetDigitalOutInitialCounter(
      device,
      channel = 1L,
      initial_state = "low",
      initial_count = 1.5
    ),
    "must be a whole number"
  )

  expect_error(
    SetDigitalOutInitialCounter(
      device,
      channel = 1L,
      initial_state = "low",
      initial_count = 99999L
    ),
    "out of supported range"
  )


  #.. GetDigitalOutCounter() ...................................................

  expect_identical(
    GetDigitalOutCounter(device, channel = 1L),
    c(low_count = 3L, high_count = 7L)
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_counter",
      handle = 123L,
      channel = 1L
    )
  )


  #.. GetDigitalOutInitialCounter() ............................................

  expect_identical(
    GetDigitalOutInitialCounter(device, channel = 1L),
    list(
      initial_state = "low",
      initial_count = 5L
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_initial_counter",
      handle = 123L,
      channel = 1L
    )
  )
})


test_that("Digital Out: custom-pattern configuration", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(
    list(device_handle = 123L)
  )

  mock_state <- new.env(parent = emptyenv())
  mock_state$last_call <- list()

  local_mocked_bindings(
    .QueryDigitalOutChannelCountC = function(handle) {
      return(16L)
    },
    .QueryDigitalOutOutputMaskC = function(handle, channel) {
      # push_pull, open_drain, three_state
      return(11L)
    },
    .QueryDigitalOutTypeMaskC = function(handle, channel) {
      # pulse, custom, random, play
      return(39L)
    },
    .QueryDigitalOutMaxDataBitsC = function(handle, channel) {
      return(8)
    },
    .QueryDigitalOutInternalClockFrequencyC = function(handle) {
      return(1e8)
    },
    .QueryDigitalOutDividerRangeC = function(handle, channel) {
      # The corresponding sampling rate is from 1e+02 to 1e+08
      return(c(1, 1e6))
    },

    .DigitalOutDataSetC = function(handle, channel, data, n_bits) {
      mock_state$last_call <- list(
        call_type = "set_custom_pattern",
        handle = handle,
        channel = channel,
        data = data,
        n_bits = n_bits
      )
      return(invisible(NULL))
    },
    .DigitalOutDividerSetC = function(handle, channel, divider) {
      mock_state$last_call <- list(
        call_type = "set_divider",
        handle = handle,
        channel = channel,
        divider = divider
      )
      return(invisible(NULL))
    },
    .DigitalOutDividerGetC = function(handle, channel) {
      mock_state$last_call <- list(
        call_type = "get_divider",
        handle = handle,
        channel = channel
      )
      return(100000)
    },

    .package = "dwf4r"
  )


  #.. SetDigitalOutCustomPattern() .............................................

  # Numeric patterns accept both integer and double whole-number values.
  # 0, 1, 1, 0 are packed LSB-first as 00000110 (0x06).
  expect_null(
    SetDigitalOutCustomPattern(
      device,
      channel = 1L,
      pattern = c(0, 1, 1, 0)
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_custom_pattern",
      handle = 123L,
      channel = 1L,
      data = as.raw(0x06),
      n_bits = 4L
    )
  )

  # Character patterns use the same one-bit encoding in ordinary output mode.
  expect_null(
    SetDigitalOutCustomPattern(
      device,
      channel = 1L,
      pattern = c("l", "h", "h", "l")
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_custom_pattern",
      handle = 123L,
      channel = 1L,
      data = as.raw(0x06),
      n_bits = 4L
    )
  )

  # Three-state samples are encoded as IO/OE pairs:
  # l -> 0,1; h -> 1,1; z -> 0,0; h -> 1,1.
  expect_null(
    SetDigitalOutCustomPattern(
      device,
      channel = 1L,
      pattern = c("l", "h", "z", "h"),
      is_three_state = TRUE
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_custom_pattern",
      handle = 123L,
      channel = 1L,
      data = as.raw(0xCE),
      n_bits = 8L
    )
  )

  expect_error(
    SetDigitalOutCustomPattern(
      device,
      channel = 1L,
      pattern = c("l", "z")
    ),
    "'z' pattern values require 'is_three_state = TRUE'"
  )

  expect_error(
    SetDigitalOutCustomPattern(
      device,
      channel = 1L,
      pattern = c(0, 0.5, 1)
    ),
    "pattern" # Assertion on 'pattern' failed
  )

  expect_error(
    SetDigitalOutCustomPattern(
      device,
      channel = 1L,
      pattern = c("l", "invalid", "h")
    ),
    "must contain only \"l\", \"h\", and \"z\""
  )

  expect_error(
    SetDigitalOutCustomPattern(
      device,
      channel = 1L,
      pattern = c("l", "h", "l", "h", "l"),
      is_three_state = TRUE
    ),
    "requires 10 bits"
  )


  #.. SetDigitalOutCustomSampleRate() ..........................................

  expect_null(
    SetDigitalOutCustomSampleRate(
      device,
      channel = 1L,
      sample_rate = 1e3
    )
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_divider",
      handle = 123L,
      channel = 1L,
      divider = 100000
    )
  )

  expect_error(
    SetDigitalOutCustomSampleRate(
      device,
      channel = 1L,
      sample_rate = 50
    ),
    "out of supported range"
  )


  #.. GetDigitalOutCustomSampleRate() ..........................................

  expect_identical(
    GetDigitalOutCustomSampleRate(
      device,
      channel = 1L
    ),
    1e3
  )
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "get_divider",
      handle = 123L,
      channel = 1L
    )
  )
})


test_that("Digital Out: timing configuration and status", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(
    list(device_handle = 123L)
  )

  mock_state <- new.env(parent = emptyenv())
  mock_state$calls <- list()

  RegisterNewCall <- function(call) {
    call_index <- length(mock_state$calls) + 1L
    mock_state$calls[[call_index]] <- call
    return(invisible(NULL))
  }

  local_mocked_bindings(
    .QueryDigitalOutRunRangeC = function(handle) {
      return(c(2e-7, 86400))
    },
    .QueryDigitalOutWaitRangeC = function(handle) {
      return(c(2e-7, 86400))
    },
    .QueryDigitalOutRepeatRangeC = function(handle) {
      return(c(0, 32768))
    },

    .DigitalOutRunSetC = function(handle, time_s) {
      RegisterNewCall(list(
        call_type = "set_run",
        handle = handle,
        time_s = time_s
      ))
      return(invisible(NULL))
    },
    .DigitalOutWaitSetC = function(handle, time_s) {
      RegisterNewCall(list(
        call_type = "set_wait",
        handle = handle,
        time_s = time_s
      ))
      return(invisible(NULL))
    },
    .DigitalOutRepeatSetC = function(handle, repeat_count) {
      RegisterNewCall(list(
        call_type = "set_repeat",
        handle = handle,
        repeat_count = repeat_count
      ))
      return(invisible(NULL))
    },

    .DigitalOutRunGetC = function(handle) {
      return(0.5)
    },
    .DigitalOutWaitGetC = function(handle) {
      return(0.1)
    },
    .DigitalOutRepeatGetC = function(handle) {
      return(3)
    },

    .DigitalOutStatusC = function(handle) {
      RegisterNewCall(list(
        call_type = "status",
        handle = handle
      ))
      return("running")
    },
    .DigitalOutRunStatusC = function(handle) {
      RegisterNewCall(list(
        call_type = "remaining_run",
        handle = handle
      ))
      return(123456789)
    },
    .DigitalOutRepeatStatusC = function(handle) {
      RegisterNewCall(list(
        call_type = "remaining_repeat",
        handle = handle
      ))
      return(5)
    },

    .package = "dwf4r"
  )


  #.. Individual setters ......................................................

  expect_null(SetDigitalOutRun(device, run_time = 0.5))
  expect_null(SetDigitalOutWait(device, wait_time = 0.1))
  expect_null(SetDigitalOutRepeat(device, repeat_count = 32000))

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "set_run",
        handle = 123L,
        time_s = 0.5
      ),
      list(
        call_type = "set_wait",
        handle = 123L,
        time_s = 0.1
      ),
      list(
        call_type = "set_repeat",
        handle = 123L,
        repeat_count = 32000
      )
    )
  )


  #.. Combined setter and special zero values ..................................

  # All reported ranges have positive lower bounds, but zero remains valid.
  mock_state$calls <- list()

  expect_null(
    SetDigitalOutTiming(
      device,
      run_time = 0,
      wait_time = 0,
      repeat_count = 0L
    )
  )

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "set_run",
        handle = 123L,
        time_s = 0
      ),
      list(
        call_type = "set_wait",
        handle = 123L,
        time_s = 0
      ),
      list(
        call_type = "set_repeat",
        handle = 123L,
        repeat_count = 0L
      )
    )
  )

  # Unspecified parameters must not be set.
  mock_state$calls <- list()

  expect_null(SetDigitalOutTiming(device, wait_time = 0.2))

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "set_wait",
        handle = 123L,
        time_s = 0.2
      )
    )
  )


  #.. Invalid timing values ...................................................

  mock_state$calls <- list()

  expect_error(
    SetDigitalOutWait(device, wait_time = -1),
    "wait_time"
  )
  expect_error(
    SetDigitalOutRun(device, run_time = 1e-10),
    "out of supported range"
  )
  expect_error(
    SetDigitalOutRun(device, run_time = 131072),
    "out of supported range"
  )
  expect_error(
    SetDigitalOutRepeat(device, repeat_count = 1.5),
    "must be a whole number"
  )
  expect_error(
    SetDigitalOutTiming(device),
    "At least one Digital Out timing parameter must be specified"
  )

  expect_identical(mock_state$calls, list())


  #.. Partial update when a later parameter is invalid .........................

  mock_state$calls <- list()

  expect_error(
    SetDigitalOutTiming(
      device,
      run_time = 0.5,
      wait_time = 131072,
      repeat_count = 3L
    ),
    "out of supported range"
  )

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "set_run",
        handle = 123L,
        time_s = 0.5
      )
    )
  )


  #.. Configured timing getters ................................................

  expect_identical(GetDigitalOutRun(device), 0.5)
  expect_identical(GetDigitalOutWait(device), 0.1)
  expect_identical(GetDigitalOutRepeat(device), 3L)

  expect_identical(
    GetDigitalOutTiming(device),
    list(
      run_time = 0.5,
      wait_time = 0.1,
      repeat_count = 3L
    )
  )


  #.. Remaining values with automatic status updates ...........................

  mock_state$calls <- list()

  expect_identical(GetDigitalOutRemainingRun(device), 123456789)
  expect_identical(GetDigitalOutRemainingRepeat(device), 5L)

  expect_identical(
    mock_state$calls,
    list(
      list(call_type = "status", handle = 123L),
      list(call_type = "remaining_run", handle = 123L),
      list(call_type = "status", handle = 123L),
      list(call_type = "remaining_repeat", handle = 123L)
    )
  )


  #.. Remaining values from one status update ..................................

  mock_state$calls <- list()

  expect_identical(GetDigitalOutStatus(device), "running")
  expect_identical(GetDigitalOutRemainingRun(device, update = FALSE), 123456789)
  expect_identical(GetDigitalOutRemainingRepeat(device, update = FALSE), 5L)

  expect_identical(
    mock_state$calls,
    list(
      list(call_type = "status", handle = 123L),
      list(call_type = "remaining_run", handle = 123L),
      list(call_type = "remaining_repeat", handle = 123L)
    )
  )


  #.. Device-validation bypass ................................................

  the_env$devices <- list()

  expect_error(GetDigitalOutRun(device))
  expect_identical(GetDigitalOutRun(device, .validate_device = FALSE), 0.5)
})


test_that("Digital Out: timing configuration and status, SDK failure", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(
    list(device_handle = 123L)
  )

  local_mocked_bindings(
    .QueryDigitalOutRunRangeC = function(handle) {
      return(c(2e-7, 86400))
    },
    .DigitalOutRunSetC = function(handle, time_s) {
      stop("DWF run time setting error.", call. = FALSE)
    },
    .DigitalOutRunGetC = function(handle) {
      stop("DWF run query error.", call. = FALSE)
    },
    .DigitalOutStatusC = function(handle) {
      stop("DWF status error.", call. = FALSE)
    },
    .DigitalOutRunStatusC = function(handle) {
      stop("DWF remaining run error.", call. = FALSE)
    },
    .package = "dwf4r"
  )

  expect_error(
    SetDigitalOutRun(device, run_time = 0.1),
    "DWF run time setting error"
  )
  expect_error(
    GetDigitalOutRun(device),
    "DWF run query error"
  )

  # A failed status update must prevent the remaining-value query.
  expect_error(
    GetDigitalOutRemainingRun(device),
    "DWF status error"
  )

  expect_error(
    GetDigitalOutRemainingRun(device, update = FALSE),
    "DWF remaining run error"
  )
})


test_that("Digital Out: trigger configuration", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(list(device_handle = 123L))

  mock_state <- new.env(parent = emptyenv())
  mock_state$last_call <- NULL

  local_mocked_bindings(
    .QueryDeviceTriggerSourceMaskC = function(handle) {
      # none, pc, analog_out_1, external_1
      return(2179L)
    },
    .QueryDeviceTriggerSlopeMaskC = function(handle) {
      # rising, either
      return(5L)
    },

    .DigitalOutTriggerSourceSetC = function(handle, source_code) {
      mock_state$last_call <- list(
        call_type = "set_trigger_source",
        handle = handle,
        source_code = source_code
      )
      return(invisible(NULL))
    },
    .DigitalOutTriggerSourceGetC = function(handle) {
      mock_state$last_call <- list(
        call_type = "get_trigger_source",
        handle = handle
      )
      return(11L) # external_1
    },
    .DigitalOutTriggerSlopeSetC = function(handle, slope_code) {
      mock_state$last_call <- list(
        call_type = "set_trigger_slope",
        handle = handle,
        slope_code = slope_code
      )
      return(invisible(NULL))
    },
    .DigitalOutTriggerSlopeGetC = function(handle) {
      mock_state$last_call <- list(
        call_type = "get_trigger_slope",
        handle = handle
      )
      return(2L) # either
    },
    .DigitalOutRepeatTriggerSetC = function(handle, repeat_trigger) {
      mock_state$last_call <- list(
        call_type = "set_repeat_trigger",
        handle = handle,
        repeat_trigger = repeat_trigger
      )
      return(invisible(NULL))
    },
    .DigitalOutRepeatTriggerGetC = function(handle) {
      mock_state$last_call <- list(
        call_type = "get_repeat_trigger",
        handle = handle
      )
      return(FALSE)
    },

    .package = "dwf4r"
  )


  #.. SetDigitalOutTriggerSource(), GetDigitalOutTriggerSource() ...............

  expect_null(SetDigitalOutTriggerSource(device, source = "external_1"))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_trigger_source",
      handle = 123L,
      source_code = 11L
    )
  )

  expect_identical(GetDigitalOutTriggerSource(device), "external_1")
  expect_identical(
    mock_state$last_call,
    list(call_type = "get_trigger_source", handle = 123L)
  )

  expect_error(
    SetDigitalOutTriggerSource(device, source = "external_2"),
    "Trigger source 'external_2' is not supported by the device"
  )


  #.. SetDigitalOutTriggerSlope(), GetDigitalOutTriggerSlope() .................

  expect_null(SetDigitalOutTriggerSlope(device, slope = "either"))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_trigger_slope",
      handle = 123L,
      slope_code = 2L
    )
  )

  expect_identical(GetDigitalOutTriggerSlope(device), "either")
  expect_identical(
    mock_state$last_call,
    list(call_type = "get_trigger_slope", handle = 123L)
  )

  expect_error(
    SetDigitalOutTriggerSlope(device, slope = "falling"),
    "Trigger slope 'falling' is not supported by the device"
  )


  #.. SetDigitalOutRepeatTrigger(), GetDigitalOutRepeatTrigger() ...............

  expect_null(SetDigitalOutRepeatTrigger(device, repeat_trigger = TRUE))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_repeat_trigger",
      handle = 123L,
      repeat_trigger = TRUE
    )
  )

  expect_null(SetDigitalOutRepeatTrigger(device, repeat_trigger = FALSE))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "set_repeat_trigger",
      handle = 123L,
      repeat_trigger = FALSE
    )
  )

  expect_false(GetDigitalOutRepeatTrigger(device))
  expect_identical(
    mock_state$last_call,
    list(call_type = "get_repeat_trigger", handle = 123L)
  )

  expect_error(
    SetDigitalOutRepeatTrigger(device, repeat_trigger = 1L),
    "repeat_trigger"
  )


  #.. Device-validation bypass ................................................

  the_env$devices <- list()

  expect_error(
    SetDigitalOutTriggerSource(device, source = "pc"),
    "session that does not exist"
  )
  expect_null(
    SetDigitalOutTriggerSource(
      device,
      source = "pc",
      .validate_device = FALSE
    )
  )
})


test_that("Digital Out: trigger configuration, SDK errors", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(list(device_handle = 123L))

  local_mocked_bindings(
    .QueryDeviceTriggerSourceMaskC = function(handle) {
      return(3L) # none, pc
    },
    .DigitalOutTriggerSourceSetC = function(handle, source_code) {
      stop("DWF trigger source error.", call. = FALSE)
    },
    .DigitalOutRepeatTriggerGetC = function(handle) {
      stop("DWF repeat trigger query error.", call. = FALSE)
    },
    .DigitalOutTriggerSourceGetC = function(handle) {
      return(999L)
    },
    .package = "dwf4r"
  )

  expect_error(
    SetDigitalOutTriggerSource(device, source = "pc"),
    "DWF trigger source error"
  )
  expect_error(
    GetDigitalOutRepeatTrigger(device),
    "DWF repeat trigger query error"
  )

  # Unknown SDK codes must not be returned as missing or invented source names.
  expect_error(
    GetDigitalOutTriggerSource(device),
    "unknown Digital Out trigger source code"
  )
})



#==[ Hardware tests ]===========================================================

test_that("Hardware: Digital Out control and channel configuration", {
  skip_if(
    !identical(Sys.getenv("DWF4R_TEST_DIGITAL_OUT"), "true"),
    paste(
      "Set 'DWF4R_TEST_DIGITAL_OUT=true' to run tests that may activate",
      "Digital Out channels."
    )
  )

  device <- .OpenHardwareTestDevice()
  channel <- 0L
  on.exit({
    try(StopDigitalOut(device), silent = TRUE)
    try(DisableDigitalOutChannel(device, channel), silent = TRUE)
    try(ResetDigitalOut(device), silent = TRUE)
    try(CloseDevice(device), silent = TRUE)
  }, add = TRUE)

  skip_if(
    GetDigitalOutChannelCount(device) == 0L,
    "Configured device has no Digital Out channels."
  )
  skip_if(
    GetDigitalOutMaxDataBits(device, channel) < 4L,
    "Digital Out channel supports fewer than four custom-data bits."
  )



  #.. Reset Digital Out ........................................................

  expect_null(ResetDigitalOut(device))


  #.. Enable/disable channel ...................................................

  expect_null(DisableDigitalOutChannel(device, channel))
  StartDigitalOut(device)
  expect_false(IsDigitalOutChannelEnabled(device, channel))

  expect_null(EnableDigitalOutChannel(device, channel))
  StartDigitalOut(device)
  expect_true(IsDigitalOutChannelEnabled(device, channel))


  #.. Set/get channel configuration ............................................

  output <- GetDigitalOutOutput(device, channel)
  func <- GetDigitalOutFunction(device, channel)
  idle <- GetDigitalOutIdle(device, channel)

  expect_null(SetDigitalOutOutput(device, channel, output))
  expect_null(SetDigitalOutFunction(device, channel, func))
  expect_null(SetDigitalOutIdle(device, channel, idle))


  #.. Get divider and counter configuration ....................................

  divider <- GetDigitalOutDivider(device, channel)
  expect_length(divider, 1L)
  expect_true(is.numeric(divider))

  initial_divider <- GetDigitalOutInitialDivider(device, channel)
  expect_length(initial_divider, 1L)
  expect_true(is.numeric(initial_divider))

  counter <- GetDigitalOutCounter(device, channel)
  expect_named(counter, c("low_count", "high_count"))
  expect_true(is.numeric(counter))

  initial_counter <- GetDigitalOutInitialCounter(device, channel)
  expect_named(initial_counter, c("initial_state", "initial_count"))
  expect_true(initial_counter$initial_state %in% c("low", "high"))
  expect_true(is.numeric(initial_counter$initial_count))


  #.. Set divider and counter configuration ....................................

  divider_range <- GetDigitalOutDividerRange(device, channel)
  counter_range <- GetDigitalOutCounterRange(device, channel)

  divider <- divider_range[[1L]]
  counter <- counter_range[[1L]]

  expect_null(
    SetDigitalOutDivider(device, channel, divider)
  )
  expect_null(
    SetDigitalOutInitialDivider(device, channel, divider)
  )
  expect_null(
    SetDigitalOutCounter(
      device,
      channel,
      low_count = counter,
      high_count = counter
    )
  )
  expect_null(
    SetDigitalOutInitialCounter(
      device,
      channel,
      initial_state = "low",
      initial_count = counter
    )
  )


  #.. Start/stop Digital Out ...................................................

  expect_null(StartDigitalOut(device))

  status <- GetDigitalOutStatus(device)
  expect_true(status %in% c("ready", "armed", "wait", "running", "done"))

  expect_null(StopDigitalOut(device))


  #.. Custom-pattern configuration .............................................

  functions <- GetDigitalOutFunctionTypes(device, channel)

  if ("custom" %in% functions) {
    expect_null(
      SetDigitalOutFunction(device, channel, func = "custom")
    )

    expect_null(
      SetDigitalOutCustomPattern(
        device,
        channel,
        pattern = c(0, 1, 1, 0)
      )
    )

    internal_clock <- GetDigitalOutInternalClockFrequency(device)
    divider <- round(mean(divider_range))
    sample_rate <- internal_clock / divider

    expect_null(
      SetDigitalOutCustomSampleRate(
        device,
        channel,
        sample_rate = sample_rate
      )
    )

    sample_rate <- GetDigitalOutCustomSampleRate(device, channel)
    expect_length(sample_rate, 1L)
    expect_true(is.numeric(sample_rate))
    expect_gt(sample_rate, 0)
  }


  #.. Timing configuration ....................................................

  repeat_range <- GetDigitalOutRepeatRange(device)
  repeat_count <- round(mean(repeat_range))

  expect_null(
    SetDigitalOutRepeat(device, repeat_count = repeat_count)
  )

  repeat_count <- GetDigitalOutRepeat(device)
  expect_length(repeat_count, 1L)
  expect_true(is.numeric(repeat_count))


  #.. Trigger configuration ....................................................

  sources <- GetDeviceTriggerSources(device)
  expect_null(
    SetDigitalOutTriggerSource(device, source = sources[[1L]])
  )
  expect_type(
    GetDigitalOutTriggerSource(device),
    "character"
  )
})


