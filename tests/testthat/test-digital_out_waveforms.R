#==[ Mock-based tests ]=========================================================

test_that("Digital Out: high-level waveform configuration", {
  device <- .MakeDeviceObject()

  mock_state <- new.env(parent = emptyenv())
  mock_state$calls <- list()

  RegisterNewCall <- function(call) {
    call_index <- length(mock_state$calls) + 1L
    mock_state$calls[[call_index]] <- call
    return(invisible(NULL))
  }

  local_mocked_bindings(
    .AssertDigitalOut = function(...) {
      return(invisible(NULL))
    },
    GetDigitalOutInternalClockFrequency = function(
    device,
    .validate_device = TRUE
    ) {
      return(1e8)
    },
    GetDigitalOutDividerRange = function(
    device,
    channel,
    .validate_digital_out = TRUE
    ) {
      return(c(1L, 1000L))
    },
    GetDigitalOutCounterRange = function(
    device,
    channel,
    .validate_digital_out = TRUE
    ) {
      return(c(1L, 100L))
    },
    SetDigitalOutFunction = function(
    device,
    channel,
    func,
    .validate_digital_out = TRUE
    ) {
      RegisterNewCall(list(
        call_type = "set_function",
        channel = channel,
        func = func,
        .validate_digital_out = .validate_digital_out
      ))
      return(invisible(NULL))
    },
    SetDigitalOutDivider = function(
    device,
    channel,
    divider,
    .validate_digital_out = TRUE
    ) {
      RegisterNewCall(list(
        call_type = "set_divider",
        channel = channel,
        divider = divider,
        .validate_digital_out = .validate_digital_out
      ))
      return(invisible(NULL))
    },
    SetDigitalOutCounter = function(
    device,
    channel,
    low_count,
    high_count,
    .validate_digital_out = TRUE
    ) {
      RegisterNewCall(list(
        call_type = "set_counter",
        channel = channel,
        low_count = low_count,
        high_count = high_count,
        .validate_digital_out = .validate_digital_out
      ))
      return(invisible(NULL))
    },
    SetDigitalOutInitialDivider = function(
    device,
    channel,
    initial_divider,
    .validate_digital_out = TRUE
    ) {
      RegisterNewCall(list(
        call_type = "set_initial_divider",
        channel = channel,
        initial_divider = initial_divider,
        .validate_digital_out = .validate_digital_out
      ))
      return(invisible(NULL))
    },
    SetDigitalOutInitialCounter = function(
    device,
    channel,
    initial_state,
    initial_count,
    .validate_digital_out = TRUE
    ) {
      RegisterNewCall(list(
        call_type = "set_initial_counter",
        channel = channel,
        initial_state = initial_state,
        initial_count = initial_count,
        .validate_digital_out = .validate_digital_out
      ))
      return(invisible(NULL))
    },
    SetDigitalOutCustomPattern = function(
    device,
    channel,
    pattern,
    is_three_state = FALSE,
    .validate_digital_out = TRUE
    ) {
      RegisterNewCall(list(
        call_type = "set_custom_pattern",
        channel = channel,
        pattern = pattern,
        is_three_state = is_three_state,
        .validate_digital_out = .validate_digital_out
      ))
      return(invisible(NULL))
    },
    EnableDigitalOutChannel = function(
    device,
    channel,
    .validate_digital_out = TRUE
    ) {
      RegisterNewCall(list(
        call_type = "enable_channel",
        channel = channel,
        .validate_digital_out = .validate_digital_out
      ))
      return(invisible(NULL))
    },
    SetDigitalOutOutput = function(
    device,
    channel,
    output,
    .validate_digital_out = TRUE
    ) {
      RegisterNewCall(list(
        call_type = "set_output",
        channel = channel,
        output = output,
        .validate_digital_out = .validate_digital_out
      ))
      return(invisible(NULL))
    },
    GetDigitalOutMaxDataBits = function(
    device,
    channel,
    .validate_digital_out = TRUE
    ) {
      return(16L)
    },

    .package = "dwf4r"
  )


  #.. ConfigureDigitalOutPulse(), exactly representable parameters .............

  mock_state$calls <- list()

  result <- ConfigureDigitalOutPulse(
    device,
    channel = 1L,
    frequency = 250000,
    duty_cycle = 25,
    phase = 90
  )

  expect_equal(
    result,
    list(
      frequency = list(
        requested = 250000,
        actual = 250000,
        abs_error = 0,
        rel_error = 0
      ),
      duty_cycle = list(
        requested = 25,
        actual = 25,
        abs_error = 0
      ),
      phase = list(
        requested = 90,
        actual = 90,
        abs_error = 0
      )
    )
  )

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "set_output",
        channel = 1L,
        output = "push_pull",
        .validate_digital_out = FALSE
      ),
      list(
        call_type = "set_function",
        channel = 1L,
        func = "pulse",
        .validate_digital_out = FALSE
      ),
      list(
        call_type = "set_divider",
        channel = 1L,
        divider = 4,
        .validate_digital_out = FALSE
      ),
      list(
        call_type = "set_counter",
        channel = 1L,
        low_count = 75,
        high_count = 25,
        .validate_digital_out = FALSE
      ),
      list(
        call_type = "set_initial_divider",
        channel = 1L,
        initial_divider = 4,
        .validate_digital_out = FALSE
      ),
      list(
        call_type = "set_initial_counter",
        channel = 1L,
        initial_state = "low",
        initial_count = 0,
        .validate_digital_out = FALSE
      ),
      list(
        call_type = "enable_channel",
        channel = 1L,
        .validate_digital_out = FALSE
      )
    )
  )


  #.. ConfigureDigitalOutPulse(), parameter quantization .......................

  mock_state$calls <- list()

  result <- ConfigureDigitalOutPulse(
    device,
    channel = 1L,
    frequency = 300000,
    duty_cycle = 33,
    phase = 200
  )

  # divider = ceiling((100e6 / 300e3) / 100) = 4
  # counts_per_period = round((100e6 / 300e3) / 4) = 83
  # high_count = round(83 * 0.33) = 27
  # phase_count = round(83 * 200 / 360) = 46

  expect_equal(result$frequency$actual, 100e6 / (4 * 83))
  expect_equal(result$duty_cycle$actual, 100 * 27 / 83)
  expect_equal(result$phase$actual, 360 * 46 / 83)


  #.. ConfigureDigitalOutPulse(), output modes .................................

  mock_state$calls <- list()

  ConfigureDigitalOutPulse(
    device,
    channel = 1L,
    frequency = 250000,
    output = "open_drain"
  )

  expect_identical(mock_state$calls[[1L]]$output, "open_drain")

  mock_state$calls <- list()

  ConfigureDigitalOutPulse(
    device,
    channel = 1L,
    frequency = 250000
  )

  expect_identical(
    mock_state$calls[[1L]],
    list(
      call_type = "set_output",
      channel = 1L,
      output = "push_pull",
      .validate_digital_out = FALSE
    )
  )
  expect_identical(
    mock_state$calls[[2L]],
    list(
      call_type = "set_function",
      channel = 1L,
      func = "pulse",
      .validate_digital_out = FALSE
    )
  )

  mock_state$calls <- list()

  expect_error(
    ConfigureDigitalOutPulse(
      device,
      channel = 1L,
      frequency = 250000,
      output = "three_state"
    ),
    "'output'"
  )

  expect_length(mock_state$calls, 0L)


  #.. ConfigureDigitalOutCustom(), frequency input .............................

  mock_state$calls <- list()

  result <- ConfigureDigitalOutCustom(
    device,
    channel = 1L,
    pattern = c(1, 1, 0, 0),
    frequency = 1e6
  )

  expect_equal(
    result,
    list(
      frequency = list(
        requested = 1e6,
        actual = 1e6,
        abs_error = 0,
        rel_error = 0
      ),
      sample_rate = list(
        requested = 4e6,
        actual = 4e6,
        abs_error = 0,
        rel_error = 0
      )
    )
  )

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "set_output",
        channel = 1L,
        output = "push_pull",
        .validate_digital_out = FALSE
      ),
      list(
        call_type = "set_custom_pattern",
        channel = 1L,
        pattern = c(1, 1, 0, 0),
        is_three_state = FALSE,
        .validate_digital_out = FALSE
      ),
      list(
        call_type = "set_function",
        channel = 1L,
        func = "custom",
        .validate_digital_out = FALSE
      ),
      list(
        call_type = "set_divider",
        channel = 1L,
        divider = 25,
        .validate_digital_out = FALSE
      ),
      list(
        call_type = "set_initial_divider",
        channel = 1L,
        initial_divider = 25,
        .validate_digital_out = FALSE
      ),
      list(
        call_type = "enable_channel",
        channel = 1L,
        .validate_digital_out = FALSE
      )
    )
  )


  #.. ConfigureDigitalOutCustom(), output modes ................................

  mock_state$calls <- list()

  ConfigureDigitalOutCustom(
    device,
    channel = 1L,
    pattern = c("l", "z", "h", "z"),
    frequency = 30000,
    output = "three_state"
  )

  expect_identical(mock_state$calls[[1L]]$output, "three_state")
  expect_true(mock_state$calls[[2L]]$is_three_state)

  mock_state$calls <- list()

  ConfigureDigitalOutCustom(
    device,
    channel = 1L,
    pattern = c(0, 1),
    frequency = 50000
  )

  expect_identical(
    mock_state$calls[[1L]],
    list(
      call_type = "set_output",
      channel = 1L,
      output = "push_pull",
      .validate_digital_out = FALSE
    )
  )
  expect_identical(
    mock_state$calls[[2L]],
    list(
      call_type = "set_custom_pattern",
      channel = 1L,
      pattern = c(0, 1),
      is_three_state = FALSE,
      .validate_digital_out = FALSE
    )
  )

  mock_state$calls <- list()

  ConfigureDigitalOutCustom(
    device,
    channel = 1L,
    pattern = c(0, 1),
    frequency = 50000,
    output = "open_drain"
  )

  expect_identical(mock_state$calls[[1L]]$output, "open_drain")
  expect_false(mock_state$calls[[2L]]$is_three_state)


  #.. ConfigureDigitalOutCustom(), sample-rate input ...........................

  result <- ConfigureDigitalOutCustom(
    device,
    channel = 1L,
    pattern = c(1, 1, 0, 0),
    sample_rate = 3.6e6
  )
  expected_divider <- 28 # round(100e6 / 3.6e6)
  expect_equal(result$frequency$actual, (1e8 / expected_divider) / 4)
  expect_equal(result$sample_rate$actual, 1e8 / expected_divider)


  #.. ConfigureDigitalOutCustom(), invalid rate specification ..................

  expect_error(
    ConfigureDigitalOutCustom(
      device,
      channel = 1L,
      pattern = c(1, 0)
    ),
    "Exactly one of 'frequency' and 'sample_rate'"
  )

  expect_error(
    ConfigureDigitalOutCustom(
      device,
      channel = 1L,
      pattern = c(1, 0),
      frequency = 1e6,
      sample_rate = 2e6
    ),
    "Exactly one of 'frequency' and 'sample_rate'"
  )


  #.. ConfigureDigitalOutCustom(), incompatible output mode ....................

  mock_state$calls <- list()

  expect_error(
    ConfigureDigitalOutCustom(
      device,
      channel = 1L,
      pattern = c("l", "z", "h"),
      frequency = 1e6,
      output = "push_pull"
    ),
    "'z' pattern values require 'output = \"three_state\"'"
  )

  expect_error(
    ConfigureDigitalOutCustom(
      device,
      channel = 1L,
      pattern = c(0, 2, 1),
      frequency = 1000
    ),
    "pattern"
  )

  expect_error(
    ConfigureDigitalOutCustom(
      device,
      channel = 1L,
      pattern = c(0, 2, 1),
      frequency = 1000
    ),
    "pattern"
  )

  expect_error(
    ConfigureDigitalOutCustom(
      device,
      channel = 1L,
      pattern = rep(1, 17L),
      frequency = 1000
    ),
    "pattern"
  )

  expect_length(mock_state$calls, 0L)
})


test_that("Digital Out: configuration from time-state points", {
  device <- .MakeDeviceObject()

  mock_state <- new.env(parent = emptyenv())
  mock_state$validated_state <- NULL
  mock_state$last_call <- NULL

  rel_error <- 0.01

  local_mocked_bindings(
    .AssertDigitalOut = function(...) {
      return(invisible(NULL))
    },
    .EncodeDigitalOutCustomPattern = function(
    pattern,
    is_three_state
    ) {
      mock_state$validated_state <- pattern
      return(list())
    },
    GetDigitalOutMaxDataBits = function(
    device,
    channel,
    .validate_digital_out = TRUE
    ) {
      # Eight custom-data bits correspond to four logical elements in
      # three-state mode.
      return(8L)
    },
    GetDigitalOutInternalClockFrequency = function(
    device,
    .validate_device = TRUE
    ) {
      return(1e7)
    },
    GetDigitalOutDividerRange = function(
    device,
    channel,
    .validate_digital_out = TRUE
    ) {
      return(c(1L, 100L))
    },
    ConfigureDigitalOutCustom = function(
    device,
    channel,
    pattern,
    frequency = NULL,
    sample_rate = NULL,
    output = "push_pull",
    .validate_digital_out = TRUE
    ) {
      mock_state$last_call <- list(
        channel = channel,
        pattern = pattern,
        frequency = frequency,
        output = output,
        .validate_digital_out = .validate_digital_out
      )

      actual_frequency <- (1 + rel_error) * frequency

      # The mock intentionally includes only the result fields used by
      # 'ConfigureDigitalOutFromPoints()'; 'ConfigureDigitalOutCustom()'
      # normally returns additional requested and error values.
      return(invisible(list(
        frequency = list(actual = actual_frequency),
        sample_rate = list(actual = actual_frequency * length(pattern))
      )))
    },
    .package = "dwf4r"
  )


  #.. Three-state waveform generation ..........................................

  result <- ConfigureDigitalOutFromPoints(
    device,
    channel = 1L,
    time_us = c(0, 1, 2, 4),
    states = c("h", "l", "z", "h"),
    output = "three_state"
  )

  expect_equal(
    mock_state$last_call,
    list(
      channel = 1L,
      pattern = c("h", "l", "z", "z"),
      frequency = 1e6 / 4,
      output = "three_state",
      .validate_digital_out = FALSE
    )
  )

  expect_identical(
    mock_state$validated_state,
    c("h", "l", "z", "h")
  )

  expect_equal(
    result$waveform,
    data.frame(
      time_us = c(0, 1, 2, 3) / (1 + rel_error),
      states = c("h", "l", "z", "z")
    )
  )

  requested_frequency <- 250000
  expect_equal(
    result$frequency,
    list(actual = (1 + rel_error) * requested_frequency)
  )

  requested_period <- 1e6 / requested_frequency
  actual_period <- requested_period / (1 + rel_error)
  period_abs_error <- abs(actual_period - requested_period)
  period_rel_error <- period_abs_error / requested_period
  expect_equal(
    result$period,
    list(
      requested = requested_period,
      actual = actual_period,
      abs_error = period_abs_error,
      rel_error = period_rel_error
    )
  )


  #.. Shorter state-vector representation ......................................

  expect_identical(
    ConfigureDigitalOutFromPoints(
      device,
      channel = 1L,
      time_us = c(0, 1, 2, 4),
      states = c("h", "l", "z"),
      output = "three_state"
    ),
    result
  )



  #.. Ordinary output mode and pattern resolution ..............................

  ConfigureDigitalOutFromPoints(
    device,
    channel = 1L,
    time_us = c(0, 1, 2, 4),
    states = c(1, 0, 1),
    output = "open_source"
  )

  expect_identical(mock_state$last_call$output, "open_source")
  expect_identical(
    mock_state$last_call$pattern,
    c(1, 1, 0, 0, 1, 1, 1, 1)
  )


  #.. Invalid waveform specification ...........................................

  mock_state$last_call <- NULL

  expect_error(
    ConfigureDigitalOutFromPoints(
      device,
      channel = 1L,
      time_us = c(0, 1, 2, 3, 4, 5),
      states = c(1, 0, 1, 0, 1),
      output = "three_state"
    ),
    "insufficient to represent every interval"
  )

  expect_error(
    ConfigureDigitalOutFromPoints(
      device,
      channel = 1L,
      time_us = c(0, 0.05),
      states = 1
    ),
    "period is too short"
  )

  expect_error(
    ConfigureDigitalOutFromPoints(
      device,
      channel = 1L,
      time_us = c(0, 1, 2, 4),
      states = c(1, 0),
      output = "three_state"
    ),
    "Length of 'states' must be equal to length of 'time_us'"
  )

  expect_error(
    ConfigureDigitalOutFromPoints(
      device,
      channel = 1L,
      time_us = c(0, 1, 2, 3),
      states = c("l", "z", "h"),
      output = "push_pull"
    ),
    "'z' state values require"
  )

  expect_null(mock_state$last_call)
})


