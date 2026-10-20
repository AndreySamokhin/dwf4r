#==[ Mock-based tests ]=========================================================

test_that("Analog Out: high-level waveform configuration", {
  device <- .MakeDeviceObject()

  mock_state <- new.env(parent = emptyenv())
  mock_state$calls <- list()

  RegisterNewCall <- function(call) {
    call_index <- length(mock_state$calls) + 1L
    mock_state$calls[[call_index]] <- call
    return(invisible(NULL))
  }

  local_mocked_bindings(
    SetAnalogOutCarrier = function(
    device,
    channel,
    func = NULL,
    frequency = NULL,
    amplitude = NULL,
    offset = NULL,
    symmetry = NULL,
    phase = NULL,
    .validate_analog_out = TRUE
    ) {
      RegisterNewCall(list(
        call_type = "set_carrier",
        channel = channel,
        func = func,
        frequency = frequency,
        amplitude = amplitude,
        offset = offset,
        symmetry = symmetry,
        phase = phase,
        .validate_analog_out = .validate_analog_out
      ))
      return(invisible(NULL))
    },
    EnableAnalogOutCarrier = function(
    device,
    channel,
    .validate_analog_out = TRUE
    ) {
      RegisterNewCall(list(
        call_type = "enable_carrier",
        channel = channel,
        .validate_analog_out = .validate_analog_out
      ))
      return(invisible(NULL))
    },
    .package = "dwf4r"
  )


  #.. ConfigureAnalogOutConstantVoltage() ......................................

  mock_state$calls <- list()

  expect_null(
    ConfigureAnalogOutConstantVoltage(
      device,
      channel = 1L,
      voltage = 1.25
    )
  )

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "set_carrier",
        channel = 1L,
        func = "dc",
        frequency = NULL,
        amplitude = NULL,
        offset = 1.25,
        symmetry = NULL,
        phase = NULL,
        .validate_analog_out = TRUE
      ),
      list(
        call_type = "enable_carrier",
        channel = 1L,
        .validate_analog_out = FALSE
      )
    )
  )


  #.. ConfigureAnalogOutSine() .................................................

  mock_state$calls <- list()

  expect_null(
    ConfigureAnalogOutSine(
      device,
      channel = 1L,
      frequency = 1000,
      amplitude = 2,
      offset = 0.5,
      phase = 90
    )
  )

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "set_carrier",
        channel = 1L,
        func = "sine",
        frequency = 1000,
        amplitude = 2,
        offset = 0.5,
        symmetry = 50,
        phase = 90,
        .validate_analog_out = TRUE
      ),
      list(
        call_type = "enable_carrier",
        channel = 1L,
        .validate_analog_out = FALSE
      )
    )
  )


  #.. ConfigureAnalogOutSquare() ...............................................

  mock_state$calls <- list()

  expect_null(
    ConfigureAnalogOutSquare(
      device,
      channel = 1L,
      frequency = 2000,
      amplitude = 1.5,
      offset = -0.25,
      duty_cycle = 25,
      phase = 180
    )
  )

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "set_carrier",
        channel = 1L,
        func = "square",
        frequency = 2000,
        amplitude = 1.5,
        offset = -0.25,
        symmetry = 25,
        phase = 180,
        .validate_analog_out = TRUE
      ),
      list(
        call_type = "enable_carrier",
        channel = 1L,
        .validate_analog_out = FALSE
      )
    )
  )


  #.. ConfigureAnalogOutTriangle() .............................................

  mock_state$calls <- list()

  expect_null(
    ConfigureAnalogOutTriangle(
      device,
      channel = 1L,
      frequency = 3000,
      amplitude = 1,
      offset = 0.25,
      phase = 270
    )
  )

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "set_carrier",
        channel = 1L,
        func = "triangle",
        frequency = 3000,
        amplitude = 1,
        offset = 0.25,
        symmetry = 50,
        phase = 270,
        .validate_analog_out = TRUE
      ),
      list(
        call_type = "enable_carrier",
        channel = 1L,
        .validate_analog_out = FALSE
      )
    )
  )


  #.. ConfigureAnalogOutRampUp() ...............................................

  mock_state$calls <- list()

  expect_null(
    ConfigureAnalogOutRampUp(
      device,
      channel = 1L,
      frequency = 4000,
      amplitude = 0.75,
      offset = -0.5,
      phase = 45
    )
  )

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "set_carrier",
        channel = 1L,
        func = "ramp_up",
        frequency = 4000,
        amplitude = 0.75,
        offset = -0.5,
        symmetry = 100,
        phase = 45,
        .validate_analog_out = TRUE
      ),
      list(
        call_type = "enable_carrier",
        channel = 1L,
        .validate_analog_out = FALSE
      )
    )
  )


  #.. ConfigureAnalogOutRampDown() .............................................

  mock_state$calls <- list()

  expect_null(
    ConfigureAnalogOutRampDown(
      device,
      channel = 1L,
      frequency = 5000,
      amplitude = 0.5,
      offset = 0.75,
      phase = 135
    )
  )

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "set_carrier",
        channel = 1L,
        func = "ramp_down",
        frequency = 5000,
        amplitude = 0.5,
        offset = 0.75,
        symmetry = 0,
        phase = 135,
        .validate_analog_out = TRUE
      ),
      list(
        call_type = "enable_carrier",
        channel = 1L,
        .validate_analog_out = FALSE
      )
    )
  )


  #.. Analog Out validation bypass .............................................

  mock_state$calls <- list()

  expect_null(
    ConfigureAnalogOutSine(
      device,
      channel = 1L,
      frequency = 1000,
      amplitude = 1,
      .validate_analog_out = FALSE
    )
  )

  expect_false(mock_state$calls[[1L]]$.validate_analog_out)
  expect_false(mock_state$calls[[2L]]$.validate_analog_out)
})


test_that("Analog Out: custom waveform configuration", {
  device <- .MakeDeviceObject()

  mock_state <- new.env(parent = emptyenv())
  mock_state$calls <- list()

  RegisterNewCall <- function(call) {
    call_index <- length(mock_state$calls) + 1L
    mock_state$calls[[call_index]] <- call
    return(invisible(NULL))
  }

  local_mocked_bindings(
    .AssertAnalogOut = function(...) {
      return(invisible(NULL))
    },
    GetAnalogOutNodeSampleCountRange = function(
    device,
    channel,
    node,
    .validate_analog_out = TRUE
    ) {
      return(c(2L, 4L))
    },
    SetAnalogOutCarrier = function(
    device,
    channel,
    func = NULL,
    frequency = NULL,
    amplitude = NULL,
    offset = NULL,
    symmetry = NULL,
    phase = NULL,
    .validate_analog_out = TRUE
    ) {
      RegisterNewCall(list(
        call_type = "set_carrier",
        channel = channel,
        func = func,
        frequency = frequency,
        amplitude = amplitude,
        offset = offset,
        phase = phase,
        .validate_analog_out = .validate_analog_out
      ))
      return(invisible(NULL))
    },
    SetAnalogOutCarrierData = function(
    device,
    channel,
    data,
    .validate_analog_out = TRUE
    ) {
      RegisterNewCall(list(
        call_type = "set_data",
        channel = channel,
        data = data,
        .validate_analog_out = .validate_analog_out
      ))
      return(invisible(NULL))
    },
    EnableAnalogOutCarrier = function(
    device,
    channel,
    .validate_analog_out = TRUE
    ) {
      RegisterNewCall(list(
        call_type = "enable_carrier",
        channel = channel,
        .validate_analog_out = .validate_analog_out
      ))
      return(invisible(NULL))
    },
    .package = "dwf4r"
  )


  #.. Waveform scaling and configuration .......................................

  expect_null(
    ConfigureAnalogOutCustom(
      device,
      channel = 1L,
      voltages = c(-1, 0, 1, 3),
      frequency = 1000,
      phase = 90
    )
  )

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "set_carrier",
        channel = 1L,
        func = "custom",
        frequency = 1000,
        amplitude = 2,
        offset = 1,
        phase = 90,
        .validate_analog_out = FALSE
      ),
      list(
        call_type = "set_data",
        channel = 1L,
        data = c(-1, -0.5, 0, 1),
        .validate_analog_out = FALSE
      ),
      list(
        call_type = "enable_carrier",
        channel = 1L,
        .validate_analog_out = FALSE
      )
    )
  )


  #.. SDK interpolation warning ................................................

  mock_state$calls <- list()

  expect_warning(
    ConfigureAnalogOutCustom(
      device,
      channel = 1L,
      voltages = c(0, 1, 0),
      frequency = 1000
    ),
    "WaveForms SDK interpolates custom waveform data"
  )


  #.. Invalid inputs ...........................................................

  mock_state$calls <- list()

  expect_error(
    ConfigureAnalogOutCustom(
      device,
      channel = 1L,
      voltages = rep(1, 4),
      frequency = 1000
    ),
    "must contain at least two different values"
  )

  expect_error(
    ConfigureAnalogOutCustom(
      device,
      channel = 1L,
      voltages = c(-1, 0, 1, 2, 3),
      frequency = 1000
    ),
    "out of supported range"
  )

  expect_error(
    ConfigureAnalogOutCustom(
      device,
      channel = 1L,
      voltages = c(0, 1, 0, 1),
      frequency = 1000,
      offset = c(0, 1)
    ),
    "offset"
  )

  expect_identical(mock_state$calls, list())


  #.. Explicit offset ..........................................................

  mock_state$calls <- list()

  expect_null(
    ConfigureAnalogOutCustom(
      device,
      channel = 1L,
      voltages = c(0, 1, 2, 1),
      frequency = 1000,
      offset = 0
    )
  )

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "set_carrier",
        channel = 1L,
        func = "custom",
        frequency = 1000,
        amplitude = 2,
        offset = 0,
        phase = 0,
        .validate_analog_out = FALSE
      ),
      list(
        call_type = "set_data",
        channel = 1L,
        data = c(0, 0.5, 1, 0.5),
        .validate_analog_out = FALSE
      ),
      list(
        call_type = "enable_carrier",
        channel = 1L,
        .validate_analog_out = FALSE
      )
    )
  )

})


test_that("Analog Out: configuration from control points", {
  device <- .MakeDeviceObject()

  mock_state <- new.env(parent = emptyenv())
  mock_state$last_call <- list()

  local_mocked_bindings(
    .AssertAnalogOut = function(...) {
      return(invisible(NULL))
    },
    GetAnalogOutNodeSampleCountRange = function(
    device,
    channel,
    node,
    .validate_analog_out = TRUE
    ) {
      return(c(2L, 4L))
    },
    ConfigureAnalogOutCustom = function(
    device,
    channel,
    voltages,
    frequency,
    offset = NULL,
    phase = 0,
    .validate_analog_out = TRUE
    ) {
      mock_state$last_call <- list(
        channel = channel,
        voltages = voltages,
        frequency = frequency,
        offset = offset,
        .validate_analog_out = .validate_analog_out
      )
      return(invisible(NULL))
    },
    .package = "dwf4r"
  )


  #.. Linear interpolation .....................................................

  expect_equal(
    ConfigureAnalogOutFromPoints(
      device,
      channel = 1L,
      time_us = c(0, 2, 4),
      voltages = c(0, 2, 0)
    ),
    data.frame(
      time_us = c(0, 1, 2, 3),
      voltage = c(0, 1, 2, 1)
    )
  )

  expect_identical(
    mock_state$last_call,
    list(
      channel = 1L,
      voltages = c(0, 1, 2, 1),
      frequency = 250000,
      offset = NULL,
      .validate_analog_out = FALSE
    )
  )


  #.. Constant interpolation, equal-length voltage vector ......................

  expect_equal(
    ConfigureAnalogOutFromPoints(
      device,
      channel = 1L,
      time_us = c(0, 2, 4),
      voltages = c(0, 2, 100),
      interpolation = "constant",
      offset = -0.5
    ),
    data.frame(
      time_us = c(0, 1, 2, 3),
      voltage = c(0, 0, 2, 2)
    )
  )

  expect_identical(
    mock_state$last_call,
    list(
      channel = 1L,
      voltages = c(0, 0, 2, 2),
      frequency = 250000,
      offset = -0.5,
      .validate_analog_out = FALSE
    )
  )


  #.. Constant interpolation, shorter voltage vector ...........................

  expect_equal(
    ConfigureAnalogOutFromPoints(
      device,
      channel = 1L,
      time_us = c(0, 2, 4),
      voltages = c(0, 1),
      interpolation = "constant"
    ),
    data.frame(
      time_us = c(0, 1, 2, 3),
      voltage = c(0, 0, 1, 1)
    )
  )


  #.. Input validation .........................................................

  mock_state$last_call <- NULL

  expect_error(
    ConfigureAnalogOutFromPoints(
      device,
      channel = 1L,
      time_us = c(1, 2, 4),
      voltages = c(0, 1, 0)
    ),
    "'time_us' must start at 0"
  )

  expect_error(
    ConfigureAnalogOutFromPoints(
      device,
      channel = 1L,
      time_us = c(0, 2, 2),
      voltages = c(0, 1, 0)
    ),
    "'time_us' must be strictly increasing"
  )

  expect_error(
    ConfigureAnalogOutFromPoints(
      device,
      channel = 1L,
      time_us = c(0, 2, 4),
      voltages = c(0, 1)
    ),
    "voltages"
  )

  expect_error(
    ConfigureAnalogOutFromPoints(
      device,
      channel = 1L,
      time_us = c(0, 2, 4),
      voltages = c(0, 1, 0),
      interpolation = "spline"
    ),
    "interpolation"
  )

  expect_error(
    ConfigureAnalogOutFromPoints(
      device,
      channel = 1L,
      time_us = c(0, 2, 4),
      voltages = c(0, 1, 0),
      offset = c(0, 1)
    ),
    "offset"
  )


  #.. Insufficient waveform resolution .........................................

  expect_error(
    ConfigureAnalogOutFromPoints(
      device,
      channel = 1L,
      time_us = c(0, 1.2, 1.8, 4),
      voltages = c(0, 1, 3, 0)
    ),
    "resolution is insufficient"
  )

  expect_null(mock_state$last_call)
})


