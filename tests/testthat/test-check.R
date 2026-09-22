#==[ .AssertDevice() ]==========================================================

test_that(".AssertDevice()", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(
    list(device_handle = 123L)
  )

  expect_null(.AssertDevice(device))
  expect_error(
    .AssertDevice(unclass(device)),
    "must be a 'dwf4r_device'"
  )

  invalid_device <- device
  invalid_device$device_handle <- 0L
  expect_error(
    .AssertDevice(invalid_device),
    "not a valid"
  )

  invalid_device <- device
  invalid_device$device_handle <- 456L
  expect_error(
    .AssertDevice(invalid_device),
    "does not match the registered device"
  )

  invalid_device <- device
  invalid_device$session_id <- 2L
  expect_error(
    .AssertDevice(invalid_device),
    "session that does not exist"
  )

  the_env$devices <- list(NULL)
  expect_error(
    .AssertDevice(device),
    "already been closed"
  )
})



#==[ .AssertAnalogOut() ]=======================================================

test_that(".AssertAnalogOut()", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(
    list(device_handle = 123L)
  )

  local_mocked_bindings(
    .QueryAnalogOutChannelCountC = function(handle) {
      return(2L)
    },
    .QueryAnalogOutChannelNodesC = function(handle, channel) {
      return(c("carrier", "fm", "am"))
    },
    .QueryAnalogOutNodeFunctionTypesC = function(handle, channel, node) {
      return(c("dc", "sine", "square"))
    },
    .package = "dwf4r"
  )

  expect_null(.AssertAnalogOut(device))
  expect_null(.AssertAnalogOut(device, channel = 0L))
  expect_null(.AssertAnalogOut(device, channel = 0L, node = "carrier"))
  expect_null(.AssertAnalogOut(
    device,
    channel = 0L,
    node = "carrier",
    func = "sine"
  ))

  expect_error(
    .AssertAnalogOut(device, channel = 2L),
    "'channel' is out of range"
  )
  expect_error(
    .AssertAnalogOut(device, node = "carrier"),
    "'node' cannot be specified without 'channel'"
  )
  expect_error(
    .AssertAnalogOut(device, channel = 0L, func = "sine"),
    "'func' cannot be specified without 'node'"
  )
  expect_error(
    .AssertAnalogOut(device, channel = 0L, node = "invalid_node"),
    "Node 'invalid_node' is not supported"
  )
  expect_error(
    .AssertAnalogOut(
      device,
      channel = 0L,
      node = "carrier",
      func = "invalid_func"
    ),
    "Function 'invalid_func' is not supported"
  )
})



#==[ .AssertDigitalOut() ]======================================================

test_that(".AssertDigitalOut()", {
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
    .package = "dwf4r"
  )

  expect_null(.AssertDigitalOut(device))
  expect_null(.AssertDigitalOut(device, channel = 0L))
  expect_null(.AssertDigitalOut(device, channel = 15L))
  expect_error(
    .AssertDigitalOut(device, channel = 16L),
    "'channel' is out of range"
  )
  expect_error(
    .AssertDigitalOut(device, channel = -1L),
    "channel"
  )
  expect_error(
    .AssertDigitalOut(device, channel = 1.5),
    "channel"
  )

  expect_null(.AssertDigitalOut(device, channel = 0L, output = "push_pull"))
  expect_error(
    .AssertDigitalOut(device, output = "push_pull"),
    "'output' cannot be specified without 'channel'"
  )
  expect_error(
    .AssertDigitalOut(device, channel = 0L, output = "invalid_output"),
    "Output mode 'invalid_output' is not supported"
  )

  expect_null(.AssertDigitalOut(device, channel = 0L, func = "custom"))
  expect_error(
    .AssertDigitalOut(device, func = "pulse"),
    "'func' cannot be specified without 'channel'"
  )
  expect_error(
    .AssertDigitalOut(device, channel = 0L, func = "invalid_func"),
    "Function 'invalid_func' is not supported"
  )

  expect_null(.AssertDigitalOut(device, channel = 0L, idle = "high"))
  expect_error(
    .AssertDigitalOut(device, idle = "initial"),
    "'idle' cannot be specified without 'channel'"
  )
  expect_error(
    .AssertDigitalOut(device, channel = 0L, idle = "invalid_idle"),
    "Idle mode 'invalid_idle' is not supported"
  )

  # Capabilities are validated independently; relationships between them are
  # intentionally not validated by '.AssertDigitalOut()'.
  expect_null(
    .AssertDigitalOut(
      device,
      channel = 0L,
      output = "three_state",
      func = "pulse",
      idle = "high"
    )
  )
})


