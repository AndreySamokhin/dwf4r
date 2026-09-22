#==[ Mock-based tests ]=========================================================

test_that("Digital Out information queries", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(list(device_handle = 123L))

  local_mocked_bindings(
    .QueryDigitalOutChannelCountC = function(handle) {
      return(16L)
    },
    .QueryDigitalOutInternalClockFrequencyC = function(handle) {
      return(1e08)
    },
    .QueryDigitalOutRunRangeC = function(handle) {
      return(c(2.00e-07, 8.64e+04))
    },
    .QueryDigitalOutWaitRangeC = function(handle) {
      return(c(2.00e-07, 8.64e+04))
    },
    .QueryDigitalOutRepeatRangeC = function(handle) {
      return(c(0, 32768))
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
    .QueryDigitalOutDividerRangeC = function(handle, channel) {
      return(c(1, 2147483649))
    },
    .QueryDigitalOutCounterRangeC = function(handle, channel) {
      return(c(1, 32768))
    },
    .QueryDigitalOutMaxDataBitsC = function(handle, channel) {
      return(1024)
    },
    .package = "dwf4r"
  )

  expect_identical(
    GetDigitalOutChannelCount(device),
    16L
  )

  expect_identical(
    GetDigitalOutInternalClockFrequency(device),
    1e08
  )

  expect_identical(
    GetDigitalOutRunRange(device),
    c(2.00e-07, 8.64e+04)
  )

  expect_identical(
    GetDigitalOutWaitRange(device),
    c(2.00e-07, 8.64e+04)
  )

  expect_identical(
    GetDigitalOutRepeatRange(device),
    c(0L, 32768L)
  )

  expect_identical(
    GetDigitalOutOutputModes(device, 0L),
    c("push_pull", "open_drain", "three_state")
  )

  expect_identical(
    GetDigitalOutFunctionTypes(device, 0L),
    c("pulse", "custom", "random", "play")
  )

  expect_identical(
    GetDigitalOutIdleModes(device, 0L),
    c("initial", "high", "three_state")
  )

  # The upper limit exceeds the maximum R integer value, so the entire range
  # remains numeric.
  expect_identical(
    GetDigitalOutDividerRange(device, 0L),
    c(1, 2147483649)
  )

  expect_identical(
    GetDigitalOutCounterRange(device, 0L),
    c(1L, 32768L)
  )

  expect_identical(
    GetDigitalOutMaxDataBits(device, 0L),
    1024L
  )

  expect_error(
    GetDigitalOutOutputModes(device, 16L),
    "'channel' is out of range"
  )
})


test_that("Digital Out information queries, SDK failure", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(list(device_handle = 123L))

  local_mocked_bindings(
    .QueryDigitalOutChannelCountC = function(handle) {
      return(16L)
    },
    .QueryDigitalOutOutputMaskC = function(handle, channel) {
      stop("DWF output mode query error.", call. = FALSE)
    },
    .package = "dwf4r"
  )

  expect_error(
    GetDigitalOutOutputModes(device, 0L),
    "DWF output mode query error"
  )
})



#==[ Hardware tests ]===========================================================

test_that("Hardware: Digital Out information", {
  device <- .OpenHardwareTestDevice()
  channel <- 0L
  on.exit(try(CloseDevice(device), silent = TRUE), add = TRUE)

  skip_if(
    GetDigitalOutChannelCount(device) == 0L,
    "Configured device has no Digital Out channels."
  )

  internal_clock <- GetDigitalOutInternalClockFrequency(device)
  expect_type(internal_clock, "double")
  expect_gt(internal_clock, 0)

  output_modes <- GetDigitalOutOutputModes(device, channel)
  expect_type(output_modes, "character")
  expect_gt(length(output_modes), 0L)

  types <- GetDigitalOutFunctionTypes(device, channel)
  expect_type(types, "character")
  expect_gt(length(types), 0L)

  idle_modes <- GetDigitalOutIdleModes(device, channel)
  expect_type(idle_modes, "character")
  expect_gt(length(idle_modes), 0L)

  ranges <- list(
    GetDigitalOutRunRange(device),
    GetDigitalOutWaitRange(device),
    GetDigitalOutRepeatRange(device),
    GetDigitalOutDividerRange(device, channel),
    GetDigitalOutCounterRange(device, channel)
  )

  for (range in ranges) {
    expect_length(range, 2L)
    expect_lte(range[[1L]], range[[2L]])
  }

  max_data_bits <- GetDigitalOutMaxDataBits(device, channel)
  expect_length(max_data_bits, 1L)
  expect_true(is.numeric(max_data_bits))
  expect_gte(max_data_bits, 0)
})


