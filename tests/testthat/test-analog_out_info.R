#==[ Mock-based tests ]=========================================================

test_that("Analog Out information queries", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(list(device_handle = 123L))

  local_mocked_bindings(
    .QueryAnalogOutChannelCountC = function(handle) {
      return(2L)
    },
    .QueryAnalogOutChannelNodesC = function(handle, channel) {
      return(c("carrier", "fm", "am"))
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
    .QueryAnalogOutIdleModesC = function(handle, channel) {
      return(c("offset", "initial", "hold"))
    },
    .QueryAnalogOutNodeFunctionTypesC = function(handle, channel, node) {
      return(c("dc", "sine", "square"))
    },
    .QueryAnalogOutNodeFrequencyRangeC = function(handle, channel, node) {
      return(c(1e-6, 1e8))
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
    .package = "dwf4r"
  )

  expect_identical(GetAnalogOutChannelCount(device), 2L)
  expect_identical(
    GetAnalogOutChannelNodes(device, 0L),
    c("carrier", "fm", "am")
  )
  expect_error(
    GetAnalogOutChannelNodes(device, 2L),
    "'channel' is out of range"
  )
  expect_identical(
    GetAnalogOutRunRange(device, 0L),
    c(0, 86400)
  )
  expect_identical(
    GetAnalogOutWaitRange(device, 0L),
    c(0, 86400)
  )
  expect_identical(
    GetAnalogOutRepeatRange(device, 0L),
    c(0L, 32000L)
  )
  expect_identical(
    GetAnalogOutIdleModes(device, 0L),
    c("offset", "initial", "hold")
  )
  expect_identical(
    GetAnalogOutNodeFunctionTypes(device, 0L, "carrier"),
    c("dc", "sine", "square")
  )
  expect_error(
    GetAnalogOutNodeFunctionTypes(device, 0L, "invalid_node"),
    "Node 'invalid_node' is not supported"
  )
  expect_identical(
    GetAnalogOutNodeFrequencyRange(device, 0L, "carrier"),
    c(1e-6, 1e8)
  )
  expect_identical(
    GetAnalogOutNodeAmplitudeRange(device, 0L, "carrier"),
    c(0.01, 5)
  )
  expect_identical(
    GetAnalogOutNodeOffsetRange(device, 0L, "carrier"),
    c(-5, 5)
  )
  expect_identical(
    GetAnalogOutNodeSymmetryRange(device, 0L, "carrier"),
    c(0, 100)
  )
  expect_identical(
    GetAnalogOutNodePhaseRange(device, 0L, "carrier"),
    c(0, 360)
  )
  expect_identical(
    GetAnalogOutNodeSampleCountRange(device, 0L, "carrier"),
    c(1L, 4096L)
  )
})


test_that("Analog Out information queries, SDK failure", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(list(device_handle = 123L))

  local_mocked_bindings(
    .QueryAnalogOutChannelCountC = function(handle) {
      return(2L)
    },
    .QueryAnalogOutRunRangeC = function(handle, channel) {
      stop("DWF run range error.", call. = FALSE)
    },
    .package = "dwf4r"
  )

  expect_error(
    GetAnalogOutRunRange(device, 0L),
    "DWF run range error"
  )
})



#==[ Hardware tests ]===========================================================

test_that("Hardware: Analog Out information", {
  device <- .OpenHardwareTestDevice()
  channel <- 0L
  on.exit(try(CloseDevice(device), silent = TRUE), add = TRUE)

  skip_if(
    GetAnalogOutChannelCount(device) == 0L,
    "Configured device has no Analog Out channels."
  )
  skip_if(
    !("carrier" %in% GetAnalogOutChannelNodes(device, channel)),
    "Carrier node is not supported."
  )

  nodes <- GetAnalogOutChannelNodes(device, channel)
  expect_type(nodes, "character")
  expect_gt(length(nodes), 0L)

  idle_modes <- GetAnalogOutIdleModes(device, channel)
  expect_type(idle_modes, "character")
  expect_gt(length(idle_modes), 0L)

  node <- nodes[[1L]]
  functions <- GetAnalogOutNodeFunctionTypes(device, channel, node)
  expect_type(functions, "character")
  expect_gt(length(functions), 0L)

  ranges <- list(
    GetAnalogOutRunRange(device, channel),
    GetAnalogOutWaitRange(device, channel),
    GetAnalogOutRepeatRange(device, channel),
    GetAnalogOutNodeFrequencyRange(device, channel, node),
    GetAnalogOutNodeAmplitudeRange(device, channel, node),
    GetAnalogOutNodeOffsetRange(device, channel, node),
    GetAnalogOutNodeSymmetryRange(device, channel, node),
    GetAnalogOutNodePhaseRange(device, channel, node),
    GetAnalogOutNodeSampleCountRange(device, channel, node)
  )

  for (range in ranges) {
    expect_length(range, 2L)
    expect_lte(range[[1L]], range[[2L]])
  }
})


