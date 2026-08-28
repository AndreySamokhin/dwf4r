#==[ Mock-based tests ]=========================================================

test_that("Device triggering", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  device <- .MakeDeviceObject()
  the_env$devices <- list(list(device_handle = 123L))

  mock_state <- new.env(parent = emptyenv())
  mock_state$last_call <- list()

  local_mocked_bindings(
    .QueryDeviceTriggerSourceMaskC = function(handle) {
      return(2179L)
    },
    .QueryDeviceTriggerSlopeMaskC = function(handle) {
      return(5L)
    },
    .DeviceTriggerPcC = function(handle) {
      mock_state$last_call <- list(
        call_type = "trigger_pc",
        handle = handle
      )
      return(invisible(NULL))
    },
    .package = "dwf4r"
  )


  #.. GetDeviceTriggerSources() ................................................

  expect_identical(
    GetDeviceTriggerSources(device),
    c("none", "pc", "analog_out_1", "external_1")
  )


  #.. GetDeviceTriggerSlopes() .................................................

  expect_identical(
    GetDeviceTriggerSlopes(device),
    c("rising", "either")
  )


  #.. TriggerDevice() ..........................................................

  expect_null(TriggerDevice(device))
  expect_identical(
    mock_state$last_call,
    list(
      call_type = "trigger_pc",
      handle = 123L
    )
  )
})



#==[ Hardware tests ]===========================================================

test_that("Hardware: device trigger capabilities", {
  testthat::skip_if(
    !identical(Sys.getenv("DWF4R_TEST_TRIGGER"), "true"),
    "Set 'DWF4R_TEST_TRIGGER=true' to run trigger hardware tests."
  )

  device <- .OpenHardwareTestDevice()
  on.exit(try(CloseDevice(device), silent = TRUE), add = TRUE)

  sources <- GetDeviceTriggerSources(device)
  expect_type(sources, "character")
  expect_gt(length(sources), 0L)

  slopes <- GetDeviceTriggerSlopes(device)
  expect_type(slopes, "character")
  expect_gt(length(slopes), 0L)
})


