#==[ .BuildFilterBitmask() ]====================================================

test_that(".BuildFilterBitmask(): expected bitmasks", {
  expect_identical(.BuildFilterBitmask(), 0L)
  expect_identical(.BuildFilterBitmask("all"), 0L)
  expect_identical(.BuildFilterBitmask("usb"), strtoi("0x08000001"))
  expect_identical(
    .BuildFilterBitmask(c("usb", "network")),
    strtoi("0x08000003")
  )
  expect_identical(
    .BuildFilterBitmask(c("usb", "usb")),
    strtoi("0x08000001")
  )
})


test_that(".BuildFilterBitmask(), invalid filters", {
  expect_error(.BuildFilterBitmask(character()))
  expect_error(.BuildFilterBitmask("invalid"))
  expect_error(
    .BuildFilterBitmask(c("all", "usb")),
    "must be either 'all'"
  )
})



#==[ ListDevices() ]============================================================

test_that("ListDevices()", {
  expected_list <- .MakeDeviceTable(n_devices = 2L)
  expected_filters <- NULL

  local_mocked_bindings(
    .ListDevicesC = function(filter) {
      expected_filters <<- filter
      return(expected_list)
    },
    .package = "dwf4r"
  )

  actual_list <- ListDevices(filters = c("usb", "network"))

  expect_identical(expected_filters, strtoi("0x08000003"))
  expect_identical(actual_list, expected_list)
})



#==[ OpenDevice() ]=============================================================

test_that("OpenDevice(): automatic selection of a single device", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)
  the_env$devices <- list()

  mock_state <- new.env(parent = emptyenv())
  mock_state$last_call <- list()

  RegisterNewCall <- function(call) {
    call_index <- length(mock_state$calls) + 1L
    mock_state$calls[[call_index]] <- call
    return(invisible(NULL))
  }

  local_mocked_bindings(
    ListDevices = function(filters = "all") {
      return(.MakeDeviceTable(n_devices = 1L))
    },
    .OpenDeviceC = function(device_index, config_index) {
      RegisterNewCall(list(
        call_type = "open_device",
        device_index = device_index,
        config_index = config_index
      ))
      return(123L)
    },
    .DeviceAutoConfigureSetC = function(handle, auto_configure) {
      RegisterNewCall(list(
        call_type = "set_auto_configure",
        handle = handle,
        auto_configure = auto_configure
      ))
      return(invisible(NULL))
    },
    .package = "dwf4r"
  )

  device <- OpenDevice()

  expect_s3_class(device, "dwf4r_device")
  expect_identical(device$device_handle, 123L)
  expect_identical(device$session_id, 1L)
  expect_identical(device$info$serial_number, "SN:ABCD00001")

  expect_identical(
    mock_state$calls,
    list(
      list(
        call_type = "open_device",
        device_index = 0L,
        config_index = -1L
      ),
      list(
        call_type = "set_auto_configure",
        handle = 123L,
        auto_configure = FALSE
      )
    )
  )

  expect_length(the_env$devices, 1L)
  expect_identical(the_env$devices[[1L]]$device_handle, 123L)
  expect_identical(the_env$devices[[1L]]$serial_number, "SN:ABCD00001")
})


test_that("OpenDevice(): selection of a device by serial number", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)
  the_env$devices <- list()

  local_mocked_bindings(
    ListDevices = function(filters = "all") {
      return(.MakeDeviceTable(n_devices = 2L))
    },
    .OpenDeviceC = function(device_index, config_index) {
      return(123L)
    },
    .DeviceAutoConfigureSetC = function(handle, auto_configure) {
      return(invisible(NULL))
    },
    .package = "dwf4r"
  )

  device <- OpenDevice(serial_number = "SN:ABCD00002")
  expect_identical(device$device_handle, 123L)
  expect_identical(device$info$serial_number, "SN:ABCD00002")
})


test_that("OpenDevice(), invalid device selection", {
  local_mocked_bindings(
    ListDevices = function(filters = "all") {
      return(.MakeDeviceTable(n_devices = 0L))
    },
    .package = "dwf4r"
  )
  expect_error(OpenDevice(), "No devices were found")

  local_mocked_bindings(
    ListDevices = function(filters = "all") {
      return(.MakeDeviceTable(n_devices = 2L))
    },
    .package = "dwf4r"
  )
  expect_error(OpenDevice(), "Multiple devices were found")

  expect_error(OpenDevice(serial_number = "SN:INVALID"), "No device found")
})


test_that("OpenDevice(): failed initialization closes opened handle", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)
  the_env$devices <- list()

  mock_state <- new.env(parent = emptyenv())

  local_mocked_bindings(
    ListDevices = function(filters = "all") {
      return(.MakeDeviceTable(n_devices = 1L))
    },
    .OpenDeviceC = function(device_index, config_index) {
      return(123L)
    },
    .DeviceAutoConfigureSetC = function(handle, auto_configure) {
      stop("DWF auto-configure error.", call. = FALSE)
    },
    .CloseDeviceC = function(handle) {
      mock_state$closed_handle <- handle
      return(invisible(NULL))
    },
    .package = "dwf4r"
  )

  expect_error(
    OpenDevice(),
    "DWF auto-configure error"
  )
  expect_identical(mock_state$closed_handle, 123L)
  expect_identical(the_env$devices, list(NULL))
})


test_that("OpenDevice(): failed initialization retains unclosed handle", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)
  the_env$devices <- list()

  local_mocked_bindings(
    ListDevices = function(filters = "all") {
      return(.MakeDeviceTable(n_devices = 1L))
    },
    .OpenDeviceC = function(device_index, config_index) {
      return(123L)
    },
    .DeviceAutoConfigureSetC = function(handle, auto_configure) {
      stop("DWF auto-configure error.", call. = FALSE)
    },
    .CloseDeviceC = function(handle) {
      stop("DWF close error.", call. = FALSE)
    },
    .package = "dwf4r"
  )

  expect_warning(
    expect_error(
      OpenDevice(),
      "DWF auto-configure error"
    ),
    "Failed to close the device after an initialization error"
  )

  expect_identical(
    the_env$devices[[1L]],
    list(
      device_handle = 123L,
      serial_number = "SN:ABCD00001"
    )
  )
})


test_that("OpenDevice(): SDK error", {
  local_mocked_bindings(
    ListDevices = function(filters = "all") {
      return(.MakeDeviceTable(n_devices = 1L))
    },
    .OpenDeviceC = function(device_index, config_index) {
      stop("DWF error.", call. = FALSE)
    },
    .package = "dwf4r"
  )
  expect_error(OpenDevice(), "DWF error")
})



#==[ CloseDevice() ]============================================================

test_that("CloseDevice(): device closure", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)
  the_env$devices <- list(
    list(
      device_handle = 123L,
      serial_number = "SN:ABCD00001"
    )
  )
  device <- .MakeDeviceObject()

  mock_state <- new.env(parent = emptyenv())
  mock_state$last_call <- list()

  local_mocked_bindings(
    .CloseDeviceC = function(handle) {
      mock_state$last_call <- list(handle = handle)
      return(invisible(NULL))
    },
    .package = "dwf4r"
  )

  expect_null(CloseDevice(device))
  expect_identical(mock_state$last_call, list(handle = 123L))
  expect_null(the_env$devices[[1L]])
  expect_error(
    CloseDevice(device),
    "'device' refers to a session that has already been closed"
  )
})


test_that("CloseDevice(): SDK error", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)
  registered_device <- list(
    device_handle = 123L,
    serial_number = "SN:ABCD00001"
  )
  the_env$devices <- list(registered_device)
  device <- .MakeDeviceObject()

  local_mocked_bindings(
    .CloseDeviceC = function(handle) {
      stop("DWF error.", call. = FALSE)
    },
    .package = "dwf4r"
  )

  expect_error(CloseDevice(device), "DWF error")
  expect_identical(the_env$devices[[1L]], registered_device)
})



#==[ CloseAllDevices() ]========================================================

test_that("CloseAllDevices(): device-registry reset", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)
  the_env$devices <- list(
    list(device_handle = 123L),
    list(device_handle = 456L)
  )

  local_mocked_bindings(
    .CloseAllDevicesC = function() {
      return(invisible(NULL))
    },
    .package = "dwf4r"
  )

  expect_null(CloseAllDevices())
  expect_identical(the_env$devices, rep(list(NULL), 2L))
})


test_that("CloseAllDevices(): SDK error", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)
  registered_devices <- list(
    list(device_handle = 123L),
    list(device_handle = 456L)
  )
  the_env$devices <- registered_devices

  local_mocked_bindings(
    .CloseAllDevicesC = function() {
      stop("DWF error.", call. = FALSE)
    },
    .package = "dwf4r"
  )

  expect_error(CloseAllDevices(), "DWF error")
  expect_identical(the_env$devices, registered_devices)
})



#==[ .CloseRegisteredDevices() ]================================================

test_that(".CloseRegisteredDevices(): successful cleanup", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  the_env$devices <- list(
    list(device_handle = 123L),
    NULL,
    list(device_handle = 456L)
  )

  mock_state <- new.env(parent = emptyenv())
  mock_state$closed_handles <- integer()

  local_mocked_bindings(
    .CloseDeviceC = function(handle) {
      mock_state$closed_handles <- c(mock_state$closed_handles, handle)
      return(invisible(NULL))
    },
    .package = "dwf4r"
  )

  expect_null(.CloseRegisteredDevices())
  expect_identical(
    mock_state$closed_handles,
    c(123L, 456L)
  )
  expect_identical(
    the_env$devices,
    rep(list(NULL), 3L)
  )
})


test_that(".CloseRegisteredDevices(): partial cleanup failure", {
  the_env <- getFromNamespace("the", "dwf4r")
  old_devices <- the_env$devices
  on.exit(the_env$devices <- old_devices, add = TRUE)

  registered_devices <- list(
    list(device_handle = 123L),
    list(device_handle = 456L),
    list(device_handle = 789L)
  )
  the_env$devices <- registered_devices

  mock_state <- new.env(parent = emptyenv())
  mock_state$closed_handles <- integer()

  local_mocked_bindings(
    .CloseDeviceC = function(handle) {
      mock_state$closed_handles <- c(mock_state$closed_handles, handle)
      if (handle %in% c(123L, 789L)) {
        stop(sprintf("DWF close error %d.", handle), call. = FALSE)
      }
      return(invisible(NULL))
    },
    .package = "dwf4r"
  )

  expect_error(
    .CloseRegisteredDevices(),
    paste(
      "Failed to close one or more WaveForms devices:",
      "DWF close error 123.;",
      "DWF close error 789."
    )
  )
  expect_identical(
    mock_state$closed_handles,
    c(123L, 456L, 789L)
  )
  expect_identical(
    the_env$devices,
    list(
      registered_devices[[1L]],
      NULL,
      registered_devices[[3L]]
    )
  )
})



#==[ Hardware tests ]===========================================================

test_that("Hardware: device management", {
  device <- .OpenHardwareTestDevice()
  on.exit(try(CloseDevice(device), silent = TRUE), add = TRUE)

  expect_s3_class(device, "dwf4r_device")

  devices <- ListDevices()
  device_row <- match(device$info$serial_number, devices$serial_number)
  expect_false(is.na(device_row))
  expect_true(devices$is_opened[[device_row]])

  expect_null(CloseDevice(device))
  devices <- ListDevices()
  device_row <- match(device$info$serial_number, devices$serial_number)
  expect_false(devices$is_opened[[device_row]])
})


