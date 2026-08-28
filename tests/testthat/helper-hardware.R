.OpenHardwareTestDevice <- function() {
  serial_number <- Sys.getenv("DWF4R_TEST_DEVICE_SN", unset = "")

  skip_if(
    !nzchar(serial_number),
    "Set 'DWF4R_TEST_DEVICE_SN' to run hardware tests."
  )

  devices <- ListDevices(filters = "all")
  device_row <- match(serial_number, devices$serial_number)

  skip_if(
    is.na(device_row),
    sprintf("Configured device '%s' was not found.", serial_number)
  )

  skip_if(
    devices$is_opened[[device_row]],
    sprintf("Configured device '%s' is already open.", serial_number)
  )

  device <- OpenDevice(serial_number)
  return(device)
}


