.MakeDeviceTable <- function(n_devices) {
  data.frame(
    device_index = seq_len(n_devices) - 1L,
    device_id = seq_len(n_devices),
    device_type = rep("Analog Discovery 2", n_devices),
    device_revision = rep(1L, n_devices),
    device_name = rep("Analog Discovery 2", n_devices),
    user_name = rep("Discovery2", n_devices),
    serial_number = sprintf("SN:ABCD%05d", seq_len(n_devices)),
    is_opened = rep(FALSE, n_devices),
    config_count = rep(8L, n_devices),
    stringsAsFactors = FALSE
  )
}


.MakeDeviceObject <- function() {
  structure(
    list(
      device_handle = 123L,
      session_id = 1L,
      info = list(
        serial_number = "SN:ABCD00001"
      )
    ),
    class = "dwf4r_device"
  )
}


