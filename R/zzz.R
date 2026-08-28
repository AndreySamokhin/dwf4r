.CloseRegisteredDevices <- function() {
  close_errors <- character()

  for (session_id in seq_along(the$devices)) {
    registered_device <- the$devices[[session_id]]
    if (is.null(registered_device)) {
      next
    }

    tryCatch(
      {
        .CloseDeviceC(registered_device$device_handle)
        the$devices[session_id] <- list(NULL)
      },
      error = function(e) {
        close_errors <<- c(close_errors, conditionMessage(e))
      }
    )
  }

  if (length(close_errors) > 0L) {
    stop(
      paste(
        "Failed to close one or more WaveForms devices:",
        paste(close_errors, collapse = "; ")
      ),
      call. = FALSE
    )
  }

  return(invisible(NULL))
}



.onUnload <- function(libpath) {
  # The DLL is not unloaded if a registered device cannot be closed, preserving
  # access to the native cleanup code for a potentially still-open device
  # handle.
  .CloseRegisteredDevices()
  library.dynam.unload("dwf4r", libpath)
  return(invisible(NULL))
}


