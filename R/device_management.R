#==============================================================================#
#' Build an SDK enumeration-filter bitmask
#'
#' @description
#'   Convert WaveForms enumeration-filter names to the integer bitmask expected
#'   by \code{.ListDevicesC()}.
#'
#' @param filters
#'   A character vector containing \code{"all"} or one or more of
#'   \code{"usb"}, \code{"network"}, \code{"axi"}, \code{"remote"},
#'   \code{"audio"}, and \code{"demo"}.
#'
#' @return An integer scalar containing the SDK enumeration-filter bitmask.
#'
#' @importFrom checkmate assertCharacter
#' @importFrom checkmate assertSubset
#'
#' @noRd
#==============================================================================#
.BuildFilterBitmask <- function(filters = "all") {

  filter_map <- c(
    usb =     strtoi("0x00000001"),
    network = strtoi("0x00000002"),
    axi =     strtoi("0x00000004"),
    remote =  strtoi("0x01000000"),
    audio =   strtoi("0x02000000"),
    demo =    strtoi("0x04000000")
  )

  checkmate::assertCharacter(filters, min.len = 1L)
  checkmate::assertSubset(filters, choices = c("all", names(filter_map)))

  filters <- unique(filters)
  if (length(filters) == 1L && identical(filters, "all")) {
    return(0L)
  }
  if ("all" %in% filters) {
    stop_msg <- sprintf(
      "'filters' must be either 'all' or one or more of: %s.",
      paste(names(filter_map), collapse = ", ")
    )
    stop(stop_msg, call. = FALSE)
  }

  filter_bitmask <- strtoi("0x08000000")
  for (type in filters) {
    filter_bitmask <- bitwOr(filter_bitmask, filter_map[[type]])
  }
  return(filter_bitmask)
}



#==============================================================================#
#' List detected devices
#'
#' @description
#'   Enumerate detected devices using the filters specified by
#'   \code{filters} and retrieve some additional information for each
#'   device.
#'
#' @param filters
#'   A character vector specifying one or more enumeration filters. The default,
#'   \code{"all"}, corresponds to the SDK's default enumeration filter. Other
#'   supported values are \code{"usb"}, \code{"network"}, \code{"axi"},
#'   \code{"remote"}, \code{"audio"}, and \code{"demo"}.
#'
#' @return
#'   A data frame with one row per enumerated device and the following columns:
#'   \describe{
#'     \item{\code{device_index}}{
#'       Zero-based index of the device in the current SDK enumeration result.
#'     }
#'     \item{\code{device_id}}{
#'       Integer device-type identifier defined by the SDK.
#'     }
#'     \item{\code{device_type}}{
#'       Character label mapped from \code{device_id}; unknown SDK identifiers
#'       are reported as \code{"Unknown"}.
#'     }
#'     \item{\code{device_revision}}{
#'       Integer device-revision identifier.
#'     }
#'     \item{\code{device_name}}{
#'       Character string containing the device name.
#'     }
#'     \item{\code{user_name}}{
#'       Character string containing the user-assigned device name.
#'     }
#'     \item{\code{serial_number}}{
#'       Character string containing the unique device serial number.
#'     }
#'     \item{\code{is_opened}}{
#'       Logical value indicating whether the device is already opened by the
#'       current or another process.
#'     }
#'     \item{\code{config_count}}{
#'       Integer number of available configurations for the device.
#'     }
#'   }
#'
#' @examples
#'   \dontrun{
#'     devices <- ListDevices()
#'     print(devices)
#'
#'     usb_devices <- ListDevices(filters = "usb")
#'     print(usb_devices)
#'   }
#'
#' @export
#==============================================================================#
ListDevices <- function(filters = "all") {
  return(.ListDevicesC(.BuildFilterBitmask(filters)))
}



#==============================================================================#
#' Open a device
#'
#' @description
#'   Open a WaveForms device, register the connection in the current R session,
#'   and return an object that can be passed to other \pkg{dwf4r} functions.
#'
#' @details
#'   The device is opened using its default configuration. Automatic
#'   configuration in the WaveForms SDK is disabled to reduce the communication
#'   overhead of individual setting calls. Instrument settings are therefore
#'   applied explicitly by the corresponding \pkg{dwf4r} configuration
#'   functions. This behavior is managed internally and is not currently
#'   user-configurable.
#'
#' @param serial_number
#'   A string containing the serial number of the device to open. If
#'   \code{NULL}, the device is selected automatically when exactly one
#'   compatible device is detected.
#'
#' @return
#'   An opaque object of class \code{"dwf4r_device"} representing the opened
#'   connection. Pass this object unchanged to other \pkg{dwf4r} functions.
#'
#' @examples
#'   \dontrun{
#'     devices <- ListDevices()
#'
#'     if (nrow(devices) >= 1L) {
#'       if (nrow(devices) == 1L) {
#'         device <- OpenDevice()
#'       } else {
#'         device_sn <- devices$serial_number[[1L]]
#'         device <- OpenDevice(device_sn)
#'       }
#'
#'       print(device)
#'
#'       CloseDevice(device)
#'     }
#'   }
#'
#' @importFrom checkmate assertString
#'
#' @export
#==============================================================================#
OpenDevice <- function(serial_number = NULL) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertString(serial_number, null.ok = TRUE, min.chars = 1L)


  #--[ Find device in the list ]------------------------------------------------

  devices <- ListDevices(filters = "all")
  if (nrow(devices) == 0L) {
    stop("No devices were found.", call. = FALSE)
  }

  if (is.null(serial_number)) {
    if (nrow(devices) > 1L) {
      stop("Multiple devices were found; specify 'serial_number'.",
           call. = FALSE)
    }
    device_row <- 1L

  } else { # i.e., '!is.null(serial_number)'
    # TODO: SDK provides a dedicated function (i.e., 'FDwfDeviceOpenEx()') for
    # serial-number opening.
    device_row <- match(serial_number, devices$serial_number)
    if (is.na(device_row)) {
      stop_msg <-
        sprintf("No device found with serial number '%s'.", serial_number)
      stop(stop_msg, call. = FALSE)
    }
  }
  device_index <- devices$device_index[[device_row]]


  #--[ Open device ]------------------------------------------------------------

  if (devices$is_opened[[device_row]]) {
    stop_msg <- sprintf(
      "Device '%s' is already open.",
      devices$serial_number[[device_row]]
    )
    stop(stop_msg, call. = FALSE)
  }
  config_index <- -1L # the default configuration
  device_handle <- .OpenDeviceC(as.integer(device_index), config_index)

  initialization_complete <- FALSE
  registered_session_id <- NULL
  on.exit({
    if (!initialization_complete) {
      tryCatch(
        {
          .CloseDeviceC(device_handle)
          if (!is.null(registered_session_id)) {
            the$devices[registered_session_id] <- list(NULL)
          }
        },
        error = function(e) {
          warning(
            "Failed to close the device after an initialization error: ",
            conditionMessage(e), "\n",
            "Call 'CloseAllDevices()' to force-close all WaveForms device ",
            "connections opened by the current R process.",
            call. = FALSE
          )
        }
      )
    }
  }, add = TRUE)


  #--[ Register opened handle ]-------------------------------------------------

  session_id <- length(the$devices) + 1L
  the$devices[[session_id]] <- list(
    device_handle = device_handle,
    serial_number = devices$serial_number[[device_row]]
  )
  registered_session_id <- session_id


  #--[ Initialize device ]------------------------------------------------------

  auto_configure <- FALSE
  .DeviceAutoConfigureSetC(
    handle = device_handle,
    auto_configure = auto_configure
  )


  #--[ Create S3 object ]-------------------------------------------------------

  session_id <- length(the$devices)

  df_col_names <- c(
    "device_type",
    "device_revision",
    "device_name",
    "user_name",
    "serial_number",
    "config_count"
  )
  device <- structure(
    list(
      device_handle = device_handle,
      session_id = registered_session_id,
      info = as.list(devices[device_row, df_col_names])
    ),
    class = "dwf4r_device"
  )

  initialization_complete <- TRUE
  return(device)
}



#==============================================================================#
#' Close a device
#'
#' @description
#'   Close an active WaveForms device connection and mark the corresponding
#'   session as closed. After this function returns, the \code{device} object
#'   and any copies of it are stale and cannot be used in subsequent \pkg{dwf4r}
#'   calls.
#'
#' @template arg_device
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @examples
#'   \dontrun{
#'     devices <- ListDevices()
#'     if (nrow(devices) >= 1L) {
#'       device_sn <- devices$serial_number[[1L]]
#'       device <- OpenDevice(device_sn)
#'       CloseDevice(device)
#'     }
#'   }
#'
#' @export
#==============================================================================#
CloseDevice <- function(device) {
  .AssertDevice(device)
  .CloseDeviceC(device$device_handle)
  the$devices[device$session_id] <- list(NULL)
  return(invisible(NULL))
}



#==============================================================================#
#' Close all WaveForms devices opened by the current process
#'
#' @description
#'   Close all WaveForms device connections opened by the current R process.
#'   This operation is process-wide and is not limited to connections opened
#'   through \pkg{dwf4r}. It may therefore also close connections created by
#'   other packages or by direct use of the WaveForms SDK in the same R process.
#'   Connections owned by other processes are not affected.
#'
#'   After this function returns, all existing \code{"dwf4r_device"} objects
#'   and their copies are stale and cannot be used in subsequent \pkg{dwf4r}
#'   calls. Any WaveForms handles created by other code in the current R
#'   process are also invalidated.
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @examples
#'   \dontrun{
#'     devices <- ListDevices()
#'     if (nrow(devices) >= 1L) {
#'       device_sn <- devices$serial_number[[1L]]
#'       device <- OpenDevice(device_sn)
#'       CloseAllDevices()
#'     }
#'   }
#'
#' @export
#==============================================================================#
CloseAllDevices <- function() {
  .CloseAllDevicesC()
  the$devices <- rep(list(NULL), length(the$devices))
  return(invisible(NULL))
}


