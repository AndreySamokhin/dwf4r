#==============================================================================#
#' Configure an Analog Out constant voltage
#'
#' @description
#'   Configure a selected Analog Out channel to generate a constant voltage.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @param voltage
#'   A numeric scalar specifying the output voltage, in volts.
#' @template arg_validate_analog_out
#'
#' @template details_analog_out_carrier_enabled
#' @template details_analog_out_pending_configuration
#' @template details_analog_out_actual_values
#' @template seealso_analog_out_vignette
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#'   # Generate a constant 1 V output for one second.
#'   device <- OpenDevice()
#'   ResetAnalogOut(device, channel = 0L)
#'   ConfigureAnalogOutConstantVoltage(
#'     device,
#'     channel = 0L,
#'     voltage = 1
#'   )
#'   StartAnalogOut(device, channel = 0L)
#'   Sys.sleep(1)
#'   StopAnalogOut(device, channel = 0L)
#'   CloseDevice(device)
#' }
#'
#' @export
#==============================================================================#
ConfigureAnalogOutConstantVoltage <- function(
    device,
    channel,
    voltage,
    .validate_analog_out = TRUE
) {
  SetAnalogOutCarrier(
    device = device,
    channel = channel,
    func = "dc",
    offset = voltage,
    .validate_analog_out = .validate_analog_out
  )
  EnableAnalogOutCarrier(
    device = device,
    channel = channel,
    .validate_analog_out = FALSE
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Configure an Analog Out sine waveform
#'
#' @description
#'   Configure a selected Analog Out channel to generate a sine waveform.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @template arg_frequency
#' @template arg_waveform_amplitude
#' @template arg_waveform_offset
#' @template arg_waveform_phase
#' @template arg_validate_analog_out
#'
#' @details
#'   The carrier symmetry is set to 50 percent.
#' @template details_analog_out_carrier_enabled
#' @template details_analog_out_pending_configuration
#' @template details_analog_out_actual_values
#' @template seealso_analog_out_vignette
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#'   # Generate a 1 kHz, 2 V peak-to-peak sine wave for one second.
#'   device <- OpenDevice()
#'   ResetAnalogOut(device, channel = 0L)
#'   ConfigureAnalogOutSine(
#'     device,
#'     channel = 0L,
#'     frequency = 1000,
#'     amplitude = 1
#'   )
#'   StartAnalogOut(device, channel = 0L)
#'   Sys.sleep(1)
#'   StopAnalogOut(device, channel = 0L)
#'   CloseDevice(device)
#' }
#'
#' @export
#==============================================================================#
ConfigureAnalogOutSine <- function(
    device,
    channel,
    frequency,
    amplitude,
    offset = 0,
    phase = 0,
    .validate_analog_out = TRUE
) {
  SetAnalogOutCarrier(
    device = device,
    channel = channel,
    func = "sine",
    frequency = frequency,
    amplitude = amplitude,
    offset = offset,
    symmetry = 50,
    phase = phase,
    .validate_analog_out = .validate_analog_out
  )
  EnableAnalogOutCarrier(
    device = device,
    channel = channel,
    .validate_analog_out = FALSE
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Configure an Analog Out square waveform
#'
#' @description
#'   Configure a selected Analog Out channel to generate a square waveform.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @template arg_frequency
#' @template arg_waveform_amplitude
#' @template arg_waveform_offset
#' @param duty_cycle
#'   A numeric scalar specifying the waveform duty cycle, in percent. A value of
#'   50 produces a symmetric square waveform.
#' @template arg_waveform_phase
#' @template arg_validate_analog_out
#'
#' @template details_analog_out_carrier_enabled
#' @template details_analog_out_pending_configuration
#' @template details_analog_out_actual_values
#' @template seealso_analog_out_vignette
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#'   # Generate a 1 kHz, 2 V peak-to-peak square wave with a 25% duty cycle for
#'   # one second.
#'   device <- OpenDevice()
#'   ResetAnalogOut(device, channel = 0L)
#'   ConfigureAnalogOutSquare(
#'     device,
#'     channel = 0L,
#'     frequency = 1000,
#'     amplitude = 1,
#'     duty_cycle = 25
#'   )
#'   StartAnalogOut(device, channel = 0L)
#'   Sys.sleep(1)
#'   StopAnalogOut(device, channel = 0L)
#'   CloseDevice(device)
#' }
#'
#' @export
#==============================================================================#
ConfigureAnalogOutSquare <- function(
    device,
    channel,
    frequency,
    amplitude,
    offset = 0,
    duty_cycle = 50,
    phase = 0,
    .validate_analog_out = TRUE
) {
  SetAnalogOutCarrier(
    device = device,
    channel = channel,
    func = "square",
    frequency = frequency,
    amplitude = amplitude,
    offset = offset,
    symmetry = duty_cycle,
    phase = phase,
    .validate_analog_out = .validate_analog_out
  )
  EnableAnalogOutCarrier(
    device = device,
    channel = channel,
    .validate_analog_out = FALSE
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Configure an Analog Out triangle waveform
#'
#' @description
#'   Configure a selected Analog Out channel to generate a triangle waveform.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @template arg_frequency
#' @template arg_waveform_amplitude
#' @template arg_waveform_offset
#' @template arg_waveform_phase
#' @template arg_validate_analog_out
#'
#' @details
#'   The carrier symmetry is set to 50 percent.
#' @template details_analog_out_carrier_enabled
#' @template details_analog_out_pending_configuration
#' @template details_analog_out_actual_values
#' @template seealso_analog_out_vignette
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#'   # Generate a 1 kHz, 2 V peak-to-peak triangle wave for one second.
#'   device <- OpenDevice()
#'   ResetAnalogOut(device, channel = 0L)
#'   ConfigureAnalogOutTriangle(
#'     device,
#'     channel = 0L,
#'     frequency = 1000,
#'     amplitude = 1
#'   )
#'   StartAnalogOut(device, channel = 0L)
#'   Sys.sleep(1)
#'   StopAnalogOut(device, channel = 0L)
#'   CloseDevice(device)
#' }
#'
#' @export
#==============================================================================#
ConfigureAnalogOutTriangle <- function(
    device,
    channel,
    frequency,
    amplitude,
    offset = 0,
    phase = 0,
    .validate_analog_out = TRUE
) {
  SetAnalogOutCarrier(
    device = device,
    channel = channel,
    func = "triangle",
    frequency = frequency,
    amplitude = amplitude,
    offset = offset,
    symmetry = 50,
    phase = phase,
    .validate_analog_out = .validate_analog_out
  )
  EnableAnalogOutCarrier(
    device = device,
    channel = channel,
    .validate_analog_out = FALSE
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Configure an Analog Out ramp waveform
#'
#' @description
#'   Configure a selected Analog Out channel to generate a ramp-up or ramp-down
#'   waveform.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @template arg_frequency
#' @template arg_waveform_amplitude
#' @template arg_waveform_offset
#' @template arg_waveform_phase
#' @template arg_validate_analog_out
#'
#' @details
#'   The carrier symmetry is set to 100 or 0 percent for ramp-up and ramp-down
#'   waveforms, respectively.
#'
#' @template details_analog_out_carrier_enabled
#' @template details_analog_out_pending_configuration
#' @template details_analog_out_actual_values
#' @template seealso_analog_out_vignette
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#'   # Generate a 1 kHz, 2 V peak-to-peak ramp-up wave for one second.
#'   device <- OpenDevice()
#'   ResetAnalogOut(device, channel = 0L)
#'   ConfigureAnalogOutRampUp(
#'     device,
#'     channel = 0L,
#'     frequency = 1000,
#'     amplitude = 1
#'   )
#'   StartAnalogOut(device, channel = 0L)
#'   Sys.sleep(1)
#'   StopAnalogOut(device, channel = 0L)
#'   CloseDevice(device)
#' }
#'
#' @name ConfigureAnalogOutRamp
#==============================================================================#
NULL


#' @rdname ConfigureAnalogOutRamp
#' @description
#'   \code{ConfigureAnalogOutRampUp()} configures a ramp-up waveform.
#'
#' @export
ConfigureAnalogOutRampUp <- function(
    device,
    channel,
    frequency,
    amplitude,
    offset = 0,
    phase = 0,
    .validate_analog_out = TRUE
) {
  SetAnalogOutCarrier(
    device = device,
    channel = channel,
    func = "ramp_up",
    frequency = frequency,
    amplitude = amplitude,
    offset = offset,
    symmetry = 100,
    phase = phase,
    .validate_analog_out = .validate_analog_out
  )
  EnableAnalogOutCarrier(
    device = device,
    channel = channel,
    .validate_analog_out = FALSE
  )
  return(invisible(NULL))
}


#' @rdname ConfigureAnalogOutRamp
#' @description
#'   \code{ConfigureAnalogOutRampDown()} configures a ramp-down waveform.
#'
#' @export
ConfigureAnalogOutRampDown <- function(
    device,
    channel,
    frequency,
    amplitude,
    offset = 0,
    phase = 0,
    .validate_analog_out = TRUE
) {
  SetAnalogOutCarrier(
    device = device,
    channel = channel,
    func = "ramp_down",
    frequency = frequency,
    amplitude = amplitude,
    offset = offset,
    symmetry = 0,
    phase = phase,
    .validate_analog_out = .validate_analog_out
  )
  EnableAnalogOutCarrier(
    device = device,
    channel = channel,
    .validate_analog_out = FALSE
  )
  return(invisible(NULL))
}



#==============================================================================#
#' Configure an Analog Out custom waveform
#'
#' @description
#'   Configure a selected Analog Out channel to generate a custom waveform
#'   specified by voltage samples.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @param voltages
#'   A numeric vector specifying one period of the waveform, in volts. Elements
#'   are assumed to represent equally spaced positions within the period.
#' @template arg_frequency
#' @param offset
#'   Optional numeric scalar specifying the carrier offset in volts. If
#'   \code{NULL}, the offset is calculated as the midpoint between the minimum
#'   and maximum values in \code{voltages}. Specify this value manually when a
#'   particular baseline voltage is required, for example when using
#'   \code{SetAnalogOutIdle(..., idle = "offset")} to define the idle output
#'   level.
#' @template arg_waveform_phase
#' @template arg_validate_analog_out
#'
#' @details
#'   The voltage samples are automatically scaled to the normalized range
#'   required by the WaveForms SDK. If \code{offset = NULL}, the offset is
#'   calculated as the midpoint between the minimum and maximum voltages, and
#'   the amplitude as half their difference, so that the normalized samples span
#'   \code{[-1, 1]}. If an offset is specified explicitly, the amplitude is
#'   calculated as the maximum absolute deviation from that offset, and the
#'   normalized samples lie within \code{[-1, 1]}.
#' @details
#'   The \code{frequency} argument specifies the repetition frequency of the
#'   complete custom waveform.
#' @details
#'   The WaveForms SDK interpolates custom waveform data to the device buffer
#'   size. If fewer than the maximum supported number of custom samples are
#'   supplied, this function issues a warning suggesting use of the maximum
#'   sample count to make full use of the supported custom-data resolution.
#' @details
#'   With the default device configuration, the Analog Out carrier buffer
#'   contains 4096 samples on Analog Discovery 2 and 16384 samples on Analog
#'   Discovery 3. The supported sample count depends on the device and its
#'   configuration; use \code{GetAnalogOutNodeSampleCountRange()} to query the
#'   actual range.
#' @details
#'   A constant \code{voltages} vector is not accepted. Use
#'   \code{ConfigureAnalogOutConstantVoltage()} to generate a constant voltage.
#' @template details_analog_out_carrier_enabled
#' @template details_analog_out_pending_configuration
#' @template details_analog_out_actual_values
#' @template seealso_analog_out_vignette
#'
#' @return
#'   Invisibly returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#'   # Generate a 1 kHz half-wave rectified sine waveform for one second.
#'   device <- OpenDevice()
#'   sample_count_range <- GetAnalogOutNodeSampleCountRange(
#'     device,
#'     channel = 0L,
#'     node = "carrier"
#'   )
#'   n_samples <- sample_count_range[[2L]]
#'   x <- seq(0, 2 * pi, length.out = n_samples + 1L)
#'   x <- x[-length(x)]
#'   sine_wave <- sin(x)
#'   voltages <- ifelse(sine_wave > 0, sine_wave, 0)
#'   ResetAnalogOut(device, channel = 0L)
#'   ConfigureAnalogOutCustom(
#'     device,
#'     channel = 0L,
#'     voltages = voltages,
#'     frequency = 1000
#'   )
#'   StartAnalogOut(device, channel = 0L)
#'   Sys.sleep(1)
#'   StopAnalogOut(device, channel = 0L)
#'   CloseDevice(device)
#' }
#'
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertNumeric
#' @importFrom checkmate assertNumber
#'
#' @export
#==============================================================================#
ConfigureAnalogOutCustom <- function(
    device,
    channel,
    voltages,
    frequency,
    offset = NULL,
    phase = 0,
    .validate_analog_out = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(.validate_analog_out)
  if (.validate_analog_out) {
    .AssertAnalogOut(
      device,
      channel = channel,
      node = "carrier",
      func = "custom"
    )
  }

  checkmate::assertNumeric(
    voltages,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 2L
  )

  checkmate::assertNumber(offset, finite = TRUE, null.ok = TRUE)

  voltage_min <- min(voltages)
  voltage_max <- max(voltages)

  if (voltage_min == voltage_max) {
    stop("'voltages' must contain at least two different values.",
         call. = FALSE)
  }

  sample_count_range <- GetAnalogOutNodeSampleCountRange(
    device,
    channel = channel,
    node = "carrier",
    .validate_analog_out = FALSE
  )

  n_samples <- length(voltages)
  if (n_samples < sample_count_range[[1L]] ||
      n_samples > sample_count_range[[2L]]) {
    stop_msg <- sprintf(
      "Length of 'voltages' (%d) is out of supported range [%d, %d].",
      n_samples,
      sample_count_range[[1L]],
      sample_count_range[[2L]]
    )
    stop(stop_msg, call. = FALSE)
  }

  max_samples <- sample_count_range[[2L]]
  if (n_samples < max_samples) {
    warning_msg <- sprintf(
      paste(
        "'voltages' contains %d samples, while the selected channel supports",
        "up to %d custom samples. The WaveForms SDK interpolates custom",
        "waveform data to the device buffer size; consider generating %d",
        "samples in R to use the full supported custom-data resolution."
      ),
      n_samples,
      max_samples,
      max_samples
    )
    warning(warning_msg, call. = FALSE)
  }


  #--[ Prepare waveform ]-------------------------------------------------------

  if (is.null(offset)) {
    offset <- (voltage_min + voltage_max) / 2
    amplitude <- (voltage_max - voltage_min) / 2
  } else {
    amplitude <- max(abs(voltages - offset))
  }

  data <- (voltages - offset) / amplitude

  # Protect against floating-point round-off at the interval boundaries.
  data <- pmax(-1, pmin(1, data))


  #--[ Configure carrier ]------------------------------------------------------

  SetAnalogOutCarrier(
    device = device,
    channel = channel,
    func = "custom",
    frequency = frequency,
    amplitude = amplitude,
    offset = offset,
    phase = phase,
    .validate_analog_out = FALSE
  )

  SetAnalogOutCarrierData(
    device = device,
    channel = channel,
    data = data,
    .validate_analog_out = FALSE
  )

  EnableAnalogOutCarrier(
    device = device,
    channel = channel,
    .validate_analog_out = FALSE
  )

  return(invisible(NULL))
}



#==============================================================================#
#' Configure Analog Out from control points
#'
#' @description
#'   Configure a selected Analog Out channel from a sequence of time-voltage
#'   control points.
#'
#' @template arg_device
#' @template arg_analog_out_channel
#' @param time_us
#'   A numeric vector specifying control-point times in microseconds. The first
#'   value must be \code{0}, values must be strictly increasing, and the last
#'   value defines the waveform period.
#' @param voltages
#'   A numeric vector specifying voltage values in volts. With
#'   \code{interpolation = "linear"}, it must have the same length as
#'   \code{time_us}, with each value specifying the voltage at the corresponding
#'   time point. With \code{interpolation = "constant"}, it may contain either
#'   one fewer element than \code{time_us} or the same number of elements;
#'   \code{voltages[i]} applies from \code{time_us[i]} up to \code{time_us[i +
#'   1]}; when equal-length vectors are supplied, the final voltage value does
#'   not affect the generated waveform.
#' @param interpolation
#'   A string specifying the interpolation method. Supported values are
#'   \code{"linear"} and \code{"constant"}. With \code{"constant"}, each voltage
#'   is held from its corresponding time point up to, but not including, the
#'   next time point. When \code{voltages} has the same length as
#'   \code{time_us}, its final value is not used.
#' @param offset
#'   Optional numeric scalar specifying the carrier offset in volts. If
#'   \code{NULL}, the offset is calculated automatically by
#'   \code{ConfigureAnalogOutCustom()}. Specify this value manually when a
#'   particular baseline voltage is required, for example when using
#'   \code{SetAnalogOutIdle(..., idle = "offset")}.
#' @template arg_validate_analog_out
#'
#' @details
#'   The waveform period is determined by the last value in \code{time_us}, and
#'   the carrier frequency is calculated automatically from this period.
#' @details
#'   If the available custom-waveform resolution is insufficient to place at
#'   least one generated sample within every interval defined by consecutive
#'   values of \code{time_us}, the function stops with an error.
#' @details
#'   The control points are resampled to the maximum number of custom waveform
#'   samples supported by the selected carrier node. Samples are generated over
#'   the interval from zero up to, but not including, the period endpoint.
#' @details
#'   Because the waveform is represented on a discrete sample grid, specified
#'   control-point times do not necessarily coincide with generated sample
#'   positions and may therefore be absent from the resampled waveform. Waveform
#'   features shorter than or comparable to one sample interval may be poorly
#'   represented or omitted entirely. The nominal time resolution is the
#'   waveform period divided by the number of generated samples; for example,
#'   with 4096 samples, it is approximately \code{1 / 4096} of the waveform
#'   period.
#' @details
#'   With linear interpolation, consecutive control points are connected by
#'   straight lines. The final voltage value determines the waveform as it
#'   approaches the period boundary, although the endpoint itself is not
#'   included in the generated sample vector.
#' @details
#'   The generated \code{voltages} vector is passed to
#'   \code{ConfigureAnalogOutCustom()} for carrier configuration.
#' @template details_analog_out_carrier_enabled
#' @template details_analog_out_pending_configuration
#' @template details_analog_out_actual_values
#' @template seealso_analog_out_vignette
#'
#' @return
#'   Invisibly returns a data frame describing the generated custom waveform:
#'   \describe{
#'     \item{\code{time_us}}{
#'       Numeric values specifying the nominal uniformly spaced sample times
#'       based on the requested period, in microseconds. The waveform-period
#'       endpoint is not included.
#'     }
#'     \item{\code{voltage}}{
#'       Numeric values specifying the interpolated voltage at each
#'       corresponding sample time.
#'     }
#'   }
#'
#' @examples
#' \dontrun{
#'   # Generate a 10 kHz piecewise-linear waveform for one second.
#'   device <- OpenDevice()
#'   ResetAnalogOut(device, channel = 0L)
#'   ConfigureAnalogOutFromPoints(
#'     device,
#'     channel = 0L,
#'     time_us = c(0, 10, 20, 40, 100),
#'     voltages = c(0, 2, -1, 0, 0),
#'     interpolation = "linear",
#'     offset = 0
#'   )
#'   StartAnalogOut(device, channel = 0L)
#'   Sys.sleep(1)
#'   StopAnalogOut(device, channel = 0L)
#'   CloseDevice(device)
#' }
#'
#' @importFrom checkmate assertChoice
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertNumber
#' @importFrom checkmate assertNumeric
#' @importFrom stats approx
#'
#' @export
#==============================================================================#
ConfigureAnalogOutFromPoints <- function(
    device,
    channel,
    time_us,
    voltages,
    interpolation = "linear",
    offset = NULL,
    .validate_analog_out = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertFlag(.validate_analog_out)
  if (.validate_analog_out) {
    .AssertAnalogOut(
      device,
      channel = channel,
      node = "carrier",
      func = "custom"
    )
  }

  checkmate::assertNumeric(time_us, finite = TRUE, min.len = 2L)
  checkmate::assertNumeric(voltages, finite = TRUE, min.len = 1L)
  checkmate::assertChoice(interpolation, c("linear", "constant"))
  checkmate::assertNumber(offset, finite = TRUE, null.ok = TRUE)

  n_time_points <- length(time_us)

  if (identical(interpolation, "linear")) {
    if (length(voltages) != n_time_points) {
      stop(
        "For linear interpolation, length of 'voltages' must be equal to ",
        "length of 'time_us'.",
        call. = FALSE
      )
    }
  } else {
    if (length(voltages) != n_time_points &&
        length(voltages) != n_time_points - 1L) {
      stop(
        "For constant interpolation, length of 'voltages' must be equal to ",
        "length of 'time_us' or one less than length of 'time_us'.",
        call. = FALSE
      )
    }
  }

  if (time_us[[1L]] != 0) {
    stop("'time_us' must start at 0.", call. = FALSE)
  }

  if (any(diff(time_us) <= 0)) {
    stop("'time_us' must be strictly increasing.", call. = FALSE)
  }


  #--[ Generate waveform samples ]----------------------------------------------

  sample_count_range <- GetAnalogOutNodeSampleCountRange(
    device,
    channel = channel,
    node = "carrier",
    .validate_analog_out = FALSE
  )
  n_samples <- sample_count_range[[2L]]

  period_us <- time_us[[length(time_us)]]
  frequency <- 1e6 / period_us

  temp <- seq(0, period_us, length.out = n_samples + 1L)
  sample_time_us <- temp[-length(temp)]

  interval <- findInterval(sample_time_us, time_us)
  interval_counts <- tabulate(interval, nbins = length(time_us) - 1L)

  # Each value returned by 'findInterval()' identifies the time interval
  # containing a waveform sample:
  #   interval <- findInterval(seq(0, 9), c(0, 1.5, 5.1, 5.9, 9, 10))
  #   print(interval)
  #   #> 1 1 2 2 2 2 4 4 4 5
  #
  # Later 'tabulate()' gives the following interval counts (where zeros identify
  # intervals that cannot be represented at the selected waveform resolution):
  #   tabulate(interval, nbins = 5L)
  #   #> 2 4 0 3 1

  if (any(interval_counts == 0L)) {
    stop(
      "The available Analog Out waveform resolution is insufficient to ",
      "represent every interval defined by 'time_us'.",
      call. = FALSE
    )
  }

  if (identical(interpolation, "linear")) {
    temp <- stats::approx(
      x = time_us,
      y = voltages,
      xout = sample_time_us,
      method = "linear",
      ties = "ordered"
    )
    sample_voltages <- temp$y
  } else { # i.e., "constant"
    sample_voltages <- voltages[interval]
  }


  #--[ Configure carrier ]------------------------------------------------------

  ConfigureAnalogOutCustom(
    device = device,
    channel = channel,
    voltages = sample_voltages,
    frequency = frequency,
    offset = offset,
    .validate_analog_out = FALSE
  )

  result <- data.frame(
    time_us = sample_time_us,
    voltage = sample_voltages
  )

  return(invisible(result))
}


