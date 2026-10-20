#==============================================================================#
#' Configure a Digital Out pulse waveform
#'
#' @description
#'   Configure a selected Digital Out channel to generate a periodic pulse
#'   waveform. With the default \code{phase = 0}, generation starts in the high
#'   state and then switches to the low state. For nonzero phase values, the
#'   initial state depends on the phase and duty cycle.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @template arg_frequency
#' @param duty_cycle
#'   A numeric scalar specifying the fraction of each period spent in the high
#'   state, in percent. The value must be greater than \code{0} and smaller than
#'   \code{100}.
#' @param phase
#'   A numeric scalar specifying the waveform phase in degrees, from \code{0} to
#'   \code{360}. A positive phase shifts the waveform to the left. A phase of
#'   \code{360} is equivalent to \code{0}.
#' @param output
#'   A string specifying the electrical output mode. Possible values are
#'   \code{"push_pull"} (default), \code{"open_drain"}, and
#'   \code{"open_source"}. The latter two modes require suitable external
#'   pull-up or pull-down resistors, respectively. The \code{"three_state"} mode
#'   is not supported for pulse generation.
#' @template arg_validate_digital_out
#'
#' @details
#'   Digital Out pulse timing is controlled by an integer clock divider and
#'   integer low and high counters. This function calculates these values from
#'   the requested frequency and duty cycle. To make frequency configuration
#'   independent of duty cycle, the total number of counter steps per period is
#'   limited to the maximum counter value supported by the selected channel.
#'
#'   Because the divider and counters are whole numbers, the resulting frequency
#'   and duty cycle may differ from the requested values.
#'
#'   Phase is represented using the initial counter configuration and is rounded
#'   to the nearest ordinary counter step. Consequently, the resulting phase may
#'   differ from the requested value. A positive phase shifts the waveform to
#'   the left relative to the zero-phase waveform.
#'
#'   The selected electrical output mode is configured automatically using
#'   \code{SetDigitalOutOutput()}; a separate call is not required. The
#'   signal-generation function is set to \code{"pulse"}, and the selected
#'   channel is enabled. The idle mode is not modified.
#'
#' @template details_digital_out_pending_configuration
#' @template seealso_digital_out_vignette
#'
#' @return
#'   Invisibly returns a named list with the following elements:
#'   \describe{
#'     \item{\code{frequency}}{
#'       A named list containing \code{requested}, \code{actual},
#'       \code{abs_error}, and \code{rel_error}. Absolute error is expressed
#'       in Hz, and relative error is calculated as the absolute error divided
#'       by the requested frequency.
#'     }
#'     \item{\code{duty_cycle}}{
#'       A named list containing \code{requested}, \code{actual}, and
#'       \code{abs_error}. Absolute error is expressed in percentage points.
#'     }
#'     \item{\code{phase}}{
#'       A named list containing \code{requested}, \code{actual}, and
#'       \code{abs_error}. Absolute error is expressed in degrees and is
#'       calculated as the shortest circular difference between the requested
#'       and resulting phases.
#'     }
#'   }
#'
#' @examples
#' \dontrun{
#'   # Generate a 1 kHz pulse waveform with a 25% duty cycle for one second.
#'   device <- OpenDevice()
#'   ResetDigitalOut(device)
#'   ConfigureDigitalOutPulse(
#'     device,
#'     channel = 0L,
#'     frequency = 1000,
#'     duty_cycle = 25
#'   )
#'   StartDigitalOut(device)
#'   Sys.sleep(1)
#'   StopDigitalOut(device)
#'   CloseDevice(device)
#' }
#'
#' @importFrom checkmate assertChoice
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertNumber
#'
#' @export
#==============================================================================#
ConfigureDigitalOutPulse <- function(
    device,
    channel,
    frequency,
    duty_cycle = 50,
    phase = 0,
    output = "push_pull",
    .validate_digital_out = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertChoice(
    output,
    c("push_pull", "open_drain", "open_source")
  )

  checkmate::assertFlag(.validate_digital_out)
  if (.validate_digital_out) {
    .AssertDigitalOut(
      device,
      channel = channel,
      output = output,
      func = "pulse"
    )
  }

  checkmate::assertNumber(frequency, lower = 0, finite = TRUE)
  if (frequency == 0) {
    stop("'frequency' must be greater than zero.", call. = FALSE)
  }

  checkmate::assertNumber(duty_cycle, lower = 0, upper = 100, finite = TRUE)
  if (duty_cycle == 0 || duty_cycle == 100) {
    stop("'duty_cycle' must be greater than 0 and smaller than 100.",
         call. = FALSE)
  }

  checkmate::assertNumber(phase, lower = 0, upper = 360, finite = TRUE)


  #--[ Calculate pulse timing ]-------------------------------------------------

  internal_clock_frequency <- GetDigitalOutInternalClockFrequency(
    device,
    .validate_device = FALSE
  )
  divider_range <- GetDigitalOutDividerRange(
    device,
    channel = channel,
    .validate_digital_out = FALSE
  )
  counter_range <- GetDigitalOutCounterRange(
    device,
    channel = channel,
    .validate_digital_out = FALSE
  )

  ticks_per_period <- internal_clock_frequency / frequency
  max_counts_per_period <- counter_range[[2L]]

  divider <- ceiling(ticks_per_period / max_counts_per_period)
  divider <- max(divider, divider_range[[1L]])

  if (divider > divider_range[[2L]]) {
    stop(
      paste(
        "Requested 'frequency' cannot be represented by the supported",
        "Digital Out divider and counter ranges."
      ),
      call. = FALSE
    )
  }

  counts_per_period <- round(ticks_per_period / divider)
  minimum_pulse_count <- max(1, counter_range[[1L]])

  if (counts_per_period < 2 * minimum_pulse_count) {
    stop(
      paste(
        "Requested 'frequency' leaves too few counter steps to represent",
        "both low and high pulse states."
      ),
      call. = FALSE
    )
  }

  duty_fraction <- duty_cycle / 100

  high_count <- round(counts_per_period * duty_fraction)
  high_count <- max(
    minimum_pulse_count,
    min(high_count, counts_per_period - minimum_pulse_count)
  )
  low_count <- counts_per_period - high_count


  #--[ Calculate initial phase ]------------------------------------------------

  phase_count <- round(counts_per_period * phase / 360) %% counts_per_period

  if (phase_count == 0) {
    initial_state <- "high"
    initial_count <- 0
  } else if (phase_count < high_count) {
    initial_state <- "high"
    initial_count <- high_count - phase_count
  } else if (phase_count == high_count) {
    initial_state <- "low"
    initial_count <- 0
  } else {
    initial_state <- "low"
    initial_count <- counts_per_period - phase_count
  }


  #--[ Calculate resulting waveform parameters ]--------------------------------

  actual_frequency <- internal_clock_frequency / (divider * counts_per_period)
  actual_duty_cycle <- 100 * high_count / counts_per_period
  actual_phase <- 360 * phase_count / counts_per_period

  frequency_abs_error <- abs(actual_frequency - frequency)
  duty_cycle_abs_error <- abs(actual_duty_cycle - duty_cycle)
  phase_difference <- abs(actual_phase - phase)
  phase_abs_error <- min(phase_difference, 360 - phase_difference)

  result <- list(
    frequency = list(
      requested = frequency,
      actual = actual_frequency,
      abs_error = frequency_abs_error,
      rel_error = frequency_abs_error / frequency
    ),
    duty_cycle = list(
      requested = duty_cycle,
      actual = actual_duty_cycle,
      abs_error = duty_cycle_abs_error
    ),
    phase = list(
      requested = phase,
      actual = actual_phase,
      abs_error = phase_abs_error
    )
  )


  #--[ Configure channel ]------------------------------------------------------

  SetDigitalOutOutput(
    device = device,
    channel = channel,
    output = output,
    .validate_digital_out = FALSE
  )

  SetDigitalOutFunction(
    device = device,
    channel = channel,
    func = "pulse",
    .validate_digital_out = FALSE
  )

  SetDigitalOutDivider(
    device = device,
    channel = channel,
    divider = divider,
    .validate_digital_out = FALSE
  )

  SetDigitalOutCounter(
    device = device,
    channel = channel,
    low_count = low_count,
    high_count = high_count,
    .validate_digital_out = FALSE
  )

  SetDigitalOutInitialDivider(
    device = device,
    channel = channel,
    initial_divider = divider,
    .validate_digital_out = FALSE
  )

  SetDigitalOutInitialCounter(
    device = device,
    channel = channel,
    initial_state = initial_state,
    initial_count = initial_count,
    .validate_digital_out = FALSE
  )

  EnableDigitalOutChannel(
    device = device,
    channel = channel,
    .validate_digital_out = FALSE
  )

  return(invisible(result))
}



#==============================================================================#
#' Configure a custom Digital Out waveform
#'
#' @description
#'   Configure a selected Digital Out channel to generate a periodic custom
#'   waveform specified by a sequence of logical pattern elements.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @param pattern
#'   A non-empty numeric or character vector specifying one period of the
#'   waveform. A numeric pattern must contain only \code{0} and \code{1}. A
#'   character pattern must contain only \code{"l"}, \code{"h"}, and \code{"z"},
#'   which represent logical low, logical high, and high-impedance samples,
#'   respectively. The \code{"z"} value can only be used when \code{output =
#'   "three_state"}. Elements are generated at equally spaced time intervals.
#' @param frequency,sample_rate
#'   Optional positive numeric scalars expressed in Hz. \code{frequency}
#'   specifies the repetition frequency of the complete pattern, whereas
#'   \code{sample_rate} specifies the rate at which individual logical pattern
#'   elements are generated. They are related by \code{sample_rate = N *
#'   frequency}, where \code{N} is the number of elements in \code{pattern}.
#'   Exactly one of \code{frequency} and \code{sample_rate} must be specified.
#' @template arg_digital_out_output
#' @template arg_validate_digital_out
#'
#' @details
#'   Custom-pattern timing is controlled by an integer clock divider. Because
#'   the divider is a whole number, the resulting frequency and sample rate may
#'   differ from the requested values.
#'
#'   When \code{output = "three_state"}, each logical pattern element is encoded
#'   as an output-value and output-enable bit pair. Other output modes use one
#'   bit per element.
#'
#'   The selected electrical output mode is configured automatically using
#'   \code{SetDigitalOutOutput()}; a separate call is not required. This ensures
#'   that the output mode and pattern encoding are consistent, avoiding errors
#'   caused by forgetting to select three-state output manually.
#'
#'   The signal-generation function is set to \code{"custom"}, and the selected
#'   channel is enabled. The ordinary and initial dividers are set to the same
#'   value so that the first pattern element has the same duration as subsequent
#'   elements. The idle mode are not modified.
#'
#' @template details_digital_out_pending_configuration
#' @template seealso_digital_out_vignette
#'
#' @return
#'   Invisibly returns a named list with the following elements:
#'   \describe{
#'     \item{\code{frequency}}{
#'       A named list containing \code{requested}, \code{actual},
#'       \code{abs_error}, and \code{rel_error}. Absolute error is expressed in
#'       Hz, and relative error is calculated as the absolute error divided by
#'       the requested frequency.
#'     }
#'     \item{\code{sample_rate}}{
#'       A named list containing \code{requested}, \code{actual},
#'       \code{abs_error}, and \code{rel_error}. Absolute error is expressed in
#'       Hz, and relative error is calculated as the absolute error divided by
#'       the requested sample rate.
#'     }
#'   }
#'
#'   For the rate parameter not supplied directly, the \code{requested} value is
#'   the corresponding value implied by the supplied rate and pattern length.
#'
#' @examples
#' \dontrun{
#'   # Generate a four-element custom pattern repeating at 1 kHz.
#'   device <- OpenDevice()
#'   ResetDigitalOut(device)
#'   ConfigureDigitalOutCustom(
#'     device,
#'     channel = 0L,
#'     pattern = c(1, 1, 0, 0),
#'     frequency = 1000
#'   )
#'   StartDigitalOut(device)
#'   Sys.sleep(1)
#'   StopDigitalOut(device)
#'   CloseDevice(device)
#' }
#'
#' @importFrom checkmate assertChoice
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertNumber
#'
#' @export
#==============================================================================#
ConfigureDigitalOutCustom <- function(
    device,
    channel,
    pattern,
    frequency = NULL,
    sample_rate = NULL,
    output = "push_pull",
    .validate_digital_out = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertChoice(
    output,
    names(.dwf_constants$digital_out$output_code)
  )

  is_three_state <- identical(output, "three_state")

  checkmate::assertFlag(.validate_digital_out)

  if (.validate_digital_out) {
    .AssertDigitalOut(
      device,
      channel = channel,
      output = output,
      func = "custom"
    )
  }

  if (length(pattern) == 0L) {
    stop("'pattern' must contain at least one element.", call. = FALSE)
  }
  if (!is_three_state && is.character(pattern) && "z" %in% pattern) {
    stop(
      "'z' pattern values require 'output = \"three_state\"'.",
      call. = FALSE
    )
  }

  if (is.null(frequency) == is.null(sample_rate)) {
    stop(
      "Exactly one of 'frequency' and 'sample_rate' must be specified.",
      call. = FALSE
    )
  }

  checkmate::assertNumber(
    frequency,
    lower = 0,
    finite = TRUE,
    null.ok = TRUE
  )
  if (!is.null(frequency) && frequency == 0) {
    stop("'frequency' must be greater than zero.", call. = FALSE)
  }

  checkmate::assertNumber(
    sample_rate,
    lower = 0,
    finite = TRUE,
    null.ok = TRUE
  )
  if (!is.null(sample_rate) && sample_rate == 0) {
    stop("'sample_rate' must be greater than zero.", call. = FALSE)
  }

  # Validate the complete pattern before modifying channel settings.
  encoded_pattern <- .EncodeDigitalOutCustomPattern(
    pattern = pattern,
    is_three_state = is_three_state
  )
  max_bits <- GetDigitalOutMaxDataBits(
    device,
    channel = channel,
    .validate_digital_out = FALSE
  )
  if (encoded_pattern$n_bits > max_bits) {
    stop_msg <- sprintf(
      "'pattern' requires %d bits, exceeding the supported maximum of %.0f.",
      encoded_pattern$n_bits,
      max_bits
    )
    stop(stop_msg, call. = FALSE)
  }


  #--[ Calculate waveform timing ]----------------------------------------------

  n_pattern_elements <- length(pattern)

  internal_clock_frequency <- GetDigitalOutInternalClockFrequency(
    device,
    .validate_device = FALSE
  )

  divider_range <- GetDigitalOutDividerRange(
    device,
    channel = channel,
    .validate_digital_out = FALSE
  )

  sample_rate_range <- internal_clock_frequency / rev(divider_range)

  if (!is.null(frequency)) {
    frequency_range <- sample_rate_range / n_pattern_elements

    if (frequency < frequency_range[[1L]] ||
        frequency > frequency_range[[2L]]) {
      stop_msg <- sprintf(
        "'frequency' (%.3g Hz) is out of supported range [%.3g, %.3g] Hz.",
        frequency,
        frequency_range[[1L]],
        frequency_range[[2L]]
      )
      stop(stop_msg, call. = FALSE)
    }
    requested_frequency <- frequency
    requested_sample_rate <- frequency * n_pattern_elements

  } else { # i.e.,'!is.null(sample_rate)'
    if (sample_rate < sample_rate_range[[1L]] ||
        sample_rate > sample_rate_range[[2L]]) {
      stop_msg <- sprintf(
        "'sample_rate' (%.3g Hz) is out of supported range [%.3g, %.3g] Hz.",
        sample_rate,
        sample_rate_range[[1L]],
        sample_rate_range[[2L]]
      )
      stop(stop_msg, call. = FALSE)
    }
    requested_sample_rate <- sample_rate
    requested_frequency <- sample_rate / n_pattern_elements
  }

  divider <- round(internal_clock_frequency / requested_sample_rate)


  #--[ Calculate resulting waveform parameters ]--------------------------------

  actual_sample_rate <- internal_clock_frequency / divider
  actual_frequency <- actual_sample_rate / n_pattern_elements

  sample_rate_abs_error <- abs(actual_sample_rate - requested_sample_rate)
  frequency_abs_error <- abs(actual_frequency - requested_frequency)

  result <- list(
    frequency = list(
      requested = requested_frequency,
      actual = actual_frequency,
      abs_error = frequency_abs_error,
      rel_error = frequency_abs_error / requested_frequency
    ),
    sample_rate = list(
      requested = requested_sample_rate,
      actual = actual_sample_rate,
      abs_error = sample_rate_abs_error,
      rel_error = sample_rate_abs_error / requested_sample_rate
    )
  )


  #--[ Configure channel ]------------------------------------------------------

  SetDigitalOutOutput(
    device = device,
    channel = channel,
    output = output,
    .validate_digital_out = FALSE
  )

  SetDigitalOutCustomPattern(
    device = device,
    channel = channel,
    pattern = pattern,
    is_three_state = is_three_state,
    .validate_digital_out = FALSE
  )

  SetDigitalOutFunction(
    device = device,
    channel = channel,
    func = "custom",
    .validate_digital_out = FALSE
  )

  SetDigitalOutDivider(
    device = device,
    channel = channel,
    divider = divider,
    .validate_digital_out = FALSE
  )

  SetDigitalOutInitialDivider(
    device = device,
    channel = channel,
    initial_divider = divider,
    .validate_digital_out = FALSE
  )

  EnableDigitalOutChannel(
    device = device,
    channel = channel,
    .validate_digital_out = FALSE
  )

  return(invisible(result))
}



#==============================================================================#
#' Configure a Digital Out waveform from time-state points
#'
#' @description
#'   Configure a selected Digital Out channel to generate a periodic waveform
#'   specified by transition times and corresponding logical states.
#'
#' @template arg_device
#' @template arg_digital_out_channel
#' @param time_us
#'   A numeric vector specifying interval boundaries in microseconds. The first
#'   value must be \code{0}, values must be strictly increasing, and the last
#'   value defines the waveform period.
#' @param states
#'   A numeric or character vector specifying the logical state between
#'   successive values of \code{time_us}. Numeric values must be \code{0} or
#'   \code{1}. Character values must be \code{"l"}, \code{"h"}, or \code{"z"},
#'   representing logical low, logical high, and high impedance, respectively.
#'   The \code{"z"} value can only be used when \code{output = "three_state"}.
#'   The vector may contain either one fewer element than \code{time_us} or the
#'   same number of elements. In both cases, \code{states[i]} applies from
#'   \code{time_us[i]} up to \code{time_us[i + 1]}. When equal-length vectors
#'   are supplied, the final element of \code{states} is accepted for
#'   compatibility with point-based representations but does not affect the
#'   generated waveform.
#' @template arg_digital_out_output
#' @template arg_validate_digital_out
#'
#' @details
#'   The supplied states are converted to an equally spaced custom Digital Out
#'   pattern. The largest feasible number of logical pattern elements is used,
#'   subject to the custom-data capacity of the selected channel and the maximum
#'   supported custom-pattern sample rate.
#'
#'   Transition times are therefore discretized onto the generated pattern grid
#'   and may differ from the requested values. If the available resolution is
#'   insufficient to represent at least one element from every specified time
#'   interval, the function stops with an error.
#'
#'   With \code{output = "three_state"}, each logical pattern element consumes
#'   two custom-data bits: one output-value bit and one output-enable bit.
#'   Consequently, the maximum number of logical pattern elements is half the
#'   corresponding custom-data bit capacity.
#'
#'   The resulting pattern and its repetition frequency are passed to
#'   \code{ConfigureDigitalOutCustom()}, which also configures the selected
#'   electrical output mode and enables the channel. A separate call to
#'   \code{SetDigitalOutOutput()} is not needed, preventing mismatches between
#'   the output mode and pattern encoding, particularly for three-state output.
#'
#' @template details_digital_out_pending_configuration
#' @template seealso_digital_out_vignette
#'
#' @return
#'   Invisibly returns a named list with the following elements:
#'   \describe{
#'     \item{\code{waveform}}{
#'       A data frame describing the resulting discretized waveform. The
#'       \code{time_us} column contains the actual time, in microseconds,
#'       associated with each generated pattern element after divider
#'       quantization. The \code{states} column contains the corresponding
#'       generated logical states. The time grid starts at zero and does not
#'       include the waveform-period endpoint.
#'     }
#'     \item{\code{frequency}}{
#'       A named list containing \code{requested}, \code{actual},
#'       \code{abs_error}, and \code{rel_error} for the waveform repetition
#'       frequency. Frequency and absolute error are expressed in Hz.
#'     }
#'     \item{\code{period}}{
#'       A named list containing \code{requested}, \code{actual},
#'       \code{abs_error}, and \code{rel_error} for the waveform period. Period
#'       and absolute error are expressed in microseconds.
#'     }
#'   }
#'
#' @examples
#' \dontrun{
#'   # Generate a 10 kHz digital waveform.
#'   device <- OpenDevice()
#'   ResetDigitalOut(device)
#'   ConfigureDigitalOutFromPoints(
#'     device,
#'     channel = 0L,
#'     time_us = c(0, 10, 20, 30, 100),
#'     states = c(1, 0, 1, 0)
#'   )
#'   StartDigitalOut(device)
#'   Sys.sleep(1)
#'   StopDigitalOut(device)
#'   CloseDevice(device)
#' }
#'
#' @importFrom checkmate assertChoice
#' @importFrom checkmate assertFlag
#' @importFrom checkmate assertNumeric
#'
#' @export
#==============================================================================#
ConfigureDigitalOutFromPoints <- function(
    device,
    channel,
    time_us,
    states,
    output = "push_pull",
    .validate_digital_out = TRUE
) {

  #--[ Check input arguments ]--------------------------------------------------

  checkmate::assertChoice(
    output,
    names(.dwf_constants$digital_out$output_code)
  )
  checkmate::assertFlag(.validate_digital_out)

  if (.validate_digital_out) {
    .AssertDigitalOut(
      device,
      channel = channel,
      output = output,
      func = "custom"
    )
  }

  checkmate::assertNumeric(time_us, finite = TRUE, min.len = 2L)
  if (time_us[[1L]] != 0) {
    stop("'time_us' must start at 0.", call. = FALSE)
  }
  if (any(diff(time_us) <= 0)) {
    stop("'time_us' must be strictly increasing.", call. = FALSE)
  }

  n_time_points <- length(time_us)
  if (length(states) != n_time_points && length(states) != n_time_points - 1L) {
    stop(
      "Length of 'states' must be equal to length of 'time_us' or ",
      "one less than length of 'time_us'.",
      call. = FALSE
    )
  }

  # Validate the complete 'states' vector, including an optional final state
  # that is not used to construct the generated pattern.
  is_three_state <- identical(output, "three_state")
  if (!is_three_state && is.character(states) && "z" %in% states) {
    stop(
      "'z' state values require 'output = \"three_state\"'.",
      call. = FALSE
    )
  }
  invisible(.EncodeDigitalOutCustomPattern(
    pattern = states,
    is_three_state = is_three_state
  ))


  #--[ Determine pattern resolution ]-------------------------------------------

  period_us <- time_us[[n_time_points]]
  frequency <- 1e6 / period_us

  max_data_bits <- GetDigitalOutMaxDataBits(
    device = device,
    channel = channel,
    .validate_digital_out = FALSE
  )

  if (is_three_state) {
    max_pattern_elements <- floor(max_data_bits / 2)
  } else {
    max_pattern_elements <- max_data_bits
  }

  internal_clock_frequency <- GetDigitalOutInternalClockFrequency(
    device,
    .validate_device = FALSE
  )

  divider_range <- GetDigitalOutDividerRange(
    device,
    channel = channel,
    .validate_digital_out = FALSE
  )

  sample_rate_range <- internal_clock_frequency / rev(divider_range)
  max_elements_by_rate <- floor(sample_rate_range[[2L]] / frequency)
  n_pattern_elements <- min(max_pattern_elements, max_elements_by_rate)

  if (n_pattern_elements < 1L) {
    stop(
      "Requested waveform period is too short to represent with the ",
      "supported Digital Out sample rate.",
      call. = FALSE
    )
  }

  if (frequency * n_pattern_elements < sample_rate_range[[1L]]) {
    stop(
      "Requested waveform period is too long to represent with the ",
      "supported Digital Out sample rate and custom-data capacity.",
      call. = FALSE
    )
  }


  #--[ Generate custom pattern ]------------------------------------------------

  temp <- seq(0, period_us, length.out = n_pattern_elements + 1L)
  sample_time_us <- temp[-length(temp)]

  interval <- findInterval(sample_time_us, time_us)
  interval_counts <- tabulate(interval, nbins = n_time_points - 1L)

  # Each value returned by 'findInterval()' identifies the time interval
  # containing a pattern element:
  #   interval <- findInterval(seq(0, 9), c(0, 1.5, 5.1, 5.9, 9, 10))
  #   print(interval)
  #   #> 1 1 2 2 2 2 4 4 4 5
  #
  # Later 'tabulate()' gives the following interval counts (where zeros identify
  # intervals that cannot be represented at the selected pattern resolution):
  #   tabulate(interval, nbins = 5L)
  #   #> 2 4 0 3 1

  if (any(interval_counts == 0L)) {
    stop(
      "The available Digital Out pattern resolution is insufficient to ",
      "represent every interval defined by 'time_us'.",
      call. = FALSE
    )
  }

  pattern <- states[interval]


  #--[ Configure channel ]------------------------------------------------------

  custom_result <- ConfigureDigitalOutCustom(
    device = device,
    channel = channel,
    pattern = pattern,
    frequency = frequency,
    output = output,
    .validate_digital_out = FALSE
  )


  #--[ Prepare result ]---------------------------------------------------------

  actual_sample_rate <- custom_result$sample_rate$actual
  actual_time_us <- seq(
    from = 0,
    by = 1e6 / actual_sample_rate,
    length.out = n_pattern_elements
  )

  actual_period_us <- 1e6 / custom_result$frequency$actual
  period_abs_error <- abs(actual_period_us - period_us)

  result <- list(
    waveform = data.frame(
      time_us = actual_time_us,
      states = pattern
    ),
    frequency = custom_result$frequency,
    period = list(
      requested = period_us,
      actual = actual_period_us,
      abs_error = period_abs_error,
      rel_error = period_abs_error / period_us
    )
  )

  return(invisible(result))
}


