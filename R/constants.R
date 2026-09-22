#==============================================================================#
#' Internal WaveForms SDK constants
#'
#' @description
#'   Integer mappings between user-facing names and WaveForms SDK constants.
#'
#' @format
#'   A named list.
#'
#' @keywords internal
#==============================================================================#
.dwf_constants <- list(

  analog_out = list(

    node_code = list(
      carrier = 0L,
      fm      = 1L,
      am      = 2L
    ),

    function_code = list(
      dc             = 0L,
      sine           = 1L,
      square         = 2L,
      triangle       = 3L,
      ramp_up        = 4L,
      ramp_down      = 5L,
      noise          = 6L,
      pulse          = 7L,
      trapezium      = 8L,
      sine_power     = 9L,
      sine_na        = 10L,
      dual_custom    = 26L,
      dual_pattern   = 27L,
      custom_pattern = 28L,
      play_pattern   = 29L,
      custom         = 30L,
      play           = 31L
    ),

    idle_code = list(
      disable = 0L,
      offset  = 1L,
      initial = 2L,
      hold    = 3L
    )

  ),


  digital_out = list(

    type_code = list(
      pulse  = 0L,
      custom = 1L,
      random = 2L,
      rom    = 3L,
      state  = 4L,
      play   = 5L
    ),

    output_code = list(
      push_pull   = 0L,
      open_drain  = 1L,
      open_source = 2L,
      three_state = 3L
    ),

    idle_code = list(
      initial     = 0L,
      low         = 1L,
      high        = 2L,
      three_state = 3L
    )

  ),


  trigger = list(

    # 'trigsrcDIO' is defined as 32 in 'dwf.h', but it is unclear how this value
    # is represented in the 32-bit mask returned by 'FDwfDeviceTriggerInfo()'.
    # It may therefore not be discoverable by 'GetDeviceTriggerSources()'.
    source_code = list(
      none = 0L,
      pc = 1L,
      detector_analog_in = 2L,
      detector_digital_in = 3L,
      analog_in = 4L,
      digital_in = 5L,
      digital_out = 6L,
      analog_out_1 = 7L,
      analog_out_2 = 8L,
      analog_out_3 = 9L,
      analog_out_4 = 10L,
      external_1 = 11L,
      external_2 = 12L,
      external_3 = 13L,
      external_4 = 14L,
      high = 15L,
      low = 16L,
      clock = 17L,
      dio = 32L
    ),

    slope_code = list(
      rising  = 0L,
      falling = 1L,
      either  = 2L
    )

  )

)


