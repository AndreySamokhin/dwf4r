## ----knitr_setup, include = FALSE---------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.height = 3.5,
  fig.width = 5.5
)

## ----session_setup, include = FALSE-------------------------------------------
library(dwf4r)
package_info <- paste0("`dwf4r_", utils::packageVersion("dwf4r"), "`")

## ----open_device, eval = FALSE------------------------------------------------
# device <- OpenDevice()
# 
# print(device$info)
# 
# #> $device_type
# #> [1] "Analog Discovery 2"
# #>
# #> $device_revision
# #> [1] 2051
# #>
# #> $device_name
# #> [1] "Analog Discovery 2 NI EditionSN:Discovery2NI"
# #>
# #> $user_name
# #> [1] "Discovery2NI"
# #>
# #> $serial_number
# #> [1] "SN:************"
# #>
# #> $config_count
# #> [1] 8

## ----generate_pulse, eval = FALSE---------------------------------------------
# ResetDigitalOut(device)
# 
# EnableDigitalOutChannel(device, channel = 0L)
# SetDigitalOutFunction(device, channel = 0L, func = "pulse")
# SetDigitalOutOutput(device, channel = 0L, output = "push_pull")
# 
# frequency <- 1000
# n_counts <- 10L
# internal_clock <- GetDigitalOutInternalClockFrequency(device)
# divider <- round(internal_clock / (n_counts * frequency))
# SetDigitalOutDivider(device, channel = 0L, divider = divider)
# 
# SetDigitalOutCounter(
#   device,
#   channel = 0L,
#   low_count = 5L,
#   high_count = 5L
# )
# 
# StartDigitalOut(device)

## -----------------------------------------------------------------------------
knitr::include_graphics(
  "figures/digital-out-basics/pulse-waveform.png"
)

## ----modify_running_waveform, eval = FALSE------------------------------------
# SetDigitalOutCounter(
#   device,
#   channel = 0L,
#   low_count = 8L,
#   high_count = 2L
# )
# StartDigitalOut(device)

## -----------------------------------------------------------------------------
knitr::include_graphics(
  "figures/digital-out-basics/pulse-waveform-20-percent.png"
)

## ----get_digital_out_status, eval = FALSE-------------------------------------
# GetDigitalOutStatus(device)
# 
# #> [1] "running"

## ----stop_digital_out, eval = FALSE-------------------------------------------
# StopDigitalOut(device)
# GetDigitalOutStatus(device)
# 
# #> [1] "ready"

## -----------------------------------------------------------------------------
local({
  old_par <- par(c("mar", "xpd"))
  on.exit(par(old_par), add = TRUE)
  par(
    mar = c(0.5, 0.5, 0.5, 0.5),
    xpd = NA
  )

  state_x <- c(
    "ready" = 1,
    "armed" = 3,
    "wait" = 5,
    "done" = 1,
    "repeat" = 3,
    "running" = 5
  )
  state_y <- c(
    "ready" = 1,
    "armed" = 1,
    "wait" = 1,
    "done" = 0,
    "repeat" = 0,
    "running" = 0
  )

  box_width <- 1
  box_height <- 0.5

  plot.new()
  plot.window(
    xlim = c(0, 6),
    ylim = c(-0.5, 1.5),
    xaxs = "i",
    yaxs = "i"
  )

  rect(
    xleft = state_x - box_width / 2,
    ybottom = state_y - box_height / 2,
    xright = state_x + box_width / 2,
    ytop = state_y + box_height / 2,
    col = "gray95",
    border = "gray30",
    lwd = 1.5
  )
  
  text(
    x = state_x,
    y = state_y,
    labels = names(state_x)
  )

  arrows(
    x0 = c(
      state_x[["ready"]] + box_width / 2,
      state_x[["armed"]] + box_width / 2,
      state_x[["wait"]],
      state_x[["running"]] - box_width / 2,
      state_x[["repeat"]] - box_width / 2,
      state_x[["repeat"]],
      state_x[["repeat"]] + box_width / 2
    ),
    y0 = c(
      state_y[["ready"]],
      state_y[["armed"]],
      state_y[["wait"]] - box_height / 2,
      state_y[["running"]],
      state_y[["repeat"]],
      state_y[["repeat"]] + box_height / 2,
      state_y[["repeat"]] + box_height / 2
    ),
    x1 = c(
      state_x[["armed"]] - box_width / 2,
      state_x[["wait"]] - box_width / 2,
      state_x[["running"]],
      state_x[["repeat"]] + box_width / 2,
      state_x[["done"]] + box_width / 2,
      state_x[["armed"]],
      state_x[["wait"]] - box_width / 2
      ),
    y1 = c(
      state_y[["armed"]],
      state_y[["wait"]],
      state_y[["running"]] + box_height / 2,
      state_y[["repeat"]],
      state_y[["done"]],
      state_y[["armed"]] - box_height / 2,
      state_y[["wait"]] - box_height / 2
    ),
    length = 0.08,
    lty = c(rep("solid", 5L), rep("dashed", 2L))
  )
  
  text(
    x = c(
      (state_x[["ready"]] + state_x[["armed"]]) / 2,
      (state_x[["armed"]] + state_x[["wait"]]) / 2
    ),
    y = c(state_y[["ready"]], state_y[["armed"]]) + 0.1,
    labels = c("start", "trigger")
  )
})

## ----generate_three_pulse_burst, eval = FALSE---------------------------------
# SetDigitalOutRun(device, run_time = 3L / frequency)
# StartDigitalOut(device)

## -----------------------------------------------------------------------------
knitr::include_graphics(
  "figures/digital-out-basics/three-pulses.png"
)

## ----inspect_remaining_run, eval = FALSE--------------------------------------
# SetDigitalOutRun(device, run_time = 1)
# StartDigitalOut(device)
# Sys.sleep(0.1)
# 
# remaining_run_value <- GetDigitalOutRemainingRun(device)
# remaining_run_value / GetDigitalOutInternalClockFrequency(device)
# 
# #> [1] 0.8450741

## ----inspect_remaining_repeat, eval = FALSE-----------------------------------
# SetDigitalOutTiming(
#   device,
#   run_time = 1 / frequency,
#   repeat_count = 1000L
# )
# StartDigitalOut(device)
# Sys.sleep(0.1)
# 
# GetDigitalOutRemainingRepeat(device)
# 
# #> [1] 842

## ----generate_four_repeated_pulses, eval = FALSE------------------------------
# SetDigitalOutTiming(
#   device,
#   run_time = 1 / frequency,
#   wait_time = 2 / frequency,
#   repeat_count = 4L
# )
# StartDigitalOut(device)

## -----------------------------------------------------------------------------
knitr::include_graphics(
  "figures/digital-out-basics/four-pulses.png"
)

## ----define_custom_pattern----------------------------------------------------
pattern <- c(0, 1, 0, 0, 1, 1, 1, 1)

## -----------------------------------------------------------------------------
local({
  old_par <- par(c("mar", "cex"))
  on.exit(par(old_par), add = TRUE)

  par(
    mar = c(4.1, 4.1, 1.1, 1.1),
    cex = 0.9
  )

  plot(
    seq(from = 0, to = length(pattern), by = 1),
    c(pattern, pattern[[length(pattern)]]),
    type = "s",
    xlab = "Sample interval",
    ylab = "Logical level",
    xaxs = "i",
    yaxs = "i",
    ylim = c(-0.1, 1.1),
    lwd = 2,
    yaxt = "n"
  )
  axis(2, at = c(0, 1), labels = c("low", "high"), las = 1)
})

## ----generate_custom_waveform, eval = FALSE-----------------------------------
# sample_rate <- 8L * 1000
# 
# SetDigitalOutFunction(device, channel = 0L, func = "custom")
# SetDigitalOutCustomPattern(
#   device,
#   channel = 0L,
#   pattern = pattern
# )
# SetDigitalOutCustomSampleRate(
#   device,
#   channel = 0L,
#   sample_rate = sample_rate
# )
# 
# StartDigitalOut(device)

## -----------------------------------------------------------------------------
knitr::include_graphics(
  "figures/digital-out-basics/custom-pattern.png"
)

## ----use_high_idle_output, eval = FALSE---------------------------------------
# SetDigitalOutIdle(device, channel = 0L, idle = "high")
# StartDigitalOut(device)

## -----------------------------------------------------------------------------
knitr::include_graphics(
  "figures/digital-out-basics/custom-pattern-idle-high.png"
)

## ----inspect_divider_range, eval = FALSE--------------------------------------
# GetDigitalOutDividerRange(device, channel = 0L)
# 
# #> [1] 1 2147483649

## ----inspect_divider, eval = FALSE--------------------------------------------
# GetDigitalOutDivider(device, channel = 0L)
# 
# #> [1] 12500

## ----configure_multiple_channels, eval = FALSE--------------------------------
# ResetDigitalOut(device)
# 
# internal_clock <- GetDigitalOutInternalClockFrequency(device)
# 
# # Use 16 counter steps per period to support 1/8-period phase shifts while
# # keeping the initial divider distinct from the ordinary divider.
# pulse_counts <- 16L
# 
# for (channel in seq.int(0L, 3L)) {
#   EnableDigitalOutChannel(device, channel = channel)
#   SetDigitalOutOutput(device, channel = channel, output = "push_pull")
# }
# 
# # Channel 0: 0.5 kHz pulse waveform.
# frequency_500hz <- 500
# divider_500hz <- internal_clock / (pulse_counts * frequency_500hz)
# 
# SetDigitalOutFunction(device, channel = 0L, func = "pulse")
# SetDigitalOutDivider(device, channel = 0L, divider = divider_500hz)
# SetDigitalOutCounter(
#   device,
#   channel = 0L,
#   low_count = (3 / 4) * pulse_counts,
#   high_count = (1 / 4) * pulse_counts
# )
# 
# # Channels 1 and 2: 1 kHz pulse waveforms.
# frequency_1khz <- 1000
# divider_1khz <- internal_clock / (pulse_counts * frequency_1khz)
# 
# SetDigitalOutFunction(device, channel = 1L, func = "pulse")
# SetDigitalOutDivider(device, channel = 1L, divider = divider_1khz)
# SetDigitalOutCounter(
#   device,
#   channel = 1L,
#   low_count = (3 / 4) * pulse_counts,
#   high_count = (1 / 4) * pulse_counts
# )
# 
# SetDigitalOutFunction(device, channel = 2L, func = "pulse")
# SetDigitalOutDivider(device, channel = 2L, divider = divider_1khz)
# SetDigitalOutCounter(
#   device,
#   channel = 2L,
#   low_count = (1 / 4) * pulse_counts,
#   high_count = (3 / 4) * pulse_counts
# )
# 
# # Channel 3: ten-element custom pattern repeating at 2 kHz.
# pattern <- c(0L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 0L)
# pattern_frequency <- 2000
# sample_rate <- length(pattern) * pattern_frequency
# divider_custom <- round(internal_clock / sample_rate)
# 
# SetDigitalOutFunction(device, channel = 3L, func = "custom")
# SetDigitalOutCustomPattern(device, channel = 3L, pattern = pattern)
# SetDigitalOutDivider(device, channel = 3L, divider = divider_custom)
# 
# StartDigitalOut(device)

## -----------------------------------------------------------------------------
knitr::include_graphics(
  "figures/digital-out-basics/multiple-channels.png"
)

## ----adjust_initial_timing, eval = FALSE--------------------------------------
# 
# # Channel 1: delay by 7/8 period, equivalent to a 1/8-period phase advance.
# counters_ch1 <- GetDigitalOutCounter(device, channel = 1L)
# SetDigitalOutInitialCounter(
#   device,
#   channel = 1L,
#   initial_state = "low",
#   initial_count = counters_ch1[["low_count"]] + (7 / 8) * pulse_counts
# )
# 
# # Channel 2: delay by 1/8 period.
# counters_ch2 <- GetDigitalOutCounter(device, channel = 2L)
# period_ticks <- internal_clock / frequency_1khz
# SetDigitalOutInitialDivider(
#   device,
#   channel = 2L,
#   initial_divider = (1 / 8) * period_ticks
# )
# SetDigitalOutInitialCounter(
#   device,
#   channel = 2L,
#   initial_state = "low",
#   initial_count = counters_ch2[["low_count"]] + 1L
# )
# 
# StartDigitalOut(device)

## -----------------------------------------------------------------------------
knitr::include_graphics(
  "figures/digital-out-basics/shifted-waveforms.png"
)

## ----close_device, eval = FALSE-----------------------------------------------
# StopDigitalOut(device)
# CloseDevice(device)

