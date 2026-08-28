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

## ----generate_sine, eval = FALSE----------------------------------------------
# ResetAnalogOut(device, channel = 0L)
# EnableCarrier(device, channel = 0L)
# SetCarrier(
#   device,
#   channel = 0L,
#   func = "sine",
#   frequency = 1000,
#   amplitude = 1,
#   offset = 0.5
# )
# StartAnalogOut(device, channel = 0L)

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/analog-out-basics/sine-waveform-1000hz.png")

## ----modify_running_waveform, eval = FALSE------------------------------------
# SetCarrierFrequency(device, channel = 0L, frequency = 2000)
# ApplyAnalogOutSettings(device, channel = 0L)

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/analog-out-basics/sine-waveform-2000hz.png")

## ----get_analog_out_status, eval = FALSE--------------------------------------
# GetAnalogOutStatus(device, channel = 0L)
# 
# #> [1] "running"

## ----stop_analog_out, eval = FALSE--------------------------------------------
# StopAnalogOut(device, channel = 0L)
# GetAnalogOutStatus(device, channel = 0L)
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

## ----generate_four_cycle_burst, eval = FALSE----------------------------------
# frequency <- 4000
# 
# SetCarrierFrequency(device, channel = 0L, frequency = frequency)
# SetAnalogOutRun(device, channel = 0L, run_time = 4 / frequency)
# StartAnalogOut(device, channel = 0L)

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/analog-out-basics/four-cycle-sine-burst.png")

## ----inspect_remaining_run, eval = FALSE--------------------------------------
# SetAnalogOutRun(device, channel = 0L, run_time = 1)
# StartAnalogOut(device, channel = 0L)
# Sys.sleep(0.1)
# 
# GetAnalogOutRemainingRun(device, channel = 0L)
# 
# #> [1] 0.8868481

## ----inspect_remaining_repeat, eval = FALSE-----------------------------------
# SetAnalogOutTiming(
#   device,
#   channel = 0L,
#   run_time = 1 / frequency,
#   repeat_count = 1000L
# )
# StartAnalogOut(device, channel = 0L)
# Sys.sleep(0.1)
# 
# GetAnalogOutRemainingRepeat(device, channel = 0L)
# 
# #> [1] 540

## ----generate_three_repeated_sine_bursts, eval = FALSE------------------------
# SetAnalogOutTiming(
#   device,
#   channel = 0L,
#   run_time = 1 / frequency,
#   wait_time = 2 / frequency,
#   repeat_count = 3L
# )
# StartAnalogOut(device, channel = 0L)

## -----------------------------------------------------------------------------
knitr::include_graphics(
  "figures/analog-out-basics/three-repeated-sine-bursts.png"
)

## ----define_custom_waveform---------------------------------------------------
waveform <- c(-0.5, -1, 0, 1, 0.5)

## -----------------------------------------------------------------------------
local({
  old_par <- par(c("mar", "cex"))
  on.exit(par(old_par), add = TRUE)
  
  par(
    mar = c(4.1, 4.1, 1.1, 1.1),
    cex = 0.9
  )
  
  plot(
    seq(from = 0, to = 1, length.out = length(waveform) + 1L),
    c(waveform, waveform[[length(waveform)]]),
    type = "s",
    xlab = "Normalized time",
    ylab = "Normalized level",
    xaxs = "i",
    ylim = c(-1, 1),
    lwd = 2
  )
  
  idle_levels <- c(waveform[[1L]], 0, waveform[[length(waveform)]])
  idle_labels <- c("initial", "offset", "hold")
  abline(
    h = idle_levels,
    col = "gray70",
    lty = "dashed"
  )
  text(
    x = 0.05,
    y = idle_levels + 0.05,
    labels = idle_labels,
    adj = c(0, 0),
    col = "gray40"
  )
})

## ----generate_custom_waveform, eval = FALSE-----------------------------------
# SetCarrierFunction(device, channel = 0L, func = "custom")
# SetCarrierData(device, channel = 0L, data = waveform)
# StartAnalogOut(device, channel = 0L)

## -----------------------------------------------------------------------------
knitr::include_graphics("figures/analog-out-basics/custom-waveform.png")

## ----use_offset_idle_output, eval = FALSE-------------------------------------
# SetAnalogOutIdle(device, channel = 0L, idle = "offset")
# StartAnalogOut(device, channel = 0L)

## -----------------------------------------------------------------------------
knitr::include_graphics(
  "figures/analog-out-basics/custom-waveform-idle-offset.png"
)

## ----use_hold_idle_output, eval = FALSE---------------------------------------
# SetAnalogOutIdle(device, channel = 0L, idle = "hold")
# StartAnalogOut(device, channel = 0L)

## -----------------------------------------------------------------------------
knitr::include_graphics(
  "figures/analog-out-basics/custom-waveform-idle-hold.png"
)

## ----inspect_configured_frequency, eval = FALSE-------------------------------
# GetCarrierFrequency(device, channel = 0L) - frequency
# 
# #> [1] 0.0001899898

## ----configure_independent_channels, eval = FALSE-----------------------------
# ResetAnalogOut(device, channel = 0L)
# ResetAnalogOut(device, channel = 1L)
# 
# EnableCarrier(device, channel = 0L)
# EnableCarrier(device, channel = 1L)
# 
# SetCarrier(
#   device,
#   channel = 0L,
#   func = "sine",
#   frequency = 1000,
#   amplitude = 1,
#   offset = 0,
#   phase = 0
# )
# SetCarrier(
#   device,
#   channel = 1L,
#   func = "sine",
#   frequency = 1000,
#   amplitude = 1.5,
#   offset = 0,
#   phase = 0
# )
# 
# StartAnalogOut(device, channel = 0L)
# StartAnalogOut(device, channel = 1L)

## -----------------------------------------------------------------------------
knitr::include_graphics(
  "figures/analog-out-basics/independent-channels.png"
)

## ----synchronize_analog_out_channels, eval = FALSE----------------------------
# StopAnalogOut(device, channel = 0L)
# StopAnalogOut(device, channel = 1L)
# 
# SetAnalogOutMaster(
#   device,
#   channel = 1L,
#   master_channel = 0L
# )
# 
# ApplyAnalogOutSettings(device, channel = 1L)
# StartAnalogOut(device, channel = 0L)

## -----------------------------------------------------------------------------
knitr::include_graphics(
  "figures/analog-out-basics/synchronized-channels.png"
)

## ----inspect_analog_out_masters, eval = FALSE---------------------------------
# GetAnalogOutMaster(device, channel = 0L)
# #> [1] 0
# 
# GetAnalogOutMaster(device, channel = 1L)
# #> [1] 0

## ----close_device, eval = FALSE-----------------------------------------------
# StopAnalogOut(device, channel = 0L)
# CloseDevice(device)

