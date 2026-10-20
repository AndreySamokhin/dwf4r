# dwf4r

## dwf4r 0.4.0

- Added high-level Analog Out functions for configuring standard and custom
  waveforms, including waveforms defined by time-voltage points.
- Added high-level Digital Out functions for configuring pulse and custom
  waveforms, including waveforms defined by time-state points.
- Expanded the Analog Out and Digital Out vignettes with high-level waveform
  generation examples.
  
## dwf4r 0.3.0

- Renamed Analog Out carrier functions for consistency with the rest of the
  public API.
- Added optional validation-control arguments to Analog Out and device-level
  functions to support efficient use in higher-level packages and controlled
  operation sequences.
- Refactored Analog Out capability handling so SDK bitmasks are decoded in R.

## dwf4r 0.2.1

- Added build support for Linux and macOS.

## dwf4r 0.2.0

- Added support for Digital Out, including pattern generation, channel
  configuration, timing, and triggering.
- Added the "Basic Digital Out Functionality" vignette.

## dwf4r 0.1.0

- Initial release providing limited access to Digilent WaveForms SDK
  functionality, with a focus on Analog Out.
