# dwf4r

`dwf4r` provides access to Digilent WaveForms hardware from R through the
WaveForms SDK. The package supports Analog Out and Digital Out functionality,
including waveform configuration, timing, triggering, and channel control.


## Installation

### Prerequisites

Before installing `dwf4r`, install the Digilent WaveForms SDK.

By default, `dwf4r` expects the SDK files in the standard locations used by
WaveForms:

- Windows: `C:/Program Files/Digilent/WaveFormsSDK`
- Linux: `/usr/include/digilent/waveforms` for SDK headers, with the DWF library
  available through the system library search path
- macOS: `/Library/Frameworks/dwf.framework`

Because `dwf4r` contains compiled C++ code, an appropriate C++ build toolchain
is also required. On Windows, install the version of Rtools appropriate for your
version of R. Linux and macOS builds additionally require GNU make.


### Install from GitHub

Install the `remotes` package if it is not already available:

```r
install.packages("remotes")
```

Then install `dwf4r` from GitHub:

```r
remotes::install_github("andreysamokhin/dwf4r")
```


### Using a non-default SDK location

If the WaveForms SDK is installed in a non-default location, set the
corresponding environment variable before installing `dwf4r`.

On Windows, `DWF_SDK_PATH` should point to the SDK root directory:

```r
Sys.setenv(DWF_SDK_PATH = "D:/Digilent/WaveFormsSDK")
```

On Linux, `DWF_INCLUDE_DIR` should point to the directory containing `dwf.h`.
If the DWF library is also outside the system library search path, set
`DWF_LIBRARY_DIR` as well:

```r
Sys.setenv(
  DWF_INCLUDE_DIR = "/opt/digilent/include/digilent/waveforms",
  DWF_LIBRARY_DIR = "/opt/digilent/lib"
)
```

On macOS, `DWF_FRAMEWORKS_DIR` should point to the directory containing
`dwf.framework`:

```r
Sys.setenv(DWF_FRAMEWORKS_DIR = "/opt/digilent/Frameworks")
```

Then install the package normally:

```r
remotes::install_github("andreysamokhin/dwf4r")
```

`Sys.setenv()` sets environment variables for the current R session. For a
persistent configuration, they can instead be defined in the user's `.Renviron`
file or as system environment variables before starting R.


## Documentation

The package reference manual and vignette are available online and, after
installation, in the package *doc/* directory.

- [Reference manual][dwf4r_manual]
- ["Basic Analog Out Functionality" vignette][analog-out-basics_vignette]
- ["Basic Digital Out Functionality" vignette][digital-out-basics_vignette]


<!-- Links -->

[dwf4r_manual]: <https://andreysamokhin.github.io/dwf4r/inst/doc/dwf4r_0.2.0.pdf>
[analog-out-basics_vignette]: <https://andreysamokhin.github.io/dwf4r/inst/doc/analog-out-basics.html>
[digital-out-basics_vignette]: <https://andreysamokhin.github.io/dwf4r/inst/doc/digital-out-basics.html>
