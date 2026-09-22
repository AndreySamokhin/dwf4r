# dwf4r

`dwf4r` provides access to Digilent WaveForms hardware from R through the
WaveForms SDK. The package supports Analog Out and Digital Out functionality,
including waveform configuration, timing, triggering, and channel control.


## Installation

### Prerequisites

Before installing `dwf4r`, install the Digilent WaveForms SDK.

On Windows, `dwf4r` expects the SDK to be installed by default in:

```text
C:/Program Files/Digilent/WaveFormsSDK
```

The directory must contain the SDK header and 64-bit library directories:

```text
inc/dwf.h
lib/x64
```

Because `dwf4r` contains compiled C++ code, Windows users also need an
appropriate version of Rtools installed for their version of R.


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

If the WaveForms SDK is installed elsewhere, set the `DWF_SDK_PATH` environment
variable before installing `dwf4r`. The variable must point to the root
directory of the SDK.

For example:

```r
Sys.setenv(DWF_SDK_PATH = "D:/Digilent/WaveFormsSDK")
remotes::install_github("andreysamokhin/dwf4r")
```

`Sys.setenv()` sets the variable for the current R session. For a persistent
configuration, `DWF_SDK_PATH` can instead be defined in the user's `.Renviron`
file or as a Windows environment variable before starting R.


## Documentation

The package reference manual and vignette are available online and, after
installation, in the package *doc/* directory.

* [Reference manual][dwf4r_manual]
* ["Basic Analog Out Functionality" vignette][analog-out-basics_vignette]
* ["Basic Digital Out Functionality" vignette][digital-out-basics_vignette]


<!-- Links -->

[dwf4r_manual]: <https://andreysamokhin.github.io/dwf4r/inst/doc/dwf4r_0.1.0.pdf>
[analog-out-basics_vignette]: <https://andreysamokhin.github.io/dwf4r/inst/doc/analog-out-basics.html>
[digital-out-basics_vignette]: <https://andreysamokhin.github.io/dwf4r/inst/doc/digital-out-basics.html>
