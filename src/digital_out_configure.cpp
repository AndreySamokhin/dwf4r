#include <Rcpp.h>

#include "dwf.h"
#include "helpers.h"



//==============================================================================
// Resets all Digital Out parameters to their SDK defaults.
//
// [[Rcpp::export(name = ".DigitalOutResetC")]]
void DigitalOutReset(const int handle) {
  const HDWF hdwf = AsHandle(handle);

  if (!FDwfDigitalOutReset(hdwf)) {
    ThrowDwfError("FDwfDigitalOutReset");
  }
}



//==============================================================================
// Starts or stops the Digital Out instrument.
//
// [[Rcpp::export(name = ".DigitalOutConfigureC")]]
void DigitalOutConfigure(const int handle, const bool start) {
  const HDWF hdwf = AsHandle(handle);

  if (!FDwfDigitalOutConfigure(hdwf, start ? 1 : 0)) {
    ThrowDwfError("FDwfDigitalOutConfigure");
  }
}



//==============================================================================
// Retrieves the current state of the Digital Out instrument as a string.
//
// [[Rcpp::export(name = ".DigitalOutStatusC")]]
std::string DigitalOutStatus(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  DwfState state = DwfStateReady;

  if (!FDwfDigitalOutStatus(hdwf, &state)) {
    ThrowDwfError("FDwfDigitalOutStatus");
  }

  return ConvertDwfStateToName(state);
}



//==============================================================================
// Sets whether a selected Digital Out channel is enabled.
//
// [[Rcpp::export(name = ".DigitalOutEnableSetC")]]
void DigitalOutEnableSet(const int handle,
                         const int channel,
                         const bool enable) {
  const HDWF hdwf = AsHandle(handle);

  if (!FDwfDigitalOutEnableSet(
      hdwf,
      channel,
      enable ? 1 : 0)) {
    ThrowDwfError("FDwfDigitalOutEnableSet");
  }
}



//==============================================================================
// Retrieves whether a selected Digital Out channel is enabled.
//
// [[Rcpp::export(name = ".DigitalOutEnableGetC")]]
bool DigitalOutEnableGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  int enable = 0;

  if (!FDwfDigitalOutEnableGet(hdwf, channel, &enable)) {
    ThrowDwfError("FDwfDigitalOutEnableGet");
  }

  return (enable != 0);
}



//==============================================================================
// Sets the output mode of a selected Digital Out channel.
//
// [[Rcpp::export(name = ".DigitalOutOutputSetC")]]
void DigitalOutOutputSet(const int handle,
                         const int channel,
                         const int output_code) {
  const HDWF hdwf = AsHandle(handle);

  if (!FDwfDigitalOutOutputSet(
      hdwf,
      channel,
      static_cast<DwfDigitalOutOutput>(output_code))) {
    ThrowDwfError("FDwfDigitalOutOutputSet");
  }
}



//==============================================================================
// Retrieves the configured output mode of a selected Digital Out channel.
//
// [[Rcpp::export(name = ".DigitalOutOutputGetC")]]
int DigitalOutOutputGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  DwfDigitalOutOutput output = DwfDigitalOutOutputPushPull;

  if (!FDwfDigitalOutOutputGet(hdwf, channel, &output)) {
    ThrowDwfError("FDwfDigitalOutOutputGet");
  }

  return static_cast<int>(output);
}



//==============================================================================
// Sets the signal-generation type of a selected Digital Out channel.
//
// [[Rcpp::export(name = ".DigitalOutTypeSetC")]]
void DigitalOutTypeSet(const int handle,
                       const int channel,
                       const int type_code) {
  const HDWF hdwf = AsHandle(handle);

  if (!FDwfDigitalOutTypeSet(
      hdwf,
      channel,
      static_cast<DwfDigitalOutType>(type_code))) {
    ThrowDwfError("FDwfDigitalOutTypeSet");
  }
}



//==============================================================================
// Retrieves the configured signal-generation type of a selected Digital Out
// channel.
//
// [[Rcpp::export(name = ".DigitalOutTypeGetC")]]
int DigitalOutTypeGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  DwfDigitalOutType type = DwfDigitalOutTypePulse;

  if (!FDwfDigitalOutTypeGet(hdwf, channel, &type)) {
    ThrowDwfError("FDwfDigitalOutTypeGet");
  }

  return static_cast<int>(type);
}



//==============================================================================
// Sets the idle output mode of a selected Digital Out channel.
//
// [[Rcpp::export(name = ".DigitalOutIdleSetC")]]
void DigitalOutIdleSet(const int handle,
                       const int channel,
                       const int idle_code) {
  const HDWF hdwf = AsHandle(handle);

  if (!FDwfDigitalOutIdleSet(
      hdwf,
      channel,
      static_cast<DwfDigitalOutIdle>(idle_code))) {
    ThrowDwfError("FDwfDigitalOutIdleSet");
  }
}



//==============================================================================
// Retrieves the configured idle output mode of a selected Digital Out channel.
//
// [[Rcpp::export(name = ".DigitalOutIdleGetC")]]
int DigitalOutIdleGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  DwfDigitalOutIdle idle = DwfDigitalOutIdleInit;

  if (!FDwfDigitalOutIdleGet(hdwf, channel, &idle)) {
    ThrowDwfError("FDwfDigitalOutIdleGet");
  }

  return static_cast<int>(idle);
}



//==============================================================================
// Sets the ordinary clock divider of a selected Digital Out channel.
//
// [[Rcpp::export(name = ".DigitalOutDividerSetC")]]
void DigitalOutDividerSet(const int handle,
                          const int channel,
                          const double divider) {
  const HDWF hdwf = AsHandle(handle);
  const unsigned int divider_uint = AsUnsignedInt(divider, "divider");

  if (!FDwfDigitalOutDividerSet(hdwf, channel, divider_uint)) {
    ThrowDwfError("FDwfDigitalOutDividerSet");
  }
}



//==============================================================================
// Retrieves the configured ordinary clock divider.
//
// [[Rcpp::export(name = ".DigitalOutDividerGetC")]]
double DigitalOutDividerGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  unsigned int divider = 0U;

  if (!FDwfDigitalOutDividerGet(hdwf, channel, &divider)) {
    ThrowDwfError("FDwfDigitalOutDividerGet");
  }

  // Use double to preserve the full 32-bit unsigned range when returned to R.
  return static_cast<double>(divider);
}



//==============================================================================
// Sets the divider value initially loaded when Digital Out enters Running.
//
// [[Rcpp::export(name = ".DigitalOutDividerInitSetC")]]
void DigitalOutDividerInitSet(const int handle,
                              const int channel,
                              const double initial_divider) {
  const HDWF hdwf = AsHandle(handle);
  const unsigned int divider_uint =
    AsUnsignedInt(initial_divider, "initial_divider");

  if (!FDwfDigitalOutDividerInitSet(hdwf, channel, divider_uint)) {
    ThrowDwfError("FDwfDigitalOutDividerInitSet");
  }
}



//==============================================================================
// Retrieves the configured initial clock divider.
//
// [[Rcpp::export(name = ".DigitalOutDividerInitGetC")]]
double DigitalOutDividerInitGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  unsigned int initial_divider = 0U;

  if (!FDwfDigitalOutDividerInitGet(hdwf, channel, &initial_divider)) {
    ThrowDwfError("FDwfDigitalOutDividerInitGet");
  }

  // Use double to preserve the full 32-bit unsigned range when returned to R.
  return static_cast<double>(initial_divider);
}



//==============================================================================
// Sets the low- and high-state counter values of a Digital Out channel.
//
// [[Rcpp::export(name = ".DigitalOutCounterSetC")]]
void DigitalOutCounterSet(const int handle,
                          const int channel,
                          const double low_count,
                          const double high_count) {
  const HDWF hdwf = AsHandle(handle);
  const unsigned int low_count_uint = AsUnsignedInt(low_count, "low_count");
  const unsigned int high_count_uint = AsUnsignedInt(high_count, "high_count");

  if (!FDwfDigitalOutCounterSet(
      hdwf,
      channel,
      low_count_uint,
      high_count_uint)) {
    ThrowDwfError("FDwfDigitalOutCounterSet");
  }
}



//==============================================================================
// Retrieves the configured low- and high-state counter values.
//
// [[Rcpp::export(name = ".DigitalOutCounterGetC")]]
Rcpp::NumericVector DigitalOutCounterGet(const int handle,
                                         const int channel) {
  const HDWF hdwf = AsHandle(handle);
  unsigned int low_count = 0U;
  unsigned int high_count = 0U;

  if (!FDwfDigitalOutCounterGet(
      hdwf,
      channel,
      &low_count,
      &high_count)) {
      ThrowDwfError("FDwfDigitalOutCounterGet");
  }

  // Use double to preserve the full 32-bit unsigned range when returned to R.
  return Rcpp::NumericVector::create(
    Rcpp::Named("low_count") = static_cast<double>(low_count),
    Rcpp::Named("high_count") = static_cast<double>(high_count)
  );
}



//==============================================================================
// Sets the initial output state and counter value of a Digital Out channel.
//
// [[Rcpp::export(name = ".DigitalOutCounterInitSetC")]]
void DigitalOutCounterInitSet(const int handle,
                              const int channel,
                              const bool initial_high,
                              const double initial_count) {
  const HDWF hdwf = AsHandle(handle);
  const unsigned int initial_count_uint =
    AsUnsignedInt(initial_count, "initial_count");

  if (!FDwfDigitalOutCounterInitSet(
      hdwf,
      channel,
      initial_high ? 1 : 0,
      initial_count_uint)) {
    ThrowDwfError("FDwfDigitalOutCounterInitSet");
  }
}



//==============================================================================
// Retrieves the configured initial output state and counter value.
//
// [[Rcpp::export(name = ".DigitalOutCounterInitGetC")]]
Rcpp::List DigitalOutCounterInitGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  int initial_high = 0;
  unsigned int initial_count = 0U;

  if (!FDwfDigitalOutCounterInitGet(
      hdwf,
      channel,
      &initial_high,
      &initial_count)) {
      ThrowDwfError("FDwfDigitalOutCounterInitGet");
  }

  // Use double to preserve the full 32-bit unsigned range when returned to R.
  return Rcpp::List::create(
    Rcpp::Named("initial_high") = (initial_high != 0),
    Rcpp::Named("initial_count") = static_cast<double>(initial_count)
  );
}



//==============================================================================
// Sets custom data of a selected Digital Out channel.
//
// [[Rcpp::export(name = ".DigitalOutDataSetC")]]
void DigitalOutDataSet(const int handle,
                       const int channel,
                       Rcpp::RawVector data,
                       const unsigned int n_bits) {
  const HDWF hdwf = AsHandle(handle);
  unsigned char* data_ptr = RAW(data);

  if (n_bits > data.size() * 8) {
    Rcpp::stop("'data' is too short for 'n_bits'.");
  }

  if (!FDwfDigitalOutDataSet(
      hdwf,
      channel,
      data_ptr,
      n_bits)) {
    ThrowDwfError("FDwfDigitalOutDataSet");
  }
}



//==============================================================================
// Sets the run duration of the Digital Out instrument in seconds.
//
// [[Rcpp::export(name = ".DigitalOutRunSetC")]]
void DigitalOutRunSet(const int handle, const double time_s) {
  const HDWF hdwf = AsHandle(handle);

  if (!FDwfDigitalOutRunSet(hdwf, time_s)) {
    ThrowDwfError("FDwfDigitalOutRunSet");
  }
}



//==============================================================================
// Retrieves the configured run duration of the Digital Out instrument.
//
// [[Rcpp::export(name = ".DigitalOutRunGetC")]]
double DigitalOutRunGet(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  double time_s = 0.0;

  if (!FDwfDigitalOutRunGet(hdwf, &time_s)) {
    ThrowDwfError("FDwfDigitalOutRunGet");
  }

  return time_s;
}



//==============================================================================
// Retrieves the remaining run value from the last Digital Out status update.
//
// [[Rcpp::export(name = ".DigitalOutRunStatusC")]]
double DigitalOutRunStatus(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  double time_s = 0.0;

  if (!FDwfDigitalOutRunStatus(hdwf, &time_s)) {
    ThrowDwfError("FDwfDigitalOutRunStatus");
  }

  return time_s;
}



//==============================================================================
// Sets the post-trigger wait duration of the Digital Out instrument in seconds.
//
// [[Rcpp::export(name = ".DigitalOutWaitSetC")]]
void DigitalOutWaitSet(const int handle, const double time_s) {
  const HDWF hdwf = AsHandle(handle);

  if (!FDwfDigitalOutWaitSet(hdwf, time_s)) {
    ThrowDwfError("FDwfDigitalOutWaitSet");
  }
}



//==============================================================================
// Retrieves the configured wait duration of the Digital Out instrument.
//
// [[Rcpp::export(name = ".DigitalOutWaitGetC")]]
double DigitalOutWaitGet(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  double time_s = 0.0;

  if (!FDwfDigitalOutWaitGet(hdwf, &time_s)) {
    ThrowDwfError("FDwfDigitalOutWaitGet");
  }

  return time_s;
}



//==============================================================================
// Sets the number of wait-run cycles performed by the Digital Out instrument.
//
// [[Rcpp::export(name = ".DigitalOutRepeatSetC")]]
void DigitalOutRepeatSet(const int handle, const double repeat_count) {
  const HDWF hdwf = AsHandle(handle);
  const unsigned int repeat_count_uint =
    AsUnsignedInt(repeat_count, "repeat_count");

  if (!FDwfDigitalOutRepeatSet(hdwf, repeat_count_uint)) {
    ThrowDwfError("FDwfDigitalOutRepeatSet");
  }
}



//==============================================================================
// Retrieves the configured repeat count of the Digital Out instrument.
//
// [[Rcpp::export(name = ".DigitalOutRepeatGetC")]]
double DigitalOutRepeatGet(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  unsigned int repeat_count = 0U;

  if (!FDwfDigitalOutRepeatGet(hdwf, &repeat_count)) {
    ThrowDwfError("FDwfDigitalOutRepeatGet");
  }

  // Use double to preserve the full 32-bit unsigned range when returned to R.
  return static_cast<double>(repeat_count);
}



//==============================================================================
// Retrieves the remaining repeat count from the last Digital Out status update
// performed with 'DigitalOutStatus()' or 'FDwfDigitalOutStatus()'.
//
// [[Rcpp::export(name = ".DigitalOutRepeatStatusC")]]
double DigitalOutRepeatStatus(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  unsigned int repeat_count = 0U;

  if (!FDwfDigitalOutRepeatStatus(hdwf, &repeat_count)) {
    ThrowDwfError("FDwfDigitalOutRepeatStatus");
  }

  // Use double to preserve the full 32-bit unsigned range when returned to R.
  return static_cast<double>(repeat_count);
}



//==============================================================================
// Sets the trigger source of the Digital Out instrument.
//
// [[Rcpp::export(name = ".DigitalOutTriggerSourceSetC")]]
void DigitalOutTriggerSourceSet(const int handle,
                                const int source_code) {
  const HDWF hdwf = AsHandle(handle);

  if (!FDwfDigitalOutTriggerSourceSet(
      hdwf,
      static_cast<TRIGSRC>(source_code))) {
    ThrowDwfError("FDwfDigitalOutTriggerSourceSet");
  }
}



//==============================================================================
// Retrieves the configured trigger source of the Digital Out instrument.
//
// [[Rcpp::export(name = ".DigitalOutTriggerSourceGetC")]]
int DigitalOutTriggerSourceGet(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  TRIGSRC source = trigsrcNone;

  if (!FDwfDigitalOutTriggerSourceGet(hdwf, &source)) {
    ThrowDwfError("FDwfDigitalOutTriggerSourceGet");
  }

  return static_cast<int>(source);
}



//==============================================================================
// Sets the trigger slope of the Digital Out instrument.
//
// [[Rcpp::export(name = ".DigitalOutTriggerSlopeSetC")]]
void DigitalOutTriggerSlopeSet(const int handle,
                               const int slope_code) {
  const HDWF hdwf = AsHandle(handle);

  if (!FDwfDigitalOutTriggerSlopeSet(
      hdwf,
      static_cast<DwfTriggerSlope>(slope_code))) {
    ThrowDwfError("FDwfDigitalOutTriggerSlopeSet");
  }
}



//==============================================================================
// Retrieves the configured trigger slope of the Digital Out instrument.
//
// [[Rcpp::export(name = ".DigitalOutTriggerSlopeGetC")]]
int DigitalOutTriggerSlopeGet(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  DwfTriggerSlope slope = DwfTriggerSlopeRise;

  if (!FDwfDigitalOutTriggerSlopeGet(hdwf, &slope)) {
    ThrowDwfError("FDwfDigitalOutTriggerSlopeGet");
  }

  return static_cast<int>(slope);
}



//==============================================================================
// Sets whether a trigger is included in each Digital Out wait-run repeat cycle.
//
// [[Rcpp::export(name = ".DigitalOutRepeatTriggerSetC")]]
void DigitalOutRepeatTriggerSet(const int handle,
                                const bool repeat_trigger) {
  const HDWF hdwf = AsHandle(handle);

  if (!FDwfDigitalOutRepeatTriggerSet(
      hdwf,
      repeat_trigger ? 1 : 0)) {
    ThrowDwfError("FDwfDigitalOutRepeatTriggerSet");
  }
}



//==============================================================================
// Retrieves whether a trigger is included in each Digital Out repeat cycle.
//
// [[Rcpp::export(name = ".DigitalOutRepeatTriggerGetC")]]
bool DigitalOutRepeatTriggerGet(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  int repeat_trigger = 0;

  if (!FDwfDigitalOutRepeatTriggerGet(hdwf, &repeat_trigger)) {
    ThrowDwfError("FDwfDigitalOutRepeatTriggerGet");
  }

  return (repeat_trigger != 0);
}


