#include <Rcpp.h>

#include "dwf.h"
#include "helpers.h"



namespace {

//==============================================================================
using AnalogOutNodeValueGetFun = int (*)(HDWF, int, AnalogOutNode, double*);
double GetAnalogOutNodeValue(
    const int handle,
    const int channel,
    const int node,
    AnalogOutNodeValueGetFun getter,
    const char* function_name) {

  const HDWF hdwf = AsHandle(handle);
  const AnalogOutNode node_enum = static_cast<AnalogOutNode>(node);
  double value = 0.0;

  if (!getter(hdwf, channel, node_enum, &value)) {
    ThrowDwfError(function_name);
  }

  return value;
}

}  // namespace



//==============================================================================
// Resets the Analog Out parameters of a selected channel to their SDK defaults.
//
// [[Rcpp::export(name = ".AnalogOutResetC")]]
void AnalogOutReset(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutReset(hdwf, channel)) {
    ThrowDwfError("FDwfAnalogOutReset");
  }
}



//==============================================================================
// Starts (action_code = 1), stops (action_code = 0), or dynamically applies
// (action_code = 3) the configuration of an Analog Out channel.
//
// [[Rcpp::export(name = ".AnalogOutConfigureC")]]
void AnalogOutConfigure(const int handle,
                        const int channel,
                        const int action_code) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutConfigure(hdwf, channel, action_code)) {
    ThrowDwfError("FDwfAnalogOutConfigure");
  }
}



//==============================================================================
// Retrieves the current state of a selected Analog Out channel as a string.
//
// [[Rcpp::export(name = ".AnalogOutStatusC")]]
std::string AnalogOutStatus(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  DwfState state = DwfStateReady;

  if (!FDwfAnalogOutStatus(hdwf, channel, &state)) {
    ThrowDwfError("FDwfAnalogOutStatus");
  }

  switch (state) {
    case DwfStateReady: return "ready";
    case DwfStateArmed: return "armed";
    case DwfStateDone: return "done";
    case DwfStateRunning: return "running";
    case DwfStateConfig: return "config";
    case DwfStatePrefill: return "prefill";
    case DwfStateNotDone: return "not_done";
    case DwfStateWait: return "wait";
    default: return "unknown";
  }
}



//==============================================================================
// Sets the enable or operating mode of a selected Analog Out node.
//
// [[Rcpp::export(name = ".AnalogOutNodeEnableSetC")]]
void AnalogOutNodeEnableSet(const int handle,
                            const int channel,
                            const int node,
                            const int mode) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutNodeEnableSet(
      hdwf,
      channel,
      static_cast<AnalogOutNode>(node),
      mode)) {
    ThrowDwfError("FDwfAnalogOutNodeEnableSet");
  }
}



//==============================================================================
// Retrieves the operating mode of a selected Analog Out node.
//
// [[Rcpp::export(name = ".AnalogOutNodeEnableGetC")]]
int AnalogOutNodeEnableGet(const int handle,
                           const int channel,
                           const int node) {
  const HDWF hdwf = AsHandle(handle);
  int mode = 0;
  if (!FDwfAnalogOutNodeEnableGet(
      hdwf,
      channel,
      static_cast<AnalogOutNode>(node),
      &mode)) {
    ThrowDwfError("FDwfAnalogOutNodeEnableGet");
  }
  return mode;
}



//==============================================================================
// Sets the generator waveform function of a selected Analog Out node.
//
// [[Rcpp::export(name = ".AnalogOutNodeFunctionSetC")]]
void AnalogOutNodeFunctionSet(const int handle,
                              const int channel,
                              const int node,
                              const int function_code) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutNodeFunctionSet(
      hdwf,
      channel,
      static_cast<AnalogOutNode>(node),
      static_cast<FUNC>(function_code))) {
    ThrowDwfError("FDwfAnalogOutNodeFunctionSet");
  }
}



//==============================================================================
// Retrieves the configured waveform function of a selected Analog Out node.
//
// [[Rcpp::export(name = ".AnalogOutNodeFunctionGetC")]]
int AnalogOutNodeFunctionGet(const int handle,
                             const int channel,
                             const int node) {
  const HDWF hdwf = AsHandle(handle);
  FUNC function_code = funcDC;
  if (!FDwfAnalogOutNodeFunctionGet(
      hdwf,
      channel,
      static_cast<AnalogOutNode>(node),
      &function_code)) {
    ThrowDwfError("FDwfAnalogOutNodeFunctionGet");
  }
  return static_cast<int>(function_code);
}



//==============================================================================
// Sets the frequency or sample-update rate of a selected Analog Out node.
//
// [[Rcpp::export(name = ".AnalogOutNodeFrequencySetC")]]
void AnalogOutNodeFrequencySet(const int handle,
                               const int channel,
                               const int node,
                               const double value) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutNodeFrequencySet(
      hdwf,
      channel,
      static_cast<AnalogOutNode>(node),
      value)) {
    ThrowDwfError("FDwfAnalogOutNodeFrequencySet");
  }
}



//==============================================================================
// Retrieves the configured frequency or sample-update rate of a selected Analog
// Out node.
//
// [[Rcpp::export(name = ".AnalogOutNodeFrequencyGetC")]]
double AnalogOutNodeFrequencyGet(const int handle,
                                 const int channel,
                                 const int node) {
  return GetAnalogOutNodeValue(
    handle,
    channel,
    node,
    FDwfAnalogOutNodeFrequencyGet,
    "FDwfAnalogOutNodeFrequencyGet"
  );
}



//==============================================================================
// Sets the carrier amplitude or modulation index of a selected Analog Out node.
//
// [[Rcpp::export(name = ".AnalogOutNodeAmplitudeSetC")]]
void AnalogOutNodeAmplitudeSet(const int handle,
                               const int channel,
                               const int node,
                               const double value) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutNodeAmplitudeSet(
      hdwf,
      channel,
      static_cast<AnalogOutNode>(node),
      value)) {
    ThrowDwfError("FDwfAnalogOutNodeAmplitudeSet");
  }
}



//==============================================================================
// Retrieves the configured carrier amplitude or modulation index of a selected
// Analog Out node.
//
// [[Rcpp::export(name = ".AnalogOutNodeAmplitudeGetC")]]
double AnalogOutNodeAmplitudeGet(const int handle,
                                 const int channel,
                                 const int node) {
  return GetAnalogOutNodeValue(
    handle,
    channel,
    node,
    FDwfAnalogOutNodeAmplitudeGet,
    "FDwfAnalogOutNodeAmplitudeGet"
  );
}



//==============================================================================
// Sets the voltage or modulation offset of a selected Analog Out node.
//
// [[Rcpp::export(name = ".AnalogOutNodeOffsetSetC")]]
void AnalogOutNodeOffsetSet(const int handle,
                            const int channel,
                            const int node,
                            const double value) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutNodeOffsetSet(
      hdwf,
      channel,
      static_cast<AnalogOutNode>(node),
      value)) {
    ThrowDwfError("FDwfAnalogOutNodeOffsetSet");
  }
}



//==============================================================================
// Retrieves the configured voltage or modulation offset of a selected Analog
// Out node.
//
// [[Rcpp::export(name = ".AnalogOutNodeOffsetGetC")]]
double AnalogOutNodeOffsetGet(const int handle,
                              const int channel,
                              const int node) {
  return GetAnalogOutNodeValue(
    handle,
    channel,
    node,
    FDwfAnalogOutNodeOffsetGet,
    "FDwfAnalogOutNodeOffsetGet"
  );
}



//==============================================================================
// Sets the waveform symmetry or duty cycle of a selected Analog Out node.
//
// [[Rcpp::export(name = ".AnalogOutNodeSymmetrySetC")]]
void AnalogOutNodeSymmetrySet(const int handle,
                              const int channel,
                              const int node,
                              const double value) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutNodeSymmetrySet(
      hdwf,
      channel,
      static_cast<AnalogOutNode>(node),
      value)) {
    ThrowDwfError("FDwfAnalogOutNodeSymmetrySet");
  }
}



//==============================================================================
// Retrieves the configured waveform symmetry or duty cycle of a selected Analog
// Out node.
//
// [[Rcpp::export(name = ".AnalogOutNodeSymmetryGetC")]]
double AnalogOutNodeSymmetryGet(const int handle,
                                const int channel,
                                const int node) {
  return GetAnalogOutNodeValue(
    handle,
    channel,
    node,
    FDwfAnalogOutNodeSymmetryGet,
    "FDwfAnalogOutNodeSymmetryGet"
  );
}



//==============================================================================
// Sets the phase of a selected Analog Out node in degrees.
//
// [[Rcpp::export(name = ".AnalogOutNodePhaseSetC")]]
void AnalogOutNodePhaseSet(const int handle,
                           const int channel,
                           const int node,
                           const double value) {
  const HDWF hdwf = AsHandle(handle);

  if (!FDwfAnalogOutNodePhaseSet(
      hdwf,
      channel,
      static_cast<AnalogOutNode>(node),
      value)) {
    ThrowDwfError("FDwfAnalogOutNodePhaseSet");
  }
}



//==============================================================================
// Retrieves the configured phase of a selected Analog Out node in degrees.
//
// [[Rcpp::export(name = ".AnalogOutNodePhaseGetC")]]
double AnalogOutNodePhaseGet(const int handle,
                             const int channel,
                             const int node) {
  return GetAnalogOutNodeValue(
    handle,
    channel,
    node,
    FDwfAnalogOutNodePhaseGet,
    "FDwfAnalogOutNodePhaseGet"
  );
}



//==============================================================================
// Sets normalized custom-waveform data or prefills the play buffer of a
// selected Analog Out node.
//
// [[Rcpp::export(name = ".AnalogOutNodeDataSetC")]]
void AnalogOutNodeDataSet(const int handle,
                          const int channel,
                          const int node,
                          Rcpp::NumericVector data) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutNodeDataSet(
      hdwf,
      channel,
      static_cast<AnalogOutNode>(node),
      REAL(data),
      static_cast<int>(data.size()))) {
    ThrowDwfError("FDwfAnalogOutNodeDataSet");
  }
}



//==============================================================================
// Sets the trigger source of a selected Analog Out channel.
//
// [[Rcpp::export(name = ".AnalogOutTriggerSourceSetC")]]
void AnalogOutTriggerSourceSet(const int handle,
                               const int channel,
                               const int source_code) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutTriggerSourceSet(
      hdwf,
      channel,
      static_cast<TRIGSRC>(source_code))) {
    ThrowDwfError("FDwfAnalogOutTriggerSourceSet");
  }
}



//==============================================================================
// Retrieves the configured trigger source of a selected Analog Out channel.
//
// [[Rcpp::export(name = ".AnalogOutTriggerSourceGetC")]]
int AnalogOutTriggerSourceGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  TRIGSRC source = trigsrcNone;
  if (!FDwfAnalogOutTriggerSourceGet(hdwf, channel, &source)) {
    ThrowDwfError("FDwfAnalogOutTriggerSourceGet");
  }
  return static_cast<int>(source);
}



//==============================================================================
// Sets the trigger slope of a selected Analog Out channel.
//
// [[Rcpp::export(name = ".AnalogOutTriggerSlopeSetC")]]
void AnalogOutTriggerSlopeSet(const int handle,
                              const int channel,
                              const int slope_code) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutTriggerSlopeSet(
      hdwf,
      channel,
      static_cast<DwfTriggerSlope>(slope_code))) {
    ThrowDwfError("FDwfAnalogOutTriggerSlopeSet");
  }
}



//==============================================================================
// Retrieves the configured trigger slope of a selected Analog Out channel.
//
// [[Rcpp::export(name = ".AnalogOutTriggerSlopeGetC")]]
int AnalogOutTriggerSlopeGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  DwfTriggerSlope slope = DwfTriggerSlopeRise;
  if (!FDwfAnalogOutTriggerSlopeGet(hdwf, channel, &slope)) {
    ThrowDwfError("FDwfAnalogOutTriggerSlopeGet");
  }
  return static_cast<int>(slope);
}



//==============================================================================
// Sets the state-machine master of a selected Analog Out channel.
//
// [[Rcpp::export(name = ".AnalogOutMasterSetC")]]
void AnalogOutMasterSet(const int handle,
                        const int channel,
                        const int master_channel) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutMasterSet(hdwf, channel, master_channel)) {
    ThrowDwfError("FDwfAnalogOutMasterSet");
  }
}



//==============================================================================
// Retrieves the state-machine master of a selected Analog Out channel.
//
// [[Rcpp::export(name = ".AnalogOutMasterGetC")]]
int AnalogOutMasterGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  int master_channel = 0;
  if (!FDwfAnalogOutMasterGet(hdwf, channel, &master_channel)) {
    ThrowDwfError("FDwfAnalogOutMasterGet");
  }
  return master_channel;
}



//==============================================================================
// Sets the run duration of a selected Analog Out channel in seconds.
//
// [[Rcpp::export(name = ".AnalogOutRunSetC")]]
void AnalogOutRunSet(const int handle,
                     const int channel,
                     const double time_s) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutRunSet(hdwf, channel, time_s)) {
    ThrowDwfError("FDwfAnalogOutRunSet");
  }
}



//==============================================================================
// Retrieves the configured run duration of a selected Analog Out channel.
//
// [[Rcpp::export(name = ".AnalogOutRunGetC")]]
double AnalogOutRunGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  double time_s = 0.0;
  if (!FDwfAnalogOutRunGet(hdwf, channel, &time_s)) {
    ThrowDwfError("FDwfAnalogOutRunGet");
  }
  return time_s;
}



//==============================================================================
// Retrieves the remaining run duration from the last Analog Out status update
// performed with 'AnalogOutStatus()' or 'FDwfAnalogOutStatus()'.
//
// [[Rcpp::export(name = ".AnalogOutRunStatusC")]]
double AnalogOutRunStatus(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  double time_s = 0.0;
  if (!FDwfAnalogOutRunStatus(hdwf, channel, &time_s)) {
    ThrowDwfError("FDwfAnalogOutRunStatus");
  }
  return time_s;
}



//==============================================================================
// Sets the post-trigger wait duration of a selected Analog Out channel in
// seconds.
//
// [[Rcpp::export(name = ".AnalogOutWaitSetC")]]
void AnalogOutWaitSet(const int handle,
                      const int channel,
                      const double time_s) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutWaitSet(hdwf, channel, time_s)) {
    ThrowDwfError("FDwfAnalogOutWaitSet");
  }
}



//==============================================================================
// Retrieves the configured wait duration of a selected Analog Out channel.
//
// [[Rcpp::export(name = ".AnalogOutWaitGetC")]]
double AnalogOutWaitGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  double time_s = 0.0;
  if (!FDwfAnalogOutWaitGet(hdwf, channel, &time_s)) {
    ThrowDwfError("FDwfAnalogOutWaitGet");
  }
  return time_s;
}



//==============================================================================
// Sets the number of wait-run cycles performed by a selected Analog Out
// channel.
//
// [[Rcpp::export(name = ".AnalogOutRepeatSetC")]]
void AnalogOutRepeatSet(const int handle,
                        const int channel,
                        const int repeat_count) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutRepeatSet(hdwf, channel, repeat_count)) {
    ThrowDwfError("FDwfAnalogOutRepeatSet");
  }
}



//==============================================================================
// Retrieves the configured repeat count of a selected Analog Out channel.
//
// [[Rcpp::export(name = ".AnalogOutRepeatGetC")]]
int AnalogOutRepeatGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  int repeat_count = 0;
  if (!FDwfAnalogOutRepeatGet(hdwf, channel, &repeat_count)) {
    ThrowDwfError("FDwfAnalogOutRepeatGet");
  }
  return repeat_count;
}



//==============================================================================
// Retrieves the remaining repeat count from the last Analog Out status update.
//
// [[Rcpp::export(name = ".AnalogOutRepeatStatusC")]]
int AnalogOutRepeatStatus(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  int repeat_count = 0;
  if (!FDwfAnalogOutRepeatStatus(hdwf, channel, &repeat_count)) {
    ThrowDwfError("FDwfAnalogOutRepeatStatus");
  }
  return repeat_count;
}



//==============================================================================
// Sets whether each Analog Out repetition requires a new trigger.
//
// [[Rcpp::export(name = ".AnalogOutRepeatTriggerSetC")]]
void AnalogOutRepeatTriggerSet(const int handle,
                               const int channel,
                               const bool repeat_trigger) {
  const HDWF hdwf = AsHandle(handle);

  if (!FDwfAnalogOutRepeatTriggerSet(
      hdwf,
      channel,
      repeat_trigger ? 1 : 0)) {
    ThrowDwfError("FDwfAnalogOutRepeatTriggerSet");
  }
}



//==============================================================================
// Retrieves whether each Analog Out repetition requires a new trigger.
//
// [[Rcpp::export(name = ".AnalogOutRepeatTriggerGetC")]]
bool AnalogOutRepeatTriggerGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  int repeat_trigger = 0;
  if (!FDwfAnalogOutRepeatTriggerGet(
      hdwf,
      channel,
      &repeat_trigger)) {
    ThrowDwfError("FDwfAnalogOutRepeatTriggerGet");
  }
  return (repeat_trigger != 0);
}



//==============================================================================
// Sets the output behavior of a selected Analog Out channel while it is not
// running.
//
// [[Rcpp::export(name = ".AnalogOutIdleSetC")]]
void AnalogOutIdleSet(const int handle,
                      const int channel,
                      const int idle_mode_code) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfAnalogOutIdleSet(
      hdwf,
      channel,
      static_cast<DwfAnalogOutIdle>(idle_mode_code))) {
    ThrowDwfError("FDwfAnalogOutIdleSet");
  }
}



//==============================================================================
// Retrieves the configured idle output mode of a selected Analog Out channel.
//
// [[Rcpp::export(name = ".AnalogOutIdleGetC")]]
int AnalogOutIdleGet(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  DwfAnalogOutIdle idle_mode = DwfAnalogOutIdleDisable;

  if (!FDwfAnalogOutIdleGet(hdwf, channel, &idle_mode)) {
    ThrowDwfError("FDwfAnalogOutIdleGet");
  }

  return static_cast<int>(idle_mode);
}


