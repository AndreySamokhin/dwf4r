#include <Rcpp.h>
#include <vector>
#include <string>

#include "dwf.h"
#include "helpers.h"

namespace {

//==============================================================================
Rcpp::CharacterVector FunctionNamesFromMask(const unsigned int mask) {

  struct FunctionDef {
    unsigned int code;
    const char* name;
  };
  static const FunctionDef kFunctions[] = {
    {static_cast<unsigned int>(funcDC), "dc"},
    {static_cast<unsigned int>(funcSine), "sine"},
    {static_cast<unsigned int>(funcSquare), "square"},
    {static_cast<unsigned int>(funcTriangle), "triangle"},
    {static_cast<unsigned int>(funcRampUp), "ramp_up"},
    {static_cast<unsigned int>(funcRampDown), "ramp_down"},
    {static_cast<unsigned int>(funcNoise), "noise"},
    {static_cast<unsigned int>(funcPulse), "pulse"},
    {static_cast<unsigned int>(funcTrapezium), "trapezium"},
    {static_cast<unsigned int>(funcSinePower), "sine_power"},
    {static_cast<unsigned int>(funcSineNA), "sine_na"},
    {static_cast<unsigned int>(funcDualCustom), "dual_custom"},
    {static_cast<unsigned int>(funcDualPattern), "dual_pattern"},
    {static_cast<unsigned int>(funcCustomPattern), "custom_pattern"},
    {static_cast<unsigned int>(funcPlayPattern), "play_pattern"},
    {static_cast<unsigned int>(funcCustom), "custom"},
    {static_cast<unsigned int>(funcPlay), "play"}
  };

  std::vector<std::string> out;
  for (const FunctionDef& item : kFunctions) {
    if (IsBitSetSafe(mask, item.code)) {
      out.push_back(item.name);
    }
  }
  return Rcpp::wrap(out);
}



//==============================================================================
Rcpp::CharacterVector IdleModeNamesFromMask(const unsigned int mask) {

  struct IdleModeDef {
    unsigned int code;
    const char* name;
  };

  static const IdleModeDef kIdleModes[] = {
    {static_cast<unsigned int>(DwfAnalogOutIdleDisable), "disable"},
    {static_cast<unsigned int>(DwfAnalogOutIdleOffset), "offset"},
    {static_cast<unsigned int>(DwfAnalogOutIdleInitial), "initial"},
    {static_cast<unsigned int>(DwfAnalogOutIdleHold), "hold"}
  };

  std::vector<std::string> out;
  for (const IdleModeDef& item : kIdleModes) {
    if (IsBitSetSafe(mask, item.code)) {
      out.push_back(item.name);
    }
  }
  return Rcpp::wrap(out);
}



//==============================================================================
using AnalogOutChannelRangeFun = int (*)(HDWF, int, double*, double*);
Rcpp::NumericVector QueryAnalogOutChannelRange(
    const int handle,
    const int channel,
    AnalogOutChannelRangeFun range_fun,
    const char* fun_name) {

  const HDWF hdwf = AsHandle(handle);
  double min_value = 0.0;
  double max_value = 0.0;

  if (!range_fun(hdwf, channel, &min_value, &max_value)) {
    ThrowDwfError(fun_name);
  }

  return Rcpp::NumericVector::create(min_value, max_value);
}



//==============================================================================
using AnalogOutRangeFun = int (*)(HDWF, int, AnalogOutNode, double*, double*);
Rcpp::NumericVector QueryAnalogOutNodeRange(
    const int handle,
    const int channel,
    const int node,
    AnalogOutRangeFun range_fun,
    const char* fun_name) {

  const HDWF hdwf = AsHandle(handle);
  const AnalogOutNode node_enum = static_cast<AnalogOutNode>(node);
  double min_value = 0.0;
  double max_value = 0.0;
  if (!range_fun(hdwf, channel, node_enum, &min_value, &max_value)) {
    ThrowDwfError(fun_name);
  }
  return Rcpp::NumericVector::create(min_value, max_value);
}

}  // namespace



//==============================================================================
// Queries the number of Analog Out channels supported by an opened device.
//
// [[Rcpp::export(name = ".QueryAnalogOutChannelCountC")]]
int QueryAnalogOutChannelCount(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  int count = 0;
  if (!FDwfAnalogOutCount(hdwf, &count)) {
    ThrowDwfError("FDwfAnalogOutCount");
  }
  return count;
}



//==============================================================================
// Queries the types of Analog Out nodes supported by a selected channel of an
// opened device.
//
// [[Rcpp::export(name = ".QueryAnalogOutChannelNodesC")]]
Rcpp::CharacterVector QueryAnalogOutChannelNodes(const int handle,
                                                 const int channel) {
  const HDWF hdwf = AsHandle(handle);

  int node_mask = 0;
  if (!FDwfAnalogOutNodeInfo(hdwf, channel, &node_mask)) {
    ThrowDwfError("FDwfAnalogOutNodeInfo");
  }

  std::vector<std::string> out;
  unsigned int node_mask_u = static_cast<unsigned int>(node_mask);
  if (IsBitSetSafe(node_mask_u, AnalogOutNodeCarrier)) {
    out.push_back("carrier");
  }
  if (IsBitSetSafe(node_mask_u, AnalogOutNodeFM)) {
    out.push_back("fm");
  }
  if (IsBitSetSafe(node_mask_u, AnalogOutNodeAM)) {
    out.push_back("am");
  }

  return Rcpp::wrap(out);
}



//==============================================================================
// Queries the supported run-time range for a selected Analog Out channel of an
// opened device.
//
// [[Rcpp::export(name = ".QueryAnalogOutRunRangeC")]]
Rcpp::NumericVector QueryAnalogOutRunRange(const int handle,
                                           const int channel) {
  return QueryAnalogOutChannelRange(
    handle,
    channel,
    FDwfAnalogOutRunInfo,
    "FDwfAnalogOutRunInfo"
  );
}



//==============================================================================
// Queries the supported wait-time range for a selected Analog Out channel of an
// opened device.
//
// [[Rcpp::export(name = ".QueryAnalogOutWaitRangeC")]]
Rcpp::NumericVector QueryAnalogOutWaitRange(const int handle,
                                            const int channel) {
  return QueryAnalogOutChannelRange(
    handle,
    channel,
    FDwfAnalogOutWaitInfo,
    "FDwfAnalogOutWaitInfo"
  );
}



//==============================================================================
// Queries the supported repeat-count range for a selected Analog Out channel of
// an opened device.
//
// [[Rcpp::export(name = ".QueryAnalogOutRepeatRangeC")]]
Rcpp::IntegerVector QueryAnalogOutRepeatRange(const int handle,
                                              const int channel) {
  const HDWF hdwf = AsHandle(handle);
  int min_value = 0;
  int max_value = 0;

  if (!FDwfAnalogOutRepeatInfo(
      hdwf,
      channel,
      &min_value,
      &max_value
  )) {
    ThrowDwfError("FDwfAnalogOutRepeatInfo");
  }

  return Rcpp::IntegerVector::create(min_value, max_value);
}



//==============================================================================
// Queries the idle output modes supported by a selected Analog Out channel of
// an opened device.
//
// [[Rcpp::export(name = ".QueryAnalogOutIdleModesC")]]
Rcpp::CharacterVector QueryAnalogOutIdleModes(const int handle,
                                              const int channel) {
  const HDWF hdwf = AsHandle(handle);
  int idle_mask = 0;

  if (!FDwfAnalogOutIdleInfo(hdwf, channel, &idle_mask)) {
    ThrowDwfError("FDwfAnalogOutIdleInfo");
  }

  return IdleModeNamesFromMask(
    static_cast<unsigned int>(idle_mask)
  );
}



//==============================================================================
// Queries the waveform functions supported by a selected Analog Out node of an
// opened device.
//
// [[Rcpp::export(name = ".QueryAnalogOutNodeFunctionTypesC")]]
Rcpp::CharacterVector QueryAnalogOutNodeFunctionTypes(const int handle,
                                                      const int channel,
                                                      const int node) {
  const HDWF hdwf = AsHandle(handle);
  const AnalogOutNode node_as_enum = static_cast<AnalogOutNode>(node);
  unsigned int mask = 0U;
  if (!FDwfAnalogOutNodeFunctionInfo(hdwf, channel, node_as_enum, &mask)) {
    ThrowDwfError("FDwfAnalogOutNodeFunctionInfo");
  }
  return FunctionNamesFromMask(mask);
}



//==============================================================================
// Queries the supported frequency range for a selected Analog Out node of an
// opened device.
//
// [[Rcpp::export(name = ".QueryAnalogOutNodeFrequencyRangeC")]]
Rcpp::NumericVector QueryAnalogOutNodeFrequencyRange(const int handle,
                                                     const int channel,
                                                     const int node) {
  return QueryAnalogOutNodeRange(
    handle,
    channel,
    node,
    FDwfAnalogOutNodeFrequencyInfo,
    "FDwfAnalogOutNodeFrequencyInfo"
  );
}



//==============================================================================
// Queries the supported amplitude range for a selected Analog Out node of an
// opened device.
//
// [[Rcpp::export(name = ".QueryAnalogOutNodeAmplitudeRangeC")]]
Rcpp::NumericVector QueryAnalogOutNodeAmplitudeRange(const int handle,
                                                     const int channel,
                                                     const int node) {
  return QueryAnalogOutNodeRange(
    handle,
    channel,
    node,
    FDwfAnalogOutNodeAmplitudeInfo,
    "FDwfAnalogOutNodeAmplitudeInfo"
  );
}



//==============================================================================
// Queries the supported offset range for a selected Analog Out node of an
// opened device.
//
// [[Rcpp::export(name = ".QueryAnalogOutNodeOffsetRangeC")]]
Rcpp::NumericVector QueryAnalogOutNodeOffsetRange(const int handle,
                                                  const int channel,
                                                  const int node) {
  return QueryAnalogOutNodeRange(
    handle,
    channel,
    node,
    FDwfAnalogOutNodeOffsetInfo,
    "FDwfAnalogOutNodeOffsetInfo"
  );
}



//==============================================================================
// Queries the supported symmetry range for a selected Analog Out node of an
// opened device.
//
// [[Rcpp::export(name = ".QueryAnalogOutNodeSymmetryRangeC")]]
Rcpp::NumericVector QueryAnalogOutNodeSymmetryRange(const int handle,
                                                    const int channel,
                                                    const int node) {
  return QueryAnalogOutNodeRange(
    handle,
    channel,
    node,
    FDwfAnalogOutNodeSymmetryInfo,
    "FDwfAnalogOutNodeSymmetryInfo"
  );
}



//==============================================================================
// Queries the supported phase range for a selected Analog Out node of an opened
// device.
//
// [[Rcpp::export(name = ".QueryAnalogOutNodePhaseRangeC")]]
Rcpp::NumericVector QueryAnalogOutNodePhaseRange(const int handle,
                                                 const int channel,
                                                 const int node) {
  return QueryAnalogOutNodeRange(
    handle,
    channel,
    node,
    FDwfAnalogOutNodePhaseInfo,
    "FDwfAnalogOutNodePhaseInfo"
  );
}



//==============================================================================
// Queries the supported range of sample counts for custom waveform data on a
// selected Analog Out node of an opened device.
//
// [[Rcpp::export(name = ".QueryAnalogOutNodeSampleCountRangeC")]]
Rcpp::IntegerVector QueryAnalogOutNodeSampleCountRange(const int handle,
                                                       const int channel,
                                                       const int node) {
  const HDWF hdwf = AsHandle(handle);
  const AnalogOutNode node_as_enum = static_cast<AnalogOutNode>(node);
  int min = 0;
  int max = 0;
  if (!FDwfAnalogOutNodeDataInfo(hdwf, channel, node_as_enum, &min, &max)) {
    ThrowDwfError("FDwfAnalogOutNodeDataInfo");
  }
  return Rcpp::IntegerVector::create(min, max);
}


