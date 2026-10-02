#include <Rcpp.h>
#include <cstring> // std::memcpy

#include "dwf.h"
#include "helpers.h"

namespace {

//==============================================================================
using AnalogOutChannelMaskFun = int (*)(HDWF, int, int*);
int QueryAnalogOutChannelMask(
    const int handle,
    const int channel,
    AnalogOutChannelMaskFun mask_fun,
    const char* fun_name) {

  const HDWF hdwf = AsHandle(handle);
  int mask = 0;

  if (!mask_fun(hdwf, channel, &mask)) {
    ThrowDwfError(fun_name);
  }

  return mask;
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
// Queries the supported Analog Out node bitmask of a selected channel.
//
// [[Rcpp::export(name = ".QueryAnalogOutNodeMaskC")]]
int QueryAnalogOutNodeMask(const int handle, const int channel) {
  return QueryAnalogOutChannelMask(
    handle,
    channel,
    FDwfAnalogOutNodeInfo,
    "FDwfAnalogOutNodeInfo"
  );
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
// Queries the supported Analog Out idle-mode bitmask of a selected channel.
//
// [[Rcpp::export(name = ".QueryAnalogOutIdleMaskC")]]
int QueryAnalogOutIdleMask(const int handle, const int channel) {
  return QueryAnalogOutChannelMask(
    handle,
    channel,
    FDwfAnalogOutIdleInfo,
    "FDwfAnalogOutIdleInfo"
  );
}



//==============================================================================
// Queries the supported waveform-function bitmask of a selected Analog Out node.
//
// [[Rcpp::export(name = ".QueryAnalogOutNodeFunctionMaskC")]]
int QueryAnalogOutNodeFunctionMask(const int handle,
                                   const int channel,
                                   const int node) {
  const HDWF hdwf = AsHandle(handle);
  const AnalogOutNode node_enum = static_cast<AnalogOutNode>(node);
  unsigned int mask = 0U;

  if (!FDwfAnalogOutNodeFunctionInfo(
      hdwf,
      channel,
      node_enum,
      &mask)) {
    ThrowDwfError("FDwfAnalogOutNodeFunctionInfo");
  }

  // Preserve the 32-bit mask for processing with intToBits() in R.
  int mask_as_int = 0;
  static_assert(
    sizeof(int) == sizeof(unsigned int),
    "Expected int and unsigned int to have equal size."
  );
  std::memcpy(&mask_as_int, &mask, sizeof(mask));
  return mask_as_int;
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


