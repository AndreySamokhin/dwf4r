#include <Rcpp.h>

#include "dwf.h"
#include "helpers.h"



namespace {

//==============================================================================
using DigitalOutTimeRangeFun = int (*)(HDWF, double*, double*);
  Rcpp::NumericVector QueryDigitalOutTimeRange(
      const int handle,
      DigitalOutTimeRangeFun range_fun,
      const char* function_name) {

    const HDWF hdwf = AsHandle(handle);
    double min_value = 0.0;
    double max_value = 0.0;

    if (!range_fun(hdwf, &min_value, &max_value)) {
      ThrowDwfError(function_name);
    }

    return Rcpp::NumericVector::create(min_value, max_value);
  }



  //============================================================================
  using DigitalOutMaskFun = int (*)(HDWF, int, int*);
  int QueryDigitalOutMask(
      const int handle,
      const int channel,
      DigitalOutMaskFun info_fun,
      const char* function_name) {

    const HDWF hdwf = AsHandle(handle);
    int mask = 0;

    if (!info_fun(hdwf, channel, &mask)) {
      ThrowDwfError(function_name);
    }

    return mask;
  }



  //============================================================================
  using DigitalOutRangeFun = int (*)(HDWF, int, unsigned int*, unsigned int*);
  Rcpp::NumericVector QueryDigitalOutUnsignedRange(
      const int handle,
      const int channel,
      DigitalOutRangeFun range_fun,
      const char* function_name) {

    const HDWF hdwf = AsHandle(handle);
    unsigned int min_value = 0U;
    unsigned int max_value = 0U;

    if (!range_fun(hdwf, channel, &min_value, &max_value)) {
      ThrowDwfError(function_name);
    }

    // Use double to preserve the full 32-bit unsigned range when returned to R.
    return Rcpp::NumericVector::create(
      static_cast<double>(min_value),
      static_cast<double>(max_value)
    );
  }

}  // namespace



//==============================================================================
// Queries the number of Digital Out channels supported by an opened device.
//
// [[Rcpp::export(name = ".QueryDigitalOutChannelCountC")]]
int QueryDigitalOutChannelCount(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  int count = 0;

  if (!FDwfDigitalOutCount(hdwf, &count)) {
    ThrowDwfError("FDwfDigitalOutCount");
  }

  return count;
}



//==============================================================================
// Queries the internal Digital Out clock frequency in Hz.
//
// [[Rcpp::export(name = ".QueryDigitalOutInternalClockFrequencyC")]]
double QueryDigitalOutInternalClockFrequency(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  double frequency = 0.0;

  if (!FDwfDigitalOutInternalClockInfo(hdwf, &frequency)) {
    ThrowDwfError("FDwfDigitalOutInternalClockInfo");
  }

  return frequency;
}



//==============================================================================
// Queries the supported Digital Out run-time range in seconds.
//
// [[Rcpp::export(name = ".QueryDigitalOutRunRangeC")]]
Rcpp::NumericVector QueryDigitalOutRunRange(const int handle) {
  return QueryDigitalOutTimeRange(
    handle,
    FDwfDigitalOutRunInfo,
    "FDwfDigitalOutRunInfo"
  );
}



//==============================================================================
// Queries the supported Digital Out wait-time range in seconds.
//
// [[Rcpp::export(name = ".QueryDigitalOutWaitRangeC")]]
Rcpp::NumericVector QueryDigitalOutWaitRange(const int handle) {
  return QueryDigitalOutTimeRange(
    handle,
    FDwfDigitalOutWaitInfo,
    "FDwfDigitalOutWaitInfo"
  );
}



//==============================================================================
// Queries the supported Digital Out repeat-count range.
//
// [[Rcpp::export(name = ".QueryDigitalOutRepeatRangeC")]]
Rcpp::NumericVector QueryDigitalOutRepeatRange(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  unsigned int min_value = 0U;
  unsigned int max_value = 0U;

  // 'DigitalOutRangeFun' cannot be used here because
  // 'FDwfDigitalOutRepeatInfo()' has no channel argument.
  if (!FDwfDigitalOutRepeatInfo(hdwf, &min_value, &max_value)) {
    ThrowDwfError("FDwfDigitalOutRepeatInfo");
  }

  // Use double to preserve the full 32-bit unsigned range when returned to R.
  return Rcpp::NumericVector::create(
    static_cast<double>(min_value),
    static_cast<double>(max_value)
  );
}



//==============================================================================
// Queries the supported output-mode bitmask of a Digital Out channel.
//
// [[Rcpp::export(name = ".QueryDigitalOutOutputMaskC")]]
int QueryDigitalOutOutputMask(const int handle, const int channel) {
  return QueryDigitalOutMask(
    handle,
    channel,
    FDwfDigitalOutOutputInfo,
    "FDwfDigitalOutOutputInfo"
  );
}



//==============================================================================
// Queries the supported type bitmask of a Digital Out channel.
//
// [[Rcpp::export(name = ".QueryDigitalOutTypeMaskC")]]
int QueryDigitalOutTypeMask(const int handle, const int channel) {
  return QueryDigitalOutMask(
    handle,
    channel,
    FDwfDigitalOutTypeInfo,
    "FDwfDigitalOutTypeInfo"
  );
}



//==============================================================================
// Queries the supported idle-mode bitmask of a Digital Out channel.
//
// [[Rcpp::export(name = ".QueryDigitalOutIdleMaskC")]]
int QueryDigitalOutIdleMask(const int handle, const int channel) {
  return QueryDigitalOutMask(
    handle,
    channel,
    FDwfDigitalOutIdleInfo,
    "FDwfDigitalOutIdleInfo"
  );
}



//==============================================================================
// Queries the supported divider range of a Digital Out channel.
//
// [[Rcpp::export(name = ".QueryDigitalOutDividerRangeC")]]
Rcpp::NumericVector QueryDigitalOutDividerRange(const int handle,
                                                const int channel) {
  return QueryDigitalOutUnsignedRange(
    handle,
    channel,
    FDwfDigitalOutDividerInfo,
    "FDwfDigitalOutDividerInfo"
  );
}



//==============================================================================
// Queries the supported counter range of a Digital Out channel.
//
// [[Rcpp::export(name = ".QueryDigitalOutCounterRangeC")]]
Rcpp::NumericVector QueryDigitalOutCounterRange(const int handle,
                                                const int channel) {
  return QueryDigitalOutUnsignedRange(
    handle,
    channel,
    FDwfDigitalOutCounterInfo,
    "FDwfDigitalOutCounterInfo"
  );
}



//==============================================================================
// Queries the maximum number of custom-data bits supported by a Digital Out
// channel.
//
// [[Rcpp::export(name = ".QueryDigitalOutMaxDataBitsC")]]
double QueryDigitalOutMaxDataBits(const int handle, const int channel) {
  const HDWF hdwf = AsHandle(handle);
  unsigned int max_bits = 0U;

  if (!FDwfDigitalOutDataInfo(hdwf, channel, &max_bits)) {
    ThrowDwfError("FDwfDigitalOutDataInfo");
  }

  // Use double to preserve the full 32-bit unsigned range when returned to R.
  return static_cast<double>(max_bits);
}


