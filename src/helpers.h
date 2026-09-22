#ifndef DWF4R_HELPERS_H_
#define DWF4R_HELPERS_H_

#include <string>
#include <Rcpp.h>
#include "dwf.h"

std::string GetLastDwfErrorMessage();
void ThrowDwfError(const char* function_name);
void ThrowDwfError(const char* function_name, const std::string& error_message);
std::string ConvertDwfStateToName(const DwfState state);

inline bool IsBitSetSafe(const unsigned int value, const unsigned int bit) {
  // Avoid undefined behavior when shifting beyond the width of unsigned int.
  if (bit >= sizeof(unsigned int) * 8U) {
    return false;
  }
  return ((value & (1U << bit)) != 0U);
}

inline HDWF AsHandle(const int handle) {
  const HDWF hdwf = static_cast<HDWF>(handle);
  if (hdwf == hdwfNone) {
    Rcpp::stop("'device' does not contain a valid device handle.");
  }
  return hdwf;
}

unsigned int AsUnsignedInt(const double value, const char* argument_name);

#endif  // DWF4R_HELPERS_H_

