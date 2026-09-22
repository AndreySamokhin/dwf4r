#include "helpers.h"
#include <Rcpp.h>
#include <string>
#include <cmath>  // std::floor
#include <limits> // std::numeric_limits
#include "dwf.h"


std::string GetLastDwfErrorMessage() {
  // DWFERC error_code = dwfercNoErc;
  // FDwfGetLastError(&error_code);

  char message[512] = {'\0'};

  if (!FDwfGetLastErrorMsg(message) || message[0] == '\0') {
    return "unknown error";
  }
  return std::string(message);
}


void ThrowDwfError(const char* function_name) {
  ThrowDwfError(function_name, GetLastDwfErrorMessage());
}


void ThrowDwfError(const char* function_name, const std::string& error_message) {
  Rcpp::stop("'%s()' failed: %s", function_name, error_message.c_str());
}


std::string ConvertDwfStateToName(const DwfState state) {
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


unsigned int AsUnsignedInt(const double value, const char* argument_name) {
  const double max_value =
    static_cast<double>(std::numeric_limits<unsigned int>::max());

  if (!std::isfinite(value) ||
      value < 0.0 ||
      value > max_value ||
      std::floor(value) != value) {
    Rcpp::stop(
      "'%s' must be a whole number in [0, %.0f].",
      argument_name,
      max_value
    );
  }

  return static_cast<unsigned int>(value);
}

