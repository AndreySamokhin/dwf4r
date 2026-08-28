#include "helpers.h"
#include <Rcpp.h>
#include <string>
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


