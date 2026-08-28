#include <Rcpp.h>

#include "dwf.h"
#include "helpers.h"



//==============================================================================
// Queries the trigger sources supported by an opened device.
//
// [[Rcpp::export(name = ".QueryDeviceTriggerSourceMaskC")]]
int QueryDeviceTriggerSourceMask(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  int source_mask = 0;
  if (!FDwfDeviceTriggerInfo(hdwf, &source_mask)) {
    ThrowDwfError("FDwfDeviceTriggerInfo");
  }
  return source_mask;
}



//==============================================================================
// Queries the trigger slopes supported by an opened device.
//
// [[Rcpp::export(name = ".QueryDeviceTriggerSlopeMaskC")]]
int QueryDeviceTriggerSlopeMask(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  int slope_mask = 0;
  if (!FDwfDeviceTriggerSlopeInfo(hdwf, &slope_mask)) {
    ThrowDwfError("FDwfDeviceTriggerSlopeInfo");
  }
  return slope_mask;
}



//==============================================================================
// Generates one pulse on the device PC trigger line.
//
// [[Rcpp::export(name = ".DeviceTriggerPcC")]]
void DeviceTriggerPc(const int handle) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfDeviceTriggerPC(hdwf)) {
    ThrowDwfError("FDwfDeviceTriggerPC");
  }
}


