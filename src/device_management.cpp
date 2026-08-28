#include <Rcpp.h>
#include <string>

#include "dwf.h"
#include "helpers.h"



namespace {

const char* GetDeviceTypeLabel(int device_id) {
  // Device labels are based on dwf.h constants and the SDK manual
  // (SDK Version - 3.25; Manual version - March 4, 2026)
  switch (device_id) {
    case devidEExplorer:  return "Electronics Explorer";
    case devidDiscovery:  return "Analog Discovery";
    case devidDiscovery2: return "Analog Discovery 2";
    case devidDDiscovery: return "Digital Discovery";
    case devidADP3X50:    return "Analog Discovery Pro 3000 series";
    case devidEclypse:    return "Eclypse";
    case devidADP5250:    return "Analog Discovery Pro 5250";
    case devidDPS3340:    return "Discovery Power Supply 3340";
    case devidDiscovery3: return "Analog Discovery 3";
    case devidADP5470:    return "Analog Discovery Pro 5470";
    case devidADP5490:    return "Analog Discovery Pro 5490";
    case devidADP2230:    return "Analog Discovery Pro 2230";
    case devidADSMax:     return "Analog Discovery Max";
    case devidADP2440:    return "Analog Discovery Pro 2440";
    case devidADP2450:    return "Analog Discovery Pro 2450";
    default:              return "Unknown";
  }
}

} // namespace



// [[Rcpp::export(name = ".ListDevicesC")]]
Rcpp::DataFrame ListDevices(const int filter) {
  int n_devices = 0;

  if (!FDwfEnum(filter, &n_devices)) {
    ThrowDwfError("FDwfEnum");
  }

  Rcpp::IntegerVector device_index(n_devices);
  Rcpp::IntegerVector device_id(n_devices);
  Rcpp::CharacterVector device_type(n_devices);
  Rcpp::IntegerVector device_revision(n_devices);
  Rcpp::CharacterVector device_name(n_devices);
  Rcpp::CharacterVector user_name(n_devices);
  Rcpp::CharacterVector serial_number(n_devices);
  Rcpp::LogicalVector is_opened(n_devices);
  Rcpp::IntegerVector config_count(n_devices);

  for (int i = 0; i < n_devices; ++i) {
    DEVID id = 0;
    DEVVER revision = 0;
    int opened = 0;
    int n_configs = 0;

    char name_buf[32] = {'\0'};
    char user_buf[32] = {'\0'};
    char sn_buf[32] = {'\0'};

    if (!FDwfEnumDeviceType(i, &id, &revision)) {
      ThrowDwfError("FDwfEnumDeviceType");
    }
    if (!FDwfEnumDeviceName(i, name_buf)) {
      ThrowDwfError("FDwfEnumDeviceName");
    }
    if (!FDwfEnumUserName(i, user_buf)) {
      ThrowDwfError("FDwfEnumUserName");
    }
    if (!FDwfEnumSN(i, sn_buf)) {
      ThrowDwfError("FDwfEnumSN");
    }
    if (!FDwfEnumDeviceIsOpened(i, &opened)) {
      ThrowDwfError("FDwfEnumDeviceIsOpened");
    }
    if (!FDwfEnumConfig(i, &n_configs)) {
      ThrowDwfError("FDwfEnumConfig");
    }

    device_index[i] = i;
    device_id[i] = static_cast<int>(id);
    device_type[i] = GetDeviceTypeLabel(static_cast<int>(id));
    device_revision[i] = static_cast<int>(revision);
    device_name[i] = name_buf;
    user_name[i] = user_buf;
    serial_number[i] = sn_buf;
    is_opened[i] = (opened != 0);
    config_count[i] = n_configs;
  }

  Rcpp::DataFrame out = Rcpp::DataFrame::create(
    Rcpp::Named("device_index") = device_index,
    Rcpp::Named("device_id") = device_id,
    Rcpp::Named("device_type") = device_type,
    Rcpp::Named("device_revision") = device_revision,
    Rcpp::Named("device_name") = device_name,
    Rcpp::Named("user_name") = user_name,
    Rcpp::Named("serial_number") = serial_number,
    Rcpp::Named("is_opened") = is_opened,
    Rcpp::Named("config_count") = config_count,
    Rcpp::Named("stringsAsFactors") = false
  );
  return out;
}



// [[Rcpp::export(name = ".OpenDeviceC")]]
int OpenDevice(const int device_index,
               const int config_index) {
  HDWF hdwf = hdwfNone;

  if (config_index < 0) {
    if (!FDwfDeviceOpen(device_index, &hdwf)) {
      ThrowDwfError("FDwfDeviceOpen");
    }
  } else {
    if (!FDwfDeviceConfigOpen(device_index, config_index, &hdwf)) {
      ThrowDwfError("FDwfDeviceConfigOpen");
    }
  }

  return static_cast<int>(hdwf);
}



// [[Rcpp::export(name = ".DeviceAutoConfigureSetC")]]
void DeviceAutoConfigureSet(const int handle,
                            const bool auto_configure) {
  const HDWF hdwf = AsHandle(handle);
  if (!FDwfDeviceAutoConfigureSet(hdwf, auto_configure ? 1 : 0)) {
    ThrowDwfError("FDwfDeviceAutoConfigureSet");
  }
}



// [[Rcpp::export(name = ".CloseDeviceC")]]
void CloseDevice(const int handle) {
  HDWF hdwf = static_cast<HDWF>(handle);
  if (hdwf == hdwfNone) {
    return;
  }
  if (!FDwfDeviceClose(hdwf)) {
    ThrowDwfError("FDwfDeviceClose");
  }
}



// [[Rcpp::export(name = ".CloseAllDevicesC")]]
void CloseAllDevices() {
  if (!FDwfDeviceCloseAll()) {
    ThrowDwfError("FDwfDeviceCloseAll");
  }
}


