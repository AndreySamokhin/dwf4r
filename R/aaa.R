# Environment to hold information about connected devices
the <- new.env(parent = emptyenv())
the$devices <- list()

# the
#  └──devices
#      ├──[[1L]]
#      |    ├──device_handle (integer)
#      |    └──serial_number (string)
#      |
#      ├──[[2L]]
#      ...

# If the connection with a particular device was closed, the respective element
# of the 'the$devices' is replaced with 'NULL', which is used to identify stale
# S3 'dwf4r_device' objects.

# S3_object
#   ├──device_handle (integer)
#   ├──session_id    (integer)
#   └──info
#       ├──device_type     (string)
#       ├──device_revision (integer)
#       ├──device_name     (string)
#       ├──user_name       (string)
#       ├──serial_number   (string)
#       └──config_count    (integer)


