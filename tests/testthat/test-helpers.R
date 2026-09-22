#==[ .ConvertMaskToNames() ]====================================================

test_that(".ConvertMaskToNames()", {
  code_map <- list(
    none = 0L,
    pc = 1L,
    analog_out_1 = 7L,
    external_1 = 11L
  )

  expect_identical(
    .ConvertMaskToNames(2177L, code_map),
    c("none", "analog_out_1", "external_1")
  )

  expect_identical(
    .ConvertMaskToNames(2082L, code_map),
    c("pc", "external_1")
  )

  expect_identical(
    .ConvertMaskToNames(32L, code_map),
    character()
  )

  expect_identical(
    .ConvertMaskToNames(0L, code_map),
    character()
  )
})



#==[ .ConvertUnsignedValues() ]=================================================

test_that(".ConvertUnsignedValues()", {
  expect_identical(
    .ConvertUnsignedValues(c(0, 100)),
    c(0L, 100L)
  )
  expect_identical(
    .ConvertUnsignedValues(c(0, .Machine$integer.max)),
    c(0L, .Machine$integer.max)
  )
  expect_identical(
    .ConvertUnsignedValues(c(1, .Machine$integer.max + 1)),
    c(1, .Machine$integer.max + 1)
  )
  expect_identical(
    .ConvertUnsignedValues(.Machine$integer.max + 1),
    .Machine$integer.max + 1
  )
})


