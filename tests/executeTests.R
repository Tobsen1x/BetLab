executeAllTests <- function() {
  test_file('tests/testthat/testPlayerFormForecast.R')
  test_file('tests/testthat/testPositionFeatureExtraction.R')
  test_file('tests/testthat/testRawData.R')
}