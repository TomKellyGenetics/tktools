library("tktools")
context("test colour inputs")

## col = col
## colours = colours
## colors = colors
### multiple color inputs

test_that("different color inputs", {
  scale <- colorscale(3, col = c("red", "green", "blue"))
  expect_length(scale, 3)
  rm(scale)
  scale <- colorscale(3, colours = c("red", "green", "blue"))
  expect_length(scale, 3)
  rm(scale)
  scale <- colorscale(3, colors = c("red", "green", "blue"))
  expect_length(scale, 3)
  rm(scale)
})

#expect_success()
#expect_warning()
test_that("different colour inputs", {
  scale <- colourscale(3, col = c("red", "green", "blue"))
  expect_length(scale, 3)
  rm(scale)
  scale <- colourscale(3, colours = c("red", "green", "blue"))
  expect_length(scale, 3)
  rm(scale)
  scale <- colourscale(3, colors = c("red", "green", "blue"))
  expect_length(scale, 3)
  rm(scale)
})

test_that("multiple colour inputs", {
  expect_warning(colourscale(3, col = "red", colours = c("green", "blue")))
  expect_warning(colourscale(3, colors = "red", colours = c("green", "blue")))
  expect_warning(colourscale(3, col = "red", colors = c("green", "blue")))
  expect_error(colourscale(col = "red"))
})

test_that("different n types", {
  scale <- colourscale(3L, col = c("red", "green", "blue"))
  expect_length(scale, 3)
  rm(scale)
  scale <- colourscale(3.0, colours = c("red", "green", "blue"))
  expect_length(scale, 3)
  rm(scale)
  scale <- colourscale(3.5, colors = c("red", "green", "blue"))
  expect_length(scale, 3)
  rm(scale)
})

test_that("different n lengths", {
  scale <- colourscale(1, col = c("red", "green", "blue"))
  expect_length(scale, 1)
  rm(scale)
  scale <- colourscale(2, col = c("red", "green", "blue"))
  expect_length(scale, 2)
  rm(scale)
  scale <- colourscale(3, col = c("red", "green", "blue"))
  expect_length(scale, 3)
  rm(scale)
  scale <- colourscale(6, col = c("red", "green", "blue"))
  expect_length(scale, 6)
  rm(scale)
  scale <- colourscale(9, colours = c("red", "green", "blue"))
  expect_length(scale, 9)
  rm(scale)
  scale <- colourscale(100, colors = c("red", "green", "blue"))
  expect_length(scale, 100)
  rm(scale)
  scale <- colourscale(0, colors = c("red", "green", "blue"))
  expect_length(scale, 0)
  rm(scale)
})
