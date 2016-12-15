context("Cpf test")

test_that("Cpf is giving with especial caracters", {
  saida <- FALSE
  saida <- valida_documento("529.982.247-25", saida)
  expect_equal(saida, TRUE)
})

test_that("Cpf is giving with just caracters", {
  saida <- FALSE
  saida <- valida_documento("52998224725", saida)
  expect_equal(saida, TRUE)
})

test_that("Cpf is giving as a numeric", {
  saida <- FALSE
  saida <- valida_documento(52998224725, saida)
  expect_equal(saida, TRUE)
})

test_that("Cpf is giving as vector", {
  saida <- FALSE
  saida <- valida_documento(c(5, 2, 9, 9, 8, 2, 2, 4, 7, 2, 5), saida)
  expect_equal(saida, TRUE)
})