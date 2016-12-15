context("Cpf test")

test_that("Cpf is giving with especial caracters", {
  saida <- valida_documento("529.982.247-25")
  expect_equal(saida, TRUE)
})

test_that("Cpf is giving with just caracters", {
  saida <- valida_documento("52998224725")
  expect_equal(saida, TRUE)
})

test_that("Cpf is giving as a numeric", {
  saida <- valida_documento(52998224725)
  expect_equal(saida, TRUE)
})

test_that("A wrong cpf is giving", {
  saida <- valida_documento(52998224720)
  expect_equal(saida, FALSE)
})

test_that("It is givin a data frame column", {
  cpfs <- c(31338763849, 70628182597, 45178861400)
  names <- c("Pedro", "Joao", "Augusto")
  df <- data.frame(names = names, cpf = cpfs)
  result <- valida_documento(df$cpf)
  expect_equal(result, c(TRUE, TRUE, TRUE))
})

