context("Pessoa test verification")

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

test_that("CPF is givin a data frame column", {
  cpfs <- c(31338763849, 70628182597, 45178861400)
  names <- c("Pedro", "Joao", "Augusto")
  df <- data.frame(names = names, cpf = cpfs)
  result <- valida_documento(df$cpf)
  expect_equal(result, c(TRUE, TRUE, TRUE))
})

test_that("CNPJ is giving with especial caracters", {
  saida <- valida_documento("60.149.443/0001-70", type = "cnpj")
  expect_equal(saida, TRUE)
})

test_that("CNPJ is giving with just caracters", {
  saida <- valida_documento("60149443000170", type = "cnpj")
  expect_equal(saida, TRUE)
})

test_that("CNPJ is giving as a numeric", {
  saida <- valida_documento(60149443000170, type = "cnpj")
  expect_equal(saida, TRUE)
})

test_that("A wrong CNPJ is giving", {
  saida <- valida_documento(60149443000171, type = "cnpj")
  expect_equal(saida, FALSE)
})

test_that("CNPJ is givin a data frame column", {
  cnpjs <- c(16784319000195, 11318414000180, 79417547000147)
  names <- c("BB", "Caixa", "Ipea")
  df <- data.frame(names = names, cnpjs = cnpjs)
  result <- valida_documento(df$cnpjs, type = "cnpj")
  expect_equal(result, c(TRUE, TRUE, TRUE))
})


test_that("PIS is giving with especial caracters", {
  saida <- valida_documento("12345678919", type = "pis")
  expect_equal(saida, TRUE)
})

test_that("PIS is giving with just caracters", {
  saida <- valida_documento("123.456.789-19", type = "pis")
  expect_equal(saida, TRUE)
})

test_that("PIS is giving as a numeric", {
  saida <- valida_documento(12345678919, type = "pis")
  expect_equal(saida, TRUE)
})

test_that("A wrong PIS is giving", {
  saida <- valida_documento(12345678918, type = "pis")
  expect_equal(saida, FALSE)
})

test_that("PIS is givin a data frame column", {
  pis <- c(45993824692, 16630925956, 16345860949)
  names <- c("BB", "Caixa", "Ipea")
  df <- data.frame(names = names, pis = pis)
  result <- valida_documento(df$pis, type = "pis")
  expect_equal(result, c(TRUE, TRUE, TRUE))
})
