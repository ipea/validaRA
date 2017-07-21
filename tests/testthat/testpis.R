context("Teste do PIS")

test_that("PIS is giving with especial caracters", {
  saida <- valida_doc("12345678919", type = "pis")
  expect_equal(saida, TRUE)
})

test_that("PIS is giving with just caracters", {
  saida <- valida_doc("123.456.789-19", type = "pis")
  expect_equal(saida, TRUE)
})

test_that("PIS is giving as a numeric", {
  saida <- valida_doc(12345678919, type = "pis")
  expect_equal(saida, TRUE)
})

test_that("A wrong PIS is giving", {
  saida <- valida_doc(12345678918, type = "pis")
  expect_equal(saida, FALSE)
})

test_that("PIS is givin a data frame column", {
  pis <- c(45993824692, 16630925956, 16345860949)
  names <- c("BB", "Caixa", "Ipea")
  df <- data.frame(names = names, pis = pis)
  result <- valida_doc(df$pis, type = "pis")
  expect_equal(result, c(TRUE, TRUE, TRUE))
})

test_that("PIS using log", {
  pis <- c(45993824692, 12345678918, 16345860949)
  names <- c("BB", "Caixa", "Ipea")
  df <- data.frame(names = names, pis = pis)
  dfr <- data.frame(dado=pis,resultado=c(TRUE,FALSE,TRUE), erros=as.numeric(c(0,1,0)), stringsAsFactors = FALSE)
  result <- valida_doc(df$pis, type = "pis", log = TRUE)
  expect_equal(result, dfr)
})

test_that("Generate pis", {
  pis_correto <- c("45993824692", "16630925956", "16345860949")
  pis_incorreto <- c("4599382469", "1663092595", "1634586094")
  gera_digito(pis_incorreto, type = "pis")
  expect_equal(pis_correto, pis_incorreto)
})
