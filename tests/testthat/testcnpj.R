context("Teste do CNPJ")

test_that("CNPJ com caracters especiais", {
  saida <- valida_doc("60.149.443/0001-70", type = "cnpj")
  expect_equal(saida, TRUE)
})

test_that("CNPJ somente com carcters", {
  saida <- valida_doc("60149443000170", type = "cnpj")
  expect_equal(saida, TRUE)
})

test_that("CNPJ como numero", {
  saida <- valida_doc(60149443000170, type = "cnpj")
  expect_equal(saida, TRUE)
})

test_that("CNPJ errado", {
  saida <- valida_doc(60149443000171, type = "cnpj")
  expect_equal(saida, FALSE)
})

test_that("Conjunto de CNPJS", {
  cnpjs <- c(16784319000195, 11318414000180, 79417547000147)
  names <- c("BB", "Caixa", "Ipea")
  df <- data.frame(names = names, cnpjs = cnpjs)
  result <- valida_doc(df$cnpjs, type = "cnpj")
  expect_equal(result, c(TRUE, TRUE, TRUE))
})