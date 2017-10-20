context("Teste do CNPJ")

test_that("CNPJ com caracters especiais", {
  saida <- valida_doc("60.149.443/0001-70", type = "cnpj")
  expect_equal(saida, TRUE)
})

test_that("CNPJ com caracters iguais", {
  saida <- valida_doc("11.111.111/1111-11", type = "cnpj")
  expect_equal(saida, FALSE)
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

test_that("CNPJ com tamanho errado", {
  saida <- valida_doc(6014944300017, type = "cnpj")
  expect_equal(saida, FALSE)
})

test_that("Conjunto de CNPJS", {
  cnpjs <- c(16784319000195, 11318414000180, 79417547000147)
  names <- c("BB", "Caixa", "Ipea")
  df <- data.frame(names = names, cnpjs = cnpjs)
  result <- valida_doc(df$cnpjs, type = "cnpj")
  expect_equal(result, c(TRUE, TRUE, TRUE))
})

test_that("Generate cnpj", {
  cnpj_incorreto <- c("522641040001", "297438670001", "855840660001")
  cnpj_correto <- c("52264104000107", "29743867000119", "85584066000129")
  gera_digito(cnpj_incorreto, type = "cnpj")
  expect_equal(cnpj_correto, cnpj_incorreto)
})
