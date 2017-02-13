context("Teste converte doc")

test_that("CPF ou pis to integer 64", {
  saida <- doc2integer64("005.666.192-45")
  expect_equal(saida,as.integer64(566619245))
  expect_equal(class(saida),"integer64")
})

test_that("CNPJ para integer 64", {
  saida <- doc2integer64("60.149.443/0001-70")
  expect_equal(saida,as.integer64(60149443000170))
  expect_equal(class(saida),"integer64")
})


