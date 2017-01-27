context("Teste trata cpf")

test_that("CPF com zeros a esquerda", {
  saida <- trata_cpf(566619245)
  expect_equal(saida,"00566619245")
  expect_equal(class(saida),"character")
})


