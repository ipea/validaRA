context("Teste trata cpf")

test_that("CPF como numerio", {
  saida <- trata_cpf(566619245)
  expect_equal(saida, "00566619245")
  expect_equal(class(saida),"character")
})


