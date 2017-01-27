context("Teste do Titulo de Eleitor")

test_that("Titulo de Eleitor com caracters especiais", {
  saida <- valida_doc("43568709/06", type = "tituloeleitor")
  expect_equal(saida, TRUE)
})

test_that("Titulo de Eleitor somente com carcters", {
  saida <- valida_doc("43568709/06", type = "tituloeleitor")
  expect_equal(saida, TRUE)
})

test_that("Titulo de Eleitor como numero", {
  saida <- valida_doc( 4356870906, type = "tituloeleitor")
  expect_equal(saida, TRUE)
})

test_that("Titulo de Eleitor errado", {
  saida <- valida_doc( 4356870907, type = "tituloeleitor")
  expect_equal(saida, FALSE)
})

