context("Teste do CPF")

test_that("Cpf com caracteres especiais", {
  saida <- valida_doc("529.982.247-25")
  expect_equal(saida, TRUE)
})

test_that("Cpf com caracteres todos iguais", {
  saida <- valida_doc("000.000.000-00")
  expect_equal(saida, FALSE)
})

test_that("Cpf somente com caracteres", {
  saida <- valida_doc("52998224725")
  expect_equal(saida, TRUE)
})

test_that("Cpf como numero", {
  saida <- valida_doc(52998224725)
  expect_equal(saida, TRUE)
})

test_that("Cpf errado", {
  saida <- valida_doc(52998224720)
  expect_equal(saida, FALSE)
})

test_that("Cpf numerico com zero no inicio", {
  saida <- valida_doc(08609070662)
  expect_equal(saida, TRUE)
})

test_that("Cpf como factor", {
  saida <- valida_doc(as.factor("086.090.706-62"))
  expect_equal(saida, TRUE)
})

test_that("Vetor de cpfs", {
  cpfs <- c(31338763849, 70628182597, 45178861400)
  names <- c("Pedro", "Joao", "Augusto")
  df <- data.frame(names = names, cpf = cpfs)
  result <- valida_doc(df$cpf)
  expect_equal(result, c(TRUE, TRUE, TRUE))
})

test_that("Data frame de cpfs", {
  load("data_frame_cpfs_teste.rda")
  result <- valida_doc_df(dados,id, type = "cpf")
  expect_equal(result, c(TRUE, TRUE, TRUE, TRUE))
})

test_that("Data frame de cpfs e log true", {
  load("data_frame_cpfs_teste.rda")
  load("resultado_cpf_log.rda")
  result <- valida_doc_df(dados,id, type = "cpf", log = TRUE)
  expect_equal(result, resultado)
})


