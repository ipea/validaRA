context("Teste do CNS")

test_that("CNS start with 7", {
  saida <- valida_doc("772367104660002", type = "CNS")
  expect_equal(saida, TRUE)
})

test_that("CNS start with 8", {
  saida <- valida_doc("874822955230003", type = "CNS")
  expect_equal(saida, TRUE)
})

test_that("CNS start with 9", {
  saida <- valida_doc("974181962520005", type = "CNS")
  expect_equal(saida, TRUE)
})

test_that("CNS start with 1", {
  saida <- valida_doc("187086233720003", type = "CNS")
  expect_equal(saida, TRUE)
})

test_that("CNS start with 2", {
  saida <- valida_doc("207545695560018", type = "CNS")
  expect_equal(saida, TRUE)
})

test_that("CNS is giving with just caracters", {
  saida <- valida_doc("934803264080006", type = "CNS")
  expect_equal(saida, TRUE)
})

test_that("CNS is giving as a numeric", {
  saida <- valida_doc("294075364490018", type = "CNS")
  expect_equal(saida, TRUE)
})

test_that("A wrong CNS is giving", {
  saida <- valida_doc("294075364490008", type = "CNS")
  expect_equal(saida, FALSE)
})

test_that("CNS is givin a data frame column", {
  CNS <- c("934803264080006", "294075364490018", "194457319220009", "772367104660002", "874822955230003")
  names <- c("João", "Maria", "Josefina","Belcholina","Pedrita")
  df <- data.frame(names = names, CNS = CNS, stringsAsFactors = F)
  result <- valida_doc(df$CNS, type = "CNS")
  expect_equal(result, c(TRUE, TRUE, TRUE, TRUE, TRUE))
})




