cpfs <- c("923.339.175-20", "563.553.822-32", "731.966.309-00", "731.966.309-00")
nomes<- c("Maria", "Josefina", "Pedro","Carmelita")
dados <- data.frame(pessoas = nomes, id = cpfs, stringsAsFactors = F)
getwd()
save(dados, file = file.path(getwd(),"tests","testthat","data_frame_cpfs_teste.rda"))
save(resultado, file = file.path(getwd(),"tests","testthat","resultado_cpf_log.rda"))
