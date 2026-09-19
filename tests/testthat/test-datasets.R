test_that("cites_fauna_peru_2018 is valid", {
  expect_equal(nrow(cites_fauna_peru_2018), 512)
  expect_equal(ncol(cites_fauna_peru_2018), 17)
  expect_true(all(c("id_especie", "phylum", "clase", "especie", "ap", "autor") %in% names(cites_fauna_peru_2018)))
})

test_that("cites_fauna_peru_2019 is valid", {
  expect_equal(nrow(cites_fauna_peru_2019), 523)
  expect_equal(ncol(cites_fauna_peru_2019), 17)
  expect_true(all(c("id", "phylum", "clase", "genero", "nombre_cientifico", "apendice", "ambito") %in% names(cites_fauna_peru_2019)))
})

test_that("cites_fauna_peru_2023 is valid", {
  expect_equal(nrow(cites_fauna_peru_2023), 568)
  expect_equal(ncol(cites_fauna_peru_2023), 17)
  expect_true(all(c("n", "fauna_silvestre_especie_hidrobiologica", "phyllum", "clase", "especie_nombre_cientifico", "inclusion_y_o_enmienda") %in% names(cites_fauna_peru_2023)))
})

test_that("cites_flora_peru_2018 is valid", {
  expect_equal(nrow(cites_flora_peru_2018), 2506)
  expect_equal(ncol(cites_flora_peru_2018), 10)
  expect_true(all(c("item", "apendice", "familia", "n", "especie", "distribucion") %in% names(cites_flora_peru_2018)))
})

test_that("codigos_departamentos_pe is valid", {
  expect_equal(nrow(codigos_departamentos_pe), 24)
  expect_equal(ncol(codigos_departamentos_pe), 3)
})
