library(tidyverse)
library(pdftools)
str_to_drop <- c("LIBRO ROJO DE LA FAUNA SILVESTRE AMENAZADA DEL PERÚ",
                 "CONTENIDO",
                 "Ministerio de Agricultura y Riego",
                 "amenaza en el Perú ",
                 "en el proceso de categorización de fauna",
                 "ESPECIES DE ANFIBIOS AMENAZADOS",
                 "ESPECIES DE AVES AMENAZADAS",
                 "ESPECIES DE INVERTEBRADOS AMENAZADOS",
                 "ESPECIES DE MAMÍFEROS AMENAZADOS",
                 "ESPECIES DE REPTILES AMENAZADOS")

str_to_drop <- paste0(str_to_drop, collapse = "|")
pdf <- pdftools::pdf_text("libro_rojo.pdf")
pdf <- gsub(str_to_drop, "", pdf)


# get index data ------------------------------------------------

get_rbf_index <- function(pdf_page){

  by_line <- stringr::str_split(pdf_page, "\\n|\\r") |>
    unlist()

  brecker <- function(x){
    breck <- stringr::str_extract(x, "\\s[0-9]{1,}\\s")
    stringr::str_replace(x, "\\s[0-9]{1,}\\s",
                         paste0(breck, "|"))
  }
  list_1 <- brecker(by_line) |>
    stringr::str_squish() |>
    stringr::str_trim() |>
    stringr::str_split("\\|") |>
    unlist() |>
    stringr::str_squish() |>
    stringr::str_trim()
  list_2 <- list_1[nchar(list_1) > 1]
  list_2 |>
    tibble::enframe( value = "especie") |>
    dplyr::mutate(pagina = stringr::str_extract(especie, "[0-9]{1,}") |>
                    as.numeric(),
                  especie = stringr::str_remove(especie, "[0-9]{1,}") |>
                    stringr::str_squish() |>
                    stringr::str_trim()) |>
    dplyr::arrange(pagina)

}
get_rbf_index(pdf[9])
get_rbf_index(pdf[9]) |>
  tail()

anfibios_1 <- get_rbf_index(pdf[5]) |>
  filter(between(pagina, 40, 114)) |>
  mutate(clase = "Anfibios")
anfibios_2 <- get_rbf_index(pdf[6]) |>
  filter(between(pagina, 115, 156)) |>
  mutate(clase = "Anfibios")

aves_1 <- get_rbf_index(pdf[6]) |>
  filter(between(pagina, 170, 218)) |>
  mutate(clase = "Aves")
aves_2 <- get_rbf_index(pdf[7]) |>
  filter(between(name, 1, 62)) |>
  mutate(clase = "Aves")


invertebrados_1 <- get_rbf_index(pdf[7]) |>
  filter(between(pagina, 308, 319)) |>
  mutate(clase = "Invertebrados")
invertebrados_2 <- get_rbf_index(pdf[8]) |>
  filter(between(pagina, 320, 326)) |>
  mutate(clase = "Invertebrados")

mamiferos_1 <- get_rbf_index(pdf[8]) |>
  filter(between(pagina, 336, 432)) |>
  mutate(clase = "Mamíferos")
mamiferos_2 <- get_rbf_index(pdf[9]) |>
  filter(between(pagina, 433, 445)) |>
  mutate(clase = "Mamíferos")

reptiles_1 <- get_rbf_index(pdf[9]) |>
  filter(between(pagina, 454, 486)) |>
  mutate(clase = "Reptiles")
lista_libro_rojo_fauna <- bind_rows(
  anfibios_1, anfibios_2,
  aves_1, aves_2, invertebrados_1, invertebrados_2,
  mamiferos_1, mamiferos_2, reptiles_1
)
lista_libro_rojo_fauna


# ESPECIES DE ANFIBIOS AMENAZADOS -------------------------------

get_info_ficha_anfibios <- function(pdf_page){
  page <- paste0(c("-pol-",
                   paste0(pdf_page,
                          collapse = "-pol-"),
                   "-pol-"), collapse = "") |>
    str_trim() |>
    str_squish()
  #especie <- str_extract(page, "\\-pol\\-\\s*(.*?)\\s*Clase:")
  especie <- gsub(".*-pol- (.+) Clase:.*",
                  "\\1", page)
  #especie
  clase <- gsub(".*Clase: (.+) Orden:.*", "\\1", page)
  #clase
  orden <- gsub(".*Orden: (.+) Familia:.*", "\\1", page)
  #orden
  familia <- gsub(".*Familia: (.+) CATEGORÍA.*", "\\1", page)
  #familia <- gsub(".*Familia: (.+) Nombres comunes:.*",
  #                "\\1", page)

  #familia
  nombres_comunes <- gsub(".*Nombres comunes: (.+) CATEGORÍA DE AMENAZA / CRITERIOS DE CATEGORIZACIÓN.*",
                          "\\1",
                          page)
  categorizacion <- gsub(".*Perú: (.+) JUSTIFICACIÓN.*",
                         "\\1",
                         page)
  justificacion <-  gsub(".*JUSTIFICACIÓN (.+) DISTRIBUCIÓN.*",
                         "\\1",
                         page)
  #justificacion
  distribucion <- gsub(".*DISTRIBUCIÓN (.+) AMENAZAS.*",
                       "\\1",
                       page)
  #distribucion
  amenazas <- gsub(".*AMENAZAS (.+) CONSERVACIÓN.*",
                   "\\1",
                   page)
  #amenazas
  #if(grepl(".*CONSERVACIÓN (.+) Autores:.*", page) == TRUE){
    conservacion <-  gsub(".*CONSERVACIÓN (.+) Autor.*",
                          "\\1",
                          page)
  #}
  #else if(grepl(".*CONSERVACIÓN (.+) Autor:.*", page) == TRUE){
  #  conservacion <-  gsub(".*CONSERVACIÓN (.+) Autor:.*",
  #                        "\\1",
  #                        page)
  #}
  #concervacion
  if(grepl(".*Autores: (.+) -pol-.*", page) == TRUE){
    autores <-  gsub(".*Autores: (.+) -pol-.*",
                     "\\1",
                     page)
  }
  else if(grepl(".*Autor: (.+) -pol-.*", page) == TRUE){
    autores <-  gsub(".*Autor: (.+) -pol-.*",
                     "\\1",
                     page)
  }
  #autores
  dplyr::tibble(especie = especie,
                clase = clase,
                orden = orden,
                familia = familia,
                nombres_comunes = nombres_comunes,
                categorizacion = categorizacion,
                justificacion = justificacion,
                distribucion  = distribucion,
                amenazas = amenazas,
                conservacion = conservacion,
                autores = autores)
}

anfibios <- pdf[c(42:158)] |>
  purrr::map_df( ~get_info_ficha_anfibios(.))

# ESPECIES DE AVES AMENAZADAS -----------------------------------
get_info_ficha_aves <- function(pdf_page){
  page <- paste0(c("-pol-",
                   paste0(pdf_page,
                          collapse = "-pol-"),
                   "-pol-"), collapse = "") |>
    str_trim() |>
    str_squish()
  #especie <- str_extract(page, "\\-pol\\-\\s*(.*?)\\s*Clase:")
  especie <- gsub(".*-pol- (.+) Clase:.*",
                  "\\1", page)
  #especie
  clase <- gsub(".*Clase: (.+) Orden:.*", "\\1", page)
  #clase
  orden <- gsub(".*Orden: (.+) Familia:.*", "\\1", page)
  #orden
  #familia <- gsub(".*Familia: (.+) CATEGORÍA.*", "\\1", page)
  familia <- gsub(".*Familia: (.+) Nombres comunes:.*",
                  "\\1", page)

  #familia
  nombres_comunes <- gsub(".*Nombres comunes: (.+) CATEGORÍA DE AMENAZA / CRITERIOS DE CATEGORIZACIÓN.*",
                          "\\1",
                          page)
  categorizacion <- gsub(".*Perú: (.+) JUSTIFICACIÓN.*",
                         "\\1",
                         page)
  justificacion <-  gsub(".*JUSTIFICACIÓN (.+) DISTRIBUCIÓN.*",
                         "\\1",
                         page)
  #justificacion
  distribucion <- gsub(".*DISTRIBUCIÓN (.+) AMENAZAS.*",
                       "\\1",
                       page)
  #distribucion
  amenazas <- gsub(".*AMENAZAS (.+) CONSERVACIÓN.*",
                   "\\1",
                   page)
  #amenazas
  if(grepl(".*CONSERVACIÓN (.+) Autores:.*", page) == TRUE){
    conservacion <-  gsub(".*CONSERVACIÓN (.+) Autores:.*",
                          "\\1",
                          page)
  }
  else if(grepl(".*CONSERVACIÓN (.+) Autor:.*", page) == TRUE){
    conservacion <-  gsub(".*CONSERVACIÓN (.+) Autor:.*",
                          "\\1",
                          page)
  }
  #concervacion
  if(grepl(".*Autores: (.+) -pol-.*", page) == TRUE){
    autores <-  gsub(".*Autores: (.+) -pol-.*",
                     "\\1",
                     page)
  }
  else if(grepl(".*Autor: (.+) -pol-.*", page) == TRUE){
    autores <-  gsub(".*Autor: (.+) -pol-.*",
                     "\\1",
                     page)
  }
  #autores
  dplyr::tibble(especie = especie,
                clase = clase,
                orden = orden,
                familia = familia,
                nombres_comunes = nombres_comunes,
                categorizacion = categorizacion,
                justificacion = justificacion,
                distribucion  = distribucion,
                amenazas = amenazas,
                conservacion = conservacion,
                autores = autores)
}

aves_1 <- pdf[c(172:176)] |>
  purrr::map_df( ~get_info_ficha_aves(.))
aves_2 <- paste0(pdf[c(177, 178)], collapse = "") |>
  get_info_ficha_aves()
aves_3 <- pdf[c(179:188)] |>
  purrr::map_df( ~get_info_ficha_aves(.))
aves_4 <- paste0(pdf[c(189, 190)], collapse = "") |>
  get_info_ficha_aves()
aves_5 <- pdf[c(191:204)] |>
  purrr::map_df( ~get_info_ficha_aves(.))
aves_6 <- paste0(pdf[c(205, 206)], collapse = "") |>
  get_info_ficha_aves()
aves_7 <- pdf[c(207:221)] |>
  purrr::map_df( ~get_info_ficha_aves(.))
aves_8 <- paste0(pdf[c(222, 223)], collapse = "") |>
  get_info_ficha_aves()
aves_9 <- pdf[c(224:239)] |>
  purrr::map_df( ~get_info_ficha_aves(.))
aves_10 <- paste0(pdf[c(240, 241)], collapse = "") |>
  get_info_ficha_aves()
aves_11 <- pdf[c(242:248)] |>
  purrr::map_df( ~get_info_ficha_aves(.))
aves_12 <- paste0(pdf[c(240, 250)], collapse = "") |>
  get_info_ficha_aves()
aves_13 <- pdf[c(251:256)] |>
  purrr::map_df( ~get_info_ficha_aves(.))
aves_14 <- paste0(pdf[c(257, 258)], collapse = "") |>
  get_info_ficha_aves()
aves_15 <- pdf[c(259:262)] |>
  purrr::map_df( ~get_info_ficha_aves(.))
aves_16 <- paste0(pdf[c(263, 264)], collapse = "") |>
  get_info_ficha_aves()
#Falta la pagina 265
aves_17 <- pdf[c(266:267)] |>
  purrr::map_df( ~get_info_ficha_aves(.))
aves_18 <- paste0(pdf[c(268, 269)], collapse = "") |>
  get_info_ficha_aves()
aves_19 <- pdf[c(270:295)] |>
  purrr::map_df( ~get_info_ficha_aves(.))
aves_20 <- paste0(pdf[c(296, 297)], collapse = "") |>
  get_info_ficha_aves()
aves_21 <- pdf[c(298:301)] |>
  purrr::map_df( ~get_info_ficha_aves(.))
aves_22 <- paste0(pdf[c(302, 303)], collapse = "") |>
  get_info_ficha_aves()
aves_23 <- pdf[304] |>
  purrr::map_df( ~get_info_ficha_aves(.))
aves <- bind_rows(aves_1, aves_2, aves_3, aves_4, aves_5,
                  aves_6, aves_7, aves_8, aves_9, aves_10,
                  aves_11, aves_12, aves_13, aves_14, aves_15,
                  aves_16, aves_17, aves_18, aves_19, aves_20,
                  aves_21, aves_22, aves_23)

aves


# ESPECIES DE INVERTEBRADOS AMENAZADOS --------------------------
get_info_ficha_invertebrados <- function(pdf_page){
  page <- paste0(c("-pol-",
                   paste0(pdf_page,
                          collapse = "-pol-"),
                   "-pol-"), collapse = "") |>
    str_trim() |>
    str_squish()
  #especie <- str_extract(page, "\\-pol\\-\\s*(.*?)\\s*Clase:")
  especie <- gsub(".*-pol- (.+) Clase:.*",
                  "\\1", page)
  #especie
  clase <- gsub(".*Clase: (.+) Orden:.*", "\\1", page)
  #clase
  orden <- gsub(".*Orden: (.+) Familia:.*", "\\1", page)
  #orden
  familia <- gsub(".*Familia: (.+) CATEGORÍA.*", "\\1", page)
  #familia <- gsub(".*Familia: (.+) Nombres comunes:.*",
  #                "\\1", page)

  #familia
  #nombres_comunes <- gsub(".*Nombres comunes: (.+) CATEGORÍA DE AMENAZA / CRITERIOS DE CATEGORIZACIÓN.*",
  #                        "\\1",
  #                        page)
  categorizacion <- gsub(".*Perú: (.+) JUSTIFICACIÓN.*",
                         "\\1",
                         page)
  justificacion <-  gsub(".*JUSTIFICACIÓN (.+) DISTRIBUCIÓN.*",
                         "\\1",
                         page)
  #justificacion
  distribucion <- gsub(".*DISTRIBUCIÓN (.+) AMENAZAS.*",
                       "\\1",
                       page)
  #distribucion
  amenazas <- gsub(".*AMENAZAS (.+) CONSERVACIÓN.*",
                   "\\1",
                   page)
  #amenazas
  if(grepl(".*CONSERVACIÓN (.+) Autores:.*", page) == TRUE){
    conservacion <-  gsub(".*CONSERVACIÓN (.+) Autores:.*",
                          "\\1",
                          page)
  }
  else if(grepl(".*CONSERVACIÓN (.+) Autor:.*", page) == TRUE){
    conservacion <-  gsub(".*CONSERVACIÓN (.+) Autor:.*",
                          "\\1",
                          page)
  }
  #concervacion
  if(grepl(".*Autores: (.+) -pol-.*", page) == TRUE){
    autores <-  gsub(".*Autores: (.+) -pol-.*",
                     "\\1",
                     page)
  }
  else if(grepl(".*Autor: (.+) -pol-.*", page) == TRUE){
    autores <-  gsub(".*Autor: (.+) -pol-.*",
                     "\\1",
                     page)
  }
  #autores
  dplyr::tibble(especie = especie,
                clase = clase,
                orden = orden,
                familia = familia,
               # nombres_comunes = nombres_comunes,
                categorizacion = categorizacion,
                justificacion = justificacion,
                distribucion  = distribucion,
                amenazas = amenazas,
                conservacion = conservacion,
                autores = autores)
}
invertebrados_1 <- pdf[c(310:328)] |>
  purrr::map_df( ~get_info_ficha_aves(.))
invertebrados_1

# ESPECIES DE MAMÍFEROS AMENAZADOS ------------------------------

get_info_ficha_mamiferos <- function(pdf_page){
  page <- paste0(c("-pol-",
                   paste0(pdf_page,
                          collapse = "-pol-"),
                   "-pol-"), collapse = "") |>
    str_trim() |>
    str_squish()
  #especie <- str_extract(page, "\\-pol\\-\\s*(.*?)\\s*Clase:")
  especie <- gsub(".*-pol- (.+) Clase:.*",
                  "\\1", page)
  #especie
  clase <- gsub(".*Clase: (.+) Orden:.*", "\\1", page)
  #clase
  orden <- gsub(".*Orden: (.+) Familia:.*", "\\1", page)
  #orden
  #familia <- gsub(".*Familia: (.+) CATEGORÍA.*", "\\1", page)
  familia <- gsub(".*Familia: (.+) Nombres comunes:.*",
                  "\\1", page)

  #familia
  nombres_comunes <- gsub(".*Nombres comunes: (.+) CATEGORÍA DE AMENAZA / CRITERIOS DE CATEGORIZACIÓN.*",
                          "\\1",
                          page)
  categorizacion <- gsub(".*Perú: (.+) JUSTIFICACIÓN.*",
                         "\\1",
                         page)
  justificacion <-  gsub(".*JUSTIFICACIÓN (.+) DISTRIBUCIÓN.*",
                         "\\1",
                         page)
  #justificacion
  distribucion <- gsub(".*DISTRIBUCIÓN (.+) AMENAZAS.*",
                       "\\1",
                       page)
  #distribucion
  amenazas <- gsub(".*AMENAZAS (.+) CONSERVACIÓN.*",
                   "\\1",
                   page)
  #amenazas
  if(grepl(".*CONSERVACIÓN (.+) Autores:.*", page) == TRUE){
    conservacion <-  gsub(".*CONSERVACIÓN (.+) Autores:.*",
                          "\\1",
                          page)
  }
  else if(grepl(".*CONSERVACIÓN (.+) Autor:.*", page) == TRUE){
    conservacion <-  gsub(".*CONSERVACIÓN (.+) Autor:.*",
                          "\\1",
                          page)
  }
  #concervacion
  if(grepl(".*Autores: (.+) -pol-.*", page) == TRUE){
    autores <-  gsub(".*Autores: (.+) -pol-.*",
                     "\\1",
                     page)
  }
  else if(grepl(".*Autor: (.+) -pol-.*", page) == TRUE){
    autores <-  gsub(".*Autor: (.+) -pol-.*",
                     "\\1",
                     page)
  }
  #autores <- stringr::str_extract(page,
  #                                    ".*Autor: (.+) -pol-.*"
  #                                    )
  #autores
  dplyr::tibble(especie = especie,
                clase = clase,
                orden = orden,
                familia = familia,
                nombres_comunes = nombres_comunes,
                categorizacion = categorizacion,
                justificacion = justificacion,
                distribucion  = distribucion,
                amenazas = amenazas,
                conservacion = conservacion,
                autores = autores)
}
mamiferos_1 <- pdf[c(338:343)] |>
  purrr::map_df( ~get_info_ficha_mamiferos(.))
mamiferos_2 <- paste0(pdf[c(344, 345)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_3 <- pdf[c(346:347)] |>
  purrr::map_df( ~get_info_ficha_mamiferos(.))
mamiferos_4 <- paste0(pdf[c(348, 349)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_5 <- pdf[c(350)] |>
  purrr::map_df( ~get_info_ficha_mamiferos(.))
mamiferos_6 <- paste0(pdf[c(351, 352)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_7 <- paste0(pdf[c(353, 354)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_8 <- pdf[c(355:359)] |>
  purrr::map_df( ~get_info_ficha_mamiferos(.))
mamiferos_9 <- paste0(pdf[c(360, 361)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_10 <- paste0(pdf[c(362, 363)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_11 <- pdf[c(364:375)] |>
  purrr::map_df( ~get_info_ficha_mamiferos(.))
mamiferos_12 <- paste0(pdf[c(376, 377)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_13 <- pdf[c(378:379)] |>
  purrr::map_df( ~get_info_ficha_mamiferos(.))
mamiferos_14 <- paste0(pdf[c(380, 381)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_15 <- pdf[c(382:383)] |>
  purrr::map_df( ~get_info_ficha_mamiferos(.))
mamiferos_16 <- paste0(pdf[c(384, 385)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_17 <- pdf[c(386)] |>
  purrr::map_df( ~get_info_ficha_mamiferos(.))
mamiferos_18 <- paste0(pdf[c(387, 388)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_19 <- pdf[c(389:403)] |>
  purrr::map_df( ~get_info_ficha_mamiferos(.))
mamiferos_20 <- paste0(pdf[c(404, 405)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_21 <- paste0(pdf[c(406, 407)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_22 <- pdf[c(408:409)] |>
  purrr::map_df( ~get_info_ficha_mamiferos(.))
mamiferos_23 <- paste0(pdf[c(410, 411)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_24 <- pdf[c(412:415)] |>
  purrr::map_df( ~get_info_ficha_mamiferos(.))
mamiferos_25 <- paste0(pdf[c(416, 417)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_26 <- paste0(pdf[c(418, 419)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_27 <- pdf[c(420:427)] |>
  purrr::map_df( ~get_info_ficha_mamiferos(.))
mamiferos_28 <- paste0(pdf[c(428, 429)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_29 <- pdf[c(430:442)] |>
  purrr::map_df( ~get_info_ficha_mamiferos(.))
mamiferos_30 <- paste0(pdf[c(443, 444)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_31 <- paste0(pdf[c(445, 446)], collapse = "") |>
  get_info_ficha_mamiferos()
mamiferos_32 <- pdf[c(447)] |>
  purrr::map_df( ~get_info_ficha_mamiferos(.))

mamiferos <- bind_rows(mamiferos_1, mamiferos_2, mamiferos_3,
                       mamiferos_4, mamiferos_5, mamiferos_6,
                       mamiferos_7, mamiferos_8, mamiferos_9,
                       mamiferos_10, mamiferos_11, mamiferos_12,
                       mamiferos_13, mamiferos_14, mamiferos_15,
                       mamiferos_16, mamiferos_17, mamiferos_18,
                       mamiferos_19, mamiferos_20, mamiferos_21,
                       mamiferos_22, mamiferos_23, mamiferos_24,
                       mamiferos_25, mamiferos_26, mamiferos_27,
                       mamiferos_28, mamiferos_29, mamiferos_30,
                       mamiferos_31, mamiferos_32
                       )

mamiferos

# ESPECIES DE REPTILES AMENAZADOS -------------------------------
get_info_ficha_reptiles <- function(pdf_page){
  page <- paste0(c("-pol-",
                   paste0(pdf_page,
                          collapse = "-pol-"),
                   "-pol-"), collapse = "") |>
    str_trim() |>
    str_squish()
  #especie <- str_extract(page, "\\-pol\\-\\s*(.*?)\\s*Clase:")
  especie <- gsub(".*-pol- (.+) Clase:.*",
                  "\\1", page)
  #especie
  clase <- gsub(".*Clase: (.+) Orden:.*", "\\1", page)
  #clase
  orden <- gsub(".*Orden: (.+) Familia:.*", "\\1", page)
  #orden
  #familia <- gsub(".*Familia: (.+) CATEGORÍA.*", "\\1", page)
  familia <- gsub(".*Familia: (.+) Nombres comunes:.*",
                  "\\1", page)

  #familia
  nombres_comunes <- gsub(".*Nombres comunes: (.+) CATEGORÍA DE AMENAZA / CRITERIOS DE CATEGORIZACIÓN.*",
                          "\\1",
                          page)
  categorizacion <- gsub(".*Perú: (.+) JUSTIFICACIÓN.*",
                         "\\1",
                         page)
  justificacion <-  gsub(".*JUSTIFICACIÓN (.+) DISTRIBUCIÓN.*",
                         "\\1",
                         page)
  #justificacion
  distribucion <- gsub(".*DISTRIBUCIÓN (.+) AMENAZAS.*",
                       "\\1",
                       page)
  #distribucion
  amenazas <- gsub(".*AMENAZAS (.+) CONSERVACIÓN.*",
                   "\\1",
                   page)
  #amenazas

  if(grepl(".*CONSERVACIÓN (.+) Autores:.*", page) == TRUE){
    conservacion <-  gsub(".*CONSERVACIÓN (.+) Autores:.*",
                     "\\1",
                     page)
  }
  else if(grepl(".*CONSERVACIÓN (.+) Autor:.*", page) == TRUE){
    conservacion <-  gsub(".*CONSERVACIÓN (.+) Autor:.*",
                     "\\1",
                     page)
  }
  #concervacion
  if(grepl(".*Autores: (.+) -pol-.*", page) == TRUE){
    autores <-  gsub(".*Autores: (.+) -pol-.*",
                     "\\1",
                     page)
  }
  else if(grepl(".*Autor: (.+) -pol-.*", page) == TRUE){
    autores <-  gsub(".*Autor: (.+) -pol-.*",
                     "\\1",
                     page)
  }
  #autores <- stringr::str_extract(page,
  #                                    ".*Autor: (.+) -pol-.*"
  #                                    )
  #autores
  dplyr::tibble(especie = especie,
                clase = clase,
                orden = orden,
                familia = familia,
                nombres_comunes = nombres_comunes,
                categorizacion = categorizacion,
                justificacion = justificacion,
                distribucion  = distribucion,
                amenazas = amenazas,
                conservacion = conservacion,
                autores = autores)
}
reptiles_1 <- pdf[c(456:460)] |>
  purrr::map_df( ~get_info_ficha_reptiles(.))
reptiles_2 <- paste0(pdf[c(461, 462)], collapse = "") |>
  get_info_ficha_reptiles()
reptiles_3 <- pdf[c(463:468)] |>
  purrr::map_df( ~get_info_ficha_reptiles(.))
reptiles_4 <- paste0(pdf[c(469, 470)], collapse = "") |>
  get_info_ficha_reptiles()
reptiles_5 <- pdf[c(471:488)] |>
  purrr::map_df( ~get_info_ficha_reptiles(.))
reptiles <- bind_rows(reptiles_1, reptiles_2, reptiles_3,
                      reptiles_4, reptiles_5)
reptiles

# combine -------------------------------------------------------


red_book_f <- bind_rows(anfibios, aves, invertebrados_1,
                        mamiferos, reptiles)
red_book_f |>
  pull(autores)
red_book_f |>
  mutate(pagina = str_extract(autores, "[0-9]{1,}"),
         autores = tm::removeNumbers(autores) |>
           str_trim() |>
           str_squish(),
         categorizacion = str_remove(categorizacion,
                                     "CR EN VU |CR . EN VU|CR EN VU") |>
           str_trim() |>
           str_squish()
           ) |>
  writexl::write_xlsx("fauna_red_book.xlsx")
