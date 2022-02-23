fauna <- readxl::read_excel("fauna_2019.xlsx") |>
  clean_names() |>
  mutate(phyllum = str_remove(phyllum, "PHYLLUM: ") |>
           str_to_sentence() |> str_squish() |> str_trim(),
         clase =  str_remove(clase, "CLASE: ") |>
           str_to_sentence() |> str_squish() |> str_trim(),
         subclase =  str_remove(subclase, "SUBCLASE: ") |>
           str_to_sentence() |> str_squish() |> str_trim(),
         orden =  str_remove(orden, "ORDEN: |ORDEN:") |>
           str_to_sentence() |> str_squish() |> str_trim(),
         familia = str_remove(familia, "Familia: |Familia:") |>
           str_to_sentence() |> str_squish() |> str_trim(),
         genero = str_remove(genero, "Género: ") |>
           str_to_sentence() |> str_squish() |>
           str_trim(),
         ds_n_004_2014 = dplyr::case_when(
           ds_n_004_2014 == "LC" ~ "Preocupación menor",
           ds_n_004_2014 == "VU" ~ "Vulnerable",
           ds_n_004_2014 == "DD" ~ "Datos insuficientes",
           ds_n_004_2014 == "NT" ~ "Casi amenazado",
           ds_n_004_2014 == "CR" ~ "En peligro crítico",
           ds_n_004_2014 == "EN" ~ "En peligro",
           TRUE ~  ds_n_004_2014
         ),
         uicn = dplyr::case_when(
           uicn == "LC" ~ "Preocupación menor",
           uicn == "VU" ~ "Vulnerable",
           uicn == "DD" ~ "Datos insuficientes",
           uicn == "NT" ~ "Casi amenazado",
           uicn == "CR" ~ "En peligro crítico",
           uicn == "EN" ~ "En peligro",
           TRUE ~  uicn
         ),
         geografia = if_else(geografia == "nativo",
                             "Nativa",
                             geografia)
         )
fauna |> head()
fauna |>
  distinct(ambito) |>
  pull()
fauna |>
  filter(is.na(ds_n_004_2014))
  filter(str_detect(nombre_cientifico, "Lagothrix"))
