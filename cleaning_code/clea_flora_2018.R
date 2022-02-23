library(tidyfst)
library(tidyverse)
flora <- readxl::read_xlsx("flora_2018.xlsx") |>
  janitor::clean_names() |>
  dplyr::mutate_all(~stringr::str_squish(.) |>
                      stringr::str_trim())
head(flora)

flora |>
  select(especie, sinonimia, distribucion_nacional) |>
  slice(1989) |>
  separate_rows(sinonimia, sep = ",") |>
  separate_rows(distribucion_nacional, sep = ",") |>
  mutate_all(~str_trim(.) |> str_squish()) |>
  distinct(sinonimia)
flora |>
  select(sinonimia) |>
  slice(1989) |>
  pull()

flora|>
  tidyr::separate_rows(sinonimia, sep = ",") |>
  tidyr::separate_rows(distribucion_nacional, sep = ",") |>
  dplyr::mutate(distribucion_nacional = stringr::str_trim(distribucion_nacional) |>
           stringr::str_squish() |>
           stringr::str_remove("\\."),
           sinonimia = str_trim(sinonimia) |>
             str_squish()) |>
  dplyr::mutate(distribucion_nacional = dplyr::case_when(
    distribucion_nacional == "AM" ~ "Amazonas",
    distribucion_nacional == "AN" ~ "Áncash",
    distribucion_nacional == "AP" ~ "Apurímac",
    distribucion_nacional == "AY" ~ "Ayacucho",
    distribucion_nacional == "AR" ~ "Arequipa",
    distribucion_nacional == "CA" ~ "Cajamarca",
    distribucion_nacional == "CU" ~ "Cusco",
    distribucion_nacional == "HV" ~ "Huancavelica",
    distribucion_nacional == "HU" ~ "Huánuco",
    distribucion_nacional == "IC" ~ "Ica",
    distribucion_nacional == "JU" ~ "Junín",
    distribucion_nacional == "LL" ~ "La Libertad",
    distribucion_nacional == "LA" ~ "Lambayeque",
    distribucion_nacional == "LI" ~ "Lima",
    distribucion_nacional == "LO" ~ "Loreto",
    distribucion_nacional == "MD" ~ "Madre de Dios",
    distribucion_nacional == "MO" ~ "Moquegua",
    distribucion_nacional == "PA" ~ "Pasco",
    distribucion_nacional == "PI" ~ "Piura",
    distribucion_nacional == "PU" ~ "Puno",
    distribucion_nacional == "SM" ~ "San Martín",
    distribucion_nacional == "TA" ~ "Tacna",
    distribucion_nacional == "TU" ~ "Tumbes",
    distribucion_nacional == "UC" ~ "Ucayali",
    TRUE ~ distribucion_nacional
  )
  ) |>
  mutate(nombre_local = str_trim(nombre_local) |>
           str_squish() |>
           str_to_sentence() |>
           str_remove_all("\\”|\\“") |>
           str_replace_all("\\.|;|,$", "")) |>
  mutate(nombre_local = if_else(str_detect(nombre_local,
                                           "registro|regsitro"),
                                "Sin registro",
                                nombre_local),
         nombre_local = if_else(is.na(nombre_local),
                                "Sin registro",
                                nombre_local),
         endemismo = if_else(endemismo == "Endemica",
                             "Endémica",
                             endemismo)) -> flora_clean

flora_clean |> head()
flora_clean |>
  distinct(endemismo) |>
  pull()
flora_clean |>
  select(especie) |>
  unique()
flora_clean |>
  writexl::write_xlsx("flora_2018_clean.xlsx")
