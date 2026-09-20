# Consultar y Contrastar Especies con los Apéndices CITES del Perú

Compara una lista de nombres científicos de plantas o animales contra
las bases de datos oficiales de CITES Perú (Fauna y Flora), ejecutando
un pipeline de concordancia secuencial optimizado:

1.  **Direct match**: Coincidencia exacta con un taxón CITES aceptado.

2.  **Synonym match**: Coincidencia con un sinónimo oficial registrado
    en las publicaciones del MINAM, resolviendo el registro al taxón
    aceptado y su respectivo Apéndice.

3.  **Suffix match**: Detección de variaciones ortográficas de género en
    sufijos latinos (`-us`, `-a`, `-um`, `-is`, `-e`) dentro del mismo
    género.

4.  **Fuzzy match**: Coincidencia aproximada por distancia de edición
    (*Levenshtein/OSA*) acotada al mismo género según el umbral
    `max_dist`.

5.  **Genus match**: Detección si un género ingresado (o con calificador
    `sp.`/`spp.`) cuenta con regulación CITES a nivel genérico o de
    familia superior.

6.  **Unmatched**: Nombres sin coincidencia en los listados oficiales
    nacionales (`is_cites = FALSE`).

## Usage

``` r
cites_match(
  splist,
  taxon = c("all", "fauna", "flora"),
  edition = c("latest", "all", "2023", "2019", "2018"),
  max_dist = 1,
  allow_synonyms = TRUE,
  genus_fallback = FALSE,
  output = c("standard", "full")
)

cites_matching(
  splist,
  taxon = c("all", "fauna", "flora"),
  edition = c("latest", "all", "2023", "2019", "2018"),
  max_dist = 1,
  allow_synonyms = TRUE,
  genus_fallback = FALSE,
  output = c("standard", "full")
)

match_cites_pe(
  splist,
  taxon = c("all", "fauna", "flora"),
  edition = c("latest", "all", "2023", "2019", "2018"),
  max_dist = 1,
  allow_synonyms = TRUE,
  genus_fallback = FALSE,
  output = c("standard", "full")
)
```

## Arguments

- splist:

  Vector de caracteres con nombres científicos, o un `data.frame` /
  `tibble` con una columna de nombres taxonómicos.

- taxon:

  Subconjunto de evaluación: `"all"` (fauna y flora, por defecto),
  `"fauna"` o `"flora"`.

- edition:

  Edición del listado oficial a consultar:

  - `"latest"` (por defecto): Listado de Fauna 2023 (v.2023 MAR) +
    Listado de Flora 2018.

  - `"all"`: Incluye registros históricos de 2018, 2019 y 2023.

  - `"2023"`: Exclusivamente la edición de Fauna 2023.

  - `"2019"`: Exclusivamente la edición de Fauna 2019.

  - `"2018"`: Edición 2018 (Fauna 2018 y Flora 2018).

- max_dist:

  Distancia máxima de edición permitida para la fase difusa (*fuzzy
  match*). Por defecto es `1` (permite 1 inserción, sustitución o
  eliminación dentro del mismo género).

- allow_synonyms:

  Valor lógico. Si es `TRUE` (por defecto), resuelve coincidencias con
  sinónimos oficiales al taxón CITES aceptado. Si es `FALSE`, solo busca
  nombres aceptados.

- genus_fallback:

  Valor lógico. Si es `FALSE` (por defecto), los nombres binominales que
  no coinciden a nivel de especie permanecen como no listados
  (`unmatched`). Si es `TRUE`, se permite que binomios sin coincidencia
  específica hagan match con el género si este está regulado.

- output:

  Formato de salida:

  - `"standard"` (por defecto): Devuelve las columnas esenciales para
    análisis de conservación.

  - `"full"`: Devuelve la totalidad de componentes de parsing, banderas
    (`has_cf`, `is_sp`) y metadatos de autoría.

## Value

Un `tibble` con los resultados de la concordancia preservando el orden
original de entrada.

## Examples

``` r
# Consulta exacta y sinónimos
cites_match(c(
  "Tremarctos ornatus",
  "Epipedobates femoralis",
  "Swietenia macrophylla",
  "Paphiopedilum besseae",
  "Homo sapiens"
))
#> # A tibble: 5 × 13
#>   input_index input_name matched_name accepted_name match_type is_cites apendice
#>         <int> <chr>      <chr>        <chr>         <chr>      <lgl>    <chr>   
#> 1           1 Tremarcto… Tremarctos … Tremarctos o… exact      TRUE     I       
#> 2           2 Epipedoba… Epipedobate… Allobates fe… synonym    TRUE     II      
#> 3           3 Swietenia… Swietenia m… Swietenia ma… exact      TRUE     II      
#> 4           4 Paphioped… Paphiopedil… Phragmipediu… synonym    TRUE     I       
#> 5           5 Homo sapi… NA           NA            unmatched  FALSE    NA      
#> # ℹ 6 more variables: taxon <chr>, clase <chr>, familia <chr>,
#> #   categoria_nacional <chr>, uicn <chr>, matched_dist <int>

# Consulta con errores tipográficos (fuzzy match)
cites_match(c("Tremarctos ornatu", "Swietenia macrofila"), max_dist = 2)
#> # A tibble: 2 × 13
#>   input_index input_name matched_name accepted_name match_type is_cites apendice
#>         <int> <chr>      <chr>        <chr>         <chr>      <lgl>    <chr>   
#> 1           1 Tremarcto… Tremarctos … Tremarctos o… fuzzy      TRUE     I       
#> 2           2 Swietenia… NA           NA            unmatched  FALSE    NA      
#> # ℹ 6 more variables: taxon <chr>, clase <chr>, familia <chr>,
#> #   categoria_nacional <chr>, uicn <chr>, matched_dist <int>

# Consulta a nivel de género
cites_match(c("Cedrela sp.", "Touit spp."))
#> # A tibble: 2 × 13
#>   input_index input_name matched_name accepted_name match_type is_cites apendice
#>         <int> <chr>      <chr>        <chr>         <chr>      <lgl>    <chr>   
#> 1           1 Cedrela s… Cedrela      Cedrela spp.  genus      TRUE     III     
#> 2           2 Touit spp. Touit        Touit spp.    genus      TRUE     II      
#> # ℹ 6 more variables: taxon <chr>, clase <chr>, familia <chr>,
#> #   categoria_nacional <chr>, uicn <chr>, matched_dist <int>
```
