# Verificación Booleana de Inclusión en los Apéndices CITES del Perú

Comprueba de forma vectorizada si cada uno de los nombres científicos
consultados se encuentra incluido en los Apéndices CITES del Perú (como
taxón aceptado o sinónimo oficial). Devuelve un vector lógico del mismo
largo y orden que el vector de entrada.

## Usage

``` r
is_cites(
  splist,
  taxon = c("all", "fauna", "flora"),
  edition = c("latest", "all", "2023", "2019", "2018"),
  allow_synonyms = TRUE
)

is_cites_pe(
  splist,
  taxon = c("all", "fauna", "flora"),
  edition = c("latest", "all", "2023", "2019", "2018"),
  allow_synonyms = TRUE
)
```

## Arguments

- splist:

  Vector de caracteres con nombres científicos, o un `data.frame` con
  una columna de nombres taxonómicos.

- taxon:

  Subconjunto de evaluación: `"all"` (fauna y flora, por defecto),
  `"fauna"` o `"flora"`.

- edition:

  Edición del listado oficial a consultar (`"latest"` por defecto).

- allow_synonyms:

  Si es `TRUE` (por defecto), considera válidos los sinónimos oficiales
  registrados en los documentos del MINAM.

## Value

Un vector lógico (`TRUE`, `FALSE` o `NA`) con la misma longitud que
`splist`.

## Examples

``` r
# Consulta vectorizada simple
is_cites(c(
  "Tremarctos ornatus",
  "Swietenia macrophylla",
  "Canis lupus familiaris",
  "Homo sapiens"
))
#> [1]  TRUE  TRUE FALSE FALSE

# Consulta diferenciando fauna y flora
is_cites("Tremarctos ornatus", taxon = "fauna")
#> [1] TRUE
is_cites("Tremarctos ornatus", taxon = "flora")
#> [1] FALSE
```
