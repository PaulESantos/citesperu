# Clasificar y Normalizar Nombres Científicos en Componentes Taxonómicos

Desglosa y normaliza nombres científicos de plantas o animales en sus
componentes taxonómicos elementales: género, epíteto específico, rango
infraespecífico, epíteto infraespecífico y autoría. Detecta además
calificadores botánicos y zoológicos habituales (`cf.`, `aff.`, `sp.`,
`spp.`, marcadores de híbrido).

Esta función sigue la convención establecida en paquetes como
`wcvpmatch`:

- `orig_genus`: Género en formato *Title Case* (primera letra
  mayúscula).

- `orig_species`: Epíteto específico en minúsculas (sin acentos).

- `infra_rank`: Rango infraespecífico en minúsculas (`"subsp."`,
  `"var."`, `"f."`).

- `orig_infraspecies`: Epíteto infraespecífico en minúsculas.

- `canonical_name`: Nombre binomial o trinominal canónico reconstruido.

- `author`: Autoría taxonómica recuperada cuando está presente.

- Banderas lógicas: `has_cf`, `has_aff`, `is_sp`, `is_spp`,
  `had_hybrid`.

## Usage

``` r
cites_classify_names(splist, name_col = NULL)

classify_spnames(splist, name_col = NULL)
```

## Arguments

- splist:

  Vector de caracteres con nombres científicos, o un data frame / tibble
  con una columna de nombres.

- name_col:

  Nombre de la columna que contiene los nombres si `splist` es un data
  frame. Por defecto busca columnas como `"species"`, `"especie"`,
  `"nombre_cientifico"` o `"name"`.

## Value

Un `tibble` con una fila por cada nombre de entrada y las columnas
normalizadas y banderas.

## Examples

``` r
cites_classify_names(c(
  "Tremarctos ornatus (F. G. Cuvier, 1825)",
  "Swietenia macrophylla King",
  "Phragmipedium boissierianum var. czerwiakowianum",
  "Cedrela cf. odorata",
  "Touit sp.",
  "X Haagespostoa albisetata"
))
#> # A tibble: 6 × 14
#>   input_index input_name       canonical_name orig_genus orig_species infra_rank
#>         <int> <chr>            <chr>          <chr>      <chr>        <chr>     
#> 1           1 Tremarctos orna… Tremarctos or… Tremarctos ornatus      NA        
#> 2           2 Swietenia macro… Swietenia mac… Swietenia  macrophylla  NA        
#> 3           3 Phragmipedium b… Phragmipedium… Phragmipe… boissierian… var.      
#> 4           4 Cedrela cf. odo… Cedrela odora… Cedrela    odorata      NA        
#> 5           5 Touit sp.        Touit          Touit      NA           NA        
#> 6           6 X Haagespostoa … Haagespostoa … Haagespos… albisetata   NA        
#> # ℹ 8 more variables: orig_infraspecies <chr>, author <chr>, rank <dbl>,
#> #   has_cf <lgl>, has_aff <lgl>, is_sp <lgl>, is_spp <lgl>, had_hybrid <lgl>
```
