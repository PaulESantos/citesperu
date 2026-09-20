# Fauna silvestre de Perú incluida en los Apéndices de la CITES (2023)

Base de datos estructurada a partir del documento oficial *"Listado de
especies de Fauna Silvestre CITES - Perú (v.2023 MAR)"*, publicado por
el Ministerio del Ambiente (MINAM) como Autoridad Científica CITES en
marzo de 2023.

## Format

Un data frame con 568 observaciones y 17 variables:

- n:

  Número correlativo oficial asignado a la especie de 1 a 568.

- fauna_silvestre_especie_hidrobiologica:

  Competencia sectorial de administración: `"FAUNA"` (recursos
  terrestres bajo rectoría de SERFOR) o `"HIDROBIOLÓGICO"` (recursos
  acuáticos/pesqueros bajo rectoría de PRODUCE).

- phyllum:

  Phyllum al que pertenece la especie (*Chordata*, *Cnidaria*,
  *Echinodermata*).

- clase:

  Clase taxonómica (*Actinopterygii*, *Amphibia*, *Anthozoa*, *Aves*,
  *Chondrichthyes*, *Holothuroidea*, *Mammalia*, *Reptilia*).

- orden:

  Orden taxonómico.

- familia:

  Familia taxonómica.

- genero:

  Género taxonómico.

- especie_nombre_cientifico:

  Nombre científico de la especie.

- nombre_comun:

  Nombres vernáculos o comunes registrados (variable multivaluada
  separada por comas).

- apendice:

  Apéndice CITES en el que se encuentra regulada la especie (`"I"`,
  `"II"`, `"III"` o `"III/w"`).

- inclusion_y_o_enmienda:

  Año de inclusión inicial o de la última enmienda de la especie en los
  Apéndices de la CITES (ej. 1975, 1987, 2019, 2023).

- categoria_nacional:

  Categoría de conservación nacional según el D.S. n.° 004-2014-MINAGRI:
  `"CR"`, `"EN"`, `"VU"`, `"NT"`, `"DD"` o `NA`.

- uicn:

  Categoría en la Lista Roja de la UICN (`"CR"`, `"EN"`, `"VU"`, `"NT"`,
  `"LC"`, `"DD"`, `"LR"`, `"NE"`).

- geografia:

  Condición de distribución geográfica en el Perú: `"Nativa"` o
  `"Endémica"`.

- autor:

  Autoría y año de descripción científica de la especie.

- sinonimos:

  Sinónimos taxonómicos consignados en la publicación (variable
  multivaluada).

- comentarios_de_referencia:

  Comentarios técnicos de referencia, alcance de inclusión por
  orden/familia o precisiones de poblaciones.

## Source

Ministerio del Ambiente (MINAM). 2023. *Listado de especies de Fauna
Silvestre CITES - Perú (v.2023 MAR)*. Dirección General de Diversidad
Biológica, Lima, Perú. Publicación oficial en Gob.pe:
<https://www.gob.pe/institucion/minam/informes-publicaciones/4109405-listado-de-fauna-cites-peru-2023>

## Details

Incorpora las resoluciones y enmiendas adoptadas en la 19.ª Conferencia
de las Partes (CoP19, Panamá 2022) que entraron en vigor internacional
el 23 de febrero de 2023. Comprende 568 especies oficiales con presencia
confirmada en el Perú (48 en Apéndice I, 503 en Apéndice II y 17 en
Apéndice III), categorizadas además por la competencia de gestión
sectorial (Fauna Silvestre bajo SERFOR o Especie Hidrobiológica bajo
PRODUCE).

**Composición Oficial por Clases (2023):** El compendio oficial reporta
un total de 568 especies:

- **Actinopterigios:** 2 especies (Apéndice II).

- **Anfibios:** 79 especies (1 en Apéndice I, 78 en Apéndice II).

- **Antozoos:** 5 especies (Apéndice II).

- **Aves:** 296 especies (10 en Apéndice I, 280 en Apéndice II, 6 en
  Apéndice III).

- **Condrictios:** 39 especies (1 en Apéndice I, 34 en Apéndice II, 4 en
  Apéndice III).

- **Holoturoideos:** 1 especie (Apéndice III).

- **Mamíferos:** 114 especies (29 en Apéndice I, 80 en Apéndice II, 5 en
  Apéndice III).

- **Reptiles:** 32 especies (7 en Apéndice I, 24 en Apéndice II, 1 en
  Apéndice III).

- **Total general:** 48 en Apéndice I, 503 en Apéndice II, 17 en
  Apéndice III = 568 especies.

## Examples

``` r
data(cites_fauna_peru_2023)

# Conteo de especies por competencia sectorial y apéndice
table(cites_fauna_peru_2023$fauna_silvestre_especie_hidrobiologica, cites_fauna_peru_2023$apendice)
#>                 
#>                    I  II III
#>   FAUNA           37 436  12
#>   HIDROBIOLÓGICO  11  67   5

# Especies incorporadas o enmendadas en la CoP19 (2023)
subset(cites_fauna_peru_2023, inclusion_y_o_enmienda == 2023)
#> # A tibble: 56 × 17
#>        n fauna_silvestre_especie_hidrobiolo…¹ phyllum clase orden familia genero
#>    <dbl> <chr>                                <chr>   <chr> <chr> <chr>   <chr> 
#>  1     5 FAUNA                                CHORDA… AMPH… ANURA Centro… Centr…
#>  2     6 FAUNA                                CHORDA… AMPH… ANURA Centro… Centr…
#>  3     7 FAUNA                                CHORDA… AMPH… ANURA Centro… Centr…
#>  4     8 FAUNA                                CHORDA… AMPH… ANURA Centro… Centr…
#>  5     9 FAUNA                                CHORDA… AMPH… ANURA Centro… Centr…
#>  6    10 FAUNA                                CHORDA… AMPH… ANURA Centro… Centr…
#>  7    11 FAUNA                                CHORDA… AMPH… ANURA Centro… Centr…
#>  8    12 FAUNA                                CHORDA… AMPH… ANURA Centro… Chime…
#>  9    13 FAUNA                                CHORDA… AMPH… ANURA Centro… Chime…
#> 10    14 FAUNA                                CHORDA… AMPH… ANURA Centro… Cochr…
#> # ℹ 46 more rows
#> # ℹ abbreviated name: ¹​fauna_silvestre_especie_hidrobiologica
#> # ℹ 10 more variables: especie_nombre_cientifico <chr>, nombre_comun <chr>,
#> #   apendice <chr>, inclusion_y_o_enmienda <dbl>, categoria_nacional <chr>,
#> #   uicn <chr>, geografia <chr>, autor <chr>, sinonimos <chr>,
#> #   comentarios_de_referencia <chr>
```
