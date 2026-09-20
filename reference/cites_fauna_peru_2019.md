# Fauna silvestre de Perú incluida en los Apéndices de la CITES (2019)

Base de datos estructurada a partir del documento oficial *"Listado de
especies de Fauna Silvestre CITES - Perú (v.2019-01)"*, publicado por el
Ministerio del Ambiente (MINAM) como Autoridad Científica CITES.

## Format

Un data frame con 523 observaciones y 17 variables:

- id:

  Identificador numérico correlativo de la especie en el listado.

- phylum:

  Phylum taxonómico (*Chordata*, *Cnidaria*, *Echinodermata*).

- clase:

  Clase taxonómica (*Actinopterygii*, *Amphibia*, *Anthozoa*, *Aves*,
  *Chondrichthyes*, *Holothuroidea*, *Mammalia*, *Reptilia*).

- subclase:

  Subclase taxonómica cuando aplica (ej. *Elasmobranchii*) o `NA`.

- orden:

  Orden taxonómico.

- familia:

  Familia taxonómica.

- genero:

  Género taxonómico.

- nombre_cientifico:

  Nombre científico de la especie.

- apendice:

  Apéndice CITES en el que se encuentra incluida la especie (`"I"`,
  `"II"`, `"III"` o `"III/w"`).

- ds_n_004_2014:

  Categoría de conservación nacional según el D.S. n.° 004-2014-MINAGRI:
  `"CR"`, `"EN"`, `"VU"`, `"NT"` o `NA`.

- uicn:

  Categoría en la Lista Roja de la UICN (`"CR"`, `"EN"`, `"VU"`, `"NT"`,
  `"LC"`, `"DD"` o `NA`).

- geografia:

  Condición de distribución geográfica en el Perú: `"Nativa"` o
  `"Endémica"`.

- nombre_comun:

  Nombre o nombres comunes vernáculos (variable multivaluada separada
  por comas).

- sinonimos:

  Sinónimos taxonómicos consignados en la publicación (variable
  multivaluada).

- autor:

  Autoría y año de descripción científica del taxón.

- comentarios_de_referencia:

  Notas aclaratorias, comentarios sobre distribución, citas o
  precisiones taxonómicas.

- ambito:

  Ámbito ecológico/geográfico principal asignado en el listado (ej.
  `"AMA"` para Amazónico, `"MAR"` para Marino, `"AND"` para Andino,
  `"COS"` para Costero).

## Source

Ministerio del Ambiente (MINAM). 2019. *Listado de Fauna CITES Perú
v.2019-01*. Dirección General de Diversidad Biológica, Lima, Perú. Ficha
oficial en Gob.pe:
<https://www.gob.pe/institucion/minam/informes-publicaciones/395694-listado-de-fauna-cites-peru-2019>

## Details

Esta actualización consolida 523 registros taxonómicos de fauna
silvestre y especies hidrobiológicas presentes en el Perú, incorporando
precisiones sobre género, ámbito ecológico (amazónico, marino, etc.) y
comentarios de referencia actualizados.

## Examples

``` r
data(cites_fauna_peru_2019)

# Distribución por ámbito ecológico
table(cites_fauna_peru_2019$ambito, useNA = "ifany")
#> 
#>              AMA        AMA - AND  AMA - AND - COS AMA - AND - COST 
#>              170              168                4                5 
#>        AMA - COS       AMA - COST              AND        AND - AMA 
#>                1                1               59                1 
#>  AND - AMA - COS        AND - COS  AND - COS - AMA       AND - COST 
#>                1               23                1                4 
#>              COS              IND              MAR             <NA> 
#>               72                1                1               11 

# Especies amenazadas a nivel nacional en Apéndice I
subset(cites_fauna_peru_2019, apendice == "I" & !is.na(ds_n_004_2014))
#> # A tibble: 35 × 17
#>       id phylum   clase subclase orden familia genero nombre_cientifico apendice
#>    <dbl> <chr>    <chr> <chr>    <chr> <chr>   <chr>  <chr>             <chr>   
#>  1    46 CHORDATA AMPH… NA       ANURA Dendro… Telma… Telmatobius cule… I       
#>  2    75 CHORDATA AVES  NA       ACCI… Accipi… Harpa… Harpia harpyja    I       
#>  3   225 CHORDATA AVES  NA       CATH… Cathar… Vultur Vultur gryphus    I       
#>  4   226 CHORDATA AVES  NA       CICO… Ciconi… Jabiru Jabiru mycteria   I       
#>  5   233 CHORDATA AVES  NA       FALC… Falcon… Falco  Falco peregrinus  I       
#>  6   245 CHORDATA AVES  NA       GALL… Cracid… Penel… Penelope albipen… I       
#>  7   261 CHORDATA AVES  NA       PSIT… Psitta… Ara    Ara macao         I       
#>  8   290 CHORDATA AVES  NA       PSIT… Psitta… Primo… Primolius couloni I       
#>  9   305 CHORDATA AVES  NA       RHEI… Rheidae Rhea   Rhea pennata gar… I       
#> 10   306 CHORDATA AVES  NA       SPHE… Spheni… Sphen… Spheniscus humbo… I       
#> # ℹ 25 more rows
#> # ℹ 8 more variables: ds_n_004_2014 <chr>, uicn <chr>, geografia <chr>,
#> #   nombre_comun <chr>, sinonimos <chr>, autor <chr>,
#> #   comentarios_de_referencia <chr>, ambito <chr>
```
