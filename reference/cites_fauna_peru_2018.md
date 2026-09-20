# Fauna silvestre de Perú incluida en los Apéndices de la CITES (2018)

Base de datos estructurada a partir del documento oficial *"Listado de
especies de Fauna Silvestre CITES - Perú"*, publicado por el Ministerio
del Ambiente (MINAM) como Autoridad Científica CITES en enero de 2018.

## Format

Un data frame con 512 observaciones (496 especies oficiales reguladas
más 16 asociadas al Apéndice III) y 17 variables:

- id_especie:

  Identificador numérico secuencial único asignado al registro.

- phylum:

  Phylum al que pertenece la especie según la clasificación de la fuente
  (*Chordata*, *Cnidaria* o *Echinodermata*).

- clase:

  Clase taxonómica (*Actinopterygii*, *Amphibia*, *Anthozoa*, *Aves*,
  *Chondrichthyes*, *Holothuroidea*, *Mammalia*, *Reptilia*).

- subclase:

  Subclase taxonómica cuando aplica (ej. *Elasmobranchii*) o `NA`.

- orden:

  Orden taxonómico.

- familia:

  Familia taxonómica.

- referencia_num:

  Número correlativo de llamada de nota explicativa o referencia
  bibliográfica al pie en la publicación.

- n:

  Número correlativo asignado dentro de cada clase en el listado
  original.

- especie:

  Nombre científico del taxón aceptado.

- categoria_nacional:

  Categoría de conservación nacional según el Decreto Supremo n.°
  004-2014-MINAGRI: `"CR"` (En peligro crítico), `"EN"` (En peligro),
  `"VU"` (Vulnerable), `"NT"` (Casi amenazado), `"DD"` (Datos
  insuficientes) o `NA` (no categorizado).

- uicn:

  Categoría en la Lista Roja de la UICN (`"CR"`, `"EN"`, `"VU"`, `"NT"`,
  `"LC"`, `"DD"` o `NA`).

- n_e:

  Condición de distribución indicada en el listado: `"Nativa"` o
  `"Endémica"`.

- nombre_comun:

  Nombre o nombres comunes vernáculos consignados en el documento fuente
  (variable multivaluada separada por comas).

- sinonimos:

  Nombres científicos tratados como sinónimos taxonómicos en la
  publicación (variable multivaluada).

- autor:

  Autoría y año de descripción científica de la especie.

- ap:

  Apéndice de la CITES en el que se encuentra incluida la especie
  (`"I"`, `"II"` o `"III"`).

- referencia_text:

  Texto explicativo de la nota técnica de referencia o aclaración sobre
  presencia geográfica y distribución.

## Source

Ministerio del Ambiente (MINAM). 2018. *Listado de especies de Fauna
Silvestre CITES - Perú*. Dirección General de Diversidad Biológica,
Lima, Perú. Compendio oficial en Gob.pe:
<https://www.gob.pe/institucion/minam/informes-publicaciones/395692-listado-fauna-cites-peru-2018>

## Details

La base contiene los registros de especies de fauna silvestre con
distribución en el Perú incluidas en los Apéndices I, II y III de la
Convención sobre el Comercio Internacional de Especies Amenazadas de
Fauna y Flora Silvestres (CITES).

La fuente organiza las especies de acuerdo con su clasificación
taxonómica superior (Phylum, Clase, Subclase, Orden y Familia) y
presenta para cada registro información sobre el nombre científico,
Apéndice CITES, categoría de conservación nacional según el D.S. n.°
004-2014-MINAGRI, categoría de la Lista Roja de la UICN, condición de
distribución (Nativa o Endémica), nombres comunes, sinónimos, autoría
taxonómica y notas explicativas.

**Composición y Cifras Oficiales:** La publicación oficial reporta un
total de 496 especies peruanas reguladas:

- **Apéndice I:** 48 especies (29 mamíferos, 10 aves, 7 reptiles, 1
  anfibio, 1 condrictio).

- **Apéndice II:** 448 especies (274 aves, 84 mamíferos, 45 anfibios, 21
  reptiles, 17 condrictios, 5 antozoos, 2 actinopterigios).

- **Apéndice III:** 16 especies con presencia nacional (7 aves, 7
  mamíferos, 1 reptil, 1 holoturoideo).

**Tratamiento del Apéndice III:** El MINAM aclara expresamente que las
especies del Apéndice III presentes en el país fueron incluidas a
solicitud de otros Estados Parte; debido a que el Perú no ha solicitado
la inclusión de ninguna especie en dicho apéndice, estos 16 registros no
forman parte de la suma total oficial de 496 especies.

**Actualización CoP17 (Johannesburgo 2016):** Esta edición integró las
resoluciones de la 17.ª Conferencia de las Partes, incluyendo especies
adoptadas recientemente como los tiburones zorro (*Alopias* spp.), las
mantarrayas (*Mobula* spp.) y la rana gigante del lago Titicaca
(*Telmatobius culeus*).

## Examples

``` r
data(cites_fauna_peru_2018)

# Filtrar especies en el Apéndice I
subset(cites_fauna_peru_2018, ap == "I")
#> # A tibble: 48 × 17
#>    id_especie phylum   clase subclase orden familia referencia_num n     especie
#>         <dbl> <chr>    <chr> <chr>    <chr> <chr>            <dbl> <chr> <chr>  
#>  1         48 CHORDATA AMPH… NA       ANURA Telmat…             17 48    Telmat…
#>  2         76 CHORDATA AVES  NA       ACCI… Accipi…             NA 76    Harpia…
#>  3        222 CHORDATA AVES  NA       CATH… Cathar…             32 222   Vultur…
#>  4        223 CHORDATA AVES  NA       CICO… Ciconi…             NA 223   Jabiru…
#>  5        230 CHORDATA AVES  NA       FALC… Falcon…             NA 230   Falco …
#>  6        242 CHORDATA AVES  NA       GALL… Cracid…             33 242   Penelo…
#>  7        257 CHORDATA AVES  NA       PSIT… Psitta…             NA 257   Ara ma…
#>  8        258 CHORDATA AVES  NA       PSIT… Psitta…             NA 258   Ara mi…
#>  9        286 CHORDATA AVES  NA       PSIT… Psitta…             NA 286   Primol…
#> 10        301 CHORDATA AVES  NA       RHEI… Rheidae             39 301   Rhea p…
#> # ℹ 38 more rows
#> # ℹ 8 more variables: categoria_nacional <chr>, uicn <chr>, n_e <chr>,
#> #   nombre_comun <chr>, sinonimos <chr>, autor <chr>, ap <chr>,
#> #   referencia_text <chr>

# Resumen por clase taxonómica
table(cites_fauna_peru_2018$clase, cites_fauna_peru_2018$ap)
#>                 
#>                    I  II III III/w
#>   ACTINOPTERYGII   0   2   0     0
#>   AMPHIBIA         1  45   0     0
#>   ANTHOZOA         0   5   0     0
#>   AVES            10 274   4     3
#>   CHONDRICHTHYES   1  17   0     0
#>   HOLOTHUROIDEA    0   0   1     0
#>   MAMMALIA        29  84   1     6
#>   REPTILIA         7  21   0     1
```
