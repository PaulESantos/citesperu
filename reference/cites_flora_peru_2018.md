# Flora silvestre de Perú incluida en los Apéndices de la CITES (2018)

Base de datos estructurada a partir del documento oficial *"Listado de
especies de Flora Silvestre CITES - Perú"*, publicado por el Ministerio
del Ambiente (MINAM) como Autoridad Científica CITES en enero de 2018.

## Format

Un data frame con 2506 observaciones y 10 variables:

- item:

  Número de orden consecutivo general del 1 al 2506.

- apendice:

  Apéndice CITES en el que se encuentra incluido el taxón (`"I"`, `"II"`
  o `"III"`).

- familia:

  Familia botánica en mayúsculas (ej. `"ORCHIDACEAE"`, `"CACTACEAE"`).

- n:

  Número correlativo asignado dentro de cada familia o sección en la
  fuente.

- especie:

  Nombre científico del taxón aceptado (especie, subespecie o variedad)
  con su correspondiente autoría botánica.

- sinonimia:

  Nombres científicos tratados como sinónimos taxonómicos en la
  publicación (variable multivaluada).

- nombre_local:

  Nombre común o vernáculo local registrado (ej. "Zapatito", "Caoba", o
  "Sin registro").

- distribucion:

  Distribución departamental en el Perú según los acrónimos de 2 letras
  de Lamas & Encarnación (1976), separados por comas. El símbolo `"?"`
  indica presencia confirmada en el Perú pero sin referencia
  departamental precisa.

- endemismo:

  Condición de endemismo: `"Si"`, `"No"` o `"Endémica"`.

- referencias:

  Citas bibliográficas botánicas que respaldan la descripción, el
  checklist o la sinonimia del taxón.

## Source

Ministerio del Ambiente (MINAM). 2018. *Listado de especies de Flora
Silvestre CITES - Perú*. Dirección General de Diversidad Biológica,
Lima, Perú. Publicación oficial en Gob.pe:
<https://www.gob.pe/institucion/minam/informes-publicaciones/395685-listado-flora-cites-peru-2018>

## Details

El listado comprende las especies nativas de plantas vasculares peruanas
incluidas en los Apéndices I, II y III de la CITES, agrupadas en 9
familias botánicas. Destaca la incorporación integral de la 3.ª edición
del *CITES Cactaceae Checklist* (Hunt, 2016) y la revisión de
colecciones de herbarios físicos (USM, MOL) y virtuales internacionales
(MO, US, NY, F).

**Distribución de Taxa por Familia Botánica:** El compendio oficial
reporta un total de 2506 taxa:

- **ORCHIDACEAE:** 2215 taxa (11 especies y 1 variedad en Apéndice I:
  *Phragmipedium* spp.; 2203 taxa en Apéndice II).

- **CACTACEAE:** 186 taxa en Apéndice II (clasificación basada
  íntegramente en David Hunt, 2016).

- **CYATHEACEAE:** 79 taxa en Apéndice II (helechos arbóreos).

- **FABACEAE:** 11 taxa en Apéndice II (*Platymiscium* spp. y otras
  maderas).

- **ZAMIACEAE:** 9 taxa en Apéndice II (cícadas).

- **DICKSONIACEAE:** 2 taxa en Apéndice II (helechos arbóreos).

- **MELIACEAE:** 2 taxa (1 en Apéndice II: *Swietenia macrophylla*; 1 en
  Apéndice III: *Cedrela odorata*).

- **EUPHORBIACEAE:** 1 taxón en Apéndice II (*Euphorbia* suculenta
  nativa).

- **LAURACEAE:** 1 taxón en Apéndice II (*Aniba rosaeodora*, palo de
  rosa).

**Acrónimos Departamentales (Lamas y Encarnación, 1976):** La variable
`distribucion` utiliza los códigos oficiales: AM (Amazonas), AN
(Áncash), AP (Apurímac), AR (Arequipa), AY (Ayacucho), CA (Cajamarca),
CU (Cusco), HU (Huánuco), HV (Huancavelica), IC (Ica), JU (Junín), LA
(Lambayeque), LI (Lima), LL (La Libertad), LO (Loreto), MD (Madre de
Dios), MO (Moquegua), PA (Pasco), PI (Piura), PU (Puno), SM (San
Martín), TA (Tacna), TU (Tumbes), UC (Ucayali).

El símbolo `"?"` **no** significa estatus taxonómico dudoso; indica que
la especie procede fehacientemente de colectas peruanas pero la etiqueta
de herbario no especifica el departamento.

## References

Hunt, D. (2016). *CITES Cactaceae Checklist*. Third Edition. Royal
Botanic Gardens, Kew.

Lamas, G. & Encarnación, J. (1976). Acrónimos departamentales para
registros biogeográficos peruanos.

## Examples

``` r
data(cites_flora_peru_2018)

# Consultar orquídeas en el Apéndice I
subset(cites_flora_peru_2018, familia == "ORCHIDACEAE" & apendice == "I")
#> # A tibble: 11 × 10
#>     item apendice familia         n especie  sinonimia nombre_local distribucion
#>    <dbl> <chr>    <chr>       <dbl> <chr>    <chr>     <chr>        <chr>       
#>  1     1 I        ORCHIDACEAE     1 Phragmi… Paphiope… Zapatito     CA, SM      
#>  2     2 I        ORCHIDACEAE     2 Phragmi… Selenipe… Zapatito de… CA, HU, SM,…
#>  3     3 I        ORCHIDACEAE     3 Phragmi… Cypriped… Sin registro LO, PU, SM  
#>  4     4 I        ORCHIDACEAE     4 Phragmi… Phragmip… Zapatito de… CU, PA, HU,…
#>  5     5 I        ORCHIDACEAE     5 Phragmi… Selenipe… Sin registro CA          
#>  6     6 I        ORCHIDACEAE     6 Phragmi… Phragmip… Sin registr… SM          
#>  7     7 I        ORCHIDACEAE     7 Phragmi… Cypriped… Sin registr… SM          
#>  8     8 I        ORCHIDACEAE     8 Phragmi… Phragmip… Sin registr… SM, HU, PA  
#>  9     9 I        ORCHIDACEAE     9 Phragmi… NA        Sin registro SM          
#> 10    10 I        ORCHIDACEAE    10 Phragmi… NA        Sin registro SM          
#> 11    11 I        ORCHIDACEAE    11 Phragmi… Phragmip… Zapatito de… SM          
#> # ℹ 2 more variables: endemismo <chr>, referencias <chr>

# Conteo de especies por familia
table(cites_flora_peru_2018$familia)
#> 
#>     CACTACEAE   CYATHEACEAE DICKSONIACEAE EUPHORBIACEAE      FABACEAE 
#>           186            79             2             1            11 
#>     LAURACEAE     MELIACEAE   ORCHIDACEAE     ZAMIACEAE 
#>             1             2          2215             9 
```
