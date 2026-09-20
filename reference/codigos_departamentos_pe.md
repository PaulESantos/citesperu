# Códigos y Acrónimos de Departamentos del Perú (Lamas y Encarnación, 1976)

Tabla de referencia con los acrónimos estándar de dos letras utilizados
históricamente en biogeografía peruana y adoptados por el MINAM en los
listados oficiales de flora CITES para registrar la distribución
geográfica departamental.

## Format

Un data frame con 24 observaciones y 3 variables:

- acronimo:

  Código de 2 letras en mayúsculas (ej. `"AM"`, `"CU"`, `"LO"`).

- departamento:

  Nombre completo del departamento o región del Perú.

- ubigeo_inei:

  Código de ubicación geográfica departamental de 2 dígitos según el
  INEI.

## Details

En las tablas de distribución botánica del MINAM, se utiliza además el
símbolo especial `"?"` para señalar que una especie cuenta con registro
o colecta confirmada en el Perú, pero el espécimen de herbario carece de
localidad o departamento explícito en su etiqueta.

## References

Lamas, G. & Encarnación, J. (1976). Acrónimos departamentales para
registros biogeográficos peruanos.

## Examples

``` r
data(codigos_departamentos_pe)
head(codigos_departamentos_pe)
#> # A tibble: 6 × 3
#>   acronimo departamento ubigeo_inei
#>   <chr>    <chr>        <chr>      
#> 1 AM       Amazonas     01         
#> 2 AN       Áncash       02         
#> 3 AP       Apurímac     03         
#> 4 AR       Arequipa     04         
#> 5 AY       Ayacucho     05         
#> 6 CA       Cajamarca    06         
```
