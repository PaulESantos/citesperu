
<!-- README.md se genera desde README.Rmd. Editar este archivo. -->

# citesperu

Paquete de R en desarrollo para consultar y organizar los listados de fauna
y flora del Perú publicados por el Ministerio del Ambiente (MINAM) en el
marco de la Convención sobre el Comercio Internacional de Especies Amenazadas
de Fauna y Flora Silvestres (CITES).

**Estado:** base documental inicial, versión `0.0.0.9000`. Todavía no incluye
datasets ni funciones de consulta, matching o actualización. Las interfaces
de este documento son propuestas y pueden cambiar durante el desarrollo.

## Autor

**Paul E. Santos Andrade** — autor, mantenedor y titular de derechos del software.

- Correo: <paulefrens@gmail.com>
- ORCID: [0000-0002-6635-0375](https://orcid.org/0000-0002-6635-0375)

## Fuentes previstas

Se utilizará el [compendio oficial del MINAM](https://www.gob.pe/institucion/minam/colecciones/609-listados-de-especies-de-fauna-y-flora-cites-peru).
La revisión inicial del 15 de septiembre de 2026 identificó estas ediciones
candidatas, cuyos adjuntos aún deben descargarse y validarse:

| Dataset previsto | Edición | Formato anunciado |
|---|---|---|
| `fauna_cites_pe` | [Fauna 2023](https://www.gob.pe/institucion/minam/informes-publicaciones/4042047-listado-de-fauna-cites-peru-2023) | XLS |
| `flora_cites_pe` | [Flora 2018](https://www.gob.pe/institucion/minam/informes-publicaciones/395685-listado-flora-cites-peru-2018) | PDF |

Cada dataset conservará su edición y procedencia. No se presentará una mezcla
de ambos como un listado actualizado a un único año. Los resultados describirán
la edición consultada; no encontrar un nombre no demuestra que esté excluido
de CITES. Véanse los [detalles de las fuentes](docs/FUENTES.md).

## Funcionalidad planificada

| Función | Propósito |
|---|---|
| `cites_pe_list()` | Consultar y filtrar por fauna/flora y apéndice. |
| `is_cites_pe()` | Devolver una coincidencia exacta como vector lógico. |
| `match_cites_pe()` | Consultar nombres, sinónimos documentados y candidatos aproximados. |
| `update_cites_pe()` | Recuperar una edición en caché, con validación y metadatos. |
| `summary.cites_pe()` | Resumir registros y taxones por apéndice, familia o grupo. |

La coincidencia aproximada propondrá candidatos; los sinónimos necesitarán
una relación taxonómica respaldada por una fuente. Las coincidencias ambiguas
se conservarán para revisión.

## Ejemplo de la API propuesta

Este ejemplo **no es ejecutable todavía**:

```r
library(citesperu)

fauna <- cites_pe_list(taxon = "fauna", apendice = c("I", "II"))
is_cites_pe(c("Vicugna vicugna", "Panthera onca"))
match_cites_pe("Pantera onca", method = "fuzzy")
summary(fauna, by = "familia")
```

## Desarrollo local

La publicación en CRAN y la URL del repositorio quedan pendientes. Desde la
raíz del proyecto se puede construir la base documental con R:

```sh
R CMD build .
```

Para regenerar la ayuda y este README se usarán `roxygen2` y `rmarkdown`,
respectivamente, como herramientas de desarrollo:

```r
roxygen2::roxygenise()
rmarkdown::render("README.Rmd")
```

El [plan de desarrollo](docs/PLAN.md) define el esquema propuesto, los contratos
de las funciones, la estructura objetivo y los criterios de validación.
La primera etapa será inspeccionar los archivos oficiales y construir una
importación reproducible antes de implementar las consultas.

## Licencia y atribución

El código se distribuye bajo [MIT](LICENSE.md). La procedencia y las condiciones
de reutilización de los datos del MINAM se documentarán por separado antes
de distribuirlos. El autor del paquete es distinto del organismo productor
de los listados; se citarán tanto el software como la edición de los datos.
