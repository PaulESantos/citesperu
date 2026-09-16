# Plan de desarrollo de citesperu

Revisión inicial: 2026-09-15. Estado: diseño; API y datasets pendientes.

## Alcance

Preparar y consultar versiones reproducibles de los listados CITES Perú del
MINAM. El nombre del paquete, del proyecto y de la futura viñeta será
`citesperu`; se reemplaza `citendemic` del borrador. Una edición del listado
describe su propia cobertura temporal, no garantiza la vigencia actual de
todos los apéndices. La ausencia de un nombre indica que no se encontró en
la edición consultada, no una determinación de exclusión de la Convención.

## Estructura objetivo

```text
citesperu/
├── DESCRIPTION, NAMESPACE, LICENSE, LICENSE.md
├── README.Rmd, README.md
├── R/
│   ├── citesperu-package.R
│   ├── data.R
│   ├── get_cites_pe.R
│   ├── check_species.R
│   ├── match_species.R
│   ├── appendix_summary.R
│   └── utils.R
├── data/
│   ├── fauna_cites_pe.rda
│   └── flora_cites_pe.rda
├── data-raw/
│   ├── README.md
│   ├── import_fauna.R
│   ├── parse_flora.R
│   └── DATASET.R
├── docs/
│   ├── PLAN.md
│   └── FUENTES.md
├── man/
├── tests/testthat/
│   ├── test-check_species.R
│   ├── test-match_species.R
│   ├── test-data.R
│   └── test-appendix_summary.R
└── vignettes/citesperu.Rmd
```

El árbol es una meta, no un inventario de archivos implementados. Se crearán
los scripts, datos y pruebas al desarrollar cada componente.

## Esquema propuesto

Conservar una fila por registro y alcance de inclusión de la fuente. No
suponer que cada fila representa una especie única: pueden existir rangos
supraespecíficos, poblaciones, anotaciones o más de un apéndice.

| Campo | Tipo | Contenido previsto |
|---|---|---|
| `registro_id` | character | Identificador estable dentro de una edición |
| `nombre_original` | character | Texto taxonómico de la fuente, sin sobrescribir |
| `nombre_cientifico` | character | Nombre normalizado para consulta |
| `autoria` | character | Autoría taxonómica, cuando esté disponible |
| `rango_taxonomico` | character | Especie, subespecie, género u otro rango documentado |
| `familia` | character | Familia consignada en la fuente |
| `taxon` | character | `fauna` o `flora` |
| `grupo_taxonomico` | character | Grupo de la fuente, si existe |
| `apendice` | character | `I`, `II` o `III`; desdoblar inclusiones múltiples con trazabilidad |
| `nombre_comun` | character | Nombre común, si está disponible |
| `distribucion` | character | Distribución según la fuente |
| `anotacion` | character | Restricciones, poblaciones y notas originales |
| `fuente_id` | character | Clave para el manifiesto de procedencia |
| `anio_fuente` | integer | Año de la edición, distinto de la fecha de descarga |
| `referencia_origen` | character | Hoja/fila o página/tabla para auditar la extracción |

Este es un contrato propuesto; debe contrastarse con los adjuntos antes de
cerrarlo. Los campos ausentes se representarán con `NA`, sin inferir valores.
El manifiesto guardará URL de publicación y descarga, fecha de publicación,
fecha de recuperación, versión de extracción, checksum SHA-256 y condiciones
de reutilización identificadas. Los sinónimos se mantendrán en una tabla
relacional separada con nombre, registro de destino y fuente de la relación.

## Contrato de funciones propuesto

| Interfaz | Resultado y decisiones |
|---|---|
| `cites_pe_list(taxon = c("ambos", "fauna", "flora"), apendice = NULL)` | Data frame con clase `cites_pe`; ambos grupos por defecto; selección con `match.arg`; apéndices válidos `I`, `II`, `III`, incluso como vector. |
| `is_cites_pe(species_name)` | Vector lógico del mismo largo y orden que la entrada; coincidencia exacta normalizada; `NA` para entradas vacías o faltantes. `FALSE` significa no encontrado en el snapshot. |
| `match_cites_pe(species_name, method = c("exact", "synonym", "fuzzy"))` | Tabla de resultados con identificador de entrada, candidato, estado, método, distancia y procedencia. Exacto por defecto; fuzzy solicitado explícitamente. |
| `update_cites_pe()` | Descarga y valida una nueva edición en caché del usuario; devuelve datos y metadatos. No sobrescribe la instalación del paquete. |
| `summary.cites_pe(object, by = c("apendice", "familia", "grupo_taxonomico"), ...)` | Método S3 para el resultado de `cites_pe_list()`; informa registros y taxones distintos por agrupación. |

`is_cites_pe()` tendrá un único tipo de retorno: los apéndices y detalles se
obtendrán mediante la tabla de coincidencias. Se preservarán entradas
duplicadas y su posición. Los resultados ambiguos mostrarán los candidatos
empatados y no elegirán uno arbitrariamente. El fuzzy propone candidatos;
la resolución de sinónimos exige relaciones documentadas. No basta una
distancia ortográfica para declarar equivalencia taxonómica.

La normalización conservará híbridos, rangos infraespecíficos y el original.
Se definirán umbrales y restricciones taxonómicas después de revisar ejemplos
reales. Un registro de género o familia no se expandirá a especies sin una
referencia taxonómica explícita. Los resúmenes distinguirán registros de
especies, ya que una inclusión múltiple puede aumentar los conteos por grupo.

## Etapas y criterios de aceptación

1. **Fuentes:** descargar e inspeccionar adjuntos; registrar procedencia y
   condiciones de reutilización. Fauna se importará desde XLS; flora requiere
   extracción de PDF. Confirmar hojas, encabezados y leyendas antes del parser.
2. **Datos:** generar tablas comunes, revisar notas y reconciliar conteos
   con los originales; documentar faltantes, duplicados y casos no interpretados.
   Solo entonces generar `.rda` y documentación roxygen2 en `R/data.R`.
3. **Consulta:** implementar filtros y búsqueda exacta con pruebas de vectores,
   `NA`, entradas vacías, nombres repetidos y apéndices inválidos.
4. **Matching:** incorporar sinónimos respaldados y fuzzy con pruebas de
   ambigüedad, errores tipográficos y falsos positivos; mantener la trazabilidad.
5. **Actualización y resumen:** probar caché, fallos de descarga, cambios de
   esquema y conteos de registros/taxones. Nunca sustituir una edición válida
   por una descarga incompleta. Las pruebas regulares no dependerán de la red.
6. **Distribución:** escribir la viñeta ejecutable, generar ayuda con roxygen2,
   ejecutar `R CMD build` y `R CMD check`, revisar licencias y volver a comprobar
   el nombre en CRAN antes del envío.

Se evaluarán `readxl` para XLS y `pdftools` para PDF durante la importación.
No se añaden dependencias anticipadas a `DESCRIPTION`. Las herramientas de
preparación se separarán de las dependencias necesarias para usar el paquete.
