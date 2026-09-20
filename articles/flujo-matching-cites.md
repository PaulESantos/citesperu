# Flujo de Trabajo y Resolución Taxonómica en citesperu

## 1. Introducción

El comercio internacional de especies silvestres amparadas por la
**Convención sobre el Comercio Internacional de Especies Amenazadas de
Fauna y Flora Silvestres (CITES)** requiere una verificación rigurosa
del estatus legal de los taxones. En el Perú, el **Ministerio del
Ambiente (MINAM)**, en su condición de Autoridad Científica CITES,
compila y publica periódicamente los listados oficiales de especies de
fauna y flora silvestres reguladas.

Sin embargo, los datos reales procedentes de inventarios biológicos,
guías de transporte forestal, manifiestos de aduana o incautaciones
suelen presentar:

1.  **Sinónimos históricos:** Especies reclasificadas en nuevos géneros
    o familias tras revisiones filogenéticas recientes.

2.  **Discrepancias en sufijos latinos:** Discordancias comunes de
    género gramatical en latín entre el sustantivo genérico y el
    adjetivo específico (`-us`, `-a`, `-um`, `-is`, `-e`).

3.  **Errores tipográficos:** Erratas de digitación u omisiones de
    caracteres en nombres biológicos complejos.

4.  **Determinaciones genéricas incompletas:** Muestras rotuladas como
    morfoespecies (`sp.`, `spp.`, `indet.`), para las que es útil
    identificar si el género tiene registros CITES y luego confirmar la
    especie y la fuente aplicable.

El paquete **`citesperu`** provee un motor de concordancia taxonómica
([`cites_match()`](https://paulesantos.github.io/citesperu/reference/cites_match.md))
y una función booleana de alta velocidad
([`is_cites()`](https://paulesantos.github.io/citesperu/reference/is_cites.md))
diseñados específicamente para resolver estas problemáticas con
trazabilidad documental. La concordancia automatizada es una ayuda para
revisar inventarios: la determinación taxonómica y la verificación
regulatoria final deben contrastarse con la fuente oficial vigente.

------------------------------------------------------------------------

## 2. Arquitectura del Backbone Taxonómico Unificado

En lugar de consultar archivos tabulares dispersos en tiempo de
ejecución, `citesperu` cuenta con un **backbone taxonómico interno
pre-indexado** (`cites_backbone`) integrado en el paquete. Para
`edition = "latest"`, el motor consulta la combinación de fauna 2023 y
flora 2018; las demás ediciones cambian el subconjunto de referencia, no
el orden del pipeline.

- **Volumen y versiones:** Más de 8,500 registros taxonómicos que
  preservan fauna 2018, 2019 y 2023, además de flora 2018; cada
  coincidencia devuelve edición y referencia de origen.
- **Fauna:** Articula las ediciones de 2018 (496 spp. oficiales), 2019
  (523 registros con ámbito ecológico) y 2023 (568 especies con las
  enmiendas CoP19 Panamá).
- **Flora:** Articula el catálogo botánico oficial 2018 (2,506 taxa en 9
  familias, incluyendo Orchidaceae con 2,215 taxa y Cactaceae con 186
  taxa tratadas según el estándar de Hunt 2016).
- **Metadatos asociados:** Cada coincidencia recupera el Apéndice CITES
  y los metadatos taxonómicos y de conservación disponibles en el
  registro de origen.

Los nombres aceptados y las sinonimias se conservan como registros
separados, vinculados mediante `accepted_name`. Para atender consultas
como `Touit sp.`, el motor construye un índice de géneros a partir del
backbone ya filtrado por `taxon` y `edition`. Una coincidencia `genus`
significa que el género tiene registros CITES en el alcance
seleccionado; no asigna una especie concreta ni debe interpretarse, por
sí sola, como cobertura automática de todas las especies del género. Por
ello conserva `is_cites = TRUE`, pero recibe
`match_assessment = "requires_species_validation"`. La salida también
conserva `edition_used`, `source_dataset`, `source_row_id`,
`source_title` y `source_url` para auditoría.

------------------------------------------------------------------------

## 3. Pipeline

El motor de
[`cites_match()`](https://paulesantos.github.io/citesperu/reference/cites_match.md)
procesa los nombres de entrada a través de un flujo secuencial en 6
etapas:

``` text
[ Entrada del Usuario ]
         │
         ▼
[ 0. Parsing con cites_classify_names() ]
         │
         ├───> 1. Direct match (Coincidencia exacta con nombre aceptado)
         │       └─ SÍ -> match_type: "exact" (dist = 0)
         │
         ├───> 2. Synonym match (Resolución de sinónimos oficiales)
         │       └─ SÍ -> match_type: "synonym" (dist = 0)
         │
         ├───> 3. Suffix match (Normalización de sufijo latino)
         │       └─ SÍ -> match_type: "suffix" (dist = 0; empate -> ambiguous_match)
         │
         ├───> 4. Fuzzy match (Distancia Levenshtein acotada)
         │       └─ SÍ -> match_type: "fuzzy" (dist = 1..max_dist; empate -> ambiguous_match)
         │
         ├───> 5. Genus match (Consulta a nivel genérico / sp.)
         │       └─ SÍ -> match_type: "genus" (dist = 0)
         │
         └───> 6. Unmatched (Especie no incluida en CITES Perú)
                 └─ match_type: "unmatched" (is_cites: FALSE)
```

### Detalle de cada etapa:

1.  **Direct Match (`exact`):** Coincidencia estricta O(1) con un nombre
    CITES aceptado. Se asigna de inmediato el Apéndice, estatus legal y
    taxonomía correspondiente.
2.  **Synonym Match (`synonym`):** Si el nombre no coincide con un
    nombre aceptado, se busca en la base de sinónimos oficiales del
    MINAM. Al encontrar coincidencia, se identifica el `accepted_name`
    oficial y se le asigna el Apéndice del taxón aceptado.
3.  **Suffix Match (`suffix`):** Cuando existe una variación de sufijo
    permitida dentro del mismo género, se vincula al candidato oficial.
    El resultado queda marcado como `requires_taxonomic_validation`.
4.  **Fuzzy Match (`fuzzy`):** Busca coincidencias aproximadas con
    distancia de edición dentro del mismo género según el umbral
    `max_dist` (por defecto `1`). Si el género no se encuentra, evalúa
    géneros candidatos dentro del mismo presupuesto total. Si hay
    empate, devuelve `ambiguous_match` sin asignar Apéndice.
5.  **Genus Match (`genus`):** Se activa cuando el nombre ingresado
    representa un género (ej. `Cedrela sp.`, `Touit spp.`, o género
    puro) o cuando se configura `genus_fallback = TRUE` para binomios
    sin coincidencia de especie. El resultado informa presencia en el
    índice del género, no sustituye una identificación a nivel de
    especie y recibe la categoría
    `match_assessment = "requires_species_validation"`.
6.  **Unmatched:** Si el nombre no supera ninguna de las fases previas,
    se marca con `is_cites = FALSE` y `match_type = "unmatched"`.

Los empates en las fases `suffix` o `fuzzy` son un resultado
transversal: se reportan como **`ambiguous_match`**, sin Apéndice y con
`candidate_names` y `candidate_count`; no se resuelven eligiendo el
primer registro disponible.

### Lectura de `match_assessment`

| Valor | Interpretación operativa |
|----|----|
| `matched` | Nombre aceptado o sinónimo oficial sin marcador de incertidumbre. |
| `requires_taxonomic_validation` | Match por sufijo o fuzzy, o entrada con `cf.`, `aff.`, híbrido o rango infraespecífico. |
| `requires_species_validation` | Coincidencia a nivel de género; debe determinarse la especie. |
| `ambiguous_match` | Empate entre candidatos; no se asigna Apéndice. |
| `not_listed` | No hubo coincidencia en el alcance elegido. |

[`is_cites()`](https://paulesantos.github.io/citesperu/reference/is_cites.md)
es una función booleana conservadora: devuelve `TRUE` solo para `exact`
y `synonym` con `match_assessment = "matched"`. Para cualquier otro caso
se debe usar
[`cites_match()`](https://paulesantos.github.io/citesperu/reference/cites_match.md).

------------------------------------------------------------------------

## 4. Ejemplos Prácticos de Concordancia

Carguemos el paquete:

``` r

library(citesperu)
#> ── citesperu ───────────────────────────────────────────────────────── v0.1.0 ──
#> ✔ cites_flora_peru_2018 2506 taxa           ✔ cites_fauna_peru_2023 568 spp.       
#> ✔ cites_fauna_peru_2018 496 spp.            ✔ cites_match()         matching engine
#> ✔ cites_fauna_peru_2019 523 spp.            
#> ℹ Listado de Especies de Flora y Fauna Silvestre CITES - Perú.
#> Autoridad Científica: MINAM / Dirección General de Diversidad Biológica
```

### Consulta integral de casos taxonómicos

``` r

nombres <- c(
  "Tremarctos ornatus",      # 1. Coincidencia exacta (Oso andino, Fauna Ap. I)
  "Epipedobates femoralis",  # 2. Sinónimo de fauna resuelto a Allobates femoralis (Ap. II)
  "Paphiopedilum besseae",   # 3. Sinónimo de flora resuelto a Phragmipedium besseae (Ap. I)
  "Cedrela odoratus",        # 4. Variación de sufijo (-us por -a en Cedrela odorata)
  "Tremarctos ornatu",       # 5. Fuzzy match en epíteto (falta 's', dist = 1)
  "Swietenia macrophyla",    # 6. Fuzzy match en epíteto (falta 'l', dist = 1)
  "Canis familiaris"         # 7. No CITES (unmatched)
)

resultado <- cites_match(nombres, max_dist = 1)
resultado[, c("input_name", "matched_name", "accepted_name", "match_type", "match_assessment", "is_cites", "apendice", "taxon", "edition_used", "source_dataset", "source_row_id")]
#> # A tibble: 7 × 11
#>   input_name     matched_name accepted_name match_type match_assessment is_cites
#>   <chr>          <chr>        <chr>         <chr>      <chr>            <lgl>   
#> 1 Tremarctos or… Tremarctos … Tremarctos o… exact      matched          TRUE    
#> 2 Epipedobates … Epipedobate… Allobates fe… synonym    matched          TRUE    
#> 3 Paphiopedilum… Paphiopedil… Phragmipediu… synonym    matched          TRUE    
#> 4 Cedrela odora… Cedrela odo… Cedrela odor… suffix     requires_taxono… TRUE    
#> 5 Tremarctos or… Tremarctos … Tremarctos o… fuzzy      requires_taxono… TRUE    
#> 6 Swietenia mac… Swietenia m… Swietenia ma… fuzzy      requires_taxono… TRUE    
#> 7 Canis familia… NA           NA            unmatched  not_listed       FALSE   
#> # ℹ 5 more variables: apendice <chr>, taxon <chr>, edition_used <chr>,
#> #   source_dataset <chr>, source_row_id <chr>
```

### Observaciones clave del resultado:

- En el caso de **`Epipedobates femoralis`**, el usuario ingresó un
  sinónimo histórico de anfibio; el motor reconoció el sinónimo y
  devolvió como nombre aceptado **`Allobates femoralis`**, asignándole
  correctamente el **Apéndice II**.
- En el caso de **`Paphiopedilum besseae`**, un sinónimo comercial
  botánico muy extendido en el mercado de orquídeas, el motor lo remonta
  a **`Phragmipedium besseae`**, asignándole el estatus de **Apéndice
  I**.
- En **`Cedrela odoratus`**, la desinencia `-us` se vincula a
  **`Cedrela odorata`** (`match_type: suffix`), pero queda marcada para
  validación taxonómica.
- Cuando una entrada se resuelve como **`genus`**, `match_assessment`
  toma el valor **`"requires_species_validation"`**. Ese resultado
  señala que el género aparece en el índice consultado, pero exige
  confirmar la especie antes de una decisión regulatoria.

------------------------------------------------------------------------

## 5. Control de Ámbitos y Ediciones

El usuario puede afinar el alcance de la búsqueda mediante parámetros
específicos:

### Restringir por reino (`taxon = "fauna"` o `"flora"`)

Evita falsos positivos o ambigüedades entre reinos biológicos:

``` r

# Restringir solo a fauna
cites_match("Tremarctos ornatus", taxon = "fauna")[, c("input_name", "is_cites", "taxon")]
#> # A tibble: 1 × 3
#>   input_name         is_cites taxon
#>   <chr>              <lgl>    <chr>
#> 1 Tremarctos ornatus TRUE     fauna

# Si se busca una planta en el filtro de fauna, resultará unmatched
cites_match("Swietenia macrophylla", taxon = "fauna")[, c("input_name", "is_cites", "match_type")]
#> # A tibble: 1 × 3
#>   input_name            is_cites match_type
#>   <chr>                 <lgl>    <chr>     
#> 1 Swietenia macrophylla FALSE    unmatched
```

### Consultar ediciones históricas (`edition`)

``` r

# Consultar la edición 2018
cites_match("Tremarctos ornatus", edition = "2018")[, c("input_name", "apendice")]
#> # A tibble: 1 × 2
#>   input_name         apendice
#>   <chr>              <chr>   
#> 1 Tremarctos ornatus I

# La edición 2019 también conserva su procedencia
cites_match("Tremarctos ornatus", edition = "2019")[, c(
  "input_name", "apendice", "edition_used", "source_dataset", "source_row_id"
)]
#> # A tibble: 1 × 5
#>   input_name         apendice edition_used source_dataset        source_row_id
#>   <chr>              <chr>    <chr>        <chr>                 <chr>        
#> 1 Tremarctos ornatus I        2019         cites_fauna_peru_2019 374
```

------------------------------------------------------------------------

## 6. Integración en Flujos de Limpieza de Datos con `dplyr`

En proyectos reales los datos suelen presentarse en tablas:

``` r

library(dplyr)

inventario <- tibble(
  codigo_muestra = c("M-01", "M-02", "M-03", "M-04"),
  nombre_declarado = c("Tremarctos ornatus", "Epipedobates femoralis", "Cedrela odoratus", "Zea mays")
)

# Evaluación con cites_match()
eval_cites <- cites_match(inventario$nombre_declarado)

# Integrar resultados al inventario
inventario_validado <- bind_cols(
  inventario,
  eval_cites %>% select(
    cites_aceptado = accepted_name,
    apendice,
    tipo_match = match_type,
    es_cites = is_cites
  )
)

inventario_validado
#> # A tibble: 4 × 6
#>   codigo_muestra nombre_declarado    cites_aceptado apendice tipo_match es_cites
#>   <chr>          <chr>               <chr>          <chr>    <chr>      <lgl>   
#> 1 M-01           Tremarctos ornatus  Tremarctos or… I        exact      TRUE    
#> 2 M-02           Epipedobates femor… Allobates fem… II       synonym    TRUE    
#> 3 M-03           Cedrela odoratus    Cedrela odora… III      suffix     TRUE    
#> 4 M-04           Zea mays            NA             NA       unmatched  FALSE
```

------------------------------------------------------------------------

## 7. Conclusión

El motor taxonómico de **`citesperu`** estandariza la revisión inicial
de listados CITES en el Perú, proporcionando una base auditable y
reproducible para investigadores, autoridades ambientales y entidades de
control. Los resultados `suffix`, `fuzzy`, `genus`, ambiguos o con
calificadores taxonómicos requieren revisión según `match_assessment`;
[`is_cites()`](https://paulesantos.github.io/citesperu/reference/is_cites.md)
reserva `TRUE` para nombres exactos o sinónimos oficiales sin
incertidumbre.
