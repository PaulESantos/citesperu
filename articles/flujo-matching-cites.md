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
suelen presentar: 1. **Sinónimos históricos:** Especies reclasificadas
en nuevos géneros o familias tras revisiones filogenéticas recientes. 2.
**Discrepancias en sufijos latinos:** Discordancias comunes de género
gramatical en latín entre el sustantivo genérico y el adjetivo
específico (`-us`, `-a`, `-um`, `-is`, `-e`). 3. **Errores
tipográficos:** Erratas de digitación u omisiones de caracteres en
nombres biológicos complejos. 4. **Determinaciones genéricas
incompletas:** Muestras rotuladas como morfoespecies (`sp.`, `spp.`,
`indet.`) que pertenecen a géneros o familias cuya regulación es
integral (como *Cedrela*, *Swietenia*, *Podocnemis* o las familias
Orchidaceae y Cactaceae).

El paquete **`citesperu`** provee un motor de concordancia taxonómica
([`cites_match()`](https://paulesantos.github.io/citesperu/reference/cites_match.md))
y una función booleana de alta velocidad
([`is_cites()`](https://paulesantos.github.io/citesperu/reference/is_cites.md))
diseñados específicamente para resolver estas problemáticas con rigor
científico y trazabilidad documental.

------------------------------------------------------------------------

## 2. Arquitectura del Backbone Taxonómico Unificado

En lugar de consultar archivos tabulares dispersos en tiempo de
ejecución, `citesperu` cuenta con un **backbone taxonómico interno
pre-indexado** (`cites_backbone`) integrado en el paquete:

- **Volumen:** Más de 5,800 registros taxonómicos (3,053 nombres
  válidos/aceptados y 2,763 sinónimos oficiales).
- **Fauna:** Articula las ediciones de 2018 (496 spp. oficiales), 2019
  (523 registros con ámbito ecológico) y 2023 (568 especies con las
  enmiendas CoP19 Panamá).
- **Flora:** Articula el catálogo botánico oficial 2018 (2,506 taxa en 9
  familias, incluyendo Orchidaceae con 2,215 taxa y Cactaceae con 186
  taxa tratadas según el estándar de Hunt 2016).
- **Gobernanza asociada:** Cada registro enlaza el Apéndice CITES (I, II
  o III), la categoría nacional de amenaza (D.S. n.° 004-2014-MINAGRI
  para fauna o D.S. n.° 043-2006-AG para flora), la categoría global de
  la UICN y la autoridad sectorial competente (SERFOR o
  PRODUCE/SANIPES).

------------------------------------------------------------------------

## 3. Pipeline Secuencial de Concordancia (`cites_match`)

El motor de
[`cites_match()`](https://paulesantos.github.io/citesperu/reference/cites_match.md)
procesa los nombres de entrada a través de una tubería secuencial en 6
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
         │       └─ SÍ -> match_type: "suffix" (dist = 0)
         │
         ├───> 4. Fuzzy match (Distancia Levenshtein/OSA acotada)
         │       └─ SÍ -> match_type: "fuzzy" (dist = 1..max_dist)
         │
         ├───> 5. Genus match (Regulación a nivel genérico / sp.)
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
3.  **Suffix Match (`suffix`):** Cuando la única diferencia respecto a
    un taxón válido dentro del mismo género es la desinencia gramatical
    latina (`-us`, `-a`, `-um`, `-is`, `-e`), se normaliza y se vincula
    al taxón oficial.
4.  **Fuzzy Match (`fuzzy`):** Busca coincidencias aproximadas
    calculando la distancia de edición (Levenshtein / Optimal String
    Alignment) dentro del mismo género según el umbral `max_dist` (por
    defecto `1`). Si el género no se encuentra, evalúa géneros CITES
    cercanos.
5.  **Genus Match (`genus`):** Se activa cuando el nombre ingresado
    representa un género (ej. `Cedrela sp.`, `Touit spp.`, o género
    puro) o cuando se configura `genus_fallback = TRUE` para nombres
    binominales sin coincidencia de especie pero pertenecientes a un
    género sujeto a control integral.
6.  **Unmatched:** Si el nombre no supera ninguna de las fases previas,
    se marca con `is_cites = FALSE` y `match_type = "unmatched"`.

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
#> Dirección General de Diversidad Biológica
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
  "Touit sp.",               # 7. Coincidencia a nivel de género (Ap. II)
  "Canis familiaris"         # 8. No CITES (unmatched)
)

resultado <- cites_match(nombres, max_dist = 1)
resultado[, c("input_name", "matched_name", "accepted_name", "match_type", "is_cites", "apendice", "taxon")]
#> # A tibble: 8 × 7
#>   input_name       matched_name accepted_name match_type is_cites apendice taxon
#>   <chr>            <chr>        <chr>         <chr>      <lgl>    <chr>    <chr>
#> 1 Tremarctos orna… Tremarctos … Tremarctos o… exact      TRUE     I        fauna
#> 2 Epipedobates fe… Epipedobate… Allobates fe… synonym    TRUE     II       fauna
#> 3 Paphiopedilum b… Paphiopedil… Phragmipediu… synonym    TRUE     I        flora
#> 4 Cedrela odoratus Cedrela odo… Cedrela odor… suffix     TRUE     III      flora
#> 5 Tremarctos orna… Tremarctos … Tremarctos o… fuzzy      TRUE     I        fauna
#> 6 Swietenia macro… Swietenia m… Swietenia ma… fuzzy      TRUE     II       flora
#> 7 Touit sp.        Touit        Touit spp.    genus      TRUE     II       fauna
#> 8 Canis familiaris NA           NA            unmatched  FALSE    NA       NA
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
- En **`Cedrela odoratus`**, la desinencia `-us` se normalizó
  automáticamente a **`Cedrela odorata`** (`match_type: suffix`) sin
  requerir penalización por distancia de edición.
- En **`Touit sp.`**, al tratarse de un psitácido determinado solo a
  género, el motor reconoció que el género *Touit* se encuentra
  enteramente listado en el **Apéndice II**.

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
#> 1 Tremarctos ornatus NA
```

------------------------------------------------------------------------

## 6. Integración en Flujos de Limpieza de Datos con `dplyr`

En proyectos reales de conservación o auditorías aduaneras, los datos
suelen presentarse en tablas de inventario:

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

El motor taxonómico de **`citesperu`** estandariza la verificación de
listados CITES en el Perú, proporcionando una base rigurosa, auditable y
de alto rendimiento para investigadores, autoridades ambientales y
entidades de control.
