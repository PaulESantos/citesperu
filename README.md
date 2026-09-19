---
output: github_document
---

<!-- README.md se genera desde README.Rmd. Por favor edita este archivo. -->

# citesperu

**citesperu** es un paquete de R para consultar, estructurar y contrastar de manera reproducible los listados oficiales de fauna y flora silvestres del Perú incluidas en los Apéndices de la **Convención sobre el Comercio Internacional de Especies Amenazadas de Fauna y Flora Silvestres (CITES)**, publicados por el **Ministerio del Ambiente (MINAM)** como Autoridad Científica CITES del país.

**Estado actual:** Versión `0.1.0`. Incorpora los 4 listados oficiales nacionales (Fauna 2018, 2019, 2023 y Flora 2018), un backbone taxonómico unificado de más de 5,800 registros con resolución de sinónimos oficiales, un motor de concordancia secuencial en 6 etapas (`cites_match()`) y mensajes interactivos estilo `{tidyverse}`.

---

## Autor

**Paul E. Santos Andrade** — autor, mantenedor y titular de derechos del software.

- Correo: <paulefrens@gmail.com>
- ORCID: [0000-0002-6635-0375](https://orcid.org/0000-0002-6635-0375)

---

## Instalación

Puedes instalar la versión de desarrollo de **citesperu** desde GitHub mediante [`pak`](https://pak.r-lib.org/) o [`remotes`](https://remotes.r-lib.org/):

```r
# Usando pak (recomendado)
pak::pak("PaulESantos/citesperu")

# O usando remotes
# remotes::install_github("PaulESantos/citesperu")
```

Al cargar la librería, `{citesperu}` despliega un banner informativo al estilo de `{tidyverse}` resumiendo las bases oficiales disponibles y herramientas activas:

```r
library(citesperu)
#> ── citesperu ───────────────────────────────────────────────────────── v0.1.0 ──
#> ✔ cites_fauna_peru_2018    496 spp.            ✔ codigos_departamentos_pe 24 depts.      
#> ✔ cites_flora_peru_2018    2506 taxa           ✔ cites_fauna_peru_2023    568 spp.       
#> ✔ cites_fauna_peru_2019    523 spp.            ✔ cites_match()            matching engine
#> ℹ Autoridad Científica: MINAM | Autoridades Administrativas: SERFOR / PRODUCE
#> ℹ Usa cites_match() para concordancia o revisa la documentación (<https://paulesantos.github.io/citesperu/>)
```

*(Si deseas silenciar el mensaje en entornos automatizados o scripts, usa `suppressPackageStartupMessages(library(citesperu))` o define `options(citesperu.quiet = TRUE)`).*

---

## Marco Institucional CITES en el Perú

El Perú es Estado signatario de la Convención CITES desde 1975 (Decreto Ley N.° 21080). La gobernanza nacional se articula a través de tres niveles institucionales:

* **Autoridad Científica:** [Ministerio del Ambiente (MINAM)](https://www.gob.pe/minam), a través de la Dirección General de Diversidad Biológica (DGDB). Asesora científicamente, emite los Dictámenes de Extracción No Perjudicial (DENP) y elabora los listados oficiales nacionales.
* **Autoridades Administrativas:** [SERFOR](https://www.gob.pe/serfor) (flora y fauna silvestre terrestre) y [PRODUCE / SANIPES](https://www.gob.pe/produce) (recursos hidrobiológicos marinos y continentales). Emiten los permisos y certificados de exportación, importación y reexportación CITES.
* **Entidades de Observancia:** SUNAT (Aduanas), Policía Nacional del Perú (DIRMEAMB), DICAPI (Autoridad Marítima / Guardacostas) y FEMA (Fiscalías Especializadas en Materia Ambiental).

Para mayor detalle sobre el marco legal (D.S. 030-2005-AG, D.S. 004-2014-MINAGRI, D.S. 043-2006-AG), consulta la [Guía Técnica de Contexto CITES en el Perú](docs/CONTEXTO_CITES_PERU.md).

---

## Fuentes Oficiales y Cobertura (MINAM Colección 609)

Los datos empaquetados proceden del [compendio oficial del MINAM](https://www.gob.pe/institucion/minam/colecciones/609-listados-de-especies-de-fauna-y-flora-cites-peru), conservando estrictamente su integridad documental y alcance temporal:

| Dataset | Edición / Título Oficial | Grupo | Cobertura Oficial | Registros | Formato de Origen |
|---|---|:---:|---|:---:|:---:|
| `cites_fauna_peru_2018` | [Listado Fauna CITES Perú - 2018](https://www.gob.pe/institucion/minam/informes-publicaciones/395692-listado-fauna-cites-peru-2018) | Fauna | **496 especies** oficiales (48 Ap. I, 448 Ap. II, más 16 en Ap. III\*) | 512 | PDF / Excel oficial |
| `cites_fauna_peru_2019` | [Listado de Fauna CITES Perú 2019](https://www.gob.pe/institucion/minam/informes-publicaciones/395694-listado-de-fauna-cites-peru-2019) | Fauna | **523 registros** (con ámbito ecológico y género) | 523 | Excel oficial |
| `cites_fauna_peru_2023` | [Listado de Fauna CITES Perú 2023](https://www.gob.pe/institucion/minam/informes-publicaciones/4109405-listado-de-fauna-cites-peru-2023) | Fauna | **568 especies** (48 Ap. I, 503 Ap. II, 17 Ap. III; CoP19 Panamá) | 568 | Excel oficial |
| `cites_flora_peru_2018` | [Listado Flora CITES Perú - 2018](https://www.gob.pe/institucion/minam/informes-publicaciones/395685-listado-flora-cites-peru-2018) | Flora | **2506 taxa** en 9 familias botánicas (12 Ap. I, 2493 Ap. II, 1 Ap. III) | 2506 | PDF / Excel oficial |
| `codigos_departamentos_pe` | Acrónimos Biogeográficos de Lamas & Encarnación (1976) | Geografía | 24 acrónimos estándar departamentales y códigos UBIGEO del INEI | 24 | Referencia técnica |

*\* En fauna 2018, las 16 especies registradas en el Apéndice III fueron incluidas a propuesta de otros países Parte. Como el Perú no ha solicitado inclusiones en dicho apéndice, el MINAM no las contabiliza en el balance oficial nacional.*

---

## Arquitectura del Backbone Taxonómico y Flujo de Trabajo de Matching

Uno de los principales desafíos en el análisis de biodiversidad y comercio de vida silvestre es que los inventarios de campo, guías de transporte o decomisos suelen contener **sinónimos históricos, variaciones ortográficas de género gramatical en latín, errores tipográficos o nombres determinados solo a género (`sp.` / `spp.`)**.

Para resolver esto, **citesperu** integra un **backbone taxonómico pre-indexado** (`cites_backbone`) con más de **5,816 registros taxonómicos** (3,053 nombres aceptados y 2,763 sinónimos oficiales) derivados directamente de las publicaciones del MINAM.

### 1. ¿Cómo se articulan las bases de Fauna y Flora?

El backbone interno unifica ambas fuentes bajo un esquema canónico homogéneo:
* **Fauna:** Integra las enmiendas de la CoP19 de Panamá vigentes en la edición 2023, enlazando cada taxón con su Clase, Orden, Familia, Apéndice CITES, categoría nacional de amenaza (D.S. n.° 004-2014-MINAGRI), categoría global UICN y autoridad sectorial competente (SERFOR para fauna terrestre, PRODUCE/SANIPES para acuática).
* **Flora:** Integra las 9 familias botánicas nativas reguladas (Orchidaceae con 2,215 taxa, Cactaceae con 186 taxa según el *CITES Cactaceae Checklist* de Hunt 2016, Cyatheaceae con 79 taxa, etc.), vinculando códigos departamentales y referencias de herbarios oficiales (USM, MOL, MO, US, NY, F).
* **Géneros regulados:** Se compila una tabla de control genérico (`cites_genera`) para taxones cuya regulación abarca a la totalidad del género o familia (por ejemplo: *Swietenia*, *Cedrela*, *Podocnemis*, *Touit*, o todas las especies no listadas individualmente en Orchidaceae y Cactaceae).

### 2. ¿Cómo resuelve los sinónimos taxonómicos?

Muchas especies han cambiado de género o epíteto debido a revisiones filogenéticas recientes. Si un usuario consulta un nombre que ya no es válido según la nomenclatura CITES actual, el motor no lo descarta como "no CITES"; en su lugar:
1. Detecta la coincidencia en la base de sinónimos oficiales del MINAM.
2. Identifica el **nombre CITES aceptado (`accepted_name`)**.
3. Recupera el **Apéndice CITES oficial** y la jerarquía taxonómica del taxón aceptado.
4. Etiqueta el resultado con `match_type = "synonym"`.

**Ejemplos reales:**
* **Fauna:** *Epipedobates femoralis* (sinónimo histórico en dendrobátidos) se resuelve a **`Allobates femoralis`** (Apéndice II).
* **Flora:** *Paphiopedilum besseae* (sinónimo comercial frecuente) se resuelve a **`Phragmipedium besseae`** (Apéndice I).

---

### 3. Pipeline Secuencial de Concordancia (`cites_match()`)

Cuando se invoca `cites_match()`, cada nombre atraviesa una tubería secuencial optimizada de 6 etapas:

```text
[ Entrada del Usuario: Nombre científico o lista ]
                         │
                         ▼
        ┌───────────────────────────────────┐
        │ 0. Clasificación y Parsing        │ -> Extrae género, epíteto, rango infraespecífico,
        │    (cites_classify_names)         │    autor y banderas (cf., aff., sp., spp.)
        └─────────────────┬─────────────────┘
                         │
                         ▼
        ┌───────────────────────────────────┐
   SÍ   │ 1. Coincidencia Directa (Exacta)  │ -> match_type: "exact" (dist: 0)
  ┌─────┤    ¿Nombre aceptado en CITES?     │    Recupera Apéndice y taxonomía
  │     └─────────────────┬─────────────────┘
  │                      NO
  │                      ▼
  │     ┌───────────────────────────────────┐
  │ SÍ  │ 2. Coincidencia por Sinónimo      │ -> match_type: "synonym" (dist: 0)
  ├─────┤    ¿Sinónimo oficial de MINAM?    │    Resuelve a accepted_name y Apéndice
  │     └─────────────────┬─────────────────┘
  │                      NO
  │                      ▼
  │     ┌───────────────────────────────────┐
  │ SÍ  │ 3. Variación de Sufijo Latino     │ -> match_type: "suffix" (dist: 0)
  ├─────┤    ¿Flexión gramatical (-us/-a)?  │    Normaliza declinación dentro del género
  │     └─────────────────┬─────────────────┘
  │                      NO
  │                      ▼
  │     ┌───────────────────────────────────┐
  │ SÍ  │ 4. Concordancia Difusa (Fuzzy)    │ -> match_type: "fuzzy" (dist: 1..max_dist)
  ├─────┤    ¿Distancia Levenshtein <= max? │    Acotada al mismo género o género cercano
  │     └─────────────────┬─────────────────┘
  │                      NO
  │                      ▼
  │     ┌───────────────────────────────────┐
  │ SÍ  │ 5. Coincidencia a Nivel de Género │ -> match_type: "genus" (dist: 0)
  ├─────┤    ¿sp., spp. o genus_fallback?   │    Verifica si el género cuenta con regulación
  │     └─────────────────┬─────────────────┘
  │                      NO
  │                      ▼
  │     ┌───────────────────────────────────┐
  └────>│ 6. No Listado (Unmatched)         │ -> match_type: "unmatched" (is_cites: FALSE)
        └───────────────────────────────────┘
```

#### Descripción de las etapas:

1. **`exact` (Direct match):** Búsqueda directa O(1) contra los nombres taxonómicamente aceptados en los listados del MINAM. Distancia: `0`.
2. **`synonym` (Synonym match):** Búsqueda contra los sinónimos oficiales recopilados por el MINAM, vinculando automáticamente el Apéndice y la ficha del taxón válido. Distancia: `0`.
3. **`suffix` (Suffix match):** Resuelve discrepancias de concordancia gramatical entre el género y los sufijos latinos del epíteto específico (`-us`, `-a`, `-um`, `-is`, `-e`, etc.). Ejemplo: *Cedrela odoratus* $\rightarrow$ *Cedrela odorata*. Distancia: `0`.
4. **`fuzzy` (Fuzzy match):** Coincidencia aproximada mediante distancia de edición (Levenshtein/OSA). Por defecto busca variaciones tipográficas en el epíteto acotadas al género (`max_dist = 1`). Si el género no existe, evalúa géneros CITES candidatos cercanos dentro del umbral.
5. **`genus` (Genus match):** Si el espécimen fue determinado únicamente a género (ej. *Touit sp.*, *Cedrela spp.*) o si se activa `genus_fallback = TRUE` para binomios sin coincidencia específica, verifica si el género posee estatus CITES integral.
6. **`unmatched`:** Si el taxón no figura en los listados CITES del Perú, se reporta con `is_cites = FALSE` y metadatos `NA`.

---

## Parámetros de Control en `cites_match()`

| Parámetro | Valores | Descripción |
|---|---|---|
| `splist` | `character` o `data.frame` | Vector de nombres científicos o data frame que contenga nombres. |
| `taxon` | `"all"` (default), `"fauna"`, `"flora"` | Permite restringir la búsqueda a un reino específico (evita ambigüedades homónimas). |
| `edition` | `"latest"` (default), `"all"`, `"2023"`, `"2019"`, `"2018"` | Edición oficial a consultar. Por defecto combina Fauna 2023 + Flora 2018. |
| `max_dist` | `integer` (default `1`) | Distancia máxima de edición permitida en la fase *fuzzy*. |
| `allow_synonyms` | `TRUE` (default) / `FALSE` | Permite o desactiva la resolución automática de sinónimos oficiales. |
| `genus_fallback` | `FALSE` (default) / `TRUE` | Si es `TRUE`, binomios sin match a nivel de especie heredan la regulación del género si está regulado. |
| `output` | `"standard"` (default) / `"full"` | `"standard"` devuelve las 13 columnas esenciales para conservación; `"full"` incluye columnas de parsing y flags. |

---

## Ejemplos de Uso

### 1. Verificación booleana ultrarrápida (`is_cites()`)

Ideal para filtros lógicos inmediatos en pipelines de datos:


``` r
library(citesperu)
```

```
## ── citesperu ───────────────────────────────────────────────────────── v0.1.0 ──
## ✔ cites_fauna_peru_2018    496 spp.            ✔ codigos_departamentos_pe 24 depts.      
## ✔ cites_flora_peru_2018    2506 taxa           ✔ cites_fauna_peru_2023    568 spp.       
## ✔ cites_fauna_peru_2019    523 spp.            ✔ cites_match()            matching engine
## ℹ Autoridad Científica: MINAM | Autoridades Administrativas: SERFOR / PRODUCE
## ℹ Usa cites_match() para concordancia o revisa la documentación (<https://paulesantos.github.io/citesperu/>)
```

``` r
especies <- c("Tremarctos ornatus", "Cedrela odorata", "Homo sapiens")
is_cites(especies)
```

```
## [1]  TRUE  TRUE FALSE
```

### 2. Motor de concordancia taxonómica (`cites_match()`)

Demostración de los diferentes tipos de coincidencia en una sola consulta:


``` r
res <- cites_match(c(
  "Tremarctos ornatus",      # 1. Exacto: Fauna Ap. I (Oso de anteojos)
  "Epipedobates femoralis",  # 2. Sinónimo: Fauna Ap. II -> Allobates femoralis
  "Paphiopedilum besseae",   # 3. Sinónimo: Flora Ap. I -> Phragmipedium besseae
  "Cedrela odoratus",        # 4. Sufijo latino: Flora Ap. III -> Cedrela odorata
  "Tremarctos ornatu",       # 5. Fuzzy (dist = 1): Falta 's' final
  "Swietenia macrophyla",    # 6. Fuzzy (dist = 1): Falta 'l'
  "Touit sp.",               # 7. Género: Psitácidos regulados en Ap. II
  "Homo sapiens"             # 8. Unmatched: No CITES
), max_dist = 1)

# Seleccionar columnas clave para visualización
res[, c("input_name", "accepted_name", "match_type", "is_cites", "apendice", "taxon")]
```

```
## # A tibble: 8 × 6
##   input_name             accepted_name        match_type is_cites apendice taxon
##   <chr>                  <chr>                <chr>      <lgl>    <chr>    <chr>
## 1 Tremarctos ornatus     Tremarctos ornatus   exact      TRUE     I        fauna
## 2 Epipedobates femoralis Allobates femoralis  synonym    TRUE     II       fauna
## 3 Paphiopedilum besseae  Phragmipedium besse… synonym    TRUE     I        flora
## 4 Cedrela odoratus       Cedrela odorata      suffix     TRUE     III      flora
## 5 Tremarctos ornatu      Tremarctos ornatus   fuzzy      TRUE     I        fauna
## 6 Swietenia macrophyla   Swietenia macrophyl… fuzzy      TRUE     II       flora
## 7 Touit sp.              Touit spp.           genus      TRUE     II       fauna
## 8 Homo sapiens           <NA>                 unmatched  FALSE    <NA>     <NA>
```

### 3. Clasificación taxonómica y extracción de componentes (`cites_classify_names()`)

Descompone nombres científicos extrayendo rangos infraespecíficos, autores y marcadores de indeterminación:


``` r
cites_classify_names(c(
  "Swietenia macrophylla King",
  "Phragmipedium boissierianum var. czerwiakowianum",
  "Cedrela cf. odorata",
  "Touit sp."
))
```

```
## # A tibble: 4 × 14
##   input_index input_name       canonical_name orig_genus orig_species infra_rank
##         <int> <chr>            <chr>          <chr>      <chr>        <chr>     
## 1           1 Swietenia macro… Swietenia mac… Swietenia  macrophylla  <NA>      
## 2           2 Phragmipedium b… Phragmipedium… Phragmipe… boissierian… var.      
## 3           3 Cedrela cf. odo… Cedrela odora… Cedrela    odorata      <NA>      
## 4           4 Touit sp.        Touit          Touit      <NA>         <NA>      
## # ℹ 8 more variables: orig_infraspecies <chr>, author <chr>, rank <dbl>,
## #   has_cf <lgl>, has_aff <lgl>, is_sp <lgl>, is_spp <lgl>, had_hybrid <lgl>
```

### 4. Integración en flujos tabulares con `{dplyr}`

`cites_match()` acepta directamente `data.frame` o `tibble`, facilitando la limpieza de inventarios biológicos o registros aduaneros:


``` r
library(dplyr)

inventario <- tibble(
  id = 1:4,
  nombre_campo = c("Tremarctos ornatus", "Epipedobates femoralis", "Cedrela odoratus", "Zea mays"),
  cantidad_individuos = c(2, 15, 1, 100)
)

# Evaluar con cites_match() e integrar resultados al inventario
eval_cites <- cites_match(inventario$nombre_campo)

inventario_evaluado <- bind_cols(
  inventario,
  eval_cites %>% select(accepted_name, apendice, match_type, is_cites)
)

inventario_evaluado
```

```
## # A tibble: 4 × 7
##      id nombre_campo       cantidad_individuos accepted_name apendice match_type
##   <int> <chr>                            <dbl> <chr>         <chr>    <chr>     
## 1     1 Tremarctos ornatus                   2 Tremarctos o… I        exact     
## 2     2 Epipedobates femo…                  15 Allobates fe… II       synonym   
## 3     3 Cedrela odoratus                     1 Cedrela odor… III      suffix    
## 4     4 Zea mays                           100 <NA>          <NA>     unmatched 
## # ℹ 1 more variable: is_cites <lgl>
```

---

## Viñeta Técnica de Consulta

Para un desglose metodológico exhaustivo, fundamentos biológicos y comparativas de rendimiento, consulta la viñeta incluida en el paquete:

```r
vignette("flujo-matching-cites", package = "citesperu")
```

---

## Licencia y Atribución

El software y código fuente de **citesperu** se distribuyen bajo la licencia de código abierto [MIT](LICENSE.md).

La información original de los listados pertenece al **Ministerio del Ambiente del Perú (MINAM)**. Los datasets empaquetados preservan íntegramente la cita a las publicaciones oficiales de origen y a sus respectivos autores técnicos.
