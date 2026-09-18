
<!-- README.md se genera desde README.Rmd. Editar este archivo. -->

# citesperu

**citesperu** es un paquete de R en desarrollo para consultar,
estructurar y analizar de manera reproducible los listados oficiales de
fauna y flora silvestres del Perú incluidas en los Apéndices de la
**Convención sobre el Comercio Internacional de Especies Amenazadas de
Fauna y Flora Silvestres (CITES)**, publicados por el **Ministerio del
Ambiente (MINAM)** como Autoridad Científica CITES del país.

**Estado:** Versión documental y metadatos `0.0.0.9000`. Integra la
especificación técnica de las ediciones oficiales de Fauna y Flora 2018,
la referencia de acrónimos departamentales y las directrices
institucionales del MINAM.

------------------------------------------------------------------------

## Autor

**Paul E. Santos Andrade** — autor, mantenedor y titular de derechos del
software.

- Correo: <paulefrens@gmail.com>
- ORCID: [0000-0002-6635-0375](https://orcid.org/0000-0002-6635-0375)

------------------------------------------------------------------------

## Marco Institucional CITES en el Perú

El Perú es Estado signatario de la Convención CITES desde 1975 (Decreto
Ley N.° 21080). La gobernanza nacional comprende:

- **Autoridad Científica:** [Ministerio del Ambiente
  (MINAM)](https://www.gob.pe/minam), a través de la Dirección General
  de Diversidad Biológica (DGDB). Asesora científicamente, formula los
  Dictámenes de Extracción No Perjudicial (DENP) y elabora los listados
  oficiales nacionales.
- **Autoridades Administrativas:** [SERFOR](https://www.gob.pe/serfor)
  (flora y fauna silvestre terrestre) y [PRODUCE /
  SANIPES](https://www.gob.pe/produce) (recursos hidrobiológicos marinos
  y continentales). Emiten los permisos y certificados CITES.
- **Entidades de Observancia:** SUNAT (Aduanas), Policía Nacional
  (DIRMEAMB), DICAPI (Marina de Guerra) y FEMA (Fiscalías Ambientales).

Para más detalles, consulta la [Guía Técnica de Contexto CITES en el
Perú](docs/CONTEXTO_CITES_PERU.md).

------------------------------------------------------------------------

## Fuentes Oficiales y Cobertura (MINAM Colección 609)

Los datos se obtienen del [compendio oficial del
MINAM](https://www.gob.pe/institucion/minam/colecciones/609-listados-de-especies-de-fauna-y-flora-cites-peru).
Cada conjunto de datos conserva estrictamente su propia edición y
alcance temporal:

| Dataset en desarrollo | Edición / Título Oficial | Grupo | Cobertura Oficial | Formato de Origen |
|----|----|:--:|----|:--:|
| `cites_fauna_peru_2018` | [Listado Fauna CITES Perú - 2018](https://www.gob.pe/institucion/minam/informes-publicaciones/395692-listado-fauna-cites-peru-2018) | Fauna | **496 especies** (48 en Ap. I, 448 en Ap. II, 16 en Ap. III\*) | PDF oficial |
| `cites_flora_peru_2018` | [Listado Flora CITES Perú - 2018](https://www.gob.pe/institucion/minam/informes-publicaciones/395685-listado-flora-cites-peru-2018) | Flora | **2506 taxa** en 9 familias botánicas (12 Ap. I, 2493 Ap. II, 1 Ap. III) | PDF oficial |
| `cites_fauna_peru_2023` | [Listado de Fauna CITES Perú 2023](https://www.gob.pe/institucion/minam/informes-publicaciones/4042047-listado-de-fauna-cites-peru-2023) | Fauna | En proceso de importación y control de calidad | XLS oficial |
| `codigos_departamentos_pe` | Acrónimos Biogeográficos de Lamas & Encarnación (1976) | Geografía | 24 acrónimos estándar departamentales | Referencia técnica |

*\* En fauna, las 16 especies registradas en el Apéndice III fueron
incluidas a propuesta de otros países Parte. Como el Perú no ha
solicitado inclusiones en dicho apéndice, el MINAM no las suma al total
nacional oficial.*

### Síntesis de Fauna Silvestre CITES Perú (2018)

- **Por Clase:** Actinoperigios (2), Anfibios (46), Antozoos (5), Aves
  (284), Condrictios (18), Mamíferos (113), Reptiles (28). Total: **496
  especies**.
- **Categorías de Conservación Integradas:** D.S. n.° 004-2014-MINAGRI y
  Lista Roja de la UICN (CR, EN, VU, NT, LC, DD).
- **Armonización CoP17:** Incorporación de novedades de Johannesburgo
  2016 (*Alopias* spp., *Mobula* spp., *Telmatobius culeus*).

### Síntesis de Flora Silvestre CITES Perú (2018)

- **Familias Botánicas Representadas:** Orchidaceae (2215 taxa),
  Cactaceae (186 taxa, según *CITES Cactaceae Checklist* 3.ª ed., Hunt
  2016), Cyatheaceae (79 taxa), Fabaceae (11 taxa), Zamiaceae (9 taxa),
  Dicksoniaceae (2 taxa), Meliaceae (2 taxa), Lauraceae (1 taxón),
  Euphorbiaceae (1 taxón). Total: **2506 taxa**.
- **Distribución Geográfica:** Codificada mediante los 24 acrónimos de
  Lamas y Encarnación (1976) (`AM`, `CU`, `LO`, etc.). El símbolo `?`
  denota registro fehaciente en el Perú sin localidad o departamento
  precisado en etiqueta.
- **Herbarios Consultados:** USM (San Marcos), MOL (La Molina), MO
  (Missouri), US (Smithsonian), NY (New York), F (Field Museum).

Consulta el historial de entregas y control de procedencia en
[FUENTES.md](docs/FUENTES.md).

------------------------------------------------------------------------

## Funcionalidad Planificada de la API

| Función | Propósito |
|----|----|
| `cites_pe_list()` | Consultar y filtrar el listado por reino (`fauna`/`flora`), apéndice (`I`, `II`, `III`), clase o familia. |
| `is_cites_pe()` | Evaluación booleana exacta y normalizada (vector del mismo largo que la entrada). |
| `match_cites_pe()` | Búsqueda y concordancia taxonómica: coincidencia exacta, resolución por sinónimos oficiales o candidatos aproximados (*fuzzy*). |
| `summary.cites_pe()` | Método S3 para resumir registros y conteo de taxones por apéndice, familia, clase o departamento. |

### Ejemplo de Uso Propuesto

``` r
library(citesperu)

# Consultar especies de fauna en Apéndice I
fauna_ap1 <- cites_pe_list(taxon = "fauna", apendice = "I")

# Verificar si una lista de especies se encuentra incluida en CITES Perú
especies <- c("Tremarctos ornatus", "Panthera onca", "Cedrela odorata", "Homo sapiens")
is_cites_pe(especies)

# Búsqueda aproximada o resolución de sinónimos
match_cites_pe("Phragmipedium caudatum", method = "synonym")

# Resumen taxonómico
summary(cites_pe_list(taxon = "flora"), by = "familia")
```

------------------------------------------------------------------------

## Desarrollo Local

Para compilar el paquete y generar la documentación en un entorno local
con R:

``` r
# Generar la documentación roxygen2
roxygen2::roxygenise()

# Renderizar el archivo README
rmarkdown::render("README.Rmd")
```

El diseño de arquitectura detallado, el contrato de las funciones y los
criterios de validación se describen en el [Plan de
Desarrollo](docs/PLAN.md).

------------------------------------------------------------------------

## Licencia y Atribución

El software y código fuente de **citesperu** se distribuyen bajo la
licencia de código abierto [MIT](LICENSE.md).

La información original de los listados pertenece al **Ministerio del
Ambiente del Perú (MINAM)**. Los datasets empaquetados preservan
íntegramente la cita a la publicación oficial de origen y a sus autores
intelectuales.
