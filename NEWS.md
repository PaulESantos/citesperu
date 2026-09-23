# citesperu (development version)

* **Alineación con el ecosistema `perufauna` (Tidyverse Style):**
  * Se añadieron alias canónicos con prefijo de dominio `cites_*`:
    * `cites_classify_spnames()`: clasificador taxonómico CITES formalmente exportado.
    * `cites_classify_names()`: clasificador taxonómico principal.
  * **Resolución de colisiones de namespace:** Se retiró la exportación de `classify_spnames()` (manteniéndose accesible vía `cites_classify_spnames()` y `cites_classify_names()`) para eliminar la colisión con `perufaunads004` al cargar el metapaquete `perufauna`.
  * Se mantiene la retrocompatibilidad en `cites_match()`, `cites_matching()`, `match_cites_pe()`, `is_cites()` e `is_cites_pe()`.

# citesperu 0.1.0

* **Base documental y gobernanza CITES:**
  * Se incorporó el marco institucional oficial del Perú: el Ministerio del Ambiente (MINAM) como Autoridad Científica CITES, el Servicio Nacional Forestal y de Fauna Silvestre (SERFOR) y el Ministerio de la Producción (PRODUCE/SANIPES) como Autoridades Administrativas, y la SUNAT, PNP (DIRMEAMB), DICAPI y FEMA como Entidades de Observancia (`docs/CONTEXTO_CITES_PERU.md`).
  * Se especificó la articulación con el Decreto Supremo n.° 030-2005-AG (Reglamento CITES Perú), el D.S. n.° 004-2014-MINAGRI (fauna silvestre amenazada) y el D.S. n.° 043-2006-AG (flora silvestre amenazada).

* **Especificación técnica del Listado de Fauna CITES Perú (2018):**
  * Documentación completa de 496 especies oficiales (48 en Apéndice I y 448 en Apéndice II) distribuidas en 8 clases taxonómicas (Actinoperigios, Anfibios, Antozoos, Aves, Condrictios, Mamíferos, Reptiles).
  * Aclaración sobre las 16 especies registradas en el Apéndice III (no contabilizadas en el total oficial nacional por no haber solicitado el Perú inclusiones en dicho apéndice).
  * Adopción de las decisiones de la CoP17 de CITES (Johannesburgo 2016) y nomenclatura normalizada (Res. Conf. 12.11 Rev. CoP17).

* **Especificación técnica del Listado de Flora CITES Perú (2018):**
  * Documentación completa de aproximadamente 2506 taxa en 9 familias botánicas (Orchidaceae, Cactaceae, Cyatheaceae, Fabaceae, Zamiaceae, Dicksoniaceae, Meliaceae, Lauraceae, Euphorbiaceae).
  * Incorporación del tratamiento taxonómico de Cactaceae según el *CITES Cactaceae Checklist* 3.ª edición (Hunt, 2016).
  * Documentación del sistema de 24 acrónimos departamentales de Lamas & Encarnación (1976), el significado del símbolo `?` y las anotaciones de derivados (`#`).
  * Registro de herbarios físicos (USM, MOL) y virtuales (MO, US, NY, F).

* **Incorporación y exportación de datos (`data/`):**
  * Se procesaron y exportaron mediante `usethis::use_data()` los cuatro listados oficiales CITES de Perú:
    * `cites_fauna_peru_2018` (512 observaciones, 17 variables: 496 especies oficiales reguladas + 16 de Apéndice III).
    * `cites_fauna_peru_2019` (523 observaciones, 17 variables: actualización oficial v.2019-01 con ámbito ecológico y género).
    * `cites_fauna_peru_2023` (568 observaciones, 17 variables: resoluciones CoP19 Panamá 2022 vigentes en 2023, competencia sectorial SERFOR/PRODUCE y año de inclusión/enmienda).
    * `cites_flora_peru_2018` (2506 observaciones, 10 variables: listado completo en 9 familias botánicas nativas).
    * `codigos_departamentos_pe` (24 observaciones, 3 variables: acrónimos estándar de Lamas & Encarnación 1976 y códigos UBIGEO del INEI).
  * Se creó el script reproducible de preparación en `data-raw/01_preparar_datos_cites.R`.
  * Se documentaron exhaustivamente las variables, fuentes oficiales y ejemplos de uso en `R/data.R` y se compilaron los manuales correspondientes en `man/`.
* **Sistema de consulta y concordancia taxonómica (matching):**
  * Se implementó el motor de matching taxonómico inspirado en la arquitectura de `wcvpmatch`, optimizado en R puro:
    * `cites_classify_names()` (y alias `classify_spnames()`): clasificador y normalizador de nombres binominales y trinominales, separando género, epíteto específico, rango infraespecífico, epíteto infraespecífico, autoría y banderas (`has_cf`, `has_aff`, `is_sp`, `is_spp`, `had_hybrid`).
    * `cites_match()` (y alias `cites_matching()`, `match_cites_pe()`): tubería secuencial optimizada con direct match (coincidencia exacta), synonym match (resolución de sinónimos oficiales a taxón CITES aceptado y su Apéndice), suffix match (flexión de género en sufijos latinos), fuzzy match acotado al género por distancia de edición (`max_dist`), y genus match para taxones regulados a nivel genérico o con calificador `sp.`/`spp.`.
    * `is_cites()` (y alias `is_cites_pe()`): evaluación booleana vectorizada ultrarrápida (`TRUE`/`FALSE`/`NA`).
  * Se compiló el backbone interno pre-indexado en `R/sysdata.rda` integrando 5,816 registros taxonómicos (3,053 aceptados y 2,763 sinónimos oficiales) con acceso hash O(1).
  * Se incorporaron pruebas unitarias completas en `tests/testthat/test-matching.R` alcanzando 72 pruebas exitosas en el paquete (`0 FAIL | 0 WARN | 72 PASS`).
* **Mensaje de inicio e interactividad estilo tidyverse (`R/zzz.R`):**
  * Se implementó el hook `.onAttach()` con la estética visual y diseño de reglas de `{cli}` al estilo `{tidyverse}`: encabezado con versión (destacando sufijos `.9000` en rojo), cuadrícula en dos columnas con marcas de verificación (`tick`) para los 4 listados oficiales y el motor de matching, enlaces interactivos y notas institucionales de autoridades CITES Perú (MINAM, SERFOR, PRODUCE).
  * Se integró detección de conflictos homónimos (`citesperu_conflicts()`) y soporte para silenciado mediante `options(citesperu.quiet = TRUE)` y `suppressPackageStartupMessages()`.


