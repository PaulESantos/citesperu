# Marco Institucional, Legal y Contexto CITES en el Perú

## 1. Introducción y Adhesión del Perú a la Convención CITES

La **Convención sobre el Comercio Internacional de Especies Amenazadas
de Fauna y Flora Silvestres (CITES)** es un tratado internacional
vinculante cuyo objetivo es garantizar que el comercio transfronterizo
de especímenes de animales y plantas silvestres no ponga en riesgo su
supervivencia en la naturaleza.

El Perú es Estado Parte de la Convención desde su ratificación en
**1975**, aprobada mediante el **Decreto Ley N.° 21080**. Desde
entonces, el Estado peruano ha consolidado un marco normativo e
institucional específico para la aplicación efectiva de la Convención,
formalizado principalmente a través del **Decreto Supremo n.°
030-2005-AG** (*Reglamento para la Implementación de la CITES en el
Perú*).

``` r

library(citesperu)
library(dplyr)
```

------------------------------------------------------------------------

## 2. Gobernanza CITES en el Perú: Tres Niveles Institucionales

En cumplimiento de los artículos VIII y IX del texto de la Convención y
de la legislación nacional peruana, la gobernanza CITES se articula en
tres niveles diferenciados:

### A. Autoridad Científica CITES

- **Entidad rectora:** **Ministerio del Ambiente (MINAM)**, a través de
  la **Dirección General de Diversidad Biológica (DGDB)**.
- **Funciones fundamentales:**
  1.  Emitir los **Dictámenes de Extracción No Perjudicial (DENP)**
      previo a la exportación de especímenes de especies de los
      Apéndices I y II.
  2.  Compilar, validar y actualizar periódicamente los **Listados
      Oficiales de Especies de Fauna y Flora Silvestre CITES Perú**
      (Colección MINAM N.° 609).
  3.  Asesorar científicamente a las Autoridades Administrativas sobre
      el estado poblacional y medidas de manejo de las especies
      reguladas.

### B. Autoridades Administrativas CITES

Son las entidades responsables de emitir los permisos, certificados y
supervisar las cuotas de comercio:

1.  **SERFOR (Servicio Nacional Forestal y de Fauna Silvestre):**
    - *Ámbito de competencia:* Especies de flora y fauna silvestre
      terrestres (aves, mamíferos, reptiles, anfibios continentales y
      plantas no acuáticas).
    - Emite autorizaciones de exportación, importación y reexportación
      para recursos forestales y de fauna terrestre.
2.  **PRODUCE (Ministerio de la Producción) / SANIPES:**
    - *Ámbito de competencia:* Recursos hidrobiológicos (peces
      continentales y marinos, tiburones, mantarrayas, corales y
      mamíferos acuáticos).

### C. Entidades de Observancia y Control Fronterizo

Encargadas de la fiscalización, interdicción y sanción de infracciones:

- **SUNAT (Superintendencia Nacional de Aduanas y de Administración
  Tributaria):** Control operativo en puntos de entrada y salida
  (puertos marítimos, aeropuertos internacionales y aduanas
  fronterizas).
- **PNP - DIRMEAMB (Dirección de Medio Ambiente de la Policía Nacional
  del Perú):** Investigación y ejecución de operaciones contra el
  tráfico ilegal de vida silvestre.
- **DICAPI (Dirección General de Capitanías y Guardacostas de la Marina
  de Guerra del Perú):** Control de actividades en el dominio marítimo,
  fluvial y lacustre.
- **FEMA (Fiscalías Especializadas en Materia Ambiental del Ministerio
  Público):** Persecución penal de delitos ambientales tipificados en el
  Código Penal peruano vinculados a la flora y fauna silvestre (Art.
  308).

------------------------------------------------------------------------

## 3. Articulación con la Legislación Nacional de Especies Amenazadas

El estatus CITES (orientado a la regulación del **comercio
internacional**) se complementa de forma sinérgica con las listas de
especies amenazadas a nivel nacional (orientadas a la **conservación in
situ** y protección del patrimonio natural de la Nación):

- **Fauna Silvestre Amenazada del Perú:** Aprobada mediante el **Decreto
  Supremo n.° 004-2014-MINAGRI**. Categoriza a las especies en Peligro
  Crítico (CR), En Peligro (EN), Vulnerable (VU) y Casi Amenazado (NT).
- **Flora Silvestre Amenazada del Perú:** Aprobada mediante el **Decreto
  Supremo n.° 043-2006-AG**.

En los datasets incluidos en `citesperu`, la categorización del D.S.
004-2014-MINAGRI y las categorías globales de la UICN se encuentran
vinculadas directamente:

``` r

# Ejemplo: Especies de fauna peruana en Apéndice I que están amenazadas nacionalmente
data(cites_fauna_peru_2023)

cites_fauna_peru_2023 %>%
  filter(apendice == "I", !is.na(categoria_nacional), categoria_nacional != "-") %>%
  select(especie = especie_nombre_cientifico, clase, apendice, categoria_nacional, uicn) %>%
  head(6)
#> # A tibble: 6 × 5
#>   especie             clase    apendice categoria_nacional uicn 
#>   <chr>               <chr>    <chr>    <chr>              <chr>
#> 1 Telmatobius culeus  AMPHIBIA I        CR                 EN   
#> 2 Harpia harpyja      AVES     I        VU                 VU   
#> 3 Vultur gryphus      AVES     I        EN                 VU   
#> 4 Jabiru mycteria     AVES     I        NT                 LC   
#> 5 Falco peregrinus    AVES     I        NT                 LC   
#> 6 Penelope albipennis AVES     I        CR                 EN
```

------------------------------------------------------------------------

## 4. Estructura de los Apéndices CITES y Particularidades Nacionales

La Convención distribuye las especies en tres apéndices:

- **Apéndice I:** Especies en peligro de extinción para las cuales se
  prohíbe el comercio internacional con fines primordialmente
  comerciales. Solo se autoriza bajo circunstancias excepcionales con
  doble permiso (importación y exportación).
- **Apéndice II:** Especies que no están necesariamente en peligro de
  extinción actual, pero cuyo comercio debe controlarse estrictamente
  para evitar una utilización incompatible con su supervivencia.
- **Apéndice III:** Especies incluidas a solicitud unilateral de un país
  Parte que ya reglamenta su comercio y necesita la cooperación de las
  demás Partes.

### Nota Técnica sobre el Apéndice III en el Perú

En el compendio oficial de fauna de 2018 figuran 16 especies listadas en
el Apéndice III. **El Perú no ha solicitado inclusiones en el Apéndice
III**; dichas especies corresponden a solicitudes formuladas por otros
países Parte de la Convención. Por esta razón técnica y legal, el MINAM
no contabiliza estas 16 especies dentro del total oficial de fauna CITES
del país (496 especies oficiales reguladas).

``` r

data(cites_fauna_peru_2018)

# Distribución por apéndices en el compendio oficial 2018 (columna 'ap')
table(cites_fauna_peru_2018$ap, useNA = "ifany")
#> 
#>     I    II   III III/w 
#>    48   448     6    10
```

------------------------------------------------------------------------

## 5. Flora CITES y Trazabilidad Biogeográfica

El listado de flora CITES del Perú (`cites_flora_peru_2018`) reúne 2506
taxa en 9 familias botánicas, donde sobresale la familia Orchidaceae con
más de 2200 taxones.

### A. Acrónimos Biogeográficos Departamentales

Para la distribución geográfica a nivel de departamento, el listado de
flora adopta el sistema estándar de 24 acrónimos propuesto por **Lamas &
Encarnación (1976)**:

``` r

data(codigos_departamentos_pe)
head(codigos_departamentos_pe, 8)
#> # A tibble: 8 × 3
#>   acronimo departamento ubigeo_inei
#>   <chr>    <chr>        <chr>      
#> 1 AM       Amazonas     01         
#> 2 AN       Áncash       02         
#> 3 AP       Apurímac     03         
#> 4 AR       Arequipa     04         
#> 5 AY       Ayacucho     05         
#> 6 CA       Cajamarca    06         
#> 7 CU       Cusco        08         
#> 8 HV       Huancavelica 09
```

- El símbolo `?` indica que existe registro comprobado de la especie en
  el Perú, pero la etiqueta del espécimen original no detalla el
  departamento o localidad específica.
- Los símbolos `#` y notas asociadas precisan anotaciones taxonómicas y
  de derivados comerciales en los Apéndices CITES.

### B. Herbarios de Referencia

La documentación botánica oficial se apoya en colecciones de herbarios
físicos nacionales y virtuales internacionales: \* **Nacionales:**
Herbario San Marcos (`USM`, UNMSM) y Herbario de la Molina (`MOL`,
UNALM). \* **Internacionales:** Missouri Botanical Garden (`MO`),
Smithsonian Institution (`US`), New York Botanical Garden (`NY`) y Field
Museum of Natural History (`F`).

------------------------------------------------------------------------

## 6. Resumen de Ediciones Oficiales Integradas

| Dataset | Edición Oficial | Cobertura | Normativa / Hito |
|----|----|----|----|
| `cites_fauna_peru_2018` | Compendio 2018 | 496 spp. (+16 Ap. III) | CoP17 Johannesburgo |
| `cites_fauna_peru_2019` | Versión v.2019-01 | 523 registros | Incorporación de ámbitos ecológicos |
| `cites_fauna_peru_2023` | Actualización 2023 | 568 especies | Decisiones CoP19 Panamá y delimitación sectorial |
| `cites_flora_peru_2018` | Catálogo Flora 2018 | 2506 taxa | Tratamiento Cactaceae (Hunt 2016) |
| `codigos_departamentos_pe` | Referencia técnica | 24 departamentos | Lamas & Encarnación (1976) e INEI |

Para consultar cómo utilizar estas bases de datos en procesos de
limpieza y validación taxonómica automatizada, consulta la viñeta:

``` r

vignette("flujo-matching-cites", package = "citesperu")
```
