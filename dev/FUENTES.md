# Fuentes y procedencia

Revisión: 2026-09-15. Responsable del paquete: Paul E. Santos Andrade.

El [compendio del MINAM](https://www.gob.pe/institucion/minam/colecciones/609-listados-de-especies-de-fauna-y-flora-cites-peru)
es el punto de entrada para seleccionar las ediciones. El MINAM se identifica
allí como Autoridad Científica CITES; contacto de la fuente: cites@minam.gob.pe.

| Grupo | Publicación candidata | Fecha de publicación | Formato anunciado |
|---|---|---|---|
| Fauna | [Listado de Fauna CITES Perú 2023](https://www.gob.pe/institucion/minam/informes-publicaciones/4042047-listado-de-fauna-cites-peru-2023) | 2023-03-28 | XLS, 296.4 KB |
| Flora | [Listado Flora CITES Perú - 2018](https://www.gob.pe/institucion/minam/informes-publicaciones/395685-listado-flora-cites-peru-2018) | 2018-01-31 | PDF, 4.7 MB |

Son las ediciones más recientes por grupo identificadas en el compendio
consultado; esto no demuestra que sean los listados normativos vigentes.
También aparecen fauna 2019 (XLS), fauna 2018 (PDF) y un catálogo de flora
2012 (PDF), útiles como antecedentes, sin mezclarlos con la edición elegida.

## Alcance de la verificación

La apertura directa inicial de gob.pe devolvió HTTP 418. Los títulos, fechas
y formatos anteriores se verificaron mediante el contenido indexado de las
páginas oficiales. Los adjuntos no se han descargado ni inspeccionado en esta
etapa: todavía no hay conteos de especies, esquema confirmado, checksums o
fechas de descarga. El año de publicación de la página se contrastará con
la portada y los metadatos internos de cada archivo.

La ficha de fauna 2023 contiene una descripción sobre nutrición que no
corresponde al título del listado; no se usa como descripción del dataset.
Se deberá verificar el adjunto identificado como
`Listado_de_Fauna_CITES_Perú_v.2023 MAR` antes de importarlo.

## Registro para cada descarga futura

- Identificador y edición de la fuente, organismo y título literal.
- URL de la publicación y URL efectiva del adjunto.
- Fecha de publicación, recuperación y cualquier fecha de vigencia explícita.
- Nombre, formato, tamaño y checksum SHA-256 del archivo original.
- Hoja/fila o página/tabla de origen; versión del script de extracción.
- Licencia o condiciones de reutilización y observaciones de validación.

La licencia MIT del software no atribuye esa misma licencia a los documentos
o datos del MINAM. Las condiciones de los datos se documentarán al revisar los
archivos y antes de incorporarlos a una distribución.

## Referencias de trabajo

- MINAM. 2023. *Listado de Fauna CITES Perú 2023*. Ministerio del Ambiente.
  Véase la ficha enlazada en la tabla.
- MINAM. 2018. *Listado Flora CITES Perú - 2018*. Ministerio del Ambiente.
  Véase la ficha enlazada en la tabla.

Las citas definitivas de los datasets se ajustarán a los créditos internos de
los documentos. Se citarán por separado el software y la edición consultada.

## Nombre en CRAN

En la revisión del [índice de paquetes disponibles de CRAN](https://cran.r-project.org/web/packages/available_packages_by_name.html)
no se encontró `citesperu`. Esto no reserva el nombre ni verifica el archivo
histórico o todas las plataformas; repetir la comprobación antes del envío.
