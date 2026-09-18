# citesperu 0.0.0.9000

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

* **Documentación del paquete y manuales:**
  * Actualización de `R/citesperu-package.R` y `R/data.R` con documentación roxygen2 para `cites_fauna_peru_2018`, `cites_flora_peru_2018` y `codigos_departamentos_pe`.
  * Generación de archivos de ayuda `.Rd` en `man/`.
  * Actualización de `README.Rmd` y renderizado con Quarto Pandoc a `README.md`.
  * Registro de control de procedencia en `docs/FUENTES.md` y plan de desarrollo en `docs/PLAN.md`.
  * Registro pormenorizado de cambios e implementaciones en `docs/IMPLEMENTACION_DOCUMENTACION.md`.
