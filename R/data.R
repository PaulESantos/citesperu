#' Fauna silvestre de Perú incluida en los Apéndices de la CITES (2018)
#'
#' Base de datos estructurada a partir del documento oficial
#' *"Listado de especies de Fauna Silvestre CITES - Perú"*,
#' publicado por el Ministerio del Ambiente (MINAM) como Autoridad Científica
#' CITES en enero de 2018.
#'
#' La base contiene los registros de especies de fauna silvestre con distribución
#' en el Perú incluidas en los Apéndices I, II y III de la Convención sobre el
#' Comercio Internacional de Especies Amenazadas de Fauna y Flora Silvestres (CITES).
#'
#' La fuente organiza las especies de acuerdo con su clasificación
#' taxonómica superior (Phylum, Clase, Subclase, Orden y Familia) y
#' presenta para cada registro información sobre el nombre científico,
#' Apéndice CITES, categoría de conservación nacional según el D.S. n.° 004-2014-MINAGRI,
#' categoría de la Lista Roja de la UICN, condición de distribución (Nativa o Endémica),
#' nombres comunes, sinónimos, autoría taxonómica y notas explicativas.
#'
#' @format Un data frame con 512 observaciones (496 especies oficiales reguladas más 16
#' asociadas al Apéndice III) y 17 variables:
#' \describe{
#'   \item{id_especie}{Identificador numérico secuencial único asignado al registro.}
#'   \item{phylum}{Phylum al que pertenece la especie según la clasificación de la fuente (\emph{Chordata}, \emph{Cnidaria} o \emph{Echinodermata}).}
#'   \item{clase}{Clase taxonómica (\emph{Actinopterygii}, \emph{Amphibia}, \emph{Anthozoa}, \emph{Aves}, \emph{Chondrichthyes}, \emph{Holothuroidea}, \emph{Mammalia}, \emph{Reptilia}).}
#'   \item{subclase}{Subclase taxonómica cuando aplica (ej. \emph{Elasmobranchii}) o \code{NA}.}
#'   \item{orden}{Orden taxonómico.}
#'   \item{familia}{Familia taxonómica.}
#'   \item{referencia_num}{Número correlativo de llamada de nota explicativa o referencia bibliográfica al pie en la publicación.}
#'   \item{n}{Número correlativo asignado dentro de cada clase en el listado original.}
#'   \item{especie}{Nombre científico del taxón aceptado.}
#'   \item{categoria_nacional}{Categoría de conservación nacional según el Decreto Supremo n.° 004-2014-MINAGRI:
#'   \code{"CR"} (En peligro crítico), \code{"EN"} (En peligro), \code{"VU"} (Vulnerable),
#'   \code{"NT"} (Casi amenazado), \code{"DD"} (Datos insuficientes) o \code{NA} (no categorizado).}
#'   \item{uicn}{Categoría en la Lista Roja de la UICN (\code{"CR"}, \code{"EN"}, \code{"VU"}, \code{"NT"}, \code{"LC"}, \code{"DD"} o \code{NA}).}
#'   \item{n_e}{Condición de distribución indicada en el listado: \code{"Nativa"} o \code{"Endémica"}.}
#'   \item{nombre_comun}{Nombre o nombres comunes vernáculos consignados en el documento fuente (variable multivaluada separada por comas).}
#'   \item{sinonimos}{Nombres científicos tratados como sinónimos taxonómicos en la publicación (variable multivaluada).}
#'   \item{autor}{Autoría y año de descripción científica de la especie.}
#'   \item{ap}{Apéndice de la CITES en el que se encuentra incluida la especie (\code{"I"}, \code{"II"} o \code{"III"}).}
#'   \item{referencia_text}{Texto explicativo de la nota técnica de referencia o aclaración sobre presencia geográfica y distribución.}
#' }
#'
#' @details
#' \strong{Composición y Cifras Oficiales:}
#' La publicación oficial reporta un total de 496 especies peruanas reguladas:
#' \itemize{
#'   \item \strong{Apéndice I:} 48 especies (29 mamíferos, 10 aves, 7 reptiles, 1 anfibio, 1 condrictio).
#'   \item \strong{Apéndice II:} 448 especies (274 aves, 84 mamíferos, 45 anfibios, 21 reptiles, 17 condrictios, 5 antozoos, 2 actinopterigios).
#'   \item \strong{Apéndice III:} 16 especies con presencia nacional (7 aves, 7 mamíferos, 1 reptil, 1 holoturoideo).
#' }
#'
#' \strong{Tratamiento del Apéndice III:}
#' El MINAM aclara expresamente que las especies del Apéndice III presentes en el país
#' fueron incluidas a solicitud de otros Estados Parte; debido a que el Perú no ha
#' solicitado la inclusión de ninguna especie en dicho apéndice, estos 16 registros
#' no forman parte de la suma total oficial de 496 especies.
#'
#' \strong{Actualización CoP17 (Johannesburgo 2016):}
#' Esta edición integró las resoluciones de la 17.ª Conferencia de las Partes,
#' incluyendo especies adoptadas recientemente como los tiburones zorro (\emph{Alopias} spp.),
#' las mantarrayas (\emph{Mobula} spp.) y la rana gigante del lago Titicaca (\emph{Telmatobius culeus}).
#'
#' @source
#' Ministerio del Ambiente (MINAM). 2018. \emph{Listado de especies de Fauna Silvestre CITES - Perú}.
#' Dirección General de Diversidad Biológica, Lima, Perú.
#' Compendio oficial en Gob.pe: \url{https://www.gob.pe/institucion/minam/informes-publicaciones/395692-listado-fauna-cites-peru-2018}
#'
#' @examples
#' data(cites_fauna_peru_2018)
#'
#' # Filtrar especies en el Apéndice I
#' subset(cites_fauna_peru_2018, ap == "I")
#'
#' # Resumen por clase taxonómica
#' table(cites_fauna_peru_2018$clase, cites_fauna_peru_2018$ap)
#'
#' @keywords datasets fauna CITES Peru biodiversidad
#' @docType data
#' @name cites_fauna_peru_2018
NULL

#' Fauna silvestre de Perú incluida en los Apéndices de la CITES (2019)
#'
#' Base de datos estructurada a partir del documento oficial
#' *"Listado de especies de Fauna Silvestre CITES - Perú (v.2019-01)"*,
#' publicado por el Ministerio del Ambiente (MINAM) como Autoridad Científica CITES.
#'
#' Esta actualización consolida 523 registros taxonómicos de fauna silvestre y especies
#' hidrobiológicas presentes en el Perú, incorporando precisiones sobre género, ámbito
#' ecológico (amazónico, marino, etc.) y comentarios de referencia actualizados.
#'
#' @format Un data frame con 523 observaciones y 17 variables:
#' \describe{
#'   \item{id}{Identificador numérico correlativo de la especie en el listado.}
#'   \item{phylum}{Phylum taxonómico (\emph{Chordata}, \emph{Cnidaria}, \emph{Echinodermata}).}
#'   \item{clase}{Clase taxonómica (\emph{Actinopterygii}, \emph{Amphibia}, \emph{Anthozoa}, \emph{Aves}, \emph{Chondrichthyes}, \emph{Holothuroidea}, \emph{Mammalia}, \emph{Reptilia}).}
#'   \item{subclase}{Subclase taxonómica cuando aplica (ej. \emph{Elasmobranchii}) o \code{NA}.}
#'   \item{orden}{Orden taxonómico.}
#'   \item{familia}{Familia taxonómica.}
#'   \item{genero}{Género taxonómico.}
#'   \item{nombre_cientifico}{Nombre científico de la especie.}
#'   \item{apendice}{Apéndice CITES en el que se encuentra incluida la especie (\code{"I"}, \code{"II"}, \code{"III"} o \code{"III/w"}).}
#'   \item{ds_n_004_2014}{Categoría de conservación nacional según el D.S. n.° 004-2014-MINAGRI:
#'   \code{"CR"}, \code{"EN"}, \code{"VU"}, \code{"NT"} o \code{NA}.}
#'   \item{uicn}{Categoría en la Lista Roja de la UICN (\code{"CR"}, \code{"EN"}, \code{"VU"}, \code{"NT"}, \code{"LC"}, \code{"DD"} o \code{NA}).}
#'   \item{geografia}{Condición de distribución geográfica en el Perú: \code{"Nativa"} o \code{"Endémica"}.}
#'   \item{nombre_comun}{Nombre o nombres comunes vernáculos (variable multivaluada separada por comas).}
#'   \item{sinonimos}{Sinónimos taxonómicos consignados en la publicación (variable multivaluada).}
#'   \item{autor}{Autoría y año de descripción científica del taxón.}
#'   \item{comentarios_de_referencia}{Notas aclaratorias, comentarios sobre distribución, citas o precisiones taxonómicas.}
#'   \item{ambito}{Ámbito ecológico/geográfico principal asignado en el listado (ej. \code{"AMA"} para Amazónico, \code{"MAR"} para Marino, \code{"AND"} para Andino, \code{"COS"} para Costero).}
#' }
#'
#' @source
#' Ministerio del Ambiente (MINAM). 2019. \emph{Listado de Fauna CITES Perú v.2019-01}.
#' Dirección General de Diversidad Biológica, Lima, Perú.
#' Ficha oficial en Gob.pe: \url{https://www.gob.pe/institucion/minam/informes-publicaciones/395694-listado-de-fauna-cites-peru-2019}
#'
#' @examples
#' data(cites_fauna_peru_2019)
#'
#' # Distribución por ámbito ecológico
#' table(cites_fauna_peru_2019$ambito, useNA = "ifany")
#'
#' # Especies amenazadas a nivel nacional en Apéndice I
#' subset(cites_fauna_peru_2019, apendice == "I" & !is.na(ds_n_004_2014))
#'
#' @keywords datasets fauna CITES Peru biodiversidad
#' @docType data
#' @name cites_fauna_peru_2019
NULL

#' Fauna silvestre de Perú incluida en los Apéndices de la CITES (2023)
#'
#' Base de datos estructurada a partir del documento oficial
#' *"Listado de especies de Fauna Silvestre CITES - Perú (v.2023 MAR)"*,
#' publicado por el Ministerio del Ambiente (MINAM) como Autoridad Científica CITES
#' en marzo de 2023.
#'
#' Incorpora las resoluciones y enmiendas adoptadas en la 19.ª Conferencia de las Partes
#' (CoP19, Panamá 2022) que entraron en vigor internacional el 23 de febrero de 2023.
#' Comprende 568 especies oficiales con presencia confirmada en el Perú (48 en Apéndice I,
#' 503 en Apéndice II y 17 en Apéndice III), categorizadas además por la competencia
#' de gestión sectorial (Fauna Silvestre bajo SERFOR o Especie Hidrobiológica bajo PRODUCE).
#'
#' @format Un data frame con 568 observaciones y 17 variables:
#' \describe{
#'   \item{n}{Número correlativo oficial asignado a la especie de 1 a 568.}
#'   \item{fauna_silvestre_especie_hidrobiologica}{Competencia sectorial de administración:
#'   \code{"FAUNA"} (recursos terrestres bajo rectoría de SERFOR) o \code{"HIDROBIOLÓGICO"}
#'   (recursos acuáticos/pesqueros bajo rectoría de PRODUCE).}
#'   \item{phyllum}{Phyllum al que pertenece la especie (\emph{Chordata}, \emph{Cnidaria}, \emph{Echinodermata}).}
#'   \item{clase}{Clase taxonómica (\emph{Actinopterygii}, \emph{Amphibia}, \emph{Anthozoa}, \emph{Aves}, \emph{Chondrichthyes}, \emph{Holothuroidea}, \emph{Mammalia}, \emph{Reptilia}).}
#'   \item{orden}{Orden taxonómico.}
#'   \item{familia}{Familia taxonómica.}
#'   \item{genero}{Género taxonómico.}
#'   \item{especie_nombre_cientifico}{Nombre científico de la especie.}
#'   \item{nombre_comun}{Nombres vernáculos o comunes registrados (variable multivaluada separada por comas).}
#'   \item{apendice}{Apéndice CITES en el que se encuentra regulada la especie (\code{"I"}, \code{"II"}, \code{"III"} o \code{"III/w"}).}
#'   \item{inclusion_y_o_enmienda}{Año de inclusión inicial o de la última enmienda de la especie en los Apéndices de la CITES (ej. 1975, 1987, 2019, 2023).}
#'   \item{categoria_nacional}{Categoría de conservación nacional según el D.S. n.° 004-2014-MINAGRI:
#'   \code{"CR"}, \code{"EN"}, \code{"VU"}, \code{"NT"}, \code{"DD"} o \code{NA}.}
#'   \item{uicn}{Categoría en la Lista Roja de la UICN (\code{"CR"}, \code{"EN"}, \code{"VU"}, \code{"NT"}, \code{"LC"}, \code{"DD"}, \code{"LR"}, \code{"NE"}).}
#'   \item{geografia}{Condición de distribución geográfica en el Perú: \code{"Nativa"} o \code{"Endémica"}.}
#'   \item{autor}{Autoría y año de descripción científica de la especie.}
#'   \item{sinonimos}{Sinónimos taxonómicos consignados en la publicación (variable multivaluada).}
#'   \item{comentarios_de_referencia}{Comentarios técnicos de referencia, alcance de inclusión por orden/familia o precisiones de poblaciones.}
#' }
#'
#' @details
#' \strong{Composición Oficial por Clases (2023):}
#' El compendio oficial reporta un total de 568 especies:
#' \itemize{
#'   \item \strong{Actinopterigios:} 2 especies (Apéndice II).
#'   \item \strong{Anfibios:} 79 especies (1 en Apéndice I, 78 en Apéndice II).
#'   \item \strong{Antozoos:} 5 especies (Apéndice II).
#'   \item \strong{Aves:} 296 especies (10 en Apéndice I, 280 en Apéndice II, 6 en Apéndice III).
#'   \item \strong{Condrictios:} 39 especies (1 en Apéndice I, 34 en Apéndice II, 4 en Apéndice III).
#'   \item \strong{Holoturoideos:} 1 especie (Apéndice III).
#'   \item \strong{Mamíferos:} 114 especies (29 en Apéndice I, 80 en Apéndice II, 5 en Apéndice III).
#'   \item \strong{Reptiles:} 32 especies (7 en Apéndice I, 24 en Apéndice II, 1 en Apéndice III).
#'   \item \strong{Total general:} 48 en Apéndice I, 503 en Apéndice II, 17 en Apéndice III = 568 especies.
#' }
#'
#' @source
#' Ministerio del Ambiente (MINAM). 2023. \emph{Listado de especies de Fauna Silvestre CITES - Perú (v.2023 MAR)}.
#' Dirección General de Diversidad Biológica, Lima, Perú.
#' Publicación oficial en Gob.pe: \url{https://www.gob.pe/institucion/minam/informes-publicaciones/4109405-listado-de-fauna-cites-peru-2023}
#'
#' @examples
#' data(cites_fauna_peru_2023)
#'
#' # Conteo de especies por competencia sectorial y apéndice
#' table(cites_fauna_peru_2023$fauna_silvestre_especie_hidrobiologica, cites_fauna_peru_2023$apendice)
#'
#' # Especies incorporadas o enmendadas en la CoP19 (2023)
#' subset(cites_fauna_peru_2023, inclusion_y_o_enmienda == 2023)
#'
#' @keywords datasets fauna CITES Peru biodiversidad
#' @docType data
#' @name cites_fauna_peru_2023
NULL

#' Flora silvestre de Perú incluida en los Apéndices de la CITES (2018)
#'
#' Base de datos estructurada a partir del documento oficial
#' *"Listado de especies de Flora Silvestre CITES - Perú"*,
#' publicado por el Ministerio del Ambiente (MINAM) como Autoridad Científica
#' CITES en enero de 2018.
#'
#' El listado comprende las especies nativas de plantas vasculares peruanas incluidas
#' en los Apéndices I, II y III de la CITES, agrupadas en 9 familias botánicas.
#' Destaca la incorporación integral de la 3.ª edición del \emph{CITES Cactaceae Checklist}
#' (Hunt, 2016) y la revisión de colecciones de herbarios físicos (USM, MOL) y
#' virtuales internacionales (MO, US, NY, F).
#'
#' @format Un data frame con 2506 observaciones y 10 variables:
#' \describe{
#'   \item{item}{Número de orden consecutivo general del 1 al 2506.}
#'   \item{apendice}{Apéndice CITES en el que se encuentra incluido el taxón (\code{"I"}, \code{"II"} o \code{"III"}).}
#'   \item{familia}{Familia botánica en mayúsculas (ej. \code{"ORCHIDACEAE"}, \code{"CACTACEAE"}).}
#'   \item{n}{Número correlativo asignado dentro de cada familia o sección en la fuente.}
#'   \item{especie}{Nombre científico del taxón aceptado (especie, subespecie o variedad) con su correspondiente autoría botánica.}
#'   \item{sinonimia}{Nombres científicos tratados como sinónimos taxonómicos en la publicación (variable multivaluada).}
#'   \item{nombre_local}{Nombre común o vernáculo local registrado (ej. "Zapatito", "Caoba", o "Sin registro").}
#'   \item{distribucion}{Distribución departamental en el Perú según los acrónimos de 2 letras de Lamas & Encarnación (1976),
#'   separados por comas. El símbolo \code{"?"} indica presencia confirmada en el Perú pero sin referencia departamental precisa.}
#'   \item{endemismo}{Condición de endemismo: \code{"Si"}, \code{"No"} o \code{"Endémica"}.}
#'   \item{referencias}{Citas bibliográficas botánicas que respaldan la descripción, el checklist o la sinonimia del taxón.}
#' }
#'
#' @details
#' \strong{Distribución de Taxa por Familia Botánica:}
#' El compendio oficial reporta un total de 2506 taxa:
#' \itemize{
#'   \item \strong{ORCHIDACEAE:} 2215 taxa (11 especies y 1 variedad en Apéndice I: \emph{Phragmipedium} spp.; 2203 taxa en Apéndice II).
#'   \item \strong{CACTACEAE:} 186 taxa en Apéndice II (clasificación basada íntegramente en David Hunt, 2016).
#'   \item \strong{CYATHEACEAE:} 79 taxa en Apéndice II (helechos arbóreos).
#'   \item \strong{FABACEAE:} 11 taxa en Apéndice II (\emph{Platymiscium} spp. y otras maderas).
#'   \item \strong{ZAMIACEAE:} 9 taxa en Apéndice II (cícadas).
#'   \item \strong{DICKSONIACEAE:} 2 taxa en Apéndice II (helechos arbóreos).
#'   \item \strong{MELIACEAE:} 2 taxa (1 en Apéndice II: \emph{Swietenia macrophylla}; 1 en Apéndice III: \emph{Cedrela odorata}).
#'   \item \strong{EUPHORBIACEAE:} 1 taxón en Apéndice II (\emph{Euphorbia} suculenta nativa).
#'   \item \strong{LAURACEAE:} 1 taxón en Apéndice II (\emph{Aniba rosaeodora}, palo de rosa).
#' }
#'
#' \strong{Acrónimos Departamentales (Lamas y Encarnación, 1976):}
#' La variable \code{distribucion} utiliza los códigos oficiales:
#' AM (Amazonas), AN (Áncash), AP (Apurímac), AR (Arequipa), AY (Ayacucho), CA (Cajamarca),
#' CU (Cusco), HU (Huánuco), HV (Huancavelica), IC (Ica), JU (Junín), LA (Lambayeque),
#' LI (Lima), LL (La Libertad), LO (Loreto), MD (Madre de Dios), MO (Moquegua), PA (Pasco),
#' PI (Piura), PU (Puno), SM (San Martín), TA (Tacna), TU (Tumbes), UC (Ucayali).
#'
#' El símbolo \code{"?"} \strong{no} significa estatus taxonómico dudoso; indica que la especie
#' procede fehacientemente de colectas peruanas pero la etiqueta de herbario no especifica el departamento.
#'
#' @source
#' Ministerio del Ambiente (MINAM). 2018. \emph{Listado de especies de Flora Silvestre CITES - Perú}.
#' Dirección General de Diversidad Biológica, Lima, Perú.
#' Publicación oficial en Gob.pe: \url{https://www.gob.pe/institucion/minam/informes-publicaciones/395685-listado-flora-cites-peru-2018}
#'
#' @references
#' Hunt, D. (2016). \emph{CITES Cactaceae Checklist}. Third Edition. Royal Botanic Gardens, Kew.
#'
#' Lamas, G. & Encarnación, J. (1976). Acrónimos departamentales para registros biogeográficos peruanos.
#'
#' @examples
#' data(cites_flora_peru_2018)
#'
#' # Consultar orquídeas en el Apéndice I
#' subset(cites_flora_peru_2018, familia == "ORCHIDACEAE" & apendice == "I")
#'
#' # Conteo de especies por familia
#' table(cites_flora_peru_2018$familia)
#'
#' @keywords datasets flora CITES Peru botanica orquideas cactaceas
#' @docType data
#' @name cites_flora_peru_2018
NULL

#' Códigos y Acrónimos de Departamentos del Perú (Lamas y Encarnación, 1976)
#'
#' Tabla de referencia con los acrónimos estándar de dos letras utilizados
#' históricamente en biogeografía peruana y adoptados por el MINAM en los listados
#' oficiales de flora CITES para registrar la distribución geográfica departamental.
#'
#' @format Un data frame con 24 observaciones y 3 variables:
#' \describe{
#'   \item{acronimo}{Código de 2 letras en mayúsculas (ej. \code{"AM"}, \code{"CU"}, \code{"LO"}).}
#'   \item{departamento}{Nombre completo del departamento o región del Perú.}
#'   \item{ubigeo_inei}{Código de ubicación geográfica departamental de 2 dígitos según el INEI.}
#' }
#'
#' @details
#' En las tablas de distribución botánica del MINAM, se utiliza además el símbolo especial
#' \code{"?"} para señalar que una especie cuenta con registro o colecta confirmada en el Perú,
#' pero el espécimen de herbario carece de localidad o departamento explícito en su etiqueta.
#'
#' @references
#' Lamas, G. & Encarnación, J. (1976). Acrónimos departamentales para registros biogeográficos peruanos.
#'
#' @examples
#' data(codigos_departamentos_pe)
#' head(codigos_departamentos_pe)
#'
#' @keywords datasets departamentos Peru geografia
#' @docType data
#' @name codigos_departamentos_pe
NULL
