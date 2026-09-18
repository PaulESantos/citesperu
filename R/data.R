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
#' taxonómica superior (Phyllum, Clase, Orden y Familia) y
#' presenta para cada registro información sobre el nombre científico,
#' Apéndice CITES, categoría de conservación nacional según el D.S. n.° 004-2014-MINAGRI,
#' categoría de la Lista Roja de la UICN, condición de distribución (Nativa o Endémica),
#' nombres comunes, sinónimos y autoría taxonómica.
#'
#' @format Un data frame o tibble con 496 especies oficiales (más 16 especies asociadas
#' al Apéndice III) y las siguientes variables:
#' \describe{
#'   \item{n}{Número correlativo asignado a la especie dentro del listado oficial.}
#'   \item{phyllum}{Phyllum al que pertenece la especie según la clasificación de la fuente (generalmente \emph{Chordata} o \emph{Cnidaria}).}
#'   \item{clase}{Clase taxonómica (Actinopterygii, Amphibia, Anthozoa, Aves, Chondrichthyes, Holothuroidea, Mammalia, Reptilia).}
#'   \item{orden}{Orden taxonómico.}
#'   \item{familia}{Familia taxonómica.}
#'   \item{especie}{Nombre científico de la especie consignado en el listado.}
#'   \item{apendice}{Apéndice de la CITES en el que se encuentra incluida la especie (\code{"I"}, \code{"II"} o \code{"III"}).}
#'   \item{categoria_nacional}{Categoría de conservación nacional según el Decreto Supremo n.° 004-2014-MINAGRI:
#'   \code{"CR"} (En peligro crítico), \code{"EN"} (En peligro), \code{"VU"} (Vulnerable),
#'   \code{"NT"} (Casi amenazado), \code{"LC"} (Preocupación menor), \code{"DD"} (Datos insuficientes) o \code{NA}.}
#'   \item{uicn}{Categoría en la Lista Roja de la UICN (\code{"CR"}, \code{"EN"}, \code{"VU"}, \code{"NT"}, \code{"LC"}, \code{"DD"} o \code{NA}).}
#'   \item{ne}{Condición de distribución indicada en el listado: \code{"Nativa"} o \code{"Endémica"}.}
#'   \item{nombre_comun}{Nombre o nombres comunes vernáculos consignados en el documento fuente.}
#'   \item{sinonimos}{Nombres científicos tratados como sinónimos taxonómicos en la publicación.}
#'   \item{autor}{Autoría y año de descripción científica de la especie.}
#'   \item{nota}{Número o texto de notas aclaratorias sobre taxonomía o poblaciones específicas.}
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
#' Asimismo, se realizó una depuración exhaustiva eliminando especies atribuidas erróneamente
#' al Perú en bases globales de CITES e incorporando aquellas con presencia confirmada.
#'
#' @source
#' Ministerio del Ambiente (MINAM). 2018. \emph{Listado de especies de Fauna Silvestre CITES - Perú}.
#' Dirección General de Diversidad Biológica, Lima, Perú.
#' Compendio oficial en Gob.pe: \url{https://www.gob.pe/institucion/minam/informes-publicaciones/395692-listado-fauna-cites-peru-2018}
#'
#' @references
#' MINAM (2011). \emph{Especies de Fauna Silvestre Peruana en los Apéndices de la CITES}. Ministerio del Ambiente, Lima.
#'
#' MINAM (2014). \emph{Especies de Fauna Silvestre Peruana en los Apéndices de la CITES}. 2.ª ed., Ministerio del Ambiente, Lima.
#'
#' CITES (2016). \emph{Resolución Conf. 12.11 (Rev. CoP17): Nomenclatura normalizada}. Secretaría CITES, Ginebra.
#'
#' @examples
#' \dontrun{
#' library(citesperu)
#'
#' # Filtrar especies en el Apéndice I
#' subset(cites_fauna_peru_2018, apendice == "I")
#'
#' # Resumen por clase taxonómica
#' table(cites_fauna_peru_2018$clase, cites_fauna_peru_2018$apendice)
#' }
#'
#' @keywords datasets fauna CITES Peru biodiversidad
#' @docType data
#' @name cites_fauna_peru_2018
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
#' @format Un data frame o tibble con 2506 observaciones y las siguientes variables:
#' \describe{
#'   \item{item}{Número de orden consecutivo.}
#'   \item{apendice}{Apéndice CITES en el que se encuentra incluido el taxón (\code{"I"}, \code{"II"} o \code{"III"}).}
#'   \item{familia}{Familia botánica en mayúsculas (ej. \code{"ORCHIDACEAE"}, \code{"CACTACEAE"}).}
#'   \item{n}{Número correlativo asignado dentro de cada familia o sección en la fuente.}
#'   \item{especie}{Nombre científico del taxón aceptado (especie, subespecie o variedad) con su autoría botánica.}
#'   \item{sinonimia}{Nombres científicos tratados como sinónimos taxonómicos en la publicación.}
#'   \item{nombre_local}{Nombre común o vernáculo local registrado (ej. "Zapatito", "Caoba", o "Sin registro").}
#'   \item{distribucion}{Distribución departamental en el Perú según los acrónimos de 2 letras de Lamas & Encarnación (1976),
#'   separados por comas. El símbolo \code{"?"} indica presencia confirmada en el Perú pero sin referencia departamental precisa.}
#'   \item{endemismo}{Condición de endemismo (\code{"Si"}, \code{"No"} o \code{NA}).}
#'   \item{referencias}{Citas bibliográficas botánicas que respaldan la descripción, el checklist o la sinonimia del taxón.}
#'   \item{anotacion}{Código de anotación CITES (ej. \code{"#1"}, \code{"#4"}) que define las partes, derivados o condiciones especiales reguladas.}
#' }
#'
#' @details
#' \strong{Distribución de Taxa por Familia Botánica:}
#' El compendio oficial reporta un total de aproximadamente 2506 taxa:
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
#' MINAM (2012). \emph{Catálogo de las especies peruanas de flora silvestre incluidas en los Apéndices de la CITES}. Lima.
#'
#' @examples
#' \dontrun{
#' library(citesperu)
#'
#' # Consultar orquídeas en el Apéndice I
#' subset(cites_flora_peru_2018, familia == "ORCHIDACEAE" & apendice == "I")
#'
#' # Conteo de especies por familia
#' table(cites_flora_peru_2018$familia)
#' }
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
#' @keywords datasets departamentos Peru geografia
#' @docType data
#' @name codigos_departamentos_pe
NULL
