#' Fauna silvestre de Perú incluida en los Apéndices de la CITES
#'
#' Base de datos estructurada a partir del documento
#' "Listado de especies de Fauna Silvestre CITES - Perú",
#' publicado por el Ministerio del Ambiente (MINAM) en 2018.
#'
#' La base contiene los registros de especies de fauna silvestre peruana
#' incluidas en los Apéndices I, II y III de la Convención sobre el Comercio
#' Internacional de Especies Amenazadas de Fauna y Flora Silvestres (CITES).
#'
#' La fuente organiza las especies de acuerdo con su clasificación
#' taxonómica superior, incluyendo Phyllum, Clase, Orden y Familia, y
#' presenta para cada registro información sobre el nombre científico,
#' Apéndice CITES, categoría nacional, categoría de la UICN, condición
#' de distribución, nombres comunes, sinónimos y autoría taxonómica.
#'
#' Los datos contenidos en este objeto corresponden a la información
#' publicada en la versión 2018 del listado. Por tanto, los nombres
#' científicos, sinónimos, categorías y demás atributos no deben
#' interpretarse como una actualización taxonómica o normativa posterior
#' a dicha publicación.
#'
#' @format Un tibble con 496 observaciones y las siguientes variables:
#' \describe{
#'   \item{n}{Número correlativo asignado a la especie dentro del listado
#'   CITES-Perú 2018.}
#'
#'   \item{phyllum}{Phyllum al que pertenece la especie, según la
#'   clasificación utilizada en el documento fuente.}
#'
#'   \item{clase}{Clase taxonómica a la que pertenece la especie.}
#'
#'   \item{orden}{Orden taxonómico a la que pertenece la especie.}
#'
#'   \item{familia}{Familia taxonómica a la que pertenece la especie.}
#'
#'   \item{especie}{Nombre científico de la especie consignado en el
#'   listado.}
#'
#'   \item{apendice}{Apéndice de la CITES en el que se encuentra incluida
#'   la especie. Los valores corresponden a I, II o III.}
#'
#'   \item{categoria_nacional}{Categoría de conservación nacional
#'   consignada en el documento. Cuando la fuente no registra una categoría,
#'   se conserva como valor faltante o indicador de ausencia, según la
#'   estructura definida durante la recuperación.}
#'
#'   \item{uicn}{Categoría de conservación de la especie según la UICN,
#'   tal como aparece en el documento fuente.}
#'
#'   \item{ne}{Condición de distribución de la especie indicada en el
#'   listado, principalmente "Nativa" o "Endémica".}
#'
#'   \item{nombre_comun}{Nombre o nombres comunes consignados para la
#'   especie en el documento fuente.}
#'
#'   \item{sinonimos}{Nombres científicos tratados como sinónimos y
#'   consignados en el listado. Cuando existe más de un sinónimo, estos se
#'   conservan en un único campo de texto.}
#'
#'   \item{autor}{Autoría taxonómica de la especie según el listado
#'   CITES-Perú 2018.}
#'
#'   \item{nota}{Número o referencia de nota al pie asociada al registro,
#'   cuando corresponde.}
#' }
#'
#' @details
#' La información fue recuperada y estructurada a partir de las tablas
#' contenidas en el PDF original. Debido a que el documento presenta
#' información distribuida en varias líneas, particularmente en los campos
#' de nombres comunes, sinónimos y autoría, la recuperación requiere un
#' proceso de reconstrucción de registros.
#'
#' El documento fuente presenta el encabezado de las tablas como:
#' "N.°", "Especie", "Ap.", "Categoría nacional", "UICN", "N/E",
#' "Nombre común", "Sinónimos" y "Autor".
#'
#' La publicación reporta un total de 496 especies: 48 especies en el
#' Apéndice I, 448 en el Apéndice II y 16 registros asociados al
#' Apéndice III. El propio documento señala que las especies del
#' Apéndice III no se incluyen en la suma total cuando Perú no ha
#' solicitado su inclusión.
#'
#' La base debe utilizarse como una representación estructurada del
#' listado publicado en 2018. Para análisis actuales de distribución,
#' nomenclatura, categorías de amenaza o vigencia de las inclusiones CITES,
#' se recomienda contrastar los registros con fuentes taxonómicas y
#' normativas actualizadas.
#'
#' @source
#' Ministerio del Ambiente (MINAM). 2018.
#' *Listado de especies de Fauna Silvestre CITES - Perú*.
#'
#' El documento presenta el cuadro resumen de especies de fauna silvestre
#' peruana en los Apéndices de la CITES y posteriormente desarrolla el
#' listado taxonómico por Phyllum, Clase, Orden y Familia.
#'
#' @references
#' Ministerio del Ambiente. 2018. *Listado de especies de Fauna Silvestre
#' CITES - Perú*. Lima, Perú.
#'
#' Convention on International Trade in Endangered Species of Wild Fauna
#' and Flora (CITES). *CITES Appendices I, II and III*.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Consultar las especies incluidas en el Apéndice I
#' cites_fauna_peru_2018 |>
#'   filter(apendice == "I")
#'
#' # Número de especies por Apéndice
#' cites_fauna_peru_2018 |>
#'   count(apendice)
#'
#' # Buscar una especie por nombre científico
#' cites_fauna_peru_2018 |>
#'   filter(especie == "Arapaima gigas")
#' }
#'
#' @keywords datasets CITES fauna taxonomia biodiversidad Peru
#' @docType data
#' @name cites_fauna_peru_2018
NULL
