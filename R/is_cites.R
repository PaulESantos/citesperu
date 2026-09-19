#' Verificación Booleana de Inclusión en los Apéndices CITES del Perú
#'
#' @description
#' Comprueba de forma vectorizada si cada uno de los nombres científicos consultados se
#' encuentra incluido en los Apéndices CITES del Perú (como taxón aceptado o sinónimo oficial).
#' Devuelve un vector lógico del mismo largo y orden que el vector de entrada.
#'
#' @param splist Vector de caracteres con nombres científicos, o un \code{data.frame} con
#'   una columna de nombres taxonómicos.
#' @param taxon Subconjunto de evaluación: \code{"all"} (fauna y flora, por defecto),
#'   \code{"fauna"} o \code{"flora"}.
#' @param edition Edición del listado oficial a consultar (\code{"latest"} por defecto).
#' @param allow_synonyms Si es \code{TRUE} (por defecto), considera válidos los sinónimos
#'   oficiales registrados en los documentos del MINAM.
#'
#' @return Un vector lógico (\code{TRUE}, \code{FALSE} o \code{NA}) con la misma longitud
#'   que \code{splist}.
#'
#' @examples
#' # Consulta vectorizada simple
#' is_cites(c(
#'   "Tremarctos ornatus",
#'   "Swietenia macrophylla",
#'   "Canis lupus familiaris",
#'   "Homo sapiens"
#' ))
#'
#' # Consulta diferenciando fauna y flora
#' is_cites("Tremarctos ornatus", taxon = "fauna")
#' is_cites("Tremarctos ornatus", taxon = "flora")
#'
#' @export
is_cites <- function(splist,
                     taxon = c("all", "fauna", "flora"),
                     edition = c("latest", "all", "2023", "2019", "2018"),
                     allow_synonyms = TRUE) {
  taxon   <- match.arg(taxon)
  edition <- match.arg(edition)

  # Si la entrada es nula o vacía
  if (length(splist) == 0L) return(logical())

  # Ejecutar matching estándar con max_dist = 0 para evaluación booleana exacta
  res <- cites_match(
    splist,
    taxon = taxon,
    edition = edition,
    max_dist = 0,
    allow_synonyms = allow_synonyms,
    output = "standard"
  )

  # Manejo de NAs de entrada
  if (is.character(splist)) {
    is_na_in <- is.na(splist) | !nzchar(trimws(splist))
    res$is_cites[is_na_in] <- NA
  }

  res$is_cites
}

#' @rdname is_cites
#' @export
is_cites_pe <- is_cites
