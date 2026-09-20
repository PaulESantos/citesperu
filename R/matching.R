#' Consultar y Contrastar Especies con los Apéndices CITES del Perú
#'
#' @description
#' Compara una lista de nombres científicos de plantas o animales contra las bases de datos
#' oficiales de CITES Perú (Fauna y Flora), ejecutando un pipeline de concordancia secuencial
#' optimizado:
#' \enumerate{
#'   \item \strong{Direct match}: Coincidencia exacta con un taxón CITES aceptado.
#'   \item \strong{Synonym match}: Coincidencia con un sinónimo oficial registrado en las publicaciones
#'         del MINAM, resolviendo el registro al taxón aceptado y su respectivo Apéndice.
#'   \item \strong{Suffix match}: Variación de un par de sufijos latinos permitido dentro del mismo género.
#'   \item \strong{Fuzzy match}: Coincidencia aproximada por distancia de edición de Levenshtein,
#'         acotada por \code{max_dist}; los empates se reportan como ambiguos.
#'   \item \strong{Genus match}: Detección de registros CITES para un género ingresado (o con
#'         calificador \code{sp.}/\code{spp.}), que requiere validación a nivel de especie.
#'   \item \strong{Unmatched}: Nombres sin coincidencia en los listados oficiales nacionales (\code{is_cites = FALSE}).
#' }
#'
#' @param splist Vector de caracteres con nombres científicos, o un \code{data.frame} / \code{tibble}
#'   con una columna de nombres taxonómicos.
#' @param taxon Subconjunto de evaluación: \code{"all"} (fauna y flora, por defecto), \code{"fauna"}
#'   o \code{"flora"}.
#' @param edition Edición del listado oficial a consultar:
#'   \itemize{
#'     \item \code{"latest"} (por defecto): Listado de Fauna 2023 (v.2023 MAR) + Listado de Flora 2018.
#'     \item \code{"all"}: Incluye registros históricos de 2018, 2019 y 2023.
#'     \item \code{"2023"}: Exclusivamente la edición de Fauna 2023.
#'     \item \code{"2019"}: Exclusivamente la edición de Fauna 2019.
#'     \item \code{"2018"}: Edición 2018 (Fauna 2018 y Flora 2018).
#'   }
#' @param max_dist Distancia máxima de edición permitida para la fase difusa (\emph{fuzzy match}).
#'   Por defecto es \code{1} (permite 1 inserción, sustitución o eliminación dentro del mismo género).
#' @param allow_synonyms Valor lógico. Si es \code{TRUE} (por defecto), resuelve coincidencias con
#'   sinónimos oficiales al taxón CITES aceptado. Si es \code{FALSE}, solo busca nombres aceptados.
#' @param genus_fallback Valor lógico. Si es \code{FALSE} (por defecto), los nombres binominales que no
#'   coinciden a nivel de especie permanecen como no listados (\code{unmatched}). Si es \code{TRUE},
#'   se permite que binomios sin coincidencia específica hagan match con el género si este está regulado.
#' @param output Formato de salida:
#'   \itemize{
#'     \item \code{"standard"} (por defecto): Devuelve las columnas esenciales para análisis de conservación.
#'     \item \code{"full"}: Devuelve la totalidad de componentes de parsing, banderas (\code{has_cf}, \code{is_sp})
#'           y metadatos de autoría.
#'   }
#'
#' @return Un \code{tibble} con los resultados de la concordancia preservando el orden original de entrada.
#' Incluye \code{match_assessment}, procedencia de la fuente y la edición efectiva.
#' Sus valores son \code{"matched"}, \code{"not_listed"},
#' \code{"requires_species_validation"}, \code{"requires_taxonomic_validation"}
#' y \code{"ambiguous_match"}.
#'
#' @examples
#' # Consulta exacta y sinónimos
#' cites_match(c(
#'   "Tremarctos ornatus",
#'   "Epipedobates femoralis",
#'   "Swietenia macrophylla",
#'   "Paphiopedilum besseae",
#'   "Homo sapiens"
#' ))
#'
#' # Consulta con errores tipográficos (fuzzy match)
#' cites_match(c("Tremarctos ornatu", "Swietenia macrofila"), max_dist = 2)
#'
#' # Consulta a nivel de género
#' cites_match(c("Cedrela sp.", "Touit spp."))
#'
#' @export
cites_match <- function(splist,
                        taxon = c("all", "fauna", "flora"),
                        edition = c("latest", "all", "2023", "2019", "2018"),
                        max_dist = 1,
                        allow_synonyms = TRUE,
                        genus_fallback = FALSE,
                        output = c("standard", "full")) {
  taxon   <- match.arg(taxon)
  edition <- match.arg(edition)
  output  <- match.arg(output)

  # 1. Parsing y clasificación
  classified <- cites_classify_names(splist)
  n_total <- nrow(classified)

  if (n_total == 0L) {
    return(classified)
  }

  # 2. Filtrar backbone de referencia
  bb <- .get_backbone(taxon = taxon, edition = edition)

  # Inicializar lista de bloques coincidentes
  matched_blocks <- list()
  unresolved <- classified

  # 3. Etapa 1: Direct Match (Exacto con nombres aceptados)
  res_exact <- .direct_match(unresolved, bb)
  if (!is.null(res_exact$matched)) {
    matched_blocks[[length(matched_blocks) + 1L]] <- res_exact$matched
  }
  unresolved <- res_exact$remaining

  # 4. Etapa 2: Synonym Match (Sinónimos oficiales)
  if (isTRUE(allow_synonyms) && nrow(unresolved) > 0L) {
    res_syn <- .synonym_match(unresolved, bb)
    if (!is.null(res_syn$matched)) {
      matched_blocks[[length(matched_blocks) + 1L]] <- res_syn$matched
    }
    unresolved <- res_syn$remaining
  }

  # 5. Etapa 3: Suffix Match (Flexiones de sufijo latino en epíteto)
  if (nrow(unresolved) > 0L) {
    res_suf <- .suffix_match(unresolved, bb)
    if (!is.null(res_suf$matched)) {
      matched_blocks[[length(matched_blocks) + 1L]] <- res_suf$matched
    }
    unresolved <- res_suf$remaining
  }

  # 6. Etapa 4: Fuzzy Match (Distancia de edición acotada al género)
  if (nrow(unresolved) > 0L && max_dist >= 1) {
    res_fuz <- .fuzzy_match_within_genus(unresolved, bb, max_dist = max_dist)
    if (!is.null(res_fuz$matched)) {
      matched_blocks[[length(matched_blocks) + 1L]] <- res_fuz$matched
    }
    unresolved <- res_fuz$remaining
  }

  # 7. Etapa 5: Genus Match (Para géneros regulados o sp./spp.)
  if (nrow(unresolved) > 0L) {
    res_gen <- .genus_match(unresolved, bb, genus_fallback = genus_fallback)
    if (!is.null(res_gen$matched)) {
      matched_blocks[[length(matched_blocks) + 1L]] <- res_gen$matched
    }
    unresolved <- res_gen$remaining
  }

  # 8. Etapa 6: Unmatched
  if (nrow(unresolved) > 0L) {
    unresolved$matched_name       <- NA_character_
    unresolved$accepted_name      <- NA_character_
    unresolved$match_type         <- "unmatched"
    unresolved$match_assessment   <- "not_listed"
    unresolved$is_cites           <- FALSE
    unresolved$apendice           <- NA_character_
    unresolved$taxon              <- NA_character_
    unresolved$familia            <- NA_character_
    unresolved$clase              <- NA_character_
    unresolved$orden              <- NA_character_
    unresolved$categoria_nacional <- NA_character_
    unresolved$uicn               <- NA_character_
    unresolved$autor_cites        <- NA_character_
    unresolved$edition_used       <- NA_character_
    unresolved$source_dataset     <- NA_character_
    unresolved$source_row_id      <- NA_character_
    unresolved$source_title       <- NA_character_
    unresolved$source_url         <- NA_character_
    unresolved$candidate_names    <- NA_character_
    unresolved$candidate_count    <- NA_integer_
    unresolved$matched_dist       <- NA_integer_
    matched_blocks[[length(matched_blocks) + 1L]] <- unresolved
  }

  # 9. Consolidar y ordenar por input_index
  out_all <- dplyr::bind_rows(matched_blocks)
  out_all <- out_all[order(out_all$input_index), , drop = FALSE]

  # El estado comunica la certeza del nombre, no solo la presencia en el índice.
  out_all$match_assessment <- ifelse(
    out_all$match_type == "genus",
    "requires_species_validation",
    ifelse(
      out_all$match_type == "ambiguous_match",
      "ambiguous_match",
      ifelse(
        out_all$match_type == "unmatched",
        "not_listed",
        ifelse(
          out_all$has_cf %in% TRUE | out_all$has_aff %in% TRUE |
            out_all$had_hybrid %in% TRUE | (!is.na(out_all$rank) & out_all$rank == 3) |
            out_all$match_type %in% c("suffix", "fuzzy"),
          "requires_taxonomic_validation",
          "matched"
        )
      )
    )
  )

  # 10. Formato de salida
  if (output == "standard") {
    cols_std <- c(
      "input_index", "input_name", "matched_name", "accepted_name",
      "match_type", "match_assessment", "is_cites", "apendice", "taxon", "clase",
      "familia", "categoria_nacional", "uicn", "matched_dist", "edition_used",
      "source_dataset", "source_row_id", "source_title", "source_url",
      "candidate_names", "candidate_count"
    )
    cols_std <- cols_std[cols_std %in% names(out_all)]
    out_all <- out_all[, cols_std, drop = FALSE]
  }

  tibble::as_tibble(out_all)
}

#' @rdname cites_match
#' @export
cites_matching <- cites_match

#' @rdname cites_match
#' @export
match_cites_pe <- cites_match
