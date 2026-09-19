#' Clasificar y Normalizar Nombres Científicos en Componentes Taxonómicos
#'
#' @description
#' Desglosa y normaliza nombres científicos de plantas o animales en sus componentes
#' taxonómicos elementales: género, epíteto específico, rango infraespecífico, epíteto
#' infraespecífico y autoría. Detecta además calificadores botánicos y zoológicos
#' habituales (`cf.`, `aff.`, `sp.`, `spp.`, marcadores de híbrido).
#'
#' Esta función sigue la convención establecida en paquetes como \code{wcvpmatch}:
#' \itemize{
#'   \item \code{orig_genus}: Género en formato \emph{Title Case} (primera letra mayúscula).
#'   \item \code{orig_species}: Epíteto específico en minúsculas (sin acentos).
#'   \item \code{infra_rank}: Rango infraespecífico en minúsculas (\code{"subsp."}, \code{"var."}, \code{"f."}).
#'   \item \code{orig_infraspecies}: Epíteto infraespecífico en minúsculas.
#'   \item \code{canonical_name}: Nombre binomial o trinominal canónico reconstruido.
#'   \item \code{author}: Autoría taxonómica recuperada cuando está presente.
#'   \item Banderas lógicas: \code{has_cf}, \code{has_aff}, \code{is_sp}, \code{is_spp}, \code{had_hybrid}.
#' }
#'
#' @param splist Vector de caracteres con nombres científicos, o un data frame / tibble
#'   con una columna de nombres.
#' @param name_col Nombre de la columna que contiene los nombres si \code{splist} es un data frame.
#'   Por defecto busca columnas como \code{"species"}, \code{"especie"}, \code{"nombre_cientifico"} o \code{"name"}.
#'
#' @return Un \code{tibble} con una fila por cada nombre de entrada y las columnas normalizadas y banderas.
#'
#' @examples
#' cites_classify_names(c(
#'   "Tremarctos ornatus (F. G. Cuvier, 1825)",
#'   "Swietenia macrophylla King",
#'   "Phragmipedium boissierianum var. czerwiakowianum",
#'   "Cedrela cf. odorata",
#'   "Touit sp.",
#'   "X Haagespostoa albisetata"
#' ))
#'
#' @export
cites_classify_names <- function(splist, name_col = NULL) {
  # Manejo si la entrada es un data.frame / tibble
  if (is.data.frame(splist)) {
    if (is.null(name_col)) {
      candidates <- c("species", "especie", "nombre_cientifico", "scientific_name", "name", "taxon")
      match_col <- candidates[candidates %in% tolower(names(splist))]
      if (length(match_col) > 0) {
        name_col <- names(splist)[tolower(names(splist)) == match_col[1]][1]
      } else {
        name_col <- names(splist)[1]
      }
    }
    splist <- splist[[name_col]]
  }

  if (!is.character(splist)) {
    splist <- as.character(splist)
  }

  n <- length(splist)
  if (n == 0L) {
    return(tibble::tibble(
      input_index       = integer(),
      input_name        = character(),
      canonical_name    = character(),
      orig_genus        = character(),
      orig_species      = character(),
      infra_rank        = character(),
      orig_infraspecies = character(),
      author            = character(),
      rank              = numeric(),
      has_cf            = logical(),
      has_aff           = logical(),
      is_sp             = logical(),
      is_spp            = logical(),
      had_hybrid        = logical()
    ))
  }

  input_index <- seq_len(n)
  input_name  <- splist

  canonical_name    <- rep(NA_character_, n)
  orig_genus        <- rep(NA_character_, n)
  orig_species      <- rep(NA_character_, n)
  infra_rank        <- rep(NA_character_, n)
  orig_infraspecies <- rep(NA_character_, n)
  author            <- rep("", n)
  rank              <- rep(NA_real_, n)

  has_cf     <- rep(FALSE, n)
  has_aff    <- rep(FALSE, n)
  is_sp      <- rep(FALSE, n)
  is_spp     <- rep(FALSE, n)
  had_hybrid <- rep(FALSE, n)

  ranks_lookup <- c(
    "subsp." = "subsp.", "subsp" = "subsp.", "ssp." = "subsp.", "ssp" = "subsp.",
    "var." = "var.", "var" = "var.",
    "f." = "f.", "fo." = "f.", "forma" = "f."
  )

  for (i in seq_len(n)) {
    raw <- splist[i]
    if (is.na(raw) || !nzchar(trimws(raw))) {
      next
    }

    txt <- gsub("[\r\n\t]+", " ", raw)
    txt <- trimws(txt)

    # Detectar calificador de híbrido
    if (grepl("^[Xx\u00D7]\\s+", txt) || grepl("\\s+[Xx\u00D7]\\s+", txt)) {
      had_hybrid[i] <- TRUE
      txt <- gsub("^[Xx\u00D7]\\s+", "", txt)
      txt <- gsub("\\s+[Xx\u00D7]\\s+", " ", txt)
    }

    # Detectar cf. / aff.
    if (grepl("\\bcf\\.?\\b", txt, ignore.case = TRUE)) {
      has_cf[i] <- TRUE
      txt <- gsub("\\bcf\\.?\\s*", "", txt, ignore.case = TRUE)
    }
    if (grepl("\\baff\\.?\\b", txt, ignore.case = TRUE)) {
      has_aff[i] <- TRUE
      txt <- gsub("\\baff\\.?\\s*", "", txt, ignore.case = TRUE)
    }

    # Normalizar espacios
    txt <- gsub("\\s+", " ", trimws(txt))
    toks <- unlist(strsplit(txt, " "))
    if (length(toks) == 0L) next

    # Género
    g_raw <- toks[1]
    g_clean <- gsub("[^A-Za-z\u00C0-\u017F\\-]", "", g_raw)
    if (nzchar(g_clean)) {
      orig_genus[i] <- paste0(toupper(substr(g_clean, 1, 1)), tolower(substr(g_clean, 2, nchar(g_clean))))
    }

    # Evaluar si es sp. / spp.
    if (length(toks) >= 2) {
      t2_lower <- tolower(gsub("[^A-Za-z]", "", toks[2]))
      if (t2_lower == "sp") {
        is_sp[i] <- TRUE
        rank[i]  <- 1
        canonical_name[i] <- orig_genus[i]
        next
      } else if (t2_lower == "spp") {
        is_spp[i] <- TRUE
        rank[i]   <- 1
        canonical_name[i] <- orig_genus[i]
        next
      }
    } else {
      # Sólo un término: tratado como género
      rank[i] <- 1
      canonical_name[i] <- orig_genus[i]
      next
    }

    # Epíteto específico
    sp_raw <- toks[2]
    sp_clean <- tolower(gsub("[^A-Za-z\u00C0-\u017F\\-]", "", sp_raw))
    sp_clean <- chartr("\u00FA\u00FC\u00F3\u00ED\u00E9\u00E1", "uuioea", sp_clean)
    orig_species[i] <- sp_clean
    rank[i] <- 2

    # Análisis de componentes subsecuentes (rango infraespecífico o autoría)
    if (length(toks) >= 3) {
      rest_toks <- toks[3:length(toks)]
      found_infra <- FALSE

      for (k in seq_along(rest_toks)) {
        rk_candidate <- tolower(rest_toks[k])
        if (rk_candidate %in% names(ranks_lookup)) {
          if (length(rest_toks) >= k + 1) {
            infra_rank[i] <- ranks_lookup[[rk_candidate]]
            i_sp <- tolower(gsub("[^A-Za-z\u00C0-\u017F\\-]", "", rest_toks[k + 1]))
            i_sp <- chartr("\u00FA\u00FC\u00F3\u00ED\u00E9\u00E1", "uuioea", i_sp)
            orig_infraspecies[i] <- i_sp
            rank[i] <- 3
            found_infra <- TRUE

            # Autoría restante después del infraepíteto
            if (length(rest_toks) > k + 1) {
              author[i] <- paste(rest_toks[(k + 2):length(rest_toks)], collapse = " ")
            }
            break
          }
        }
      }

      if (!found_infra) {
        author[i] <- paste(rest_toks, collapse = " ")
      }
    }

    # Construir nombre canónico
    if (!is.na(orig_genus[i]) && !is.na(orig_species[i])) {
      if (!is.na(infra_rank[i]) && !is.na(orig_infraspecies[i])) {
        canonical_name[i] <- paste(orig_genus[i], orig_species[i], infra_rank[i], orig_infraspecies[i])
      } else {
        canonical_name[i] <- paste(orig_genus[i], orig_species[i])
      }
    }
  }

  tibble::tibble(
    input_index       = input_index,
    input_name        = input_name,
    canonical_name    = canonical_name,
    orig_genus        = orig_genus,
    orig_species      = orig_species,
    infra_rank        = infra_rank,
    orig_infraspecies = orig_infraspecies,
    author            = author,
    rank              = rank,
    has_cf            = has_cf,
    has_aff           = has_aff,
    is_sp             = is_sp,
    is_spp            = is_spp,
    had_hybrid        = had_hybrid
  )
}

#' @rdname cites_classify_names
#' @export
classify_spnames <- cites_classify_names
