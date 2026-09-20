# ==============================================================================
# Funciones Internas de Concordancia y Filtrado de Backbone CITES
# ==============================================================================

#' Obtener y filtrar el backbone CITES según reino y edición
#' @noRd
.get_backbone <- function(taxon = c("all", "fauna", "flora"),
                          edition = c("latest", "all", "2023", "2019", "2018")) {
  taxon <- match.arg(taxon)
  edition <- match.arg(edition)

  bb <- cites_backbone

  if (taxon != "all") {
    bb <- bb[bb$taxon == taxon, , drop = FALSE]
  }

  if (edition == "latest") {
    # Fauna 2023 + Flora 2018
    bb <- bb[(bb$taxon == "fauna" & bb$edition == "2023") |
             (bb$taxon == "flora" & bb$edition == "2018"), , drop = FALSE]
  } else if (edition != "all") {
    bb <- bb[bb$edition == edition, , drop = FALSE]
  } else {
    # En consultas históricas combinadas se prioriza la edición más reciente.
    bb <- bb[order(bb$taxon, bb$canonical_name, -as.integer(bb$edition)), , drop = FALSE]
  }

  bb
}

#' Normalizar sufijos latinos de concordancia gramatical en epítetos
#' @noRd
.latin_suffix <- function(ep) {
  suffixes <- c("us", "um", "is", "os", "on", "ae", "a", "e")
  vapply(ep, function(x) {
    if (is.na(x)) return(NA_character_)
    hit <- suffixes[endsWith(x, suffixes)]
    if (length(hit) == 0L) NA_character_ else hit[which.max(nchar(hit))]
  }, character(1))
}

.is_allowed_latin_variation <- function(input_epithet, candidate_epithet) {
  input_suffix <- .latin_suffix(input_epithet)
  candidate_suffix <- .latin_suffix(candidate_epithet)
  allowed_pairs <- c("a:us", "a:um", "um:us", "e:is", "on:os")
  pair <- paste(sort(c(input_suffix, candidate_suffix)), collapse = ":")
  !is.na(input_suffix) && !is.na(candidate_suffix) &&
    input_suffix != candidate_suffix && pair %in% allowed_pairs
}

.stem_latin_epithet <- function(ep) {
  suffix <- .latin_suffix(ep)
  ifelse(is.na(suffix), ep, substr(ep, 1L, nchar(ep) - nchar(suffix)))
}

.set_match_from_hit <- function(row, hit, match_type, matched_dist) {
  row$matched_name <- hit$canonical_name
  row$accepted_name <- hit$accepted_name
  row$match_type <- match_type
  row$is_cites <- TRUE
  row$apendice <- hit$apendice
  row$taxon <- hit$taxon
  row$familia <- hit$familia
  row$clase <- hit$clase
  row$orden <- hit$orden
  row$categoria_nacional <- hit$categoria_nacional
  row$uicn <- hit$uicn
  row$autor_cites <- hit$autor
  row$edition_used <- hit$edition
  row$source_dataset <- hit$source_dataset
  row$source_row_id <- hit$source_row_id
  row$source_title <- hit$source_title
  row$source_url <- hit$source_url
  row$matched_dist <- as.integer(matched_dist)
  row
}

.set_ambiguous_match <- function(row, candidates, matched_dist) {
  row$matched_name <- NA_character_
  row$accepted_name <- NA_character_
  row$match_type <- "ambiguous_match"
  row$is_cites <- FALSE
  row$apendice <- NA_character_
  row$taxon <- NA_character_
  row$familia <- NA_character_
  row$clase <- NA_character_
  row$orden <- NA_character_
  row$categoria_nacional <- NA_character_
  row$uicn <- NA_character_
  row$autor_cites <- NA_character_
  row$edition_used <- NA_character_
  row$source_dataset <- NA_character_
  row$source_row_id <- NA_character_
  row$source_title <- NA_character_
  row$source_url <- NA_character_
  row$candidate_names <- paste(unique(candidates$accepted_name), collapse = " | ")
  row$candidate_count <- nrow(candidates)
  row$matched_dist <- as.integer(matched_dist)
  row
}

#' Coincidencia directa exacta sobre nombres aceptados
#' @noRd
.direct_match <- function(df_unresolved, bb) {
  bb_acc <- bb[bb$taxon_status == "accepted", , drop = FALSE]
  idx <- match(tolower(df_unresolved$canonical_name), tolower(bb_acc$canonical_name))

  matched_mask <- !is.na(idx)
  if (!any(matched_mask)) return(list(matched = NULL, remaining = df_unresolved))

  matched_input <- df_unresolved[matched_mask, , drop = FALSE]
  bb_hits <- bb_acc[idx[matched_mask], , drop = FALSE]
  matched_df <- dplyr::bind_rows(lapply(seq_len(nrow(matched_input)), function(i) {
    .set_match_from_hit(matched_input[i, , drop = FALSE], bb_hits[i, , drop = FALSE], "exact", 0L)
  }))

  remaining_df <- df_unresolved[!matched_mask, , drop = FALSE]
  list(matched = matched_df, remaining = remaining_df)
}

#' Coincidencia con sinónimos oficiales y resolución al taxón aceptado
#' @noRd
.synonym_match <- function(df_unresolved, bb) {
  bb_syn <- bb[bb$taxon_status == "synonym", , drop = FALSE]
  idx <- match(tolower(df_unresolved$canonical_name), tolower(bb_syn$canonical_name))

  matched_mask <- !is.na(idx)
  if (!any(matched_mask)) return(list(matched = NULL, remaining = df_unresolved))

  matched_input <- df_unresolved[matched_mask, , drop = FALSE]
  bb_hits <- bb_syn[idx[matched_mask], , drop = FALSE]
  matched_df <- dplyr::bind_rows(lapply(seq_len(nrow(matched_input)), function(i) {
    .set_match_from_hit(matched_input[i, , drop = FALSE], bb_hits[i, , drop = FALSE], "synonym", 0L)
  }))

  remaining_df <- df_unresolved[!matched_mask, , drop = FALSE]
  list(matched = matched_df, remaining = remaining_df)
}

#' Coincidencia por variación de sufijo latino en epítetos específicos
#' @noRd
.suffix_match <- function(df_unresolved, bb) {
  bb_acc <- bb[bb$taxon_status == "accepted", , drop = FALSE]
  bb_stems <- .stem_latin_epithet(bb_acc$species)
  bb_genus <- tolower(bb_acc$genus)

  matched_rows <- list()
  remaining_idx <- integer()

  for (i in seq_len(nrow(df_unresolved))) {
    row <- df_unresolved[i, , drop = FALSE]
    if (is.na(row$orig_genus) || is.na(row$orig_species)) {
      remaining_idx <- c(remaining_idx, i)
      next
    }

    g_in <- tolower(row$orig_genus)
    sp_stem <- .stem_latin_epithet(row$orig_species)

    # Buscar en mismo género
    cand_idx <- which(bb_genus == g_in & bb_stems == sp_stem)
    cand_idx <- cand_idx[vapply(cand_idx, function(j) {
      .is_allowed_latin_variation(row$orig_species, bb_acc$species[j])
    }, logical(1))]

    if (length(cand_idx) == 1L) {
      matched_rows[[length(matched_rows) + 1L]] <- .set_match_from_hit(
        row, bb_acc[cand_idx, , drop = FALSE], "suffix", 0L
      )
    } else if (length(cand_idx) > 1L) {
      matched_rows[[length(matched_rows) + 1L]] <- .set_ambiguous_match(
        row, bb_acc[cand_idx, , drop = FALSE], 0L
      )
    } else {
      remaining_idx <- c(remaining_idx, i)
    }
  }

  matched_df <- if (length(matched_rows) > 0) dplyr::bind_rows(matched_rows) else NULL
  remaining_df <- df_unresolved[remaining_idx, , drop = FALSE]

  list(matched = matched_df, remaining = remaining_df)
}

#' Coincidencia difusa (fuzzy) acotada al género o con género difuso
#' @noRd
.fuzzy_match_within_genus <- function(df_unresolved, bb, max_dist = 1) {
  if (max_dist < 1) return(list(matched = NULL, remaining = df_unresolved))

  bb_acc <- bb[bb$taxon_status == "accepted", , drop = FALSE]
  if (nrow(bb_acc) == 0L) return(list(matched = NULL, remaining = df_unresolved))
  bb_genus <- tolower(bb_acc$genus)
  unique_bb_genus <- unique(stats::na.omit(bb_genus))

  matched_rows <- list()
  remaining_idx <- integer()

  for (i in seq_len(nrow(df_unresolved))) {
    row <- df_unresolved[i, , drop = FALSE]
    if (is.na(row$orig_genus) || is.na(row$orig_species)) {
      remaining_idx <- c(remaining_idx, i)
      next
    }

    g_in <- tolower(row$orig_genus)
    cand_idx <- which(bb_genus == g_in)
    if (length(cand_idx) > 0L) {
      distances <- as.integer(utils::adist(row$orig_species, bb_acc$species[cand_idx]))
    } else {
      if (length(unique_bb_genus) == 0L) {
        remaining_idx <- c(remaining_idx, i)
        next
      }
      genus_distances <- as.integer(utils::adist(g_in, unique_bb_genus))
      eligible_genera <- unique_bb_genus[genus_distances <= max_dist]
      cand_idx <- which(bb_genus %in% eligible_genera)
      if (length(cand_idx) == 0L) {
        remaining_idx <- c(remaining_idx, i)
        next
      }
      genus_distance_by_candidate <- genus_distances[match(bb_genus[cand_idx], unique_bb_genus)]
      distances <- genus_distance_by_candidate +
        as.integer(utils::adist(row$orig_species, bb_acc$species[cand_idx]))
    }

    min_distance <- min(distances, na.rm = TRUE)
    best_idx <- cand_idx[distances == min_distance]
    if (is.finite(min_distance) && min_distance <= max_dist && length(best_idx) == 1L) {
      matched_rows[[length(matched_rows) + 1L]] <- .set_match_from_hit(
        row, bb_acc[best_idx, , drop = FALSE], "fuzzy", min_distance
      )
    } else if (is.finite(min_distance) && min_distance <= max_dist && length(best_idx) > 1L) {
      matched_rows[[length(matched_rows) + 1L]] <- .set_ambiguous_match(
        row, bb_acc[best_idx, , drop = FALSE], min_distance
      )
    } else {
      remaining_idx <- c(remaining_idx, i)
    }
  }

  matched_df <- if (length(matched_rows) > 0) dplyr::bind_rows(matched_rows) else NULL
  remaining_df <- df_unresolved[remaining_idx, , drop = FALSE]

  list(matched = matched_df, remaining = remaining_df)
}

#' Coincidencia a nivel de género (para sp., spp. o taxón regulado a nivel de género)
#' @noRd
.genus_match <- function(df_unresolved, bb, genus_fallback = FALSE) {
  # El índice se deriva del backbone ya filtrado para respetar taxon y edition.
  gen_table <- bb |>
    dplyr::filter(.data$taxon_status == "accepted")
  matched_rows <- list()
  remaining_idx <- integer()

  for (i in seq_len(nrow(df_unresolved))) {
    row <- df_unresolved[i, , drop = FALSE]
    if (is.na(row$orig_genus)) {
      remaining_idx <- c(remaining_idx, i)
      next
    }

    # Solo hacer match a género si fue solicitado a nivel de género (sp./spp./rank 1)
    # o si genus_fallback = TRUE explícitamente
    can_match_genus <- isTRUE(row$is_sp) || isTRUE(row$is_spp) || (row$rank == 1) || isTRUE(genus_fallback)
    if (!can_match_genus) {
      remaining_idx <- c(remaining_idx, i)
      next
    }

    g_in <- tolower(row$orig_genus)
    candidates <- gen_table[tolower(gen_table$genus) == g_in, , drop = FALSE]

    if (nrow(candidates) > 0L) {
      if (dplyr::n_distinct(candidates$taxon) > 1L) {
        matched_rows[[length(matched_rows) + 1L]] <- .set_ambiguous_match(row, candidates, 0L)
        next
      }
      unique_appendices <- unique(stats::na.omit(candidates$apendice))
      row$matched_name       <- candidates$genus[1]
      row$accepted_name      <- paste0(candidates$genus[1], " spp.")
      row$match_type         <- "genus"
      row$is_cites           <- TRUE
      row$apendice           <- if (length(unique_appendices) == 1L) unique_appendices else NA_character_
      row$taxon              <- candidates$taxon[1]
      row$familia            <- if (dplyr::n_distinct(candidates$familia) == 1L) candidates$familia[1] else NA_character_
      row$clase              <- NA_character_
      row$orden              <- NA_character_
      row$categoria_nacional <- NA_character_
      row$uicn               <- NA_character_
      row$autor_cites        <- NA_character_
      row$edition_used       <- paste(unique(candidates$edition), collapse = " | ")
      row$source_dataset     <- paste(unique(candidates$source_dataset), collapse = " | ")
      row$source_row_id      <- paste(unique(candidates$source_row_id), collapse = " | ")
      row$source_title       <- paste(unique(candidates$source_title), collapse = " | ")
      row$source_url         <- paste(unique(candidates$source_url), collapse = " | ")
      row$candidate_names    <- paste(unique(candidates$accepted_name), collapse = " | ")
      row$candidate_count    <- nrow(candidates)
      row$matched_dist       <- 0L
      matched_rows[[length(matched_rows) + 1L]] <- row
    } else {
      remaining_idx <- c(remaining_idx, i)
    }
  }

  matched_df <- if (length(matched_rows) > 0) dplyr::bind_rows(matched_rows) else NULL
  remaining_df <- df_unresolved[remaining_idx, , drop = FALSE]

  list(matched = matched_df, remaining = remaining_df)
}
