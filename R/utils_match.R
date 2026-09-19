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
  }

  bb
}

#' Normalizar sufijos latinos de concordancia gramatical en epítetos
#' @noRd
.stem_latin_epithet <- function(ep) {
  ifelse(
    is.na(ep) | nchar(ep) < 3,
    ep,
    sub("(us|um|is|os|on|ae|a|e)$", "", ep)
  )
}

#' Coincidencia directa exacta sobre nombres aceptados
#' @noRd
.direct_match <- function(df_unresolved, bb) {
  bb_acc <- bb[bb$taxon_status == "accepted", , drop = FALSE]
  idx <- match(tolower(df_unresolved$canonical_name), tolower(bb_acc$canonical_name))

  matched_mask <- !is.na(idx)
  if (!any(matched_mask)) return(list(matched = NULL, remaining = df_unresolved))

  matched_df <- df_unresolved[matched_mask, , drop = FALSE]
  bb_hits    <- bb_acc[idx[matched_mask], , drop = FALSE]

  matched_df$matched_name       <- bb_hits$canonical_name
  matched_df$accepted_name      <- bb_hits$accepted_name
  matched_df$match_type         <- "exact"
  matched_df$is_cites           <- TRUE
  matched_df$apendice           <- bb_hits$apendice
  matched_df$taxon              <- bb_hits$taxon
  matched_df$familia            <- bb_hits$familia
  matched_df$clase              <- bb_hits$clase
  matched_df$orden              <- bb_hits$orden
  matched_df$categoria_nacional <- bb_hits$categoria_nacional
  matched_df$uicn               <- bb_hits$uicn
  matched_df$autor_cites        <- bb_hits$autor
  matched_df$matched_dist       <- 0L

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

  matched_df <- df_unresolved[matched_mask, , drop = FALSE]
  bb_hits    <- bb_syn[idx[matched_mask], , drop = FALSE]

  matched_df$matched_name       <- bb_hits$canonical_name
  matched_df$accepted_name      <- bb_hits$accepted_name
  matched_df$match_type         <- "synonym"
  matched_df$is_cites           <- TRUE
  matched_df$apendice           <- bb_hits$apendice
  matched_df$taxon              <- bb_hits$taxon
  matched_df$familia            <- bb_hits$familia
  matched_df$clase              <- bb_hits$clase
  matched_df$orden              <- bb_hits$orden
  matched_df$categoria_nacional <- bb_hits$categoria_nacional
  matched_df$uicn               <- bb_hits$uicn
  matched_df$autor_cites        <- bb_hits$autor
  matched_df$matched_dist       <- 0L

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

    if (length(cand_idx) > 0) {
      hit <- bb_acc[cand_idx[1], , drop = FALSE]
      row$matched_name       <- hit$canonical_name
      row$accepted_name      <- hit$accepted_name
      row$match_type         <- "suffix"
      row$is_cites           <- TRUE
      row$apendice           <- hit$apendice
      row$taxon              <- hit$taxon
      row$familia            <- hit$familia
      row$clase              <- hit$clase
      row$orden              <- hit$orden
      row$categoria_nacional <- hit$categoria_nacional
      row$uicn               <- hit$uicn
      row$autor_cites        <- hit$autor
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

#' Coincidencia difusa (fuzzy) acotada al género o con género difuso
#' @noRd
.fuzzy_match_within_genus <- function(df_unresolved, bb, max_dist = 1) {
  if (max_dist < 1) return(list(matched = NULL, remaining = df_unresolved))

  bb_acc <- bb[bb$taxon_status == "accepted", , drop = FALSE]
  bb_genus <- tolower(bb_acc$genus)
  unique_bb_genus <- unique(bb_genus)

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
    dist_total <- NA_integer_
    hit_idx <- NA_integer_

    if (length(cand_idx) > 0) {
      # Caso 1: Género coincide exactamente, fuzzy en epíteto específico
      cand_species <- bb_acc$species[cand_idx]
      dists_sp <- as.numeric(utils::adist(row$orig_species, cand_species))
      min_d_sp <- min(dists_sp)

      if (min_d_sp <= max_dist) {
        hit_idx <- cand_idx[which.min(dists_sp)]
        dist_total <- as.integer(min_d_sp)
      }
    } else {
      # Caso 2: Error tipográfico en el género
      d_gen <- as.numeric(utils::adist(g_in, unique_bb_genus))
      min_d_gen <- min(d_gen)

      if (min_d_gen <= max_dist) {
        cand_g_name <- unique_bb_genus[which.min(d_gen)]
        cand_idx_g  <- which(bb_genus == cand_g_name)
        cand_species <- bb_acc$species[cand_idx_g]
        dists_sp <- as.numeric(utils::adist(row$orig_species, cand_species))
        min_d_sp <- min(dists_sp)

        # Si la especie coincide exacta o con distancia complementaria
        if (min_d_gen + min_d_sp <= max_dist) {
          hit_idx <- cand_idx_g[which.min(dists_sp)]
          dist_total <- as.integer(min_d_gen + min_d_sp)
        }
      }
    }

    if (!is.na(hit_idx)) {
      hit <- bb_acc[hit_idx, , drop = FALSE]
      row$matched_name       <- hit$canonical_name
      row$accepted_name      <- hit$accepted_name
      row$match_type         <- "fuzzy"
      row$is_cites           <- TRUE
      row$apendice           <- hit$apendice
      row$taxon              <- hit$taxon
      row$familia            <- hit$familia
      row$clase              <- hit$clase
      row$orden              <- hit$orden
      row$categoria_nacional <- hit$categoria_nacional
      row$uicn               <- hit$uicn
      row$autor_cites        <- hit$autor
      row$matched_dist       <- dist_total
      matched_rows[[length(matched_rows) + 1L]] <- row
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
  gen_table <- cites_genera
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
    idx <- match(g_in, tolower(gen_table$genus))

    if (!is.na(idx)) {
      hit <- gen_table[idx, , drop = FALSE]
      row$matched_name       <- hit$genus
      row$accepted_name      <- paste0(hit$genus, " spp.")
      row$match_type         <- "genus"
      row$is_cites           <- TRUE
      row$apendice           <- hit$apendice
      row$taxon              <- hit$taxon
      row$familia            <- hit$familia
      row$clase              <- NA_character_
      row$orden              <- NA_character_
      row$categoria_nacional <- NA_character_
      row$uicn               <- NA_character_
      row$autor_cites        <- NA_character_
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
