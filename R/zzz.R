# Startup hook and attach messaging in tidyverse style

.onAttach <- function(libname, pkgname) {
  if (is_loading_for_tests()) {
    return(invisible())
  }

  if (isTRUE(getOption("citesperu.quiet"))) {
    return(invisible())
  }

  msg <- citesperu_attach_message()
  inform_startup(msg)

  conflicts <- citesperu_conflicts()
  if (length(conflicts) > 0) {
    inform_startup(citesperu_conflict_message(conflicts))
  }

  invisible()
}

inform_startup <- function(msg, ...) {
  if (is.null(msg) || !nzchar(msg)) {
    return(invisible())
  }
  packageStartupMessage(msg, ...)
}

is_loading_for_tests <- function() {
  !interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "citesperu")
}

package_version_h <- function(pkg) {
  highlight_version(utils::packageVersion(pkg))
}

highlight_version <- function(x) {
  x <- as.character(x)

  is_dev <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    !is.na(x) & x >= 9000
  }

  pieces <- strsplit(x, ".", fixed = TRUE)
  pieces <- lapply(pieces, function(x) ifelse(is_dev(x), cli::col_red(x), x))
  vapply(pieces, paste, collapse = ".", FUN.VALUE = character(1))
}

citesperu_attach_message <- function() {
  header <- cli::rule(
    left = cli::style_bold("citesperu"),
    right = paste0("v", package_version_h("citesperu"))
  )

  items <- c(
    "cites_flora_peru_2018"    = "2506 taxa",
    "cites_fauna_peru_2018"    = "496 spp.",
    "cites_fauna_peru_2019"    = "523 spp.",
    "cites_fauna_peru_2023"    = "568 spp.",
    "cites_match()"            = "matching engine"
  )

  names_col <- names(items)
  vals_col <- unname(items)

  bullets <- paste0(
    cli::col_green(cli::symbol$tick), " ",
    cli::col_blue(format(names_col)), " ",
    cli::col_grey(cli::ansi_align(vals_col, max(cli::ansi_nchar(vals_col))))
  )

  if (length(bullets) %% 2 == 1) {
    bullets <- append(bullets, "")
  }

  col1 <- seq_len(length(bullets) / 2)
  grid <- paste0(bullets[col1], "     ", bullets[-col1])

  footer_auth <- paste0(
    cli::col_cyan(cli::symbol$info), " ",
    cli::style_italic("Listado de Especies de Flora y Fauna Silvestre CITES - Per\u00fa.\n",
                      "Autoridad Cient\u00edfica: MINAM / Direcci\u00f3n General de Diversidad Biol\u00f3gica")
  )

  #footer_hint <- paste0(
  #  cli::col_cyan(cli::symbol$info), " ",
  #  "Usa ", cli::col_yellow("cites_match()"), " para concordancia o revisa la ",
  #  cli::format_inline("{.href [documentaci\u00f3n](https://paulesantos.github.io/citesperu/)}")
  #)

  paste0(
    header, "\n",
    paste(grid, collapse = "\n"), "\n",
    footer_auth#, "\n",
    #footer_hint
  )
}

citesperu_conflicts <- function() {
  my_funs <- c(
    "cites_classify_names",
    "cites_classify_spnames",
    "cites_match",
    "cites_matching",
    "is_cites",
    "is_cites_pe",
    "match_cites_pe"
  )

  envs <- grep("^package:", search(), value = TRUE)
  envs <- setdiff(envs, c("package:citesperu", "package:base", "package:methods", "Autoloads"))

  conflicts <- list()
  for (f in my_funs) {
    masked <- character()
    for (env in envs) {
      if (exists(f, where = env, inherits = FALSE)) {
        pkg <- sub("^package:", "", env)
        masked <- c(masked, pkg)
      }
    }
    if (length(masked) > 0) {
      conflicts[[f]] <- masked
    }
  }

  conflicts
}

citesperu_conflict_message <- function(conflicts) {
  if (length(conflicts) == 0) {
    return(NULL)
  }

  header <- cli::rule(
    left = cli::style_bold("citesperu conflicts"),
    right = "conflicts"
  )

  lines <- character()
  for (fn in names(conflicts)) {
    others <- conflicts[[fn]]
    other_calls <- paste0(cli::col_blue(others), "::", fn, "()", collapse = ", ")
    winner_call <- paste0(cli::col_blue("citesperu"), "::", cli::col_green(paste0(fn, "()")))
    lines <- c(
      lines,
      paste0(cli::col_red(cli::symbol$cross), " ", winner_call, " masks ", other_calls)
    )
  }

  paste0(header, "\n", paste(lines, collapse = "\n"))
}
