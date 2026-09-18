#' citesperu: Peruvian Species Checklists from CITES Publications
#'
#' @description
#' `citesperu` provides structured, tidy datasets and query tools for Peruvian
#' wildlife species listed in the Appendices of the Convention on International
#' Trade in Endangered Species of Wild Fauna and Flora (CITES), as officially
#' compiled and published by Peru's Ministry of the Environment (MINAM).
#'
#' @details
#' In Peru, CITES implementation is coordinated across competent national authorities:
#' \itemize{
#'   \item \strong{Scientific Authority:} Ministry of the Environment (MINAM),
#'   through the Directorate General of Biological Diversity (DGDB), responsible
#'   for Non-Detriment Findings (NDF / DENP), scientific advisory, and publishing
#'   the official national checklists.
#'   \item \strong{Administrative Authorities:} SERFOR (National Forest and Wildlife
#'   Service) for terrestrial wildlife and flora; PRODUCE / SANIPES for aquatic and
#'   marine species. They issue CITES permits and certificates.
#'   \item \strong{Enforcement Entities:} SUNAT (Customs), Peruvian National Police
#'   (DIRMEAMB), DICAPI (Coast Guard), and Environmental Prosecutors (FEMA).
#' }
#'
#' @section Baseline Datasets:
#' \itemize{
#'   \item \code{\link{cites_fauna_peru_2018}}: Official checklist of Peruvian fauna
#'   listed in CITES Appendices I, II, and III (2018 edition). Contains 496 official
#'   species (48 in Appendix I, 448 in Appendix II) across 8 taxonomic classes, plus
#'   16 Appendix III species recorded in Peru (not included in official sums as Peru
#'   has not requested Appendix III listings). Includes threat categories under
#'   Supreme Decree No. 004-2014-MINAGRI and IUCN Red List ratings.
#'   \item \code{\link{cites_flora_peru_2018}}: Official checklist of Peruvian flora
#'   listed in CITES (2018 edition). Contains 2506 taxa across 9 botanical families
#'   (dominated by Orchidaceae with 2215 taxa, and Cactaceae with 186 taxa based on
#'   the CITES Cactaceae Checklist by Hunt, 2016). Includes departmental distributions
#'   coded under the Lamas & Encarnación (1976) two-letter standard, herbarium references
#'   (USM, MOL, MO, US, NY, F), and trade annotations (#).
#'   \item \code{\link{codigos_departamentos_pe}}: Reference lookup table of the 24
#'   departmental abbreviations defined by Lamas & Encarnación (1976).
#' }
#'
#' @seealso
#' \itemize{
#'   \item Official MINAM Compendium: \url{https://www.gob.pe/institucion/minam/colecciones/609-listados-de-especies-de-fauna-y-flora-cites-peru}
#'   \item CITES Official Appendices: \url{https://cites.org/eng/app/appendices.php}
#'   \item Contact for inquiries or contributions: \email{cites@@minam.gob.pe}
#' }
#'
#' @keywords internal
"_PACKAGE"
