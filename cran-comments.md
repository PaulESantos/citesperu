## Resubmission

This is a resubmission of citesperu 0.1.0 addressing comments from Uwe Ligges:

* Regarding DOIs in citations:
  - The datasets and species checklists included in this package are official government publications and open data released directly by the Ministry of the Environment of Peru (MINAM; https://www.gob.pe/minam) as official ministerial reports and compendia (Colección MINAM N.° 609). They do not possess DOIs. Direct official URLs to the Peruvian government portal (Gob.pe) are provided in the documentation of each dataset.

* Invalid file URIs in README.md:
  - Fixed. All internal relative file links ('docs/CONTEXTO_CITES_PERU.md', 'vignettes/flujo-matching-cites.html', and 'LICENSE.md') in README.md have been replaced with canonical, absolute URLs.

* Note on MINAM in DESCRIPTION:
  - MINAM is the official Spanish acronym for 'Ministerio del Ambiente' (Ministry of the Environment of Peru), the national CITES scientific authority in Peru.

## Test environments
* local Windows 11 x64, R 4.6.1 (ucrt)
* win-builder (R-release, R-devel)
* Debian GNU/Linux (R-devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* Possibly misspelled words in DESCRIPTION:
  - MINAM (9:72): Official acronym for 'Ministerio del Ambiente' (Ministry of the Environment of Peru).

## Downstream dependencies
There are currently no downstream dependencies for this package.



