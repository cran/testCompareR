#' US Cystic Fibrosis Patient Registry data
#'
#' This data from the Cystic Fibrosis Foundation's Patient Registry (USA)
#' evaluates risk factors for pulmonary exacerbation in patients with cystic
#' fibrosis. The two risk factors evaluated are previous pulmonary exacerbation
#' and previous colonisation with Pseudomonas aeruginosa.
#' Each of the two risk factors was evaluated using data from 1995. If an
#' instance occurred at any point in 1995 the 'test' was considered positive.
#' If negative throughout 1995 the 'test' was considered negative. The gold
#' standard was evidence of pulmonary exacerbation at any point in 1996.
#'
#' All three variables are dichotomous. 1 indicates presence; 0 indicates
#' absence.
#'
#' This data was originally presented in Moskowitz and Pepe (2006).
#'
#' @format
#' A data frame with 11,960 rows and 3 columns:
#' \describe{
#'   \item{pulm.exac}{Presence or absence of previous pulmonary exacerbation.}
#'   \item{pseudomonas}{Presence or absence of Pseudomonas aeruginosa
#'   infection.}
#'   \item{infection}{Presence or absence of severe infection (gold standard).}
#' }
#'
#' @keywords datasets
#'
#' @references Moskowitz and Pepe (2006) Clinical Trials. 2006;3(3):272-9.
#' \doi{10.1191/1740774506cn147oa}
#'
#' @source
#' Data was sourced directly from the referenced paper. For up-to-date data
#' requests contact:
#' \href{https://www.cff.org/researchers/patient-registry-data-requests}{Cystic Fibrosis Foundation}
"cfpr"

#' Coronary Artery Surgery Study data
#'
#' This data from the Coronary Artery Surgery Study evaluates two tests to
#' determine the presence or absence of coronary artery disease by comparing to
#' coronary angiography, the gold standard. Test 1 is an exercise stress test
#' and Test 2 is a clinical history of chest pain.
#'
#' All three variables are dichotomous. 1 indicates a positive result;
#' 0 indicates a negative result.
#'
#' This data was originally presented in Weiner et al. (1979).
#'
#' @format
#' A data frame with 871 rows and 3 columns:
#' \describe{
#'   \item{exercise}{Dichotomous result on exercise stress testing.}
#'   \item{cp}{Presence of absence of chest pain based on medical history.}
#'   \item{angio}{Dichotomous result on coronary angiography.}
#' }
#'
#' @keywords datasets
#'
#' @references Weiner et al. (1979)) N Engl J Med. 1979;301(5):230-5
#' \doi{10.1056/NEJM197908023010502}
#'
#' @source
#' \doi{10.1056/NEJM197908023010502}
"cass"
