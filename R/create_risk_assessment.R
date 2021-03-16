#' FHIR RiskAssessment
#'
#' Creats a FHIR RiskAssessment Resource
#'
#' @param assessment_date date.time, when was the assessment made
#' @param patient_id The patient id
#' @param risk Predicted Risk for Major Ampuation
#'
#' @return A FHIR RiskAssessment Resource (json)
#' @export
#'
#' @importFrom lubridate dmonths
#' @import jsonlite
#'
#' @examples
#' assessment_date <- Sys.time()
#' patient_id <- 123
#' risk <- 1 / 10
#' create_risk_assessment(assessment_date, patient_id, risk)
create_risk_assessment <- function(assessment_date, patient_id, risk) {

  # Time, when the risk is calculated
  time <- format(Sys.time(), "%Y-%m-%d")
  assessment_date <- as.Date(assessment_date)

  # Period, for which the risk applies
  date_start <- format(assessment_date, "%Y-%m-%d")
  date_end <- format(assessment_date + lubridate::dmonths(6), "%Y-%m-%d")

  resource <- generate_risk_assessment()
  id <- paste0(sample(c(letters, LETTERS, 0:9), size = 40), collapse = "")

  resource$id <- id
  resource$subject$reference <- paste0("Patient/", patient_id)

  resource$occurrenceDateTime <- time
  resource$prediction[[1]]$whenPeriod$start <- date_start
  resource$prediction[[1]]$whenPeriod$end <- date_end

  resource$prediction[[1]]$probabilityDecimal <- risk

  # risk_assessment <- jsonlite::toJSON(resource, auto_unbox = TRUE)

  return(resource)
}


#' Returns JSON Template
#' Template for the FHIR RiskAssessment Resource
#' Risk values has to be entered
#'
#' @export
#'
#' @return A parsed JSON
generate_risk_assessment <- function() {

  path <- system.file("fhir-templates", "RiskAssessment_Template.json", package= "bayes.pedis")
  resource <- jsonlite::fromJSON(txt = path, simplifyDataFrame = FALSE, simplifyVector = FALSE)
  return(resource)

}
