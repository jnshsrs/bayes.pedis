## code to prepare `FhirRiskAssessment` dataset goes here

FhirRiskAssessment <- jsonlite::fromJSON(txt = "fhir-resource-templates/risk-assessment.json", simplifyDataFrame = FALSE, simplifyVector = FALSE)

usethis::use_data(FhirRiskAssessment, overwrite = TRUE)

