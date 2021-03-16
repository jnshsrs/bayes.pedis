
## code to prepare `param_dist` "params for model pedis-any-posterior" dataset goes here
files <- dir("data-raw/")[-5]
params <- purrr::map(paste0("data-raw/", files), read.csv)

names(params) <- stringr::str_extract(string = files, pattern = "[a-z]+\\-[a-z]+")
param_dist <- as.matrix(params[["noninformed-any"]])

usethis::use_data(param_dist, overwrite = TRUE)
