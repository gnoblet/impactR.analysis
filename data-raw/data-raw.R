# ## code to prepare `DATASET` dataset goes here
#
#
# #------ Prepare data

# Data must have been imported with `ìmport_xlsx()` or `import_csv()` or some other way. Yet column names for multiple choices must follow this pattern "variable_choice1", with an underscore between the variable name from the survey sheet and the choices from the choices sheet. For instance, for the main drinking water source (if multiple choice), it could be "w_water_source_surface_water" or "w_water_source_stand_pipe".
# Data must have been imported with `ìmport_xlsx()` or `import_csv()` or with `janitor::clean_names()`. This is to ensure that column names for multiple choices follow this pattern: "variable_choice1", with an underscore between the variable name from the survey sheet and the choices from the choices sheet. For instance, for the main drinking water source (if multiple choice), it could be "w_water_source_surface_water" or "w_water_source_stand_pipe".

vars <- c(
  # Unique id survey
  "uuid",
  # Stratum
  "stratum",
  # 1st stratum variable
  "admin1",
  # 2nd stratum variable
  "milieu",
  # Cluster id
  "i_cluster",
  # Weights for design
  "weights",
  # For further disagg
  "c_chef_menage_genre",
  # For ratio
  "c_total_3_17_femmes",
  "e_abandont_3a_4a_fille",
  "e_abandont_5a_11a_fille",
  "e_abandont_12a_17a_fille",
  # For median
  "f_5_depenses_ba",
  # For select one
  "h_2_type_latrine",
  # For select multiple and interaction
  "e_typ_ecole",
  "e_typ_ecole_publique",
  "e_typ_ecole_non_publique",
  "e_typ_ecole_nsp",
  "e_typ_ecole_pnpr"
)

# Sheet main contains the household-level data
main <- rio::import("https://www.impact-repository.org/document/reach/e2e66cb1/REACH_HTI_dataset_MSNA-2022.xlsx",
  sheet = "main",
  # To ensure that no NA are produced on columns mainly composed of NAs
  guess_max = 21474836
)

# # Select vars
# main <- dplyr::select(main, dplyr::all_of(vars))
#
# # To ensure appropriate types, for instance weights is considered "character"
main <- dplyr::mutate(main, weights = as.double(weights))
#
# # As tibble
# main <- tibble::as_tibble(main)
#
# # Sheet survey contains the survey sheet
# survey <- rio::import("https://www.impact-repository.org/document/reach/e2e66cb1/REACH_HTI_dataset_MSNA-2022.xlsx",
#   sheet = "survey",
#   # To ensure that no NA are produced on columns mainly composed of NAs
#   guess_max = 21474836
# )
#
# # Get clean names
# survey <- janitor::clean_names(survey)
#
# # Split survey type column into type and list_name
# survey <- impactR::split_survey(survey, type)
#
# # Filter survey question that are in "main"
# survey <- dplyr::filter(survey, name %in% vars)
#
# # Rename one language label column to labelsurvey <- dplyr::rename(survey, label = label_francais)
#
#
# # Sheet choices contains, well, the choices sheet
# choices <- rio::import("https://www.impact-repository.org/document/reach/e2e66cb1/REACH_HTI_dataset_MSNA-2022.xlsx",
#   sheet = "choices",
#   # To ensure that no NA are produced on columns mainly composed of NAs
#   guess_max = 21474836
# )
#
# # Get clean names
# choices <- janitor::clean_names(choices)
#
# # Rename one language label column to label
# choices <- dplyr::rename(choices, label = label_francais)
#
# # Subset
# choices <- dplyr::filter(choices, list_name %in% survey$list_name)
#

# Now that the dataset and the Kobo tool are loaded, we can prepare the survey design:
design <-  srvyr::as_survey_design(
    main,
    strata = stratum,
    weights = weights,
    ids = i_cluster
  )

# # Save data
# usethis::use_data(main, overwrite = TRUE)
# usethis::use_data(survey, overwrite = TRUE)
# usethis::use_data(choices, overwrite = TRUE)
# usethis::use_data(design, overwrite = TRUE)
