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
hti_msna_2022 <- readRDS("data-raw/REACH_HTI_dataset_MSNA-2022.RDS")

main <- hti_msna_2022$main

# # Select vars
main <- dplyr::select(main, dplyr::all_of(vars))

# To ensure appropriate types, for instance weights is considered "character"
main <- dplyr::mutate(main, weights = as.double(weights))

# Sheet survey contains the survey sheet
survey <- hti_msna_2022$survey

# Split type column
survey <- impactR::split_survey(survey, type)

# Rename one language label column to label
survey <- dplyr::rename(survey, label = label_francais)

# Keep only the design variables for the sake of simplicity
# Maybe to be changed later for checks
survey <- dplyr::filter(survey, name %in% vars)

# Sheet choices contains, well, the choices sheet
choices <- hti_msna_2022$choices

# Rename one language label column to label
choices <- dplyr::rename(choices, label = label_francais)

# Subset
choices <- dplyr::filter(choices, list_name %in% survey$list_name)


# Now that the dataset and the Kobo tool are loaded, we can prepare the survey design:
design <-  srvyr::as_survey_design(
    main,
    strata = stratum,
    weights = weights,
    ids = i_cluster
  )

# Save data
usethis::use_data(main, overwrite = TRUE)
usethis::use_data(survey, overwrite = TRUE)
usethis::use_data(choices, overwrite = TRUE)
usethis::use_data(design, overwrite = TRUE)
