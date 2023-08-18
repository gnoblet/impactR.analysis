# ## code to prepare `DATASET` dataset goes here
#
#
# #------ Prepare data

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



# ## code to prepare `DATASET` dataset goes here
#
#
# #------ Prepare data
msni <- data.frame(
  uuid = paste0("x_", 1:10000),
  health_crit_1 = sample(c(1:3, NA_integer_), 10000, replace = TRUE),
  health_noncrit_1 = sample(c(0:1, NA_integer_), 10000, replace = TRUE),
  health_noncrit_2 = sample(0:1, 10000, replace = TRUE),
  health_noncrit_3 = sample(0:1, 10000, replace = TRUE),
  fs_crit_1 = sample(c(1:5, NA_integer_), 10000, replace = TRUE),
  liv_crit_1 = sample(1:4, 10000, replace = TRUE),
  liv_crit_2 = sample(1:4, 10000, replace = TRUE),
  liv_crit_3 = sample(1:3, 10000, replace = TRUE),
  liv_crit_4 = sample(c(1:4, NA_integer_), 10000, replace = TRUE),
  liv_noncrit_1 = sample(c(0:1, NA_integer_), 10000, replace = TRUE),
  wash_crit_1 = sample(1:5, 10000, replace = TRUE),
  wash_crit_2 = sample(c(1:5, NA_integer_), 10000, replace = TRUE),
  wash_crit_3 = sample(1:5, 10000, replace = TRUE),
  wash_crit_4 = sample(c(1:2, NA_integer_), 10000, replace = TRUE),
  educ_crit_1 = sample(c(1,3,4, NA_integer_), 10000, replace = TRUE),
  educ_crit_2 = sample(c(1,2,4), 10000, replace = TRUE),
  educ_noncrit_1 = sample(c(0:1, NA_integer_), 10000, replace = TRUE),
  educ_noncrit_2 = sample(0:1, 10000, replace = TRUE),
  educ_noncrit_3 = sample(0:1, 10000, replace = TRUE),
  shelter_crit_1 = sample(c(1,2,3, 5, NA_integer_), 10000, replace = TRUE),
  prot_crit_1 = sample(c(1:4, NA_integer_), 10000, replace = TRUE),
  prot_crit_2 = sample(c(1, 3, 4), 10000, replace = TRUE)
)

usethis::use_data(msni, overwrite = TRUE)
