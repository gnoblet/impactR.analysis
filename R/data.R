#' Some MSNA data produced
#'
#' The dataset comes from a Kobo tool and pre-processed (weights for instance)
#'
#' @format A data frame with 8 rows and 36 variables:
#' \describe{
#'   \item{uuid}{The survey id.}
#'   \item{stratum}{The stratum id.}
#'   \item{admin1}{The admin 1 id.}
#'   \item{milieu}{The setting id (rural vs. urban).}
#'   \item{i_cluster}{Cluster id.}
#'   \item{weights}{Weights.}
#'   \item{c_chef_menage_genre}{Head of household's gender.}
#'   \item{c_total_3_17_femmes}{Number of 3 to 17 yo female in the household.}
#'   \item{e_abandont_3a_4a_fille}{Nomber of 3 to 4 yo female that abandoned school.}
#'   \item{e_abandont_5a_11a_fille}{Nomber of 5 to 11 yo female that abandoned school.}
#'   \item{e_abandont_12a_17a_fille}{Nomber of 12 to 17 yo female that abandoned school.}
#'   \item{f_5_depenses_ba}{Food items expenses in the past 30 days.}
#'   \item{h_2_type_latrine}{Usual sanitation facility.}
#'   \item{e_typ_ecole}{School type.}
#'   \item{e_typ_ecole_publique}{School type dummy: public.}
#'   \item{e_typ_ecole_non_publique}{School type dummy: private.}
#'   \item{e_typ_ecole_nsp}{School type dummy: do not know.}
#'   \item{e_type_ecole_pnpr}{School type dummy: prefer not to answer}
#'   ...
#' }
"main"

#' Some survey sheet from a Kobo tool
#'
#' A dataset (in French) containing a basic survey sheet with consensus, admin, enumerator info, chief of hh, and sanitary
#'
#' @format A data frame with 43 rows and 12 variables:
#' \describe{
#'   \item{type}{Type of the question.}
#'   \item{list_name}{Choices list name.}
#'   \item{name}{Variable name.}
#'   \item{label}{Question label in French.}
#'   \item{label_creol}{Question label in Haitian Creole.}
#'   \item{hint_francais}{Hint in French.}
#'   \item{hint_creol}{Hint in Haitian Creole.}
#'   \item{required}{Boolean. Question required?}
#'   \item{relevant}{Skip logic.}
#'   \item{}{}
#'   \item{}{}

#'   \item{}{}

#'   \item{}{}
#'   ...
#' }
"survey"


#' Some choices sheet from a Kobo tool
#'
#' A dataset (in French) containing a basic survey sheet with consensus, admin, enumerator info, chief of hh, and sanitary
#'
#' @format A data frame with 55 rows and 7 variables:
#' \describe{
#'   \item{list_name}{The list name}
#'   \item{name}{Tne choices's names}
#'   ...
#' }
"choices"


#' Some table of checks
#'
#' A dataset (in French) containing a list of checks to pass to `make_log()`
#'
#' @format A data frame with a few rows and variables:
#' \describe{
#'   \item{id_check}{The check id}
#'   \item{question_name}{Tne Kobo survey question name}
#'   ...
#' }
"check_list"


#' Some random dap test
#'
#' A dataset (in French) containing a basic dap, just to test. Very experimental
#'
#' @format A data frame with 5 rows and 13 variables:
#' \describe{
#'   \item{id_analysis}{The analysis id. Each line is a different one.}
#'   \item{rq}{The topic/sector of the indicator.}
#'   \item{sub_rq}{The sub-topic/sub-sector of the indicator.}
#'   \item{indicator}{The label of the indicator, e.g. "\% of HHs by type of shelter".}
#'   \item{recall}{The recall period (not to be included in the indicator), e.g. "30 days prior to data collection".}
#'   \item{question_name}{The name of the variable in the data or, for a ratio, the two names separated by a comma (no space).}
#'   \item{subset}{If calculated on a subset, name it, e.g. "among households that received a humanitarian aid".}
#'   \item{analysis_name}{The analysis type label, e.g. "Percentage" or "Mean".}
#'   \item{analysis}{The R type of analysis to pass to `make_analysis()`.}
#'   \item{none_label}{The label for all NA values if "prop_simple_overall" or "prop_multiple_overall", e.g. "None or missing data".}
#'   \item{group_name}{The label of the grouping variable/column, e.g. "Household displacement status" with codes that may be labelled "IDP" or "Non IDP".}
#'   \item{group}{The variable/column name to group by, e.g. "hh_status" that may be coded "idp" or non_idp".}
#'   \item{level}{Level of confidence. Parameter to pass to `svy_*()` functions.}
#'   \item{na_rm}{Should NAs be removed? Parameter to pass to `svy_*()` functions.}
#'   \item{vartype}{Variance type in outputs. Parameter to pass to `svy_*()` functions.}
#' }
"dap"



#' Some random cleaning log
#'
#' A dataset (in French) containing a cleaning_log. Columns are necessary and comes from the `make_log()` function
#'
#' @format A data frame with 17 rows and 17 variables:
#' \describe{
#'   \item{id_check}{The check id}
#'   \item{name}{Tne choices's names}
#'   ...
#' }
"cleaning_log"
