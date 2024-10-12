get_key_vars <- function(data_tb,
                         activity_1L_chr = "Activity",
                         athlete_roles_chr = c("Athlete", "AlumniAthlete"),
                         appointments_var_1L_chr = "Appointments",
                         cancellations_var_1L_chr = "Cancellations",
                         clinical_team_1L_chr = "Clinical Team",
                         clinician_1L_chr = "Clinician",
                         clinician_discipline_1L_chr = "Service",
                         components_chr = c("Year","Quarter", "Week"),
                         cost_var_1L_chr = "Cost",
                         days_1L_chr = "Weekday",
                         duration_1L_chr = "Duration",
                         exclude_chr = "Group",#character(0),
                         group_1L_chr = character(0),
                         index_1L_chr = "Date", # rename as index
                         referrals_var_1L_chr = "Referrals",
                         referrers_1L_chr = "Referrer Role",
                         severity_1L_chr = "Severity",
                         team_disciplines_1L_chr = "Disciplines",
                         uid_var_1L_chr = "UID"){

  data_tb <- transform_to_prep(data_tb,
                               activity_1L_chr = activity_1L_chr,
                               athlete_roles_chr = athlete_roles_chr,
                               appointments_var_1L_chr = appointments_var_1L_chr,
                               cancellations_var_1L_chr = cancellations_var_1L_chr,
                               clinical_team_1L_chr = clinical_team_1L_chr,
                               clinician_1L_chr = clinician_1L_chr,
                               clinician_discipline_1L_chr =clinician_discipline_1L_chr,
                               components_chr = components_chr,
                               cost_var_1L_chr = cost_var_1L_chr,
                               days_1L_chr = days_1L_chr,
                               duration_1L_chr = duration_1L_chr,
                               exclude_chr = exclude_chr,#character(0),
                               group_1L_chr = group_1L_chr,
                               index_1L_chr = index_1L_chr,
                               referrals_var_1L_chr = referrals_var_1L_chr,
                               referrers_1L_chr = referrers_1L_chr,
                               severity_1L_chr = severity_1L_chr,
                               team_disciplines_1L_chr = team_disciplines_1L_chr,
                               uid_var_1L_chr = uid_var_1L_chr,
                               what_1L_chr = "prep")
  metrics_chr <- serious::make_metric_vars(appointments_var_1L_chr = appointments_var_1L_chr,
                                  cancellations_var_1L_chr = cancellations_var_1L_chr,
                                  cost_var_1L_chr = cost_var_1L_chr,
                                  referrals_var_1L_chr = referrals_var_1L_chr)
  key_vars_chr <- names(data_tb) %>% setdiff(c(index_1L_chr, metrics_chr))
  return(key_vars_chr)
}
get_raw_data <- function(path_1L_chr,
                         referrals_cols_int = 4L:8L,
                         sheets_ls = NULL,
                         sheets_int = 1L,
                         tabs_chr = character(0)){
  if(identical(tabs_chr, character(0))){
    if(!is.null(sheets_ls)){
      tabs_chr <- names(sheets_ls)
      if(!"sports_tb" %in% names(sheets_ls)){
        sheets_int <- setdiff(sheets_int,2)
        tabs_chr <- append(tabs_chr,"sports_tb", after = 1)
      }
    }else{
      tabs_chr <- c("appointments", "cancellations", "referrals", "retainer",  "notes")
    }
  }
  datasets_ls <- purrr::map(sheets_int,
                            ~readxl::read_xlsx(path_1L_chr,
                                               sheet = .x)) %>% stats::setNames(tabs_chr[sheets_int])
  datasets_ls <- datasets_ls %>% purrr::map2(names(datasets_ls),
                                             ~ {
                                               ds_tb <- .x
                                               if(!is.null(sheets_ls)){
                                                 indices_int <- sheets_ls %>% purrr::pluck(.y)
                                                 ds_tb <- ds_tb %>% dplyr::slice(indices_int[1]:indices_int[2])
                                               }
                                               if(.y == "referrals"){
                                                 ds_tb <- ds_tb[,referrals_cols_int]
                                               }
                                               ds_tb
                                             })
  return(datasets_ls)
}
get_sports_vars <- function(data_df = NULL,
                            exclude_chr = character(0),
                            group_1L_chr = character(0)){
  sports_vars_chr <- c(group_1L_chr, "Risky", "Subjective", "Team", "Type", "Weighed", "Winter")

  if(!is.null(data_df)){
    sports_vars_chr <- intersect(names(data_df),sports_vars_chr)
  }
  if(!identical(exclude_chr, character(0))){
    sports_vars_chr <- setdiff(sports_vars_chr, exclude_chr)
  }
  return(sports_vars_chr)
}
