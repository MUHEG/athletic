transform_to_prep <- function(data_tb,
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
                              uid_var_1L_chr = "UID",
                              what_1L_chr = c("wide","prep")
){
  ### VALIDATION REQ:
  ### IF transforming to wide no var names should have "__"
  what_1L_chr <- match.arg(what_1L_chr)
  vars_ls <- c("clinical","metrics", "sports", "temporal", "other") %>%
    purrr::map(~  serious::get_vars(data_tb,
                           activity_1L_chr = activity_1L_chr,
                           appointments_var_1L_chr = appointments_var_1L_chr,
                           cancellations_var_1L_chr = cancellations_var_1L_chr,
                           clinical_team_1L_chr = clinical_team_1L_chr,
                           clinician_1L_chr = clinician_1L_chr,
                           clinician_discipline_1L_chr = clinician_discipline_1L_chr,
                           components_chr = components_chr,
                           cost_var_1L_chr = cost_var_1L_chr,
                           days_1L_chr = days_1L_chr,
                           duration_1L_chr = duration_1L_chr,
                           exclude_chr = exclude_chr,
                           group_1L_chr = group_1L_chr,
                           index_1L_chr =  index_1L_chr,
                           referrals_var_1L_chr = referrals_var_1L_chr,
                           referrers_1L_chr = referrers_1L_chr,
                           severity_1L_chr = severity_1L_chr,
                           team_disciplines_1L_chr =  team_disciplines_1L_chr,
                           what_1L_chr = .x)) %>%
    stats::setNames(c("clinical","metrics", "sports", "temporal", "other"))
  data_tb <- data_tb %>%
    dplyr::select(tidyselect::all_of(c(vars_ls$temporal[1],
                                       vars_ls$clinical[c(1,4)],#"Clinician",
                                       vars_ls$metrics,
                                       vars_ls$clinical[8],
                                       vars_ls$other,
                                       vars_ls$sports)))
  if(what_1L_chr == "prep"){
    data_wide_tb <- data_tb
  }else{
    data_tb <- data_tb %>% dplyr::mutate(Record = dplyr::row_number())
    #athlete_roles_chr = c("Athlete", "AlumniAthlete")
    role_var_1L_chr = vars_ls$other[1]
    data_tb <- intersect(ready4use::get_vars_with_cdn(data_tb, is.character), vars_ls$sports) %>%
      purrr::reduce(.init = data_tb,
                    ~{
                      var_nm_1L_chr <- .y
                      .x %>%
                        dplyr::mutate(!!rlang::sym(var_nm_1L_chr) := !!rlang::sym(var_nm_1L_chr) %>%
                                        purrr::map2_chr(!!rlang::sym(role_var_1L_chr), ~ ifelse(is.na(.x),
                                                                                                ifelse(.y %in% athlete_roles_chr, paste0(#var_nm_1L_chr,
                                                                                                  "UNCATEGORISED"),paste0(#var_nm_1L_chr,
                                                                                                    "NOTAPPLICABLE")),
                                                                                                paste0(#var_nm_1L_chr,
                                                                                                  "",.x))))
                    })
    data_tb <- ready4use::get_vars_with_cdn(data_tb, is.logical) %>%
      purrr::reduce(.init = data_tb,
                    ~{
                      var_nm_1L_chr <- .y
                      .x %>%
                        dplyr::mutate(!!rlang::sym(var_nm_1L_chr) := !!rlang::sym(var_nm_1L_chr) %>%
                                        purrr::map2_chr(!!rlang::sym(role_var_1L_chr), ~ ifelse(is.na(.x),
                                                                                                ifelse(.y %in% athlete_roles_chr, paste0(#var_nm_1L_chr,
                                                                                                  "UNCATEGORISED"),
                                                                                                  paste0(#var_nm_1L_chr,
                                                                                                    "FALSE")),
                                                                                                ifelse(isTRUE(.x),
                                                                                                       paste0(#var_nm_1L_chr,
                                                                                                         "TRUE"),
                                                                                                       paste0(#var_nm_1L_chr,
                                                                                                         "FALSE")))))
                    })
    data_wide_tb <- purrr::reduce(c(vars_ls$clinical[c(1,4,8)],#"Referrer Role", "Service", "Severity",
                                    vars_ls$other, #"Role", "Para", "Age", "Sex",
                                    vars_ls$sports),
                                  .init = data_tb,
                                  ~ dplyr::inner_join(.x %>%
                                                        dplyr::select(tidyselect::all_of(c(vars_ls$temporal[1],
                                                                                           vars_ls$metrics,#"Referrals", "Appointments", "Cancellations", "Cost",
                                                                                           "Record"))),
                                                      .x %>% tidyr::pivot_wider(names_from = .y, values_from = vars_ls$metrics,
                                                                                names_prefix = paste0("_",.y,"__"), # ??
                                                                                values_fill = 0)))
    data_wide_tb <- data_wide_tb %>%
      dplyr::select(-tidyselect::all_of("Record")) %>%
      dplyr::group_by(!!rlang::sym(vars_ls$temporal[1])) %>%
      dplyr::summarise(dplyr::across(.cols = dplyr::everything(),.fns = sum)) %>%
      dplyr::ungroup()
  }
  return(data_wide_tb)
}
