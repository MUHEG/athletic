add_counts <- function(datasets_ls,
                       sum_chr = c("Role"),
                       uid_1L_chr = "Client ID"){
  counts_tb <- datasets_ls$appointments$Group %>% unique() %>% purrr::map(~datasets_ls$appointments %>% dplyr::filter(Group == .x)) %>%
    purrr::map_dfr(~{
      partial_tb <- .x
      purrr::map_dfc(sum_chr,
                     ~{
                       var_1L_chr <- .x
                       datasets_ls$appointments %>% dplyr::pull(var_1L_chr) %>% unique() %>% sort() %>%
                         purrr::map_dfc(~ {
                           filtered_tb <- partial_tb %>%
                             dplyr::filter(!duplicated(dplyr::pull(partial_tb,!!rlang::sym(uid_1L_chr)))) %>%
                             dplyr::filter(!!rlang::sym(var_1L_chr) == .x) %>%
                             dplyr::summarise(!!rlang::sym(.x) := dplyr::n())
                         } )
                     }) %>%
        dplyr::mutate(Group = partial_tb$Group[1])
    })
  datasets_ls$group_lup <- datasets_ls$group_lup %>% dplyr::left_join(counts_tb)
  return(datasets_ls)
}
add_imputed_costs <- function(data_tb,
                              arrange_by_1L_chr = character(0),
                              cost_var_1L_chr = "Cost",
                              provider_id_1L_chr = "ProviderID"){
  complete_tb <- data_tb %>% dplyr::filter(!is.na(!!rlang::sym(cost_var_1L_chr)))
  missing_tb <- data_tb %>% dplyr::filter(is.na(!!rlang::sym(cost_var_1L_chr)))
  lookup_tb <- complete_tb %>%
    dplyr::filter(!is.na(!!rlang::sym(provider_id_1L_chr))) %>%
    dplyr::group_by(!!rlang::sym(provider_id_1L_chr)) %>%
    dplyr::summarise(!!rlang::sym(cost_var_1L_chr) := mean(!!rlang::sym(cost_var_1L_chr))) %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::all_of(c(provider_id_1L_chr, cost_var_1L_chr)))
  mean_cost_1L_dbl <- complete_tb$Cost %>% mean()
  imputed_tb <- missing_tb %>% dplyr::select(-!!rlang::sym(cost_var_1L_chr)) %>% dplyr::left_join(lookup_tb) %>%
    dplyr::mutate(!!rlang::sym(cost_var_1L_chr) := dplyr::case_when(is.na(!!rlang::sym(cost_var_1L_chr)) ~ mean_cost_1L_dbl,
                                                                    T ~ !!rlang::sym(cost_var_1L_chr)))
  data_tb <- dplyr::bind_rows(complete_tb, imputed_tb)
  if(!identical(arrange_by_1L_chr, character(0))){
    data_tb <- dplyr::arrange(data_tb, !!rlang::sym(arrange_by_1L_chr))
  }
  return(data_tb)
}
add_severity <- function(data_tb,
                         severity_args_ls,
                         appointments_var_1L_chr = "Appointments",
                         date_var_1L_chr = "Date",
                         #disciplines_1L_lgl = TRUE,
                         disciplines_chr = c("DE Psychology", "Dietetics", "Psychiatry", "Psychology"),
                         end_date_dtm = lubridate::ymd("2024-06-30"),
                         provider_var_1L_chr = "ProviderID",
                         service_var_1L_chr = "Service",
                         #sessions_moderate_int = c(4,15),
                         severity_var_1L_chr = "Severity",
                         tenure_var_1L_chr = "Tenure",
                         uid_var_1L_chr = "UID"){
  full_tenure_tb <- data_tb %>% dplyr::filter(!is.na(UID)) %>% serious::update_to_full_tenure(end_date_dtm = end_date_dtm)
  censored_tb <- setdiff(data_tb, full_tenure_tb)
  cuts_1L_int <- ceiling(max(full_tenure_tb %>% dplyr::pull(!!rlang::sym(tenure_var_1L_chr))))
  severity_vars_chr <- names(severity_args_ls$sessions_ls)
  full_tenure_tb <- 1:cuts_1L_int %>%
    purrr::map_dfr(~{
      floor_1L_int <- .x-1
      ceiling_1L_int <- .x
      filtered_tb <- full_tenure_tb %>% dplyr::filter(!!rlang::sym(tenure_var_1L_chr) %>% purrr::map_lgl(~.x < ceiling_1L_int  && .x >= floor_1L_int))
      lookup_tb <- filtered_tb %>%
        dplyr::group_by(!!rlang::sym(uid_var_1L_chr)) %>%
        dplyr::mutate(`Annual DE Psychology Appointments` = dplyr::case_when(any(is.na(!!rlang::sym(service_var_1L_chr))) ~ NA_real_,
                                                                             all(!is.na(!!rlang::sym(service_var_1L_chr))) & !!rlang::sym(service_var_1L_chr) == disciplines_chr[1] ~ !!rlang::sym(appointments_var_1L_chr),
                                                                             TRUE ~ 0),
                      `Annual Dietetics Appointments` = dplyr::case_when(any(is.na(!!rlang::sym(service_var_1L_chr))) ~ NA_real_,
                                                                         all(!is.na(!!rlang::sym(service_var_1L_chr))) & !!rlang::sym(service_var_1L_chr) == disciplines_chr[2] ~ !!rlang::sym(appointments_var_1L_chr),
                                                                         TRUE ~ 0),
                      `Annual Psychiatry Appointments` = dplyr::case_when(any(is.na(!!rlang::sym(service_var_1L_chr))) ~ NA_real_,
                                                                          all(!is.na(!!rlang::sym(service_var_1L_chr))) & !!rlang::sym(service_var_1L_chr) == disciplines_chr[3] ~ !!rlang::sym(appointments_var_1L_chr),
                                                                          TRUE ~ 0),
                      `Annual Psychology Appointments` = dplyr::case_when(any(is.na(!!rlang::sym(service_var_1L_chr))) ~ NA_real_,
                                                                          all(!is.na(!!rlang::sym(service_var_1L_chr))) & !!rlang::sym(service_var_1L_chr) == disciplines_chr[4] ~ !!rlang::sym(appointments_var_1L_chr),
                                                                          TRUE ~ 0)) %>%
        dplyr::summarise(`Annual appointments` = sum(!!rlang::sym(appointments_var_1L_chr)),
                         `Annual DE Psychology Appointments` = sum(`Annual DE Psychology Appointments`),
                         `Annual Dietetics Appointments` = sum(`Annual Dietetics Appointments`),
                         `Annual Psychiatry Appointments` = sum(`Annual Psychiatry Appointments`),
                         `Annual Psychology Appointments` = sum(`Annual Psychology Appointments`),
                         `Annual Disciplines` = list(!!rlang::sym(service_var_1L_chr) %>% unique() %>% sort()),
                         `Annual Providers` = list(!!rlang::sym(provider_var_1L_chr) %>% unique() %>% sort())) %>%
        dplyr::ungroup()
      lookup_tb <- 1:length(severity_args_ls$sessions_ls) %>%
        purrr::reduce(.init = lookup_tb,
                      ~ {
                        #suffix_1L_chr <- ifelse(.y==1,"",paste0("_",LETTERS[.y-1]))
                        cutoffs_int <- severity_args_ls$sessions_ls[[.y]]
                        test_1L_lgl <- severity_args_ls$disciplines_ls[[.y]]
                        dplyr::mutate(.x,
                                      !!rlang::sym(severity_vars_chr[.y]#paste0(severity_var_1L_chr, suffix_1L_chr)
                                      ) := .x %>%
                                        purrr::pmap_chr(~{
                                          test_1L_lgl <- ifelse(test_1L_lgl, sum(..4,..5) ==0, TRUE)
                                          ifelse(is.na(cutoffs_int[1]),
                                                 ifelse(..2 > cutoffs_int[2],
                                                        "Severe",
                                                        "Mild To Moderate"),
                                                 ifelse(..2 < cutoffs_int[1] && test_1L_lgl,
                                                        "Mild",
                                                        ifelse(..2 > cutoffs_int[2],
                                                               "Severe",
                                                               "Moderate")))

                                        }))
                      })

      filtered_tb %>%
        dplyr::select(-tidyselect::any_of(severity_vars_chr)) %>%
        dplyr::left_join(lookup_tb)
    }) %>%
    dplyr::arrange(!!rlang::sym(date_var_1L_chr))
  # severity_vars_chr <- make_severity_vars(severity_args_ls,
  #                                         severity_var_1L_chr = severity_var_1L_chr)
  data_tb <- dplyr::bind_rows(full_tenure_tb,
                              severity_vars_chr %>% purrr::reduce(.init = censored_tb, ~ .x %>% dplyr::mutate(!!rlang::sym(.y) := NA_character_))
  ) %>%
    dplyr::arrange(!!rlang::sym(uid_var_1L_chr))

  return(data_tb)
}
add_sports_data <- function(datasets_ls,
                            categories_chr = make_sports_categories(),
                            drop_sport_1L_lgl = FALSE,
                            path_1L_chr = character(0),
                            sports_1L_int = 81L,
                            sports_tab_1L_int = 2L,
                            sport_var_1L_chr = "Medlinks Sport categories",
                            sum_chr = c("Role"),
                            uid_1L_chr = "Client ID"){

  if(identical(path_1L_chr, character(0))){
    datasets_ls$sports_tb <- make_sports_tb(datasets_ls, categories_chr = categories_chr)
  }else{
    if(sports_tab_1L_int>1){
      other_chr <- letters[1:(sports_tab_1L_int-1)]
    }else{
      other_chr <- character(0)
    }
    datasets_ls$sports_tb <- get_raw_data(path_1L_chr,
                                          sheets_ls = list(sports_tb = c(1,sports_1L_int)),
                                          tabs_chr = c(other_chr, "sports_tb"),
                                          # sports_1L_int = sports_1L_int,
                                          sheets_int = sports_tab_1L_int
    ) %>% purrr::pluck(1) %>%
      dplyr::rename(Sport = tidyr::all_of(sport_var_1L_chr)) %>%
      dplyr::select(tidyr::all_of(c("Sport", categories_chr))) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.logical))
    if(all(is.na(datasets_ls$sports_tb %>% dplyr::pull(categories_chr[1]))))
      datasets_ls$sports_tb <- datasets_ls$sports_tb %>%
      dplyr::mutate(!!rlang::sym(categories_chr[1]) := dplyr::case_when(Sport %in% c("Equestrian", "Rugby", "Skateboarding", "Football") ~ T, # Eq, Wheeled Motor, Roller, Rugby, AFL
                                                                        Sport %in% c(#"Miscellaneous", "Sport no longer funded",
                                                                          "N/A (missing)", "Does not belong to a sport - accessing MHRN under a Critical Incident", "Works across multiple sports") ~ NA,
                                                                        T ~ F))

  }
  datasets_ls$group_lup <- make_sports_groups(datasets_ls,
                                              categories_chr = categories_chr)
  datasets_ls$grouped_tb <- make_sports_groups(datasets_ls,
                                               categories_chr = categories_chr,
                                               simple_1L_lgl = FALSE)
  datasets_ls$appointments <- datasets_ls$appointments %>%
    dplyr::mutate(Group = purrr::map_chr(Sport,
                                         ~ {
                                           if(is.na(.x)){
                                             NA_character_
                                           }else{
                                             sport_1L_chr <- .x
                                             dplyr::filter(datasets_ls$group_lup, Sports %>% purrr::map_lgl(~sport_1L_chr %in% .x)) %>% dplyr::pull(Group)
                                           }

                                         }))
  datasets_ls$appointments <- dplyr::left_join(datasets_ls$appointments, datasets_ls$grouped_tb)
  if(drop_sport_1L_lgl)
    datasets_ls$appointments <- datasets_ls$appointments %>%
    dplyr::select(-Sport)
  datasets_ls <- add_counts(datasets_ls,
                            sum_chr = sum_chr,
                            uid_1L_chr = uid_1L_chr)
  return(datasets_ls)
}
