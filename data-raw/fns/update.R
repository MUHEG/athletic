update_data_dict <- function(X_Ready4useDyad = ready4use::Ready4useDyad(),
                             dictionary_lups_ls = make_dictionary_lups(),
                             arrange_by_1L_chr = c("category",
                                                   "name")){
  arrange_by_1L_chr <- match.arg(arrange_by_1L_chr)
  X_Ready4useDyad <- 1:length(dictionary_lups_ls) %>%
    purrr::reduce(.init = X_Ready4useDyad,
                  ~ {
                    var_1L_chr <- names(dictionary_lups_ls)[.y]
                    values_lup <- dictionary_lups_ls[[.y]]
                    values_chr <- ready4show::manufacture.ready4show_correspondences(values_lup, .x@dictionary_r3$var_nm_chr, flatten_1L_lgl = T)
                    renewSlot(.x,
                              "dictionary_r3",
                              .x@dictionary_r3 %>% dplyr::mutate(!!rlang::sym(var_1L_chr) := values_chr))

                  })
  X_Ready4useDyad@dictionary_r3 <- X_Ready4useDyad@dictionary_r3 %>%
    dplyr::arrange(!!rlang::sym(ifelse(arrange_by_1L_chr ==
                                         "name", "var_nm_chr", "var_ctg_chr")),
                   !!rlang::sym(ifelse(arrange_by_1L_chr ==
                                         "name", "var_ctg_chr", "var_nm_chr")))
  X_Ready4useDyad@dictionary_r3 <- X_Ready4useDyad@dictionary_r3 %>%
    dplyr::filter(var_nm_chr %in% names(X_Ready4useDyad@ds_tb))
  return(X_Ready4useDyad)
}
update_fake_ages <- function(ages_chr){
  ages_chr <- ages_chr %>%
    stringr::str_replace_all("16-20","16-19") %>%
    stringr::str_replace_all("20-25","20-24") %>%
    stringr::str_replace_all("25-30","25-29") %>%
    stringr::str_replace_all("30-35","30-34") %>%
    stringr::str_replace_all("16-21","16-19") %>%
    stringr::str_replace_all("20-26","20-24") %>%
    stringr::str_replace_all("25-31","25-29") %>%
    stringr::str_replace_all("30-36","30-34") %>%
    stringr::str_replace_all("16-22","16-19") %>%
    stringr::str_replace_all("20-27","20-24") %>%
    stringr::str_replace_all("25-32","25-29") %>%
    stringr::str_replace_all("30-37","30-34") %>%
    stringr::str_replace_all("16-23","16-19") %>%
    stringr::str_replace_all("20-28","20-24") %>%
    stringr::str_replace_all("25-33","25-29") %>%
    stringr::str_replace_all("30-38","30-34")
  return(ages_chr)
}
update_ingested_data <- function(datasets_ls,
                                 categories_chr = c("Individual Sports", "Aesthetic Sports", "Winter Sports"),
                                 exclude_chr = c("Cost","Duration"),
                                 imputed_uid_pfx_chr = "UNK",
                                 missing_1L_chr = "0",
                                 provider_id_1L_chr = "ProviderID",
                                 provider_location_1L_chr = "ProviderState",
                                 uid_vars_chr = c("MedlinksID", "AISID")){
  ## Wrangle Appointments
  if("AIScode" %in% names(datasets_ls$appointments))
    datasets_ls$appointments <- datasets_ls$appointments %>% dplyr::rename(AISID = `AIScode`)
  if("Client role" %in% names(datasets_ls$appointments))
    datasets_ls$appointments <- datasets_ls$appointments %>% dplyr::rename(Role = `Client role`)
  if("Age (categorical)" %in% names(datasets_ls$appointments))
    datasets_ls$appointments <- datasets_ls$appointments %>% dplyr::rename(Age = `Age (categorical)`)
  if("Appointmentdate" %in% names(datasets_ls$appointments))
    datasets_ls$appointments <- datasets_ls$appointments %>% dplyr::rename(Date = `Appointmentdate`)
  if("Appointment type" %in% names(datasets_ls$appointments))
    datasets_ls$appointments <- datasets_ls$appointments %>% dplyr::rename(Service = `Appointment type`)
  if("Appointment clinician" %in% names(datasets_ls$appointments))
    datasets_ls$appointments <- datasets_ls$appointments %>% dplyr::rename(ProviderID = `Appointment clinician`)
  if("Appointment cost" %in% names(datasets_ls$appointments))
    datasets_ls$appointments <- datasets_ls$appointments %>% dplyr::rename(Cost = `Appointment cost`)
  if("Appointment duration" %in% names(datasets_ls$appointments))
    datasets_ls$appointments <- datasets_ls$appointments %>% dplyr::rename(Duration = `Appointment duration`)
  if("Clinician State" %in% names(datasets_ls$appointments))
    datasets_ls$appointments <- datasets_ls$appointments %>% dplyr::rename(!!rlang::sym(provider_location_1L_chr) := `Clinician State`)
  if(!identical(missing_1L_chr, character(0))){
    datasets_ls$appointments <- datasets_ls$appointments %>%
      dplyr::mutate(dplyr::across(where(is.character),
                                  ~ purrr::map_chr(.x,
                                                   ~ ifelse(.x == missing_1L_chr, NA_character_, .x))
      )
      )
  }
  datasets_ls$appointments <- datasets_ls$appointments %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(categories_chr),
                                ~ .x %>% as.numeric() %>% as.logical()))

  datasets_ls$appointments <- datasets_ls$appointments %>%
    dplyr::mutate(Para = `Para/Able` %>% purrr::map_lgl(~ifelse(is.na(.x), NA,.x=="Para")))
  datasets_ls$appointments <- datasets_ls$appointments %>%
    dplyr::mutate(Activity = "Appointment", Appointments = 1, Cancellations = 0, Referrals = 0)
  ## Wrangle Cancellations
  if("AIS code" %in% names(datasets_ls$cancellations))
    datasets_ls$cancellations <- datasets_ls$cancellations %>% dplyr::rename(AISID = `AIS code`)
  if("Cancellation date" %in% names(datasets_ls$cancellations))
    datasets_ls$cancellations <- datasets_ls$cancellations %>% dplyr::rename(Date = `Cancellation date`)
  if("Cancellation type" %in% names(datasets_ls$cancellations))
    datasets_ls$cancellations <- datasets_ls$cancellations %>% dplyr::rename(Service = `Cancellation type`)
  if("Cancellation cost" %in% names(datasets_ls$cancellations))
    datasets_ls$cancellations <- datasets_ls$cancellations %>% dplyr::rename(Cost = `Cancellation cost`)
  datasets_ls$cancellations <- datasets_ls$cancellations %>%
    dplyr::mutate(Activity = "Cancellation", Appointments = 0, Cancellations = 1, Referrals = 0)
  ## Wrangle Referrals
  if("Referral date2" %in% names(datasets_ls$referrals))
    datasets_ls$referrals <- datasets_ls$referrals %>% dplyr::rename(Date = `Referral date2`)
  datasets_ls$referrals <- datasets_ls$referrals %>%
    dplyr::mutate(Activity = "Referral", Appointments = 0, Cancellations = 0, Referrals = 1)
  ## Wrangle Retainers
  if("Retainer date" %in% names(datasets_ls$retainer))
    datasets_ls$retainer <- datasets_ls$retainer %>% dplyr::rename(Date = `Retainer date`)
  if("Retainer amount" %in% names(datasets_ls$retainer))
    datasets_ls$retainer <- datasets_ls$retainer %>% dplyr::rename(Cost = `Retainer amount`)
  ## Munge
  datasets_ls$appointments <- datasets_ls$appointments %>%
    ready4use::add_latest_match(dynamic_lup = datasets_ls$referrals, target_var_nm_1L_chr = "Referrer Role", match_var_nm_1L_chr = uid_vars_chr[1]) %>%
    ready4use::add_latest_match(dynamic_lup = datasets_ls$referrals, target_var_nm_1L_chr = "Referrer Role", match_var_nm_1L_chr = uid_vars_chr[1], invert_1L_lgl = T)
  datasets_ls$appointments <- datasets_ls$appointments %>%
    ready4use::add_from_lup_prototype(arrange_1L_chr = "Date", match_var_nm_1L_chr = uid_vars_chr[1],
                                      exclude_chr = c(uid_vars_chr[2],"Date","Service", exclude_chr, provider_id_1L_chr, provider_location_1L_chr, "Activity", "Appointments", "Cancellations", "Referrals"),
                                      type_1L_chr = "self")
  datasets_ls$cancellations <- datasets_ls$cancellations %>%
    ready4use::add_latest_match(dynamic_lup = datasets_ls$referrals, target_var_nm_1L_chr = "Referrer Role", match_var_nm_1L_chr = uid_vars_chr[1])
  datasets_ls$cancellations <- datasets_ls$cancellations %>%
    ready4use::add_from_lup_prototype(lup_prototype_tb = datasets_ls$appointments, match_var_nm_1L_chr = provider_id_1L_chr, vars_chr = provider_location_1L_chr) %>%
    ready4use::add_from_lup_prototype(lup_prototype_tb = datasets_ls$appointments, match_var_nm_1L_chr = uid_vars_chr[1], exclude_chr = exclude_chr, type_1L_chr = "batch")
  datasets_ls$referrals <- datasets_ls$referrals  %>%
    ready4use::add_from_lup_prototype(lup_prototype_tb = datasets_ls$appointments, match_var_nm_1L_chr = provider_id_1L_chr, vars_chr = c(provider_location_1L_chr))
  datasets_ls$referrals <- datasets_ls$referrals %>%
    ready4use::add_from_lup_prototype(lup_prototype_tb = datasets_ls$appointments %>% dplyr::group_by(ProviderID) %>% dplyr::select(Service) %>% dplyr::summarise(Service = unique(Service)[1]) %>% dplyr::ungroup(), match_var_nm_1L_chr = provider_id_1L_chr, vars_chr = c("Service")) %>%
    ready4use::add_from_lup_prototype(lup_prototype_tb = datasets_ls$appointments, match_var_nm_1L_chr = uid_vars_chr[1], exclude_chr = exclude_chr, type_1L_chr = "batch")
  return(datasets_ls)
}
update_patterns_ls <- function(base_ls = list(c("[[:space:]]", "")),
                               prefix_1L_chr = "Cumulative"){
  if(!identical(prefix_1L_chr, character(0))){
    patterns_ls <-  append(base_ls,
                           list(c(prefix_1L_chr, "")))
  }else{
    patterns_ls <- base_ls

  }
  return(patterns_ls)
}
