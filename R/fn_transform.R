#' Transform to focused tsibble
#' @description transform_to_focused_tsb() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform to focused tsibble. The function returns Data (a tsibble).
#' @param data_tb Data (a tibble)
#' @param activity_1L_chr Activity (a character vector of length one), Default: 'Activity'
#' @param athlete_roles_chr Athlete roles (a character vector), Default: c("Athlete", "AlumniAthlete")
#' @param appointments_var_1L_chr Appointments variable (a character vector of length one), Default: 'Appointments'
#' @param cancellations_var_1L_chr Cancellations variable (a character vector of length one), Default: 'Cancellations'
#' @param clinical_team_1L_chr Clinical team (a character vector of length one), Default: 'Clinical Team'
#' @param clinician_1L_chr Clinician (a character vector of length one), Default: 'Clinician'
#' @param clinician_discipline_1L_chr Clinician discipline (a character vector of length one), Default: 'Service'
#' @param components_chr Components (a character vector), Default: c("Year", "Quarter", "Week")
#' @param cost_var_1L_chr Cost variable (a character vector of length one), Default: 'Cost'
#' @param date_tfmn_fn Date transformation (a function), Default: identity
#' @param days_1L_chr Days (a character vector of length one), Default: 'Weekday'
#' @param duration_1L_chr Duration (a character vector of length one), Default: 'Duration'
#' @param exclude_chr Exclude (a character vector), Default: 'Group'
#' @param fiscal_start_1L_int Fiscal start (an integer vector of length one), Default: 7
#' @param group_1L_chr Group (a character vector of length one), Default: character(0)
#' @param index_1L_chr Index (a character vector of length one), Default: 'Date'
#' @param is_wide_1L_lgl Is wide (a logical vector of length one), Default: F
#' @param key_vars_chr Key variables (a character vector), Default: character(0)
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param referrals_var_1L_chr Referrals variable (a character vector of length one), Default: 'Referrals'
#' @param referrers_1L_chr Referrers (a character vector of length one), Default: 'Referrer Role'
#' @param severity_1L_chr Severity (a character vector of length one), Default: 'Severity'
#' @param team_disciplines_1L_chr Team disciplines (a character vector of length one), Default: 'Disciplines'
#' @param uid_var_1L_chr Unique identifier variable (a character vector of length one), Default: 'UID'
#' @param what_1L_chr What (a character vector of length one), Default: c("all", "totals")
#' @return Data (a tsibble)
#' @rdname transform_to_focused_tsb
#' @export 
#' @importFrom dplyr select mutate group_by across summarise ungroup everything
#' @importFrom tidyr all_of
#' @importFrom rlang sym
#' @importFrom tsibble as_tsibble
#' @keywords internal
transform_to_focused_tsb <- function (data_tb, activity_1L_chr = "Activity", athlete_roles_chr = c("Athlete", 
    "AlumniAthlete"), appointments_var_1L_chr = "Appointments", 
    cancellations_var_1L_chr = "Cancellations", clinical_team_1L_chr = "Clinical Team", 
    clinician_1L_chr = "Clinician", clinician_discipline_1L_chr = "Service", 
    components_chr = c("Year", "Quarter", "Week"), cost_var_1L_chr = "Cost", 
    date_tfmn_fn = identity, days_1L_chr = "Weekday", duration_1L_chr = "Duration", 
    exclude_chr = "Group", fiscal_start_1L_int = 7L, group_1L_chr = character(0), 
    index_1L_chr = "Date", is_wide_1L_lgl = F, key_vars_chr = character(0), 
    metrics_chr = make_metric_vars(), referrals_var_1L_chr = "Referrals", 
    referrers_1L_chr = "Referrer Role", severity_1L_chr = "Severity", 
    team_disciplines_1L_chr = "Disciplines", uid_var_1L_chr = "UID", 
    what_1L_chr = c("all", "totals")) 
{
    if (!is_wide_1L_lgl) {
        data_tb <- transform_to_prep(data_tb, activity_1L_chr = activity_1L_chr, 
            athlete_roles_chr = athlete_roles_chr, appointments_var_1L_chr = appointments_var_1L_chr, 
            cancellations_var_1L_chr = cancellations_var_1L_chr, 
            clinical_team_1L_chr = clinical_team_1L_chr, clinician_1L_chr = clinician_1L_chr, 
            clinician_discipline_1L_chr = clinician_discipline_1L_chr, 
            components_chr = components_chr, cost_var_1L_chr = cost_var_1L_chr, 
            days_1L_chr = days_1L_chr, duration_1L_chr = duration_1L_chr, 
            exclude_chr = exclude_chr, group_1L_chr = group_1L_chr, 
            index_1L_chr = index_1L_chr, referrals_var_1L_chr = referrals_var_1L_chr, 
            referrers_1L_chr = referrers_1L_chr, severity_1L_chr = severity_1L_chr, 
            team_disciplines_1L_chr = team_disciplines_1L_chr, 
            uid_var_1L_chr = uid_var_1L_chr, what_1L_chr = "prep")
        metrics_chr <- make_metric_vars(appointments_var_1L_chr = appointments_var_1L_chr, 
            cancellations_var_1L_chr = cancellations_var_1L_chr, 
            cost_var_1L_chr = cost_var_1L_chr, referrals_var_1L_chr = referrals_var_1L_chr)
        if (identical(key_vars_chr, character(0))) {
            key_vars_chr <- get_key_vars(data_tb, activity_1L_chr = activity_1L_chr, 
                athlete_roles_chr = athlete_roles_chr, appointments_var_1L_chr = appointments_var_1L_chr, 
                cancellations_var_1L_chr = cancellations_var_1L_chr, 
                clinical_team_1L_chr = clinical_team_1L_chr, 
                clinician_1L_chr = clinician_1L_chr, clinician_discipline_1L_chr = clinician_discipline_1L_chr, 
                components_chr = components_chr, cost_var_1L_chr = cost_var_1L_chr, 
                days_1L_chr = days_1L_chr, duration_1L_chr = duration_1L_chr, 
                exclude_chr = exclude_chr, group_1L_chr = group_1L_chr, 
                index_1L_chr = index_1L_chr, referrals_var_1L_chr = referrals_var_1L_chr, 
                referrers_1L_chr = referrers_1L_chr, severity_1L_chr = severity_1L_chr, 
                team_disciplines_1L_chr = team_disciplines_1L_chr, 
                uid_var_1L_chr = uid_var_1L_chr)
        }
        data_tb <- transform_to_prep(data_tb, appointments_var_1L_chr = metrics_chr[2], 
            cancellations_var_1L_chr = metrics_chr[3], cost_var_1L_chr = metrics_chr[4], 
            index_1L_chr = index_1L_chr, referrals_var_1L_chr = metrics_chr[1], 
            what_1L_chr = "prep")
        data_tb <- dplyr::select(data_tb, tidyr::all_of(c(index_1L_chr, 
            metrics_chr, key_vars_chr)))
        data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(index_1L_chr), 
            date_tfmn_fn(!!rlang::sym(index_1L_chr)))) %>% dplyr::group_by(dplyr::across(tidyr::all_of(c(index_1L_chr, 
            key_vars_chr)))) %>% dplyr::summarise(`:=`(!!rlang::sym(metrics_chr[1]), 
            sum(!!rlang::sym(metrics_chr[1]), na.rm = TRUE)), 
            `:=`(!!rlang::sym(metrics_chr[2]), sum(!!rlang::sym(metrics_chr[2]), 
                na.rm = TRUE)), `:=`(!!rlang::sym(metrics_chr[3]), 
                sum(!!rlang::sym(metrics_chr[3]), na.rm = TRUE)), 
            `:=`(!!rlang::sym(metrics_chr[4]), sum(!!rlang::sym(metrics_chr[4]), 
                na.rm = TRUE))) %>% dplyr::ungroup()
        data_tsb <- data_tb %>% tsibble::as_tsibble(key = key_vars_chr, 
            index = index_1L_chr)
        if (what_1L_chr == "totals") {
            data_tsb <- data_tsb %>% dplyr::select(tidyr::all_of(c(index_1L_chr, 
                metrics_chr))) %>% dplyr::summarise(`:=`(!!rlang::sym(metrics_chr[1]), 
                sum(!!rlang::sym(metrics_chr[1]), na.rm = TRUE)), 
                `:=`(!!rlang::sym(metrics_chr[2]), sum(!!rlang::sym(metrics_chr[2]), 
                  na.rm = TRUE)), `:=`(!!rlang::sym(metrics_chr[3]), 
                  sum(!!rlang::sym(metrics_chr[3]), na.rm = TRUE)), 
                `:=`(!!rlang::sym(metrics_chr[4]), sum(!!rlang::sym(metrics_chr[4]), 
                  na.rm = TRUE)))
        }
    }
    else {
        data_tsb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(index_1L_chr), 
            date_tfmn_fn(!!rlang::sym(index_1L_chr)))) %>% dplyr::group_by(!!rlang::sym(index_1L_chr)) %>% 
            dplyr::summarise(dplyr::across(.cols = dplyr::everything(), 
                .fns = sum)) %>% dplyr::ungroup() %>% tsibble::as_tsibble(index = index_1L_chr)
    }
    return(data_tsb)
}
#' Transform to prep
#' @description transform_to_prep() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform to prep. The function returns Data wide (a tibble).
#' @param data_tb Data (a tibble)
#' @param activity_1L_chr Activity (a character vector of length one), Default: 'Activity'
#' @param athlete_roles_chr Athlete roles (a character vector), Default: c("Athlete", "AlumniAthlete")
#' @param appointments_var_1L_chr Appointments variable (a character vector of length one), Default: 'Appointments'
#' @param cancellations_var_1L_chr Cancellations variable (a character vector of length one), Default: 'Cancellations'
#' @param clinical_team_1L_chr Clinical team (a character vector of length one), Default: 'Clinical Team'
#' @param clinician_1L_chr Clinician (a character vector of length one), Default: 'Clinician'
#' @param clinician_discipline_1L_chr Clinician discipline (a character vector of length one), Default: 'Service'
#' @param components_chr Components (a character vector), Default: c("Year", "Quarter", "Week")
#' @param cost_var_1L_chr Cost variable (a character vector of length one), Default: 'Cost'
#' @param days_1L_chr Days (a character vector of length one), Default: 'Weekday'
#' @param duration_1L_chr Duration (a character vector of length one), Default: 'Duration'
#' @param exclude_chr Exclude (a character vector), Default: 'Group'
#' @param group_1L_chr Group (a character vector of length one), Default: character(0)
#' @param index_1L_chr Index (a character vector of length one), Default: 'Date'
#' @param referrals_var_1L_chr Referrals variable (a character vector of length one), Default: 'Referrals'
#' @param referrers_1L_chr Referrers (a character vector of length one), Default: 'Referrer Role'
#' @param severity_1L_chr Severity (a character vector of length one), Default: 'Severity'
#' @param team_disciplines_1L_chr Team disciplines (a character vector of length one), Default: 'Disciplines'
#' @param uid_var_1L_chr Unique identifier variable (a character vector of length one), Default: 'UID'
#' @param what_1L_chr What (a character vector of length one), Default: c("wide", "prep")
#' @return Data wide (a tibble)
#' @rdname transform_to_prep
#' @export 
#' @importFrom purrr map reduce map2_chr
#' @importFrom serious get_vars
#' @importFrom stats setNames
#' @importFrom dplyr select mutate row_number inner_join group_by summarise across everything ungroup
#' @importFrom tidyselect all_of
#' @importFrom ready4use get_vars_with_cdn
#' @importFrom rlang sym
#' @importFrom tidyr pivot_wider
#' @keywords internal
transform_to_prep <- function (data_tb, activity_1L_chr = "Activity", athlete_roles_chr = c("Athlete", 
    "AlumniAthlete"), appointments_var_1L_chr = "Appointments", 
    cancellations_var_1L_chr = "Cancellations", clinical_team_1L_chr = "Clinical Team", 
    clinician_1L_chr = "Clinician", clinician_discipline_1L_chr = "Service", 
    components_chr = c("Year", "Quarter", "Week"), cost_var_1L_chr = "Cost", 
    days_1L_chr = "Weekday", duration_1L_chr = "Duration", exclude_chr = "Group", 
    group_1L_chr = character(0), index_1L_chr = "Date", referrals_var_1L_chr = "Referrals", 
    referrers_1L_chr = "Referrer Role", severity_1L_chr = "Severity", 
    team_disciplines_1L_chr = "Disciplines", uid_var_1L_chr = "UID", 
    what_1L_chr = c("wide", "prep")) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    vars_ls <- c("clinical", "metrics", "sports", "temporal", 
        "other") %>% purrr::map(~serious::get_vars(data_tb, activity_1L_chr = activity_1L_chr, 
        appointments_var_1L_chr = appointments_var_1L_chr, cancellations_var_1L_chr = cancellations_var_1L_chr, 
        clinical_team_1L_chr = clinical_team_1L_chr, clinician_1L_chr = clinician_1L_chr, 
        clinician_discipline_1L_chr = clinician_discipline_1L_chr, 
        components_chr = components_chr, cost_var_1L_chr = cost_var_1L_chr, 
        days_1L_chr = days_1L_chr, duration_1L_chr = duration_1L_chr, 
        exclude_chr = exclude_chr, group_1L_chr = group_1L_chr, 
        index_1L_chr = index_1L_chr, referrals_var_1L_chr = referrals_var_1L_chr, 
        referrers_1L_chr = referrers_1L_chr, severity_1L_chr = severity_1L_chr, 
        team_disciplines_1L_chr = team_disciplines_1L_chr, what_1L_chr = .x)) %>% 
        stats::setNames(c("clinical", "metrics", "sports", "temporal", 
            "other"))
    data_tb <- data_tb %>% dplyr::select(tidyselect::all_of(c(vars_ls$temporal[1], 
        vars_ls$clinical[c(1, 4)], vars_ls$metrics, vars_ls$clinical[8], 
        vars_ls$other, vars_ls$sports)))
    if (what_1L_chr == "prep") {
        data_wide_tb <- data_tb
    }
    else {
        data_tb <- data_tb %>% dplyr::mutate(Record = dplyr::row_number())
        role_var_1L_chr = vars_ls$other[1]
        data_tb <- intersect(ready4use::get_vars_with_cdn(data_tb, 
            is.character), vars_ls$sports) %>% purrr::reduce(.init = data_tb, 
            ~{
                var_nm_1L_chr <- .y
                .x %>% dplyr::mutate(`:=`(!!rlang::sym(var_nm_1L_chr), 
                  !!rlang::sym(var_nm_1L_chr) %>% purrr::map2_chr(!!rlang::sym(role_var_1L_chr), 
                    ~ifelse(is.na(.x), ifelse(.y %in% athlete_roles_chr, 
                      paste0("UNCATEGORISED"), paste0("NOTAPPLICABLE")), 
                      paste0("", .x)))))
            })
        data_tb <- ready4use::get_vars_with_cdn(data_tb, is.logical) %>% 
            purrr::reduce(.init = data_tb, ~{
                var_nm_1L_chr <- .y
                .x %>% dplyr::mutate(`:=`(!!rlang::sym(var_nm_1L_chr), 
                  !!rlang::sym(var_nm_1L_chr) %>% purrr::map2_chr(!!rlang::sym(role_var_1L_chr), 
                    ~ifelse(is.na(.x), ifelse(.y %in% athlete_roles_chr, 
                      paste0("UNCATEGORISED"), paste0("FALSE")), 
                      ifelse(isTRUE(.x), paste0("TRUE"), paste0("FALSE"))))))
            })
        data_wide_tb <- purrr::reduce(c(vars_ls$clinical[c(1, 
            4, 8)], vars_ls$other, vars_ls$sports), .init = data_tb, 
            ~dplyr::inner_join(.x %>% dplyr::select(tidyselect::all_of(c(vars_ls$temporal[1], 
                vars_ls$metrics, "Record"))), .x %>% tidyr::pivot_wider(names_from = .y, 
                values_from = vars_ls$metrics, names_prefix = paste0("_", 
                  .y, "__"), values_fill = 0)))
        data_wide_tb <- data_wide_tb %>% dplyr::select(-tidyselect::all_of("Record")) %>% 
            dplyr::group_by(!!rlang::sym(vars_ls$temporal[1])) %>% 
            dplyr::summarise(dplyr::across(.cols = dplyr::everything(), 
                .fns = sum)) %>% dplyr::ungroup()
    }
    return(data_wide_tb)
}
