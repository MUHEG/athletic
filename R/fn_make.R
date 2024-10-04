#' Make clinical variables
#' @description make_clinical_vars() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make clinical variables. The function returns Clinical variables (a character vector).
#' @param activity_1L_chr Activity (a character vector of length one), Default: 'Activity'
#' @param clinical_team_1L_chr Clinical team (a character vector of length one), Default: 'Clinical Team'
#' @param clinician_1L_chr Clinician (a character vector of length one), Default: 'Clinician'
#' @param clinician_discipline_1L_chr Clinician discipline (a character vector of length one), Default: 'Service'
#' @param duration_1L_chr Duration (a character vector of length one), Default: 'Duration'
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @param referrers_1L_chr Referrers (a character vector of length one), Default: 'Referrer Role'
#' @param severity_1L_chr Severity (a character vector of length one), Default: 'Severity'
#' @param team_disciplines_1L_chr Team disciplines (a character vector of length one), Default: 'Disciplines'
#' @return Clinical variables (a character vector)
#' @rdname make_clinical_vars
#' @export 
#' @keywords internal
make_clinical_vars <- function (activity_1L_chr = "Activity", clinical_team_1L_chr = "Clinical Team", 
    clinician_1L_chr = "Clinician", clinician_discipline_1L_chr = "Service", 
    duration_1L_chr = "Duration", exclude_chr = character(0), 
    referrers_1L_chr = "Referrer Role", severity_1L_chr = "Severity", 
    team_disciplines_1L_chr = "Disciplines") 
{
    clinical_vars_chr <- c(referrers_1L_chr, team_disciplines_1L_chr, 
        clinical_team_1L_chr, clinician_discipline_1L_chr, clinician_1L_chr, 
        activity_1L_chr, duration_1L_chr, severity_1L_chr)
    if (!identical(exclude_chr, character(0))) {
        clinical_vars_chr <- setdiff(clinical_vars_chr, exclude_chr)
    }
    return(clinical_vars_chr)
}
#' Make keepers
#' @description make_keepers() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make keepers. The function returns Keep (a character vector).
#' @param names_chr Names (a character vector)
#' @param clinical_vars_chr Clinical variables (a character vector), Default: make_clinical_vars()
#' @param keep_cdn_1L_chr Keep condition (a character vector of length one), Default: c("All", "Personal", "Provider", "Severity", "Sports")
#' @param severity_1L_int Severity (an integer vector of length one), Default: 8
#' @param sports_vars_chr Sports variables (a character vector), Default: get_sports_vars()
#' @return Keep (a character vector)
#' @rdname make_keepers
#' @export 
#' @keywords internal
make_keepers <- function (names_chr, clinical_vars_chr = make_clinical_vars(), 
    keep_cdn_1L_chr = c("All", "Personal", "Provider", "Severity", 
        "Sports"), severity_1L_int = 8L, sports_vars_chr = get_sports_vars()) 
{
    keep_cdn_1L_chr <- match.arg(keep_cdn_1L_chr)
    keep_chr <- names_chr
    if (keep_cdn_1L_chr != "All") {
        if (keep_cdn_1L_chr == "Personal") 
            keep_chr <- setdiff(keep_chr, c(clinical_vars_chr, 
                sports_vars_chr))
        if (keep_cdn_1L_chr == "Provider") 
            keep_chr <- intersect(keep_chr, clinical_vars_chr[-severity_1L_int])
        if (keep_cdn_1L_chr == "Severity") 
            keep_chr <- intersect(keep_chr, clinical_vars_chr[severity_1L_int])
        if (keep_cdn_1L_chr == "Sports") 
            keep_chr <- intersect(keep_chr, sports_vars_chr)
    }
    return(keep_chr)
}
#' Make keys datasets
#' @description make_keys_dss() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make keys datasets. The function returns Totals datasets (a list).
#' @param data_tb Data (a tibble)
#' @param key_vars_chr Key variables (a character vector)
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
#' @param fns_ls Functions (a list), Default: NULL
#' @param group_1L_chr Group (a character vector of length one), Default: character(0)
#' @param index_1L_chr Index (a character vector of length one), Default: 'Date'
#' @param periods_chr Periods (a character vector), Default: c("sub", "daily", "weekly", "monthly", "quarterly", "yearly")
#' @param referrals_var_1L_chr Referrals variable (a character vector of length one), Default: 'Referrals'
#' @param referrers_1L_chr Referrers (a character vector of length one), Default: 'Referrer Role'
#' @param severity_1L_chr Severity (a character vector of length one), Default: 'Severity'
#' @param team_disciplines_1L_chr Team disciplines (a character vector of length one), Default: 'Disciplines'
#' @param uid_var_1L_chr Unique identifier variable (a character vector of length one), Default: 'UID'
#' @return Totals datasets (a list)
#' @rdname make_keys_dss
#' @export 
#' @importFrom lubridate ymd_hms year
#' @importFrom tsibble yearweek yearmonth yearquarter
#' @importFrom purrr keep_at map
#' @importFrom stats setNames
#' @keywords internal
make_keys_dss <- function (data_tb, key_vars_chr, activity_1L_chr = "Activity", 
    athlete_roles_chr = c("Athlete", "AlumniAthlete"), appointments_var_1L_chr = "Appointments", 
    cancellations_var_1L_chr = "Cancellations", clinical_team_1L_chr = "Clinical Team", 
    clinician_1L_chr = "Clinician", clinician_discipline_1L_chr = "Service", 
    components_chr = c("Year", "Quarter", "Week"), cost_var_1L_chr = "Cost", 
    days_1L_chr = "Weekday", duration_1L_chr = "Duration", exclude_chr = "Group", 
    fns_ls = NULL, group_1L_chr = character(0), index_1L_chr = "Date", 
    periods_chr = c("sub", "daily", "weekly", "monthly", "quarterly", 
        "yearly"), referrals_var_1L_chr = "Referrals", referrers_1L_chr = "Referrer Role", 
    severity_1L_chr = "Severity", team_disciplines_1L_chr = "Disciplines", 
    uid_var_1L_chr = "UID") 
{
    if (is.null(fns_ls)) {
        fns_ls <- list(sub = lubridate::ymd_hms, daily = identity, 
            weekly = tsibble::yearweek, monthly = tsibble::yearmonth, 
            quarterly = tsibble::yearquarter, yearly = lubridate::year)
    }
    selected_ls <- fns_ls %>% purrr::keep_at(periods_chr)
    totals_dss_ls <- selected_ls %>% purrr::map(~{
        date_tfmn_fn <- .x
        key_vars_chr %>% purrr::map(~data_tb %>% transform_to_tsibble(activity_1L_chr = activity_1L_chr, 
            athlete_roles_chr = athlete_roles_chr, appointments_var_1L_chr = appointments_var_1L_chr, 
            cancellations_var_1L_chr = cancellations_var_1L_chr, 
            clinical_team_1L_chr = clinical_team_1L_chr, clinician_1L_chr = clinician_1L_chr, 
            clinician_discipline_1L_chr = clinician_discipline_1L_chr, 
            components_chr = components_chr, cost_var_1L_chr = cost_var_1L_chr, 
            date_tfmn_fn = date_tfmn_fn, days_1L_chr = days_1L_chr, 
            duration_1L_chr = duration_1L_chr, exclude_chr = exclude_chr, 
            group_1L_chr = group_1L_chr, index_1L_chr = index_1L_chr, 
            is_wide_1L_lgl = F, key_vars_chr = .x, metrics_chr = c(referrals_var_1L_chr, 
                appointments_var_1L_chr, cancellations_var_1L_chr, 
                cost_var_1L_chr), referrals_var_1L_chr = referrals_var_1L_chr, 
            referrers_1L_chr = referrers_1L_chr, severity_1L_chr = severity_1L_chr, 
            team_disciplines_1L_chr = team_disciplines_1L_chr, 
            type_1L_chr = "focused", uid_var_1L_chr = uid_var_1L_chr)) %>% 
            stats::setNames(key_vars_chr)
    }) %>% stats::setNames(names(selected_ls))
    return(totals_dss_ls)
}
#' Make severity arguments list
#' @description make_severity_args_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make severity arguments list. The function returns Severity arguments (a list).
#' @param disciplines_ls Disciplines (a list)
#' @param sessions_ls Sessions (a list)
#' @param names_chr Names (a character vector), Default: character(0)
#' @param severity_var_1L_chr Severity variable (a character vector of length one), Default: 'Severity'
#' @return Severity arguments (a list)
#' @rdname make_severity_args_ls
#' @export 
#' @importFrom purrr map
#' @importFrom stats setNames
#' @keywords internal
make_severity_args_ls <- function (disciplines_ls, sessions_ls, names_chr = character(0), 
    severity_var_1L_chr = "Severity") 
{
    severity_args_ls <- list(disciplines_ls = disciplines_ls, 
        sessions_ls = sessions_ls)
    if (identical(names_chr, character(0))) {
        names_chr <- make_severity_vars(severity_args_ls, severity_var_1L_chr = severity_var_1L_chr)
    }
    severity_args_ls <- severity_args_ls %>% purrr::map(~.x %>% 
        stats::setNames(names_chr))
    return(severity_args_ls)
}
#' Make severity variables
#' @description make_severity_vars() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make severity variables. The function returns Severity variables (a character vector).
#' @param severity_args_ls Severity arguments (a list)
#' @param severity_var_1L_chr Severity variable (a character vector of length one), Default: 'Severity'
#' @return Severity variables (a character vector)
#' @rdname make_severity_vars
#' @export 
#' @importFrom purrr map_chr
#' @keywords internal
make_severity_vars <- function (severity_args_ls, severity_var_1L_chr = "Severity") 
{
    severity_vars_chr <- 1:1:length(severity_args_ls$sessions_ls) %>% 
        purrr::map_chr(~paste0(severity_var_1L_chr, ifelse(.x == 
            1, "", paste0("_", LETTERS[.x - 1]))))
    return(severity_vars_chr)
}
#' Make sports categories
#' @description make_sports_categories() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make sports categories. The function returns Categories (a character vector).
#' @param type_1L_int Type (an integer vector of length one), Default: c("current", "deprecated")
#' @param which_int Which (an integer vector), Default: integer(0)
#' @return Categories (a character vector)
#' @rdname make_sports_categories
#' @export 
#' @keywords internal
make_sports_categories <- function (type_1L_int = c("current", "deprecated"), which_int = integer(0)) 
{
    type_1L_int <- match.arg(type_1L_int)
    if (type_1L_int == "current") {
        categories_chr <- c("High burden of injury (TBA)", "Individual or other", 
            "Criteria-based officiating (aesthetic)", "Winter", 
            "Endurance / long distance", "Middle distance / power", 
            "Speed / strength", "Precision / skill-dependent", 
            "Combat / weight-making")
    }
    if (type_1L_int == "deprecated") {
        categories_chr <- c("Risky", "Subjective", "Team", "Type", 
            "Weighed", "Winter")
    }
    if (!identical(which_int, integer(0))) {
        categories_chr <- categories_chr[which_int]
    }
    return(categories_chr)
}
#' Make sports groups
#' @description make_sports_groups() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make sports groups. The function returns Grouped (a tibble).
#' @param datasets_ls Datasets (a list)
#' @param categories_chr Categories (a character vector), Default: make_sports_categories()
#' @param simple_1L_lgl Simple (a logical vector of length one), Default: TRUE
#' @return Grouped (a tibble)
#' @rdname make_sports_groups
#' @export 
#' @importFrom dplyr select distinct mutate slice pull filter everything arrange group_by summarise ungroup
#' @importFrom tidyselect all_of
#' @importFrom purrr map_dfr reduce map_lgl
#' @importFrom rlang sym
#' @importFrom stringr str_replace_all
#' @keywords internal
make_sports_groups <- function (datasets_ls, categories_chr = make_sports_categories(), 
    simple_1L_lgl = TRUE) 
{
    sports_tb <- datasets_ls$sports_tb
    combinations_tb <- sports_tb %>% dplyr::select(tidyselect::all_of(categories_chr))
    distinct_tb <- combinations_tb %>% dplyr::distinct()
    distinct_tb <- distinct_tb %>% dplyr::mutate(Group = paste0("Grouping ", 
        1:nrow(distinct_tb)))
    grouped_tb <- purrr::map_dfr(1:nrow(sports_tb), ~{
        sliced_tb <- sports_tb %>% dplyr::slice(.x) %>% dplyr::select(tidyselect::all_of(c("Sport", 
            categories_chr)))
        filtered_tb <- purrr::reduce(setdiff(names(sliced_tb), 
            "Sport"), .init = distinct_tb, ~{
            test_1L_lgl <- sliced_tb %>% dplyr::pull(!!rlang::sym(.y))
            dplyr::filter(.x, !!rlang::sym(.y) %>% purrr::map_lgl(~identical(.x, 
                test_1L_lgl)))
        })
        filtered_tb
        if ("Sport" %in% names(sliced_tb)) {
            filtered_tb <- dplyr::mutate(filtered_tb, Sport = sliced_tb$Sport) %>% 
                dplyr::select(Sport, dplyr::everything())
        }
        filtered_tb
    }) %>% dplyr::arrange(Group %>% stringr::str_replace_all("Grouping ", 
        "") %>% as.numeric())
    if (simple_1L_lgl) {
        grouped_tb <- grouped_tb %>% dplyr::group_by(Group) %>% 
            dplyr::summarise(Sports = list(paste0(Sport))) %>% 
            dplyr::ungroup() %>% dplyr::arrange(Group %>% stringr::str_replace_all("Grouping ", 
            "") %>% as.numeric())
    }
    return(grouped_tb)
}
#' Make sports tibble
#' @description make_sports_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make sports tibble. The function returns Sports (a tibble).
#' @param datasets_ls Datasets (a list)
#' @param categories_chr Categories (a character vector), Default: make_sports_categories("deprecated")
#' @return Sports (a tibble)
#' @rdname make_sports_tb
#' @export 
#' @importFrom tibble tibble
#' @importFrom dplyr mutate case_when select
#' @importFrom tidyselect all_of
#' @keywords internal
make_sports_tb <- function (datasets_ls, categories_chr = make_sports_categories("deprecated")) 
{
    sports_tb <- tibble::tibble(Sport = datasets_ls$appointments$Sport %>% 
        sort() %>% unique()) %>% dplyr::mutate(Risky = dplyr::case_when(Sport %in% 
        c("Equestrian", "Rugby", "Skateboarding") ~ T, Sport %in% 
        c("Miscellaneous", "Sport no longer funded") ~ NA, T ~ 
        F), Subjective = dplyr::case_when(Sport %in% c("Artistic Swimming", 
        "Diving", "Gymnastics", "Surfing") ~ T, Sport %in% c("Miscellaneous", 
        "Sport no longer funded") ~ NA, T ~ F), Team = dplyr::case_when(Sport %in% 
        c("Artistic Swimming", "Baseball", "Basketball", "Football", 
            "Hockey", "Netball", "Rugby", "Sailing", "Softball", 
            "Volleyball") ~ T, Sport %in% c("Miscellaneous", 
        "Sport no longer funded") ~ NA, T ~ F), Type = dplyr::case_when(Sport %in% 
        c("Archery", "Equestrian", "Golf", "Sailing", "Table Tennis") ~ 
        "Skill", Sport %in% c("Basketball", "Football", "Rugby", 
        "Soccer", "Volleyball", "Waterpolo") ~ "Mixed", Sport %in% 
        c("Cycling", "Triathlon") ~ "Endurance", T ~ NA_character_), 
        Weighed = dplyr::case_when(Sport %in% c("Combat sports") ~ 
            T, Sport %in% c("Miscellaneous", "Sport no longer funded") ~ 
            NA, T ~ F), Winter = dplyr::case_when(Sport %in% 
            c("Winter sports") ~ T, T ~ F))
    sports_tb <- sports_tb %>% dplyr::select(tidyselect::all_of(c("Sport", 
        categories_chr)))
    return(sports_tb)
}
