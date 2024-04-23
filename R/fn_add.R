#' Add counts
#' @description add_counts() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add counts. The function returns Datasets (a list).
#' @param datasets_ls Datasets (a list)
#' @return Datasets (a list)
#' @rdname add_counts
#' @export 
#' @importFrom purrr map map_dfr map_dfc
#' @importFrom dplyr filter summarise n mutate left_join
#' @importFrom rlang sym
#' @keywords internal
add_counts <- function (datasets_ls) 
{
    counts_tb <- datasets_ls$appointments$Group %>% unique() %>% 
        purrr::map(~datasets_ls$appointments %>% dplyr::filter(Group == 
            .x)) %>% purrr::map_dfr(~{
        partial_tb <- .x
        datasets_ls$appointments$Role %>% unique() %>% sort() %>% 
            purrr::map_dfc(~{
                filtered_tb <- partial_tb %>% dplyr::filter(Role == 
                  .x) %>% dplyr::summarise(`:=`(!!rlang::sym(.x), 
                  dplyr::n()))
            }) %>% dplyr::mutate(Group = partial_tb$Group[1])
    })
    datasets_ls$group_lup <- datasets_ls$group_lup %>% dplyr::left_join(counts_tb)
    return(datasets_ls)
}
#' Add sports data
#' @description add_sports_data() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add sports data. The function returns Datasets (a list).
#' @param datasets_ls Datasets (a list)
#' @param categories_chr Categories (a character vector), Default: c("Risky", "Subjective", "Team", "Type", "Weighed", "Winter")
#' @param drop_sport_1L_lgl Drop sport (a logical vector of length one), Default: FALSE
#' @return Datasets (a list)
#' @rdname add_sports_data
#' @export 
#' @importFrom dplyr mutate filter pull left_join select
#' @importFrom purrr map_chr map_lgl
#' @keywords internal
add_sports_data <- function (datasets_ls, categories_chr = c("Risky", "Subjective", 
    "Team", "Type", "Weighed", "Winter"), drop_sport_1L_lgl = FALSE) 
{
    datasets_ls$sports_tb <- make_sports_tb(datasets_ls, categories_chr = categories_chr)
    datasets_ls$group_lup <- make_sports_groups(datasets_ls, 
        categories_chr = categories_chr)
    datasets_ls$grouped_tb <- make_sports_groups(datasets_ls, 
        categories_chr = categories_chr, simple_1L_lgl = FALSE)
    datasets_ls$appointments <- datasets_ls$appointments %>% 
        dplyr::mutate(Group = purrr::map_chr(Sport, ~{
            sport_1L_chr <- .x
            dplyr::filter(datasets_ls$group_lup, Sports %>% purrr::map_lgl(~sport_1L_chr %in% 
                .x)) %>% dplyr::pull(Group)
        }))
    datasets_ls$appointments <- dplyr::left_join(datasets_ls$appointments, 
        datasets_ls$grouped_tb)
    if (drop_sport_1L_lgl) 
        datasets_ls$appointments <- datasets_ls$appointments %>% 
            dplyr::select(-Sport)
    datasets_ls <- add_counts(datasets_ls)
    return(datasets_ls)
}
