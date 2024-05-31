add_counts <- function(datasets_ls,
                       sum_chr = c("Role")){
  counts_tb <- datasets_ls$appointments$Group %>% unique() %>% purrr::map(~datasets_ls$appointments %>% dplyr::filter(Group == .x)) %>%
    purrr::map_dfr(~{
      partial_tb <- .x
      purrr::map_dfc(sum_chr,
                     ~{
                       var_1L_chr <- .x
                       datasets_ls$appointments %>% dplyr::pull(var_1L_chr) %>% unique() %>% sort() %>%
                         purrr::map_dfc(~ {
                           filtered_tb <- partial_tb %>%
                             dplyr::filter(!!rlang::sym(var_1L_chr) == .x) %>%
                             dplyr::summarise(!!rlang::sym(.x) := dplyr::n())
                         } )
                     }) %>%
        dplyr::mutate(Group = partial_tb$Group[1])
    })
  datasets_ls$group_lup <- datasets_ls$group_lup %>% dplyr::left_join(counts_tb)
  return(datasets_ls)
}
add_sports_data <- function(datasets_ls,
                            categories_chr = c("Risky", "Subjective", "Team",  "Type", "Weighed", "Winter"),
                            drop_sport_1L_lgl = FALSE,
                            path_1L_chr = character(0),
                            sports_1L_int = 81,
                            sport_var_1L_chr = "Medlinks Sport categories",
                            sum_chr = c("Role")){
  if(identical(path_1L_chr, character(0))){
    datasets_ls$sports_tb <- make_sports_tb(datasets_ls, categories_chr = categories_chr)
  }else{
    datasets_ls$sports_tb <- get_raw_data(path_1L_chr,
                                          sports_1L_int = sports_1L_int,
                                          sheets_int = 2) %>% purrr::pluck(1) %>%
      dplyr::rename(Sport = tidyr::all_of(sport_var_1L_chr)) %>%
      dplyr::select(tidyr::all_of(c("Sport",categories_chr))) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.logical))

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
                            sum_chr = sum_chr)
  return(datasets_ls)
}
