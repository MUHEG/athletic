make_sports_tb <- function(datasets_ls,
                           categories_chr = c("Risky", "Subjective", "Team",  "Type", "Weighed", "Winter")) {
  sports_tb <- tibble::tibble(Sport = datasets_ls$appointments$Sport %>% sort() %>% unique()) %>%
    dplyr::mutate(Risky = dplyr::case_when(Sport %in% c("Equestrian", "Rugby", "Skateboarding") ~ T, # Eq, Wheeled Motor, Roller, Rugby, AFL
                                           Sport %in% c("Miscellaneous", "Sport no longer funded") ~ NA,
                                           T ~ F),
                  Subjective = dplyr::case_when(Sport %in% c("Artistic Swimming", "Diving", "Gymnastics", "Surfing") ~ T,
                                                Sport %in% c("Miscellaneous", "Sport no longer funded") ~ NA,
                                                T ~ F),
                  Team = dplyr::case_when(Sport %in% c("Artistic Swimming", "Baseball", "Basketball", "Football", "Hockey", "Netball", "Rugby", "Sailing", "Softball", "Volleyball") ~ T,
                                          Sport %in% c("Miscellaneous", "Sport no longer funded") ~ NA,
                                          T ~ F),
                  Type = dplyr::case_when(Sport %in% c("Archery", "Equestrian", "Golf", "Sailing", "Table Tennis") ~ "Skill",
                                          Sport %in% c("Basketball", "Football", "Rugby", "Soccer", "Volleyball", "Waterpolo"  ) ~ "Mixed",# check meaning of football
                                          Sport %in% c("Cycling","Triathlon") ~ "Endurance",
                                          T ~ NA_character_), # manually categorise "Artistic Swimming", "Badminton" , "Baseball" , "Boccia/Bowls", "Combat sports", "Diving", "Gymnastics", Hockey", "Netball" , "Paddle sports",  "Skateboarding", "Softball" , "Surfing"
                  Weighed = dplyr::case_when(Sport %in% c("Combat sports") ~ T,
                                             Sport %in% c("Miscellaneous", "Sport no longer funded") ~ NA,
                                             T ~ F),
                  Winter = dplyr::case_when(Sport %in% c("Winter sports") ~ T, #
                                            T ~ F))
  sports_tb <- sports_tb %>% dplyr::select(tidyselect::all_of(c("Sport", categories_chr)))
  return(sports_tb)
}
make_sports_groups <- function(datasets_ls,
                               categories_chr = c("Risky", "Subjective", "Team",  "Type", "Weighed", "Winter"),
                               simple_1L_lgl = TRUE){
  sports_tb <- datasets_ls$sports_tb
  combinations_tb <- sports_tb %>% dplyr::select(tidyselect::all_of(categories_chr))
  distinct_tb <- combinations_tb %>% dplyr::distinct()
  distinct_tb <- distinct_tb %>% dplyr::mutate(Group = paste0("Grouping ",1:nrow(distinct_tb)))
  grouped_tb <- purrr::map_dfr(1:nrow(sports_tb),
                               ~{
                                 sliced_tb <- sports_tb %>% dplyr::slice(.x) %>% dplyr::select(tidyselect::all_of(c("Sport", categories_chr)))
                                 filtered_tb <- purrr::reduce(setdiff(names(sliced_tb),"Sport"),
                                                              .init = distinct_tb,
                                                              ~ {
                                                                test_1L_lgl <- sliced_tb %>% dplyr::pull(!!rlang::sym(.y))
                                                                dplyr::filter(.x, !!rlang::sym(.y) %>% purrr::map_lgl(~identical(.x,test_1L_lgl)))

                                                              })
                                 filtered_tb
                                 if("Sport" %in% names(sliced_tb)){
                                   filtered_tb <- dplyr::mutate(filtered_tb, Sport = sliced_tb$Sport) %>%
                                     dplyr::select(Sport, dplyr::everything())
                                 }
                                 filtered_tb
                               }) %>% dplyr::arrange(Group %>% stringr::str_replace_all("Grouping ","") %>% as.numeric())
  if(simple_1L_lgl){
    grouped_tb <- grouped_tb %>% dplyr::group_by(Group) %>%
      dplyr::summarise(Sports = list(paste0(Sport))) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(Group %>% stringr::str_replace_all("Grouping ","") %>% as.numeric())
  }
  return(grouped_tb)
}
