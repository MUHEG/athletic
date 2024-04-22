get_raw_data <- function(path_1L_chr,
                         appointments_rows_1L_int = 30,
                         referrals_cols_int = 4:8,
                         referrals_rows_1L_int  = 9,
                         cancellations_rows_1L_int  = 247,
                         retainer_rows_1L_int  = 12,
                         neuropsychological_rows_1L_int  = 1,
                         notes_rows_1L_int  = 3
){
  datasets_ls <- purrr::map(1:6,
                            ~readxl::read_xlsx(path_1L_chr,
                                               sheet = .x)) %>% stats::setNames(c("appointments","referrals","cancellations","retainer", "neuropsychological","notes"))
  datasets_ls <- datasets_ls %>% purrr::map2(names(datasets_ls),
                                             ~ {
                                               index_1L_int <- switch(.y,
                                                                      "appointments" = appointments_rows_1L_int,
                                                                      "referrals" = referrals_rows_1L_int,
                                                                      "cancellations" = cancellations_rows_1L_int,
                                                                      "retainer" = retainer_rows_1L_int,
                                                                      "neuropsychological" = neuropsychological_rows_1L_int,
                                                                      "notes" = notes_rows_1L_int)
                                               ds_tb <- .x %>% dplyr::slice(1:index_1L_int)
                                               if(.y == "referrals"){
                                                 ds_tb <- ds_tb[,referrals_cols_int]
                                               }
                                               ds_tb
                                             })
  return(datasets_ls)
}
