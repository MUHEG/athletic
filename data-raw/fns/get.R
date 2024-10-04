get_raw_data <- function(path_1L_chr,
                         referrals_cols_int = 4L:8L,
                         sheets_ls = NULL,
                         sheets_int = 1L,
                         tabs_chr = character(0)
){
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
