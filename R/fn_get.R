#' Get raw data
#' @description get_raw_data() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get raw data. The function returns Datasets (a list).
#' @param path_1L_chr Path (a character vector of length one)
#' @param appointments_rows_1L_int Appointments rows (an integer vector of length one), Default: 30
#' @param referrals_cols_int Referrals columns (an integer vector), Default: 4:8
#' @param referrals_rows_1L_int Referrals rows (an integer vector of length one), Default: 9
#' @param cancellations_rows_1L_int Cancellations rows (an integer vector of length one), Default: 247
#' @param retainer_rows_1L_int Retainer rows (an integer vector of length one), Default: 12
#' @param neuropsychological_rows_1L_int Neuropsychological rows (an integer vector of length one), Default: 1
#' @param notes_rows_1L_int Notes rows (an integer vector of length one), Default: 3
#' @param sports_1L_int Sports (an integer vector of length one), Default: integer(0)
#' @param sheets_int Sheets (an integer vector), Default: 1
#' @return Datasets (a list)
#' @rdname get_raw_data
#' @export 
#' @importFrom purrr map map2
#' @importFrom readxl read_xlsx
#' @importFrom stats setNames
#' @importFrom dplyr slice
#' @keywords internal
get_raw_data <- function (path_1L_chr, appointments_rows_1L_int = 30, referrals_cols_int = 4:8, 
    referrals_rows_1L_int = 9, cancellations_rows_1L_int = 247, 
    retainer_rows_1L_int = 12, neuropsychological_rows_1L_int = 1, 
    notes_rows_1L_int = 3, sports_1L_int = integer(0), sheets_int = 1) 
{
    if (identical(sports_1L_int, integer(0))) {
        sports_1L_chr <- character(0)
    }
    else {
        sports_1L_chr <- "sports_tb"
    }
    datasets_ls <- purrr::map(sheets_int, ~readxl::read_xlsx(path_1L_chr, 
        sheet = .x)) %>% stats::setNames(c("appointments", sports_1L_chr, 
        "referrals", "cancellations", "retainer", "neuropsychological", 
        "notes")[sheets_int])
    datasets_ls <- datasets_ls %>% purrr::map2(names(datasets_ls), 
        ~{
            index_1L_int <- switch(.y, appointments = appointments_rows_1L_int, 
                sports_tb = sports_1L_int, referrals = referrals_rows_1L_int, 
                cancellations = cancellations_rows_1L_int, retainer = retainer_rows_1L_int, 
                neuropsychological = neuropsychological_rows_1L_int, 
                notes = notes_rows_1L_int)
            ds_tb <- .x %>% dplyr::slice(1:index_1L_int)
            if (.y == "referrals") {
                ds_tb <- ds_tb[, referrals_cols_int]
            }
            ds_tb
        })
    return(datasets_ls)
}
