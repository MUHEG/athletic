make_betas_plot <- function(data_ls, #ath_coef_plot
                            alpha_1L_dbl = 1,
                            bar_width_1L_dbl = 0.25,
                            colours_chr = NULL,
                            dot_size_1L_dbl = 2.5,
                            drop_var_1_chr = NULL,
                            drop_var_2_chr = NULL,
                            gap_1L_dbl = 0.1,
                            grouping_1L_lgl = FALSE,
                            hline_xx = ggplot2::geom_hline(yintercept=0, colour = "grey40"),
                            keep_var_1_chr = NULL, keep_var_2_chr = NULL,
                            labels_chr = NULL,
                            legend_name_1L_chr = "Models",
                            line_size_1L_dbl = 0.5,
                            line_width_1L_dbl = 0,
                            plot_type_1L_chr = "dot",
                            point_shapes_int = NULL,
                            reference_row_1L_lgl = TRUE,
                            shade_1L_lgl = FALSE,
                            style_1L_chr = "lancet",
                            theme_choice_xx = ggplot2::theme_classic(base_family = "Arial Narrow"),
                            title_1L_chr = NULL,
                            type_1L_chr = "ggsci",
                            vertical_1L_lgl = FALSE,
                            wrap_width_1L_dbl = 8,
                            x_label_1L_chr = "",
                            y_label_1L_chr = "Estimates and 95% CIs") {

  # Initialize an empty ggplot object
  plot_plt <- ggplot2::ggplot()
  # Conditionally add the hline if hline is not NULL
  if (!is.null(hline_xx)) {
    plot_plt <- plot_plt + hline_xx
  }
  # Define the reference x positions based on unique labels in the first dataset
  reference_labels_chr <- unique(data_ls[[1]]$label)
  reference_x_positions_chr <- stats::setNames(1:length(reference_labels_chr), reference_labels_chr)
  # Calculate the centered offset based on the number of datasets
  n_datasets_1L_int <- length(data_ls)
  center_offset_1L_dbl <- (n_datasets_1L_int - 1) / 2  # Center point for even/odd numbers of datasets
  # Get the original order of `var_label` values from the first dataset
  original_var_order_chr <- unique(data_ls[[1]]$var_label)
  # Ensure colours is a list of appropriate length
  colours_chr <- ready4use::get_colour_codes(n_datasets_1L_int, manual_chr = colours_chr, style_1L_chr = style_1L_chr, type_1L_chr = type_1L_chr)
  # Ensure labels is a list of appropriate length
  if (is.null(labels_chr)) {
    labels_chr <- paste0("Model ", seq_len(n_datasets_1L_int))  # Default labels if labels is NULL
  } else if (length(labels) < n_datasets_1L_int) {
    labels_chr <- c(labels_chr, paste0("Model ", seq_len(n_datasets_1L_int - length(labels_chr))))  # Fill with default labels
  }
  # Set label_colours using updated colours and labels
  labels_colours_chr <- stats::setNames(colours_chr[1:n_datasets_1L_int], labels_chr[1:n_datasets_1L_int])
  # Ensure point_shapes is a list of appropriate length
  if (is.null(point_shapes_int)) {
    point_shapes_int <- rep(16, n_datasets_1L_int)  # Default to shape 16 if not provided
  } else if (length(point_shapes_int) < n_datasets_1L_int) {
    point_shapes_int <- c(point_shapes_int, rep(16, n_datasets_1L_int - length(point_shapes_int)))  # Fill with default
  }
  # Loop through each dataset in the list
  for (i in seq_along(data_ls)) {
    data_tb <- data_ls[[i]]
    model_label <- names(labels_colours_chr)[i]  # Use the model's name as label
    main_color <- labels_colours_chr[model_label]
    shape_1L_int <- point_shapes_int[[i]]  # Assign shape for each dataset
    # Drop and filter conditions as before
    if (!is.null(drop_var_1_chr)) data_tb <- data_tb[!data_tb$term %in% drop_var_1_chr, ]
    if (!is.null(drop_var_2_chr)) data_tb <- data_tb[!data_tb$variable %in% drop_var_2_chr, ]
    if (!is.null(keep_var_1_chr)) data_tb <- data_tb[data_tb$term %in% keep_var_1_chr, ]
    if (!is.null(keep_var_2_chr)) data_tb <- data_tb[data_tb$variable %in% keep_var_2_chr, ]
    if (!reference_row_1L_lgl) data_tb <- data_tb[!data_tb$reference_row_lgl, ]

    # Map labels and adjust x positions
    data_tb$x_position <- as.numeric(reference_x_positions_chr[as.character(data_tb$label)]) + (i - 1 - center_offset_1L_dbl) * gap_1L_dbl
    data_tb$model <- factor(model_label, levels = model_label)
    data_tb <- data_tb %>% mutate(var_label = factor(var_label, levels = original_var_order_chr))

    # Add shaded area or error bars based on shade argument
    if (shade_1L_lgl && plot_type_1L_chr != "bar") {
      plot_plt <- plot_plt +
        ggplot2::geom_ribbon(
          data = data_tb,
          mapping = ggplot2::aes(x = x_position, ymin = conf.low, ymax = conf.high, fill = model),
          alpha = alpha_1L_dbl * 0.3, color = NA
        )
    } else if (!shade_1L_lgl) {
      plot_plt <- plot_plt +
        ggplot2::geom_errorbar(
          data = data_tb,
          mapping = ggplot2::aes(x = x_position, ymin = conf.low, ymax = conf.high, color = model),
          size = line_size_1L_dbl, color = colorspace::lighten(main_color, 0.2), width = line_width_1L_dbl
        )
    }

    # Main plot type with customizable point shape for each dataset
    if (plot_type_1L_chr == "dot") {
      plot_plt <- plot_plt +
        ggplot2::geom_point(
          data = data_tb,
          mapping = ggplot2::aes(x = x_position, y = estimate, color = model),
          size = dot_size_1L_dbl,
          shape = shape_1L_int,  # Use shape for each dataset
          alpha = alpha_1L_dbl
        )

    } else if (plot_type_1L_chr == "connected") {
      plot_plt <- plot_plt +
        ggplot2::geom_line(
          data = data_tb,
          mapping = ggplot2::aes(x = x_position, y = estimate, color = model, group = model),
          size = line_size_1L_dbl, alpha = alpha_1L_dbl
        ) +
        ggplot2::geom_point(
          data = data_tb,
          mapping = ggplot2::aes(x = x_position, y = estimate, color = model),
          size = dot_size_1L_dbl, shape = shape_1L_int, alpha = alpha_1L_dbl
        )

    } else if (plot_type_1L_chr == "bar") {
      plot_plt <- plot_plt +
        ggplot2::geom_bar(
          data = data_tb,
          mapping = ggplot2::aes(x = x_position, y = estimate, fill = model),
          stat = "identity", width = bar_width_1L_dbl, alpha = alpha_1L_dbl,
          position = position_dodge(width = gap_1L_dbl)
        )
    }
  }

  # Create a unified color and fill scale for the legend
  legend_scale <- list(
    ggplot2::scale_color_manual(values = labels_colours_chr, name = legend_name_1L_chr),
    ggplot2::scale_fill_manual(values = labels_colours_chr, name = legend_name_1L_chr)
  )

  # Add axis labels, title, and legend scale to the plot
  plot_plt <- plot_plt +
    ggplot2::scale_x_continuous(
      breaks = reference_x_positions_chr,
      labels = function(labels) stringr::str_wrap(names(reference_x_positions_chr), width = wrap_width_1L_dbl),
      expand = c(0, gap_1L_dbl * 2)
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma, n.breaks = 6) +
    legend_scale +
    ggplot2::theme(panel.grid.minor = element_blank()) +
    ggplot2::labs(x = x_label_1L_chr, y = y_label_1L_chr, title = title_1L_chr)

  # Conditionally add facet_grid based on grouping_1L_lgl and apply theme adjustments
  if (grouping_1L_lgl) {
    if (vertical_1L_lgl) {
      plot_plt <- plot_plt +
        ggplot2::facet_grid(rows = vars(var_label), scales = "free", switch = "y", space = "free_y") +
        ggplot2::coord_flip() +
        theme_choice_xx +
        ggplot2::theme(strip.placement = "outside", strip.background = ggplot2::element_blank(),
                       strip.text.y.left = ggplot2::element_text(angle = 0))
    } else {
      plot_plt <- plot_plt +
        ggplot2::facet_grid(. ~ var_label, scales = "free", switch = "x", space = "free_x") +
        theme_choice_xx +
        ggplot2::theme(strip.placement = "outside", strip.background = ggplot2::element_blank())
    }
  } else if (vertical_1L_lgl) {
    plot_plt <- plot_plt + ggplot2::coord_flip() + theme_choice_xx
  } else {
    plot_plt <- plot_plt + theme_choice_xx
  }
  return(plot_plt)
}
make_betas_tbls_ls <- function(models_ls, labels_ls, indices_int = NULL, element_names_chr = NULL) { #ath_coef_tbl

  # Only subset models_ls if necessary
  if (!is.null(element_names_chr) || !is.null(indices_int)) {
    models_ls <- models_ls[intersect(names(models_ls), element_names_chr)][indices_int]
  }

  # Transform models with tidy_plus_plus using vapply for efficiency if structure is consistent
  transformed_tables_ls <- lapply(models_ls, function(model_mdl) {
    broom.helpers::tidy_plus_plus(model_mdl, variable_labels = labels_ls)
  })

  return(transformed_tables_ls)
}
make_clinical_vars <- function(activity_1L_chr = "Activity",
                               clinical_team_1L_chr = "Clinical Team",
                               clinician_1L_chr = "Clinician",
                               clinician_discipline_1L_chr = "Service",
                               duration_1L_chr = "Duration",
                               exclude_chr = character(0),
                               referrers_1L_chr = "Referrer Role",
                               severity_1L_chr = "Severity",
                               team_disciplines_1L_chr = "Disciplines"){

  clinical_vars_chr <- c(referrers_1L_chr, team_disciplines_1L_chr, clinical_team_1L_chr, clinician_discipline_1L_chr, clinician_1L_chr, activity_1L_chr, duration_1L_chr, severity_1L_chr)
  if(!identical(exclude_chr, character(0))){
    clinical_vars_chr <- setdiff(clinical_vars_chr, exclude_chr)
  }
  return(clinical_vars_chr)
}
make_continuous_dss_ls <- function(datasets_ls, outcomes_chr, vars_chr, method_1L_chr = "mean", labels_ls = NULL, tab_spanner_chr = NULL) { # ath_tbl_cont_merge_perDataset
  # Loop over each outcome to create merged tables
  merged_tables_ls <- lapply(datasets_ls, function(ds_tb) {

    # Generate tables for each dataset
    tables_ls <- lapply(outcomes_chr, function(outcome_1L_chr) {
      make_continuous_smry(data_tb = ds_tb, outcome_1L_chr = rlang::sym(outcome_1L_chr),
                           vars_chr = vars_chr, method_1L_chr = method_1L_chr, labels_ls = labels_ls)
    })

    # Merge tables with specified tab spanner for each dataset
    gtsummary::tbl_merge(tbls = tables_ls, tab_spanner = tab_spanner_chr)

  }) %>% stats::setNames(names(datasets_ls))

  # Return the list of merged tables for each outcome
  return(merged_tables_ls)
}
make_continuous_smry <- function(data_tb, outcome_1L_chr, vars_chr, method_1L_chr = "mean", labels_ls = NULL, pval_method_1L_chr = NULL, pval_1L_lgl = TRUE) { # ath_tbl_continuous
  # Define summary stat type (mean or median) for continuous vars
  statistic_continuous_1L_chr <- switch(method_1L_chr,
                                        "mean" = "{mean} ({sd})",
                                        "median" = "{median} ({p25} - {p75})",
                                        stop("Invalid method. Choose 'mean' or 'median'."))

  # Define default p-value method if not provided
  if (is.null(pval_method_1L_chr)) {
    pval_method_1L_chr <- if (method_1L_chr == "mean") "oneway.test" else "kruskal.test"
  }

  # Generate summary table (excluding all subjects)
  table_xx <- data_tb %>%
    gtsummary::tbl_continuous(
      variable = {{ outcome_1L_chr }},
      statistic = ~statistic_continuous_1L_chr,
      include = vars_chr,
      label = labels_ls
    )

  # Conditionally add p-values
  if (pval_1L_lgl) {
    table_xx <- table_xx %>%
      gtsummary::add_p(test = vars_chr ~ pval_method_1L_chr)
  }
  # Return table
  return(table_xx)
}
make_dictionary_lups <- function(periods_chr = paste0("Year",1:3),
                                 period_1L_chr = "Year"){
  category_lup <- tibble::tribble(~ old_nms_chr, ~ new_nms_chr,
                                  "Severity", "Clinical",
                                  "Severity_7_to_12_plus_Disc" , "Clinical",
                                  "Age", "Demographic",
                                  "Role", "Demographic",
                                  "Sex", "Demographic",
                                  "Active", "Healthcare",
                                  "Active_6", "Healthcare",
                                  "Activity", "Healthcare",
                                  "Appointments", "Healthcare",
                                  "Cancellations", "Healthcare",
                                  "Cost", "Financial",
                                  "CumulativeAppointments", "Healthcare",
                                  "CumulativeCancellations",  "Healthcare",
                                  "CumulativeCost",  "Healthcare",
                                  "CumulativeEpisodes", "Healthcare",
                                  "CumulativeEpisodes_6", "Healthcare",
                                  "CumulativeReferrals", "Healthcare",
                                  "CumulativeSeparations",  "Healthcare",
                                  "CumulativeSeparations_6", "Healthcare",
                                  "Episodes", "Healthcare",
                                  "Episodes_6", "Healthcare",
                                  "ProviderID", "Healthcare",
                                  "Referrals", "Healthcare",
                                  "Referrer", "Healthcare",
                                  "Separations", "Healthcare",
                                  "Separations_6", "Healthcare",
                                  "Service", "Temporal",
                                  "Tenure", "Temporal",
                                  "UID", "Identifier",
                                  "ProviderState", "Spatial",
                                  "Aesthetic", "Sporting",
                                  "Categorisation", "Sporting",
                                  "Individual", "Sporting",
                                  "Para", "Sporting",
                                  "Winter", "Sporting",
                                  "Date", "Temporal",
                                  "Day", "Temporal",
                                  "Duration", "Temporal",
                                  "FiscalQuarter", "Temporal",
                                  "FiscalYQ",  "Temporal",
                                  "FiscalYear", "Temporal",
                                  "Month", "Temporal",
                                  "Quarter", "Temporal",
                                  "Week", "Temporal",
                                  "Weekday", "Temporal",
                                  "Year", "Temporal",
                                  "DEPsychology", "Healthcare",
                                  "Dietetics", "Healthcare",
                                  "Psychiatry", "Healthcare",
                                  "Psychology", "Healthcare",
                                  "Clinicians", "Healthcare",
                                  "Retainer", "Financial",
                                  "CumulativeRetainer", "Financial",
                                  "CumulativeClinicians", "Healthcare"
  ) %>%
    ready4show::ready4show_correspondences()
  description_lup <- tibble::tribble(~ old_nms_chr, ~ new_nms_chr,
                                     "Severity", "Clinical severity (derived from 12 month service use history) - base case definition",
                                     "Severity_7_to_12_plus_Disc" , "Clinical severity (derived from 12 month service use history) - sensitivity definition",
                                     "Age", "Age in years",
                                     "Role", "Staff, athlete or supporter role",
                                     "Sex", "Sex",
                                     "Active", "Currently active client - base case definition",
                                     "Active_6", "Currently active client - sensitivity definition",
                                     "Activity", "Type of service activity",
                                     "Appointments", "Number of appointments on this date",
                                     "Cancellations", "Number of cancellations on this date",
                                     "Cost", "Cost incurred on this date",
                                     "CumulativeAppointments", "Cumulative number of appointments to date",
                                     "CumulativeCancellations",  "Cumulative number of cancellations to date",
                                     "CumulativeCost",  "Cumulative cost to date",
                                     "CumulativeEpisodes", "Cumulative number of episodes of care to date - base case definition",
                                     "CumulativeEpisodes_6", "Cumulative number of episodes of care to date - sensitivity definition",
                                     "CumulativeReferrals", "Cumulative number of referrals to date",
                                     "CumulativeSeparations",  "Cumulative number of separations to date - base case definition",
                                     "CumulativeSeparations_6", "Cumulative number of separations to date - sensitivity definition",
                                     "Episodes", "Number of episodes of care opened on this date - base case definition",
                                     "Episodes_6", "Number of episodes of care opened on this date - sensitivity definition",
                                     "ProviderID", "Unique identifier of provider of clinical service",
                                     "Referrals", "Number of referrals on this date",
                                     "Referrer", "Referral source",
                                     "Separations", "Number of separations made on this date - base case definition",
                                     "Separations_6", "Number of separations made on this date - sensitivity definition",
                                     "Service", "Type of clinical service provided",
                                     "Tenure", "Total length of time between index service activity and latest service activity",
                                     "UID", "Unique identifier",
                                     "ProviderState", "State and Territory of the provider of the clinical service",
                                     "Aesthetic", "Involved in an aesthetic sport",
                                     "Categorisation", "Sporting categorisation",
                                     "Individual", "Involved in an individual sport",
                                     "Para", "A para-athlete",
                                     "Winter", "Involved in a winter sport",
                                     "Date", "Date",
                                     "Day", "Date day, month and year",
                                     "Duration", "Duration of service encounter",
                                     "FiscalQuarter", "Date fiscal year quarter number",
                                     "FiscalYQ",  "Date fiscal year end year and quarter",
                                     "FiscalYear", "Date fiscal year",
                                     "Month", "Date calendar year month",
                                     "Quarter", "Date calendar year year quarter",
                                     "Week", "Date calendar year year and the week number",
                                     "Weekday", "Date day of the week",
                                     "Year", "Date calendar year",
                                     "DEPsychology", "Number of disordered eating psychology appointments on this date",
                                     "Dietetics", "Number of dietetics appointments on this date",
                                     "Psychiatry", "Number of psychiatry appointments on this date",
                                     "Psychology", "Number of pscyhology (excluding disordered eating psychology) appointments on this date",
                                     "Clinicians", "Number of clinicians added to network on this date",
                                     "Retainer", "Annual retainer fees incurred on this date",
                                     "CumulativeRetainer", "Cumulative retainer fees incurred up to this date",
                                     "CumulativeClinicians", "Cumulative number of clinicians added to network up to this date"
  ) %>%
    ready4show::ready4show_correspondences()
  if(!identical(periods_chr, character(0))){
    base_vars_chr <- c("Appointments", "Cancellations", "Cost",
                       "DEPsychology", "Dietetics", "Episodes",
                       "Psychiatry", "Psychology", "Referrals", "Separations" )
    extra_vars_chr <- purrr::reduce(periods_chr,.init = character(0),
                                    ~c(.x,
                                       paste0(.y,
                                              base_vars_chr
                                       )))
    base_repeated_chr <- periods_chr %>% purrr::reduce(.init = extra_vars_chr, ~ .x %>% stringr::str_remove_all(.y))
    prefixes_chr <- base_vars_chr %>% purrr::reduce(.init = extra_vars_chr,
                                                    ~ .x %>% stringr::str_remove_all(.y))
    extra_ctgs_chr <- base_repeated_chr %>% purrr::map_chr(~ready4::get_from_lup_obj(category_lup,
                                                                                     match_var_nm_1L_chr = "old_nms_chr",
                                                                                     match_value_xx = .x,
                                                                                     target_var_nm_1L_chr = "new_nms_chr"))
    extra_descs_chr <- base_repeated_chr %>% purrr::map2_chr(prefixes_chr,
                                                             ~ready4::get_from_lup_obj(description_lup,
                                                                                       match_var_nm_1L_chr = "old_nms_chr",
                                                                                       match_value_xx = .x,
                                                                                       target_var_nm_1L_chr = "new_nms_chr") %>%
                                                               stringr::str_replace_all("on this date", paste0("during ", period_1L_chr %>% tolower(),
                                                                                                               " ", stringr::str_remove_all(.y, period_1L_chr)
                                                               )))
    category_lup <- ready4show::renew.ready4show_correspondences(category_lup, old_nms_chr = extra_vars_chr, new_nms_chr = extra_ctgs_chr)
    description_lup <- ready4show::renew.ready4show_correspondences(description_lup, old_nms_chr = extra_vars_chr, new_nms_chr = extra_descs_chr)
  }
  category_lup <- dplyr::arrange(category_lup, old_nms_chr)
  description_lup <- dplyr::arrange(description_lup, old_nms_chr)
  dictionary_lups_ls <- list(var_ctg_chr = category_lup,
                             var_desc_chr = description_lup)
  return(dictionary_lups_ls)
}
make_fake_clients <- function(datasets_ls = NULL,
                              add_sports_1L_lgl = T,
                              age_var_nm_1L_chr = "Age",###
                              annual_referrals_int = c(350, 500, 550),
                              appointments_rows_1L_int = 30,
                              athlete_roles_chr = c("Athlete", "AlumniAthlete"),###
                              burn_from_1L_chr  = "2020-03-01",
                              burn_referrals_int = c(200,250),
                              cancellations_probs_chr = c(0.05,0.05,0.03,0.08,0.1),
                              cancellations_rows_1L_int = 247,
                              categories_chr = make_sports_categories(),###
                              cleanse_ages_1L_lgl = T,###
                              clinician_dbl = c(0.15,0.06,0.01,0.06,0.68), ###
                              clinicians_int = c(10, 5, 3, 4, 50), ###
                              clinicians_severity_1L_int = 2, ###
                              durations_chr = c("Under 30 mins", "Between 30 and 45 mins", "Between 45 and 75 mins", "50 mins"),
                              end_date_1L_chr = "2024-03-30",
                              neuropsychological_rows_1L_int = 1, ###
                              prefix_1L_chr = "Client_",
                              retain_1L_dbl = 0.3,
                              scale_1L_dbl = 100,
                              missing_1L_chr = character(0),
                              moderate_int = c(2L,4L),###
                              notes_rows_1L_int = 3L,
                              para_probs_dbl = c(0.2, 0.3),
                              path_1L_chr = character(0),
                              referrals_cols_int = 4:8,
                              retainer_rows_1L_int = 12,
                              sessions_cuts_ls = list(c(1,2),c(3,5),c(6,10),c(10,15),c(16,20),c(20,25)),
                              sessions_probs_ls = purrr::map(1:5,~c(0.06,0.26,0.35,0.17,0.07,0.09)),
                              sessions_moderate_int = c(4,15), ###
                              share_at_weekend_dbl = 0.05,
                              share_by_age_athletes_dbl = c(0.05,0.25,0.25,0.2,0.15,0.1),###
                              share_by_age_other_dbl = c(0.0,0.05,0.1,0.15,0.15,0.55),###
                              share_by_quarter_dbl = c(0.2,0.25,0.25,0.3),
                              share_by_reason_dbl = c(0.15,0.06,0.01,0.06,0.68),
                              share_by_referrer_dbl = c(rep(0.1,3),rep(0.06,2),0.1,0.08,0.1,0.3),
                              share_by_role_dbl = c(0.12,0.65,0.01,0.2,0.02),
                              share_by_sex_dbl = c(0.05,0.55,0.4),
                              share_is_para_1L_dbl = 0.28,###
                              sheets_int = c(1,3:8),
                              sports_1L_int = integer(0),###
                              sports_tab_1L_int = 2L,###
                              start_date_1L_chr = "2022-03-01",
                              uid_vars_chr = c("Client ID", "UID"),
                              unit_cost_lup = NULL){
  sheets_ls <- list(appointments = c(1L,appointments_rows_1L_int),
                    referrals = c(1L,9L),
                    cancellations = c(1L,cancellations_rows_1L_int),
                    retainer = c(1L,retainer_rows_1L_int),
                    neuropsychological = c(1L,neuropsychological_rows_1L_int),
                    notes = c(1L,notes_rows_1L_int),
                    mbs = c(1L,10L))
  if(!identical(sports_1L_int, integer(0))){
    sheets_ls <- append(sheets_ls, list(sports_tb = c(1, sports_1L_int)), after = 1)
  }
  if(is.null(datasets_ls)){
    datasets_ls <- get_raw_data(path_1L_chr = path_1L_chr,
                                sheets_ls = sheets_ls,
                                referrals_cols_int = referrals_cols_int,
                                sheets_int = sheets_int)
    if("Client role" %in% names(datasets_ls$appointments))
      datasets_ls$appointments <- datasets_ls$appointments %>% dplyr::rename(Role = `Client role`)
    if("Referrer role" %in% names(datasets_ls$referrals)){
      datasets_ls$referrals <- datasets_ls$referrals %>% dplyr::rename(`Referrer Role` = `Referrer role`)
    }
    if(!identical(missing_1L_chr, character(0))){
      datasets_ls$appointments <- datasets_ls$appointments %>%
        dplyr::mutate(dplyr::across(where(is.character), ~stringr::str_replace_all(.x, missing_1L_chr, NA_character_)))
    }
    if(cleanse_ages_1L_lgl){
      datasets_ls$appointments$`Age` <- update_fake_ages(datasets_ls$appointments$`Age`)
    }
    if(add_sports_1L_lgl){
      datasets_ls <- add_sports_data(datasets_ls,
                                     categories_chr = categories_chr,
                                     path_1L_chr = path_1L_chr,
                                     sports_1L_int = sports_1L_int,
                                     sports_tab_1L_int = sports_tab_1L_int,
                                     drop_sport_1L_lgl = T)
    }else{
      datasets_ls$appointments <- datasets_ls$appointments %>%
        dplyr::mutate(dplyr::across(tidyselect::any_of(categories_chr),
                                    ~ .x %>% as.numeric() %>% as.logical()))
    }
  }
  if(length(unique(uid_vars_chr)) != length(uid_vars_chr)){
    stop("Unique identifier variable names cannot be the same for all datasets")
  }
  if(is.null(unit_cost_lup)){
    unit_cost_lup <- tibble::tibble(Cost = c(160, 220, 280, 200),
                                    Duration = durations_chr)
  }
  sessions_probs_ls <- sessions_probs_ls %>% stats::setNames(datasets_ls$referrals$`Referral type` %>% unique() %>% sort())
  df <- data.frame(Date = seq(lubridate::as_date(burn_from_1L_chr), lubridate::as_date(end_date_1L_chr),by='days'))
  df <- serious::add_date_vars(df,
                      date_var_1L_chr = "Date")
  lamdas_lup <- c(burn_referrals_int,annual_referrals_int) %>% purrr::map2_dfr(df$Year %>% unique() %>% sort(),
                                                                               ~tibble::tibble(Year = .y,  Quarter = 1:4,
                                                                                               N_Quarter_dbl = share_by_quarter_dbl * .x,0)) %>%
    dplyr::mutate(lambda_Weekdays_dbl = N_Quarter_dbl*(1 - share_at_weekend_dbl)*7/365.25,
                  lambda_Weekends_dbl = (N_Quarter_dbl *share_at_weekend_dbl)*7/365.25) %>%
    dplyr::mutate(uid_chr = paste0(Year,"_",Quarter)) %>%
    dplyr::select(uid_chr, dplyr::everything())
  df <- df %>% dplyr::mutate(Referrals_int = purrr::pmap_dbl(df,~ rpois(1,ready4::get_from_lup_obj(lamdas_lup,
                                                                                                   match_value_xx = paste0(..3,"_",..5),
                                                                                                   match_var_nm_1L_chr = "uid_chr",
                                                                                                   target_var_nm_1L_chr = ifelse(..2 %in% c("Saturday","Sunday"),
                                                                                                                                 "lambda_Weekends_dbl",
                                                                                                                                 "lambda_Weekdays_dbl")))))
  df <- as.data.frame(lapply(df, rep, df$Referrals_int)) %>%
    dplyr::select(-Referrals_int)
  df <- youthvars::add_uids_to_tbs_ls(list(df), prefix_1L_chr = prefix_1L_chr, id_var_nm_1L_chr = uid_vars_chr[2]) %>% purrr::pluck(1)
  seed_lup <- datasets_ls$appointments %>%
    dplyr::select(tidyselect::all_of(c(uid_vars_chr[1], intersect(names(datasets_ls$grouped_tb), names(datasets_ls$appointments)))))
  clients_tb <- serious::add_sampled_records(tibble::as_tibble(df), seed_lup = seed_lup, uid_var_nm_1L_chr = uid_vars_chr[1]) %>%
    serious::add_sampled_variable(scale_1L_dbl = scale_1L_dbl,
                         seed_ds_tb = datasets_ls$appointments,
                         shares_dbl = share_by_sex_dbl,
                         uid_var_nm_1L_chr = uid_vars_chr[1],
                         var_nm_1L_chr = "Sex") %>%
    serious::add_sampled_variable(scale_1L_dbl = scale_1L_dbl,
                         seed_ds_tb = datasets_ls$appointments,
                         shares_dbl = share_by_role_dbl ,
                         uid_var_nm_1L_chr = uid_vars_chr[1],
                         var_nm_1L_chr = "Role")
  clients_tb <- list(athlete_roles_chr, setdiff(datasets_ls$appointments$Role %>% unique(), athlete_roles_chr)) %>%
    purrr::map2_dfr(list(share_by_age_athletes_dbl, share_by_age_other_dbl),
                    ~ clients_tb %>% dplyr::filter(Role %in% .x) %>%
                      serious::add_sampled_variable(scale_1L_dbl = scale_1L_dbl,
                                           seed_ds_tb = datasets_ls$appointments,
                                           shares_dbl = .y,
                                           uid_var_nm_1L_chr = uid_vars_chr[1],
                                           var_nm_1L_chr = age_var_nm_1L_chr)) %>%
    dplyr::arrange(Date)
  clients_tb$ParaOne <- runif(nrow(clients_tb))< para_probs_dbl[1]
  clients_tb$ParaTwo <- runif(nrow(clients_tb))< para_probs_dbl[2]
  clients_tb <- clients_tb %>% dplyr::mutate(Para = dplyr::case_when(Role == athlete_roles_chr[1] ~ ParaOne,
                                                                     Role == athlete_roles_chr[2] ~ ParaTwo,
                                                                     T ~ NA))
  clients_tb <- clients_tb %>% dplyr::select(-c(ParaOne,ParaTwo))
  clients_tb <- clients_tb %>%
    serious::add_sampled_variable(scale_1L_dbl = scale_1L_dbl,
                         seed_ds_tb = datasets_ls$referrals,
                         shares_dbl = share_by_referrer_dbl,
                         uid_var_nm_1L_chr = uid_vars_chr[1],
                         var_nm_1L_chr = "Referrer Role")
  clients_tb <- clients_tb %>%
    serious::add_sampled_variable(scale_1L_dbl = scale_1L_dbl,
                         seed_ds_tb = datasets_ls$referrals,
                         shares_dbl = share_by_reason_dbl,
                         uid_var_nm_1L_chr = uid_vars_chr[1],
                         var_nm_1L_chr = "Referral type")
  clients_tb <- clients_tb %>%
    dplyr::rename(Service = "Referral type")
  clinicians_chr <- datasets_ls$referrals$`Referral type` %>% unique() %>% sort()
  clients_tb <- clients_tb %>%
    dplyr::mutate(Activity = "Referral",
                  Appointments = 0,
                  Cancellations = 0,
                  `Referrals` = 1,
                  Clinician = Service %>% purrr::map_chr(~paste0(.x,"_",sample(1:clinicians_int[which(clinicians_chr == .x)], size = 1))),
                  Duration = 0,
                  Cost = 0)
  clients_tb <- dplyr::mutate(clients_tb,
                              Disciplines = Service %>% purrr::map(~{
                                anchor_1L_chr <- .x
                                index_1L_int <- which(clinicians_chr==.x)
                                c(anchor_1L_chr,(clinicians_chr %>% purrr::discard_at(index_1L_int))[clinician_dbl %>% purrr::discard_at(index_1L_int) %>%
                                                                                                       purrr::map_lgl(~.x >  runif(1))])

                              }))
  clients_tb <- clinicians_chr %>%
    purrr::reduce(.init = clients_tb,
                  ~ {
                    total_nm_1L_chr <- paste0("Annual Sessions ", .y)
                    clinician_1L_chr <- .y
                    .x %>% dplyr::mutate(!!rlang::sym(total_nm_1L_chr) := dplyr::case_when(Disciplines %>%
                                                                                             purrr::map_lgl(~clinician_1L_chr %in% .x) ~ rep(clinician_1L_chr, nrow(clients_tb)) %>%
                                                                                             purrr::map_int(~{
                                                                                               probs_dbl <- sessions_probs_ls %>% purrr::pluck(.x)
                                                                                               draw_1L_dbl <- runif(1)
                                                                                               if (draw_1L_dbl<1){
                                                                                                 index_1L_int <- (which(1:length(probs_dbl) %>% purrr::map_dbl(~probs_dbl[1:.x] %>% sum())>draw_1L_dbl))[1]
                                                                                               }else{
                                                                                                 index_1L_int <- length(probs_dbl)
                                                                                               }
                                                                                               limits_int <- sessions_cuts_ls %>% purrr::pluck(index_1L_int)
                                                                                               truncnorm::rtruncnorm(1, a = limits_int[1], b = limits_int[2], mean = mean(limits_int)) %>% round()
                                                                                             }),
                                                                                           TRUE ~ 0
                    ))

                  })
  clients_tb <- dplyr::mutate(clients_tb, !!rlang::sym(paste0("Annual Sessions ", clinicians_chr[5])) := !!rlang::sym(paste0("Annual Sessions ", clinicians_chr[1])) %>%
                                purrr::map2_int(!!rlang::sym(paste0("Annual Sessions ", clinicians_chr[5])),
                                                ~ max(.y-.x,0))
  )
  clients_tb <- dplyr::mutate(clients_tb, `Annual Sessions All Psychology` = !!rlang::sym(paste0("Annual Sessions ", clinicians_chr[5])) + !!rlang::sym(paste0("Annual Sessions ", clinicians_chr[1])))
  clients_tb <- dplyr::mutate(clients_tb, `Annual Sessions All` = dplyr::select(clients_tb, paste0("Annual Sessions ", clinicians_chr)) %>% rowSums(na.rm = TRUE))
  clients_tb <- dplyr::mutate(clients_tb, Severity = clients_tb %>% dplyr::select(c(`Annual Sessions All`, paste0("Annual Sessions ",clinicians_chr[moderate_int]))) %>%
                                purrr::pmap_chr(~ifelse(..1 < sessions_moderate_int[1] && sum(..2,..3) ==0,
                                                        "Mild",
                                                        ifelse(..1> sessions_moderate_int[2],
                                                               "Severe",
                                                               "Moderate"))))
  clients_tb <- dplyr::mutate(clients_tb, `Clinical Team` = Disciplines %>% purrr::map2(Clinician,~{
    disciplines_chr <- .x
    additional_chr <- disciplines_chr[-1] %>%
      purrr::map_chr(~paste0(.x,"_",sample(1:clinicians_int[which(clinicians_chr == .x)], size = 1)))
    c(.y, additional_chr)
  }))
  clients_tb <- clients_tb %>% dplyr::select(tidyselect::any_of(c(uid_vars_chr[2],
                                                                  "Date", "Weekday",  "Year", "Week","Quarter",
                                                                  "Activity", "Referrer Role", "Disciplines", "Clinical Team", "Service", "Clinician", "Duration",
                                                                  "Referrals", "Appointments", "Cancellations",
                                                                  c(paste0("Annual Sessions ", clinicians_chr), "Annual Sessions All Psychology",  "Annual Sessions All"),"Cost",
                                                                  "Severity", "Role", "Para", age_var_nm_1L_chr, "Sex", "Group")),
                                             dplyr::everything())
  clients_tb <- clients_tb %>% dplyr::select(-uid_vars_chr[1])

  appointments_tb <- clinicians_chr %>%
    purrr::reduce(.init = clients_tb %>% dplyr::filter(F),
                  ~ {
                    discipline_1L_chr <- .y
                    starter_tb <- .x
                    expanded_tb <- tibble::as_tibble(lapply(clients_tb %>% dplyr::mutate(Activity = "Appointment"), rep,
                                                            clients_tb %>% dplyr::pull(!!rlang::sym(paste0("Annual Sessions ", discipline_1L_chr))))) %>%
                      dplyr::mutate(Service = discipline_1L_chr)
                    expanded_tb <- expanded_tb %>%
                      dplyr::mutate(Clinician = expanded_tb$Disciplines %>% purrr::map2_chr(expanded_tb$`Clinical Team`,
                                                                                            ~{.y[which(.x==discipline_1L_chr)]}))
                    expanded_tb <- expanded_tb %>%
                      dplyr::group_by(!!rlang::sym(uid_vars_chr[2])) %>%
                      dplyr::mutate(Add_Weeks = round(52/!!rlang::sym(paste0("Annual Sessions ", discipline_1L_chr))) %>%
                                      purrr::map_int(~round(truncnorm::rtruncnorm(1,a=1,b=.x, mean=4))))
                    cumulatives_int <- expanded_tb %>% dplyr::pull(!!rlang::sym(uid_vars_chr[2]))%>% unique() %>%
                      purrr::map(~ expanded_tb %>% dplyr::filter(!!rlang::sym(uid_vars_chr[2]) == .x)  %>%
                                   dplyr::pull(Add_Weeks) %>% serious::calculate_running_totals())  %>% purrr::flatten() %>% purrr::flatten_int()
                    expanded_tb <-  expanded_tb %>%
                      dplyr::ungroup()
                    expanded_tb <- expanded_tb %>% dplyr::mutate(Add_Weeks = cumulatives_int)
                    expanded_tb <- expanded_tb %>%
                      dplyr::mutate(Date = Date + lubridate::weeks(Add_Weeks)) %>%
                      dplyr::select(-Add_Weeks)

                    expanded_tb <- dplyr::bind_rows(starter_tb,expanded_tb)
                  }
    ) %>%
    dplyr::mutate(Referrals = 0,
                  Appointments = 1)
  appointments_tb <- appointments_tb %>%
    dplyr::mutate(Duration = dplyr::case_when(Service == clinicians_chr[4] ~ sample(durations_chr[1:3], size = nrow(appointments_tb), replace = T),
                                              TRUE ~ durations_chr[4]))

  appointments_tb <- appointments_tb %>%
    dplyr::mutate(Cost = Duration %>% purrr::map2_dbl(Service, ~ready4::get_from_lup_obj(unit_cost_lup,
                                                                                         match_value_xx = .x,
                                                                                         match_var_nm_1L_chr = "Duration",
                                                                                         target_var_nm_1L_chr = "Cost")))

  cancellations_tb <- 1:length(clinicians_chr) %>% purrr::reduce(.init = appointments_tb,
                                                                 ~{
                                                                   runif(nrow(appointments_tb))<cancellations_probs_chr[.y]
                                                                   dplyr::mutate(.x,
                                                                                 !!rlang::sym(paste0("cancel_",
                                                                                                     clinicians_chr[.y],
                                                                                                     "_lgl")) := runif(nrow(appointments_tb))<cancellations_probs_chr[.y]
                                                                   )

                                                                 })
  cancellations_lgl <- cancellations_tb$Service %>%
    purrr::map2_lgl(1:nrow(cancellations_tb),
                    ~ {
                      discipline_1L_chr <- .x
                      cancellations_tb %>% dplyr::slice(.y) %>% dplyr::pull(!!rlang::sym(paste0("cancel_",
                                                                                                discipline_1L_chr,
                                                                                                "_lgl")))
                    })
  cancellations_tb <- dplyr::mutate(cancellations_tb,
                                    Activity = dplyr::case_when(cancellations_lgl ~ "Cancellation",
                                                                T ~ Activity)) %>%
    dplyr::select(-tidyselect::all_of(paste0("cancel_",
                                             clinicians_chr,
                                             "_lgl"))) %>%
    dplyr::filter(Activity == "Cancellation")

  cancellations_tb <- cancellations_tb %>%
    dplyr::inner_join(clients_tb %>% dplyr::rename(ENTRY_DATE = Date) %>% dplyr::select(c(uid_vars_chr[2], "ENTRY_DATE"))) %>%
    dplyr::mutate(ADD_DAYS = round(runif(nrow(cancellations_tb),min = -30, max = 30),0),
                  NEW_DATE = Date + lubridate::days(ADD_DAYS)) %>%
    dplyr::mutate(Date = dplyr::case_when(ENTRY_DATE > NEW_DATE ~ ENTRY_DATE,
                                          T ~ NEW_DATE)) %>%
    dplyr::select(-c(NEW_DATE, ADD_DAYS, ENTRY_DATE))
  cancellations_tb <- cancellations_tb %>%
    dplyr::mutate(Duration = "0") %>%
    dplyr::mutate(dplyr::across(c("Appointments", "Referrals"), ~0)) %>%
    dplyr::mutate(Cancellations = 1)
  cancellations_tb <- cancellations_tb$Year %>% unique() %>% sort() %>%
    purrr::reduce(.init = cancellations_tb %>% dplyr::filter(FALSE),
                  ~ rbind(.x,
                          dplyr::filter(cancellations_tb,Year == .y) %>%
                            dplyr::group_by(!!rlang::sym(uid_vars_chr[2])) %>%
                            dplyr::mutate(Cumulative_Cancellations = cumsum(Cancellations)) %>%
                            dplyr::ungroup())) %>%
    dplyr::mutate(Cost = dplyr::case_when(Cumulative_Cancellations < 3 ~ Cost*0.5,
                                          T ~ 0)) %>%
    dplyr::select(-Cumulative_Cancellations)
  clients_tb <- rbind(clients_tb, appointments_tb, cancellations_tb) %>%
    dplyr::arrange(Date) %>%
    dplyr::select(-dplyr::starts_with("Annual Sessions") )
  clients_tb <- serious::add_date_vars(clients_tb, date_var_1L_chr = "Date") %>%
    dplyr::filter(!Date > end_date_1L_chr) %>%
    dplyr::filter(!Date < start_date_1L_chr)

  return(clients_tb)

}
make_focused_args <- function(activity_1L_chr = "Activity",
                              athlete_roles_chr = c("Athlete", "AlumniAthlete"),
                              appointments_var_1L_chr = "Appointments",
                              cancellations_var_1L_chr = "Cancellations",
                              clinical_team_1L_chr = "Clinical Team",
                              clinician_1L_chr = "Clinician",
                              clinician_discipline_1L_chr = "Service",
                              components_chr = c("Year","Quarter", "Week"),
                              cost_var_1L_chr = "Cost",
                              date_tfmn_fn = identity,
                              days_1L_chr = "Weekday",
                              duration_1L_chr = "Duration",
                              exclude_chr = "Group",#character(0),
                              fiscal_start_1L_int = 7L,
                              group_1L_chr = character(0),
                              index_1L_chr = "Date",
                              is_wide_1L_lgl = F,
                              key_vars_chr = character(0),
                              metrics_chr = make_metric_vars(),
                              referrals_var_1L_chr = "Referrals",
                              referrers_1L_chr = "Referrer Role",
                              severity_1L_chr = "Severity",
                              team_disciplines_1L_chr = "Disciplines",
                              uid_var_1L_chr = "UID",
                              what_1L_chr = c("all", "totals")){
  what_1L_chr <- match.arg(what_1L_chr)
  focused_args_ls <- list(
    activity_1L_chr = activity_1L_chr,
    athlete_roles_chr = athlete_roles_chr,
    appointments_var_1L_chr = appointments_var_1L_chr,
    cancellations_var_1L_chr = cancellations_var_1L_chr,
    clinical_team_1L_chr = clinical_team_1L_chr,
    clinician_1L_chr = clinician_1L_chr,
    clinician_discipline_1L_chr = clinician_discipline_1L_chr,
    components_chr = components_chr,
    cost_var_1L_chr = cost_var_1L_chr,
    date_tfmn_fn = date_tfmn_fn,
    days_1L_chr = days_1L_chr,
    duration_1L_chr = duration_1L_chr,
    exclude_chr = exclude_chr,#character(0),
    group_1L_chr = group_1L_chr,
    index_1L_chr = index_1L_chr,
    is_wide_1L_lgl = is_wide_1L_lgl,
    key_vars_chr = key_vars_chr,
    metrics_chr = metrics_chr,
    referrals_var_1L_chr = referrals_var_1L_chr,
    referrers_1L_chr = referrers_1L_chr,
    severity_1L_chr = severity_1L_chr,
    team_disciplines_1L_chr = team_disciplines_1L_chr,
    uid_var_1L_chr = uid_var_1L_chr,
    what_1L_chr = what_1L_chr
  )
  return(focused_args_ls)
}
make_grouped_boxplot_ls <- function(plot_data_ls, outcomes_chr, labels_ls) { # ath_boxplot_group
  plots_ls <- list()

  for (outcome_1L_chr in outcomes_chr) {
    outcome_label_1L_chr <- labels_ls[[outcome_1L_chr]]

    plots_ls[[outcome_1L_chr]] <- list(
      "Age and Sex" =
        ggpubr::ggarrange(
          plotlist = plot_data_ls[[outcome_1L_chr]][c("Age_alt", "Sex")],
          nrow = 1, widths = c(2, 1)
        ) %>% ggpubr::annotate_figure(top = outcome_label_1L_chr) ,

      "Role and Provider State" =
        ggpubr::ggarrange(
          plotlist = plot_data_ls[[outcome_1L_chr]][c("Role", "ProviderState")],
          nrow = 2
        ) %>% ggpubr::annotate_figure(top = outcome_label_1L_chr),

      "Others" =
        ggpubr::ggarrange(
          plotlist = plot_data_ls[[outcome_1L_chr]][c("Aesthetic", "Individual", "Para", "Winter")],
          nrow = 1
        ) %>% ggpubr::annotate_figure(top = outcome_label_1L_chr)
    )
  }
  return(plots_ls)
}
make_keepers <- function(names_chr,
                         clinical_vars_chr = make_clinical_vars(),
                         keep_cdn_1L_chr = c("All", "Personal", "Provider", "Severity", "Sports"),
                         severity_1L_int = 8L,
                         sports_vars_chr = get_sports_vars()){
  keep_cdn_1L_chr <- match.arg(keep_cdn_1L_chr)
  keep_chr <- names_chr
  if(keep_cdn_1L_chr != "All"){
    if(keep_cdn_1L_chr == "Personal")
      keep_chr <-setdiff(keep_chr, c(clinical_vars_chr, sports_vars_chr))
    if(keep_cdn_1L_chr == "Provider")
      keep_chr <- intersect(keep_chr, clinical_vars_chr[-severity_1L_int])
    if(keep_cdn_1L_chr == "Severity")
      keep_chr <- intersect(keep_chr, clinical_vars_chr[severity_1L_int])
    if(keep_cdn_1L_chr == "Sports")
      keep_chr <- intersect(keep_chr, sports_vars_chr)
  }
  return(keep_chr)
}
make_keys_dss <- function(data_tb,
                          key_vars_chr,
                          activity_1L_chr = "Activity",
                          athlete_roles_chr = c("Athlete", "AlumniAthlete"),
                          appointments_var_1L_chr = "Appointments",
                          cancellations_var_1L_chr = "Cancellations",
                          clinical_team_1L_chr = "Clinical Team",
                          clinician_1L_chr = "Clinician",
                          clinician_discipline_1L_chr = "Service",
                          components_chr = c("Year","Quarter", "Week"),
                          cost_var_1L_chr = "Cost",
                          days_1L_chr = "Weekday",
                          duration_1L_chr = "Duration",
                          exclude_chr = "Group",#character(0),
                          fns_ls = NULL,
                          group_1L_chr = character(0),
                          index_1L_chr = "Date", # rename as index
                          periods_chr = c("sub","daily", "weekly", "monthly", "quarterly", "yearly"),
                          referrals_var_1L_chr = "Referrals",
                          referrers_1L_chr = "Referrer Role",
                          severity_1L_chr = "Severity",
                          team_disciplines_1L_chr = "Disciplines",
                          uid_var_1L_chr = "UID"){
  if(is.null(fns_ls)){
    fns_ls <- list(sub = lubridate::ymd_hms, daily = identity, weekly = tsibble::yearweek,
                   monthly = tsibble::yearmonth, quarterly = tsibble::yearquarter, yearly = lubridate::year)
  }
  focused_args_ls <- make_focused_args(activity_1L_chr = activity_1L_chr,
                                       athlete_roles_chr = athlete_roles_chr,
                                       appointments_var_1L_chr = appointments_var_1L_chr,
                                       cancellations_var_1L_chr = cancellations_var_1L_chr,
                                       clinical_team_1L_chr = clinical_team_1L_chr,
                                       clinician_1L_chr = clinician_1L_chr,
                                       clinician_discipline_1L_chr = clinician_discipline_1L_chr,
                                       components_chr = components_chr,
                                       cost_var_1L_chr = cost_var_1L_chr,
                                       days_1L_chr = days_1L_chr,
                                       duration_1L_chr = duration_1L_chr,
                                       exclude_chr = exclude_chr,
                                       group_1L_chr = group_1L_chr,
                                       index_1L_chr = index_1L_chr,
                                       metrics_chr = c(referrals_var_1L_chr, appointments_var_1L_chr, cancellations_var_1L_chr, cost_var_1L_chr),
                                       referrals_var_1L_chr = referrals_var_1L_chr,
                                       referrers_1L_chr = referrers_1L_chr,
                                       severity_1L_chr = severity_1L_chr,
                                       team_disciplines_1L_chr = team_disciplines_1L_chr,
                                       uid_var_1L_chr = uid_var_1L_chr,
                                       what_1L_chr = "all")
  selected_ls <- fns_ls %>% purrr::keep_at(periods_chr)
  totals_dss_ls <- selected_ls %>% purrr::map(~{
    iteration_args_ls <- focused_args_ls
    date_tfmn_fn <- .x
    iteration_args_ls$date_tfmn_fn <- date_tfmn_fn
    iteration_args_ls
    key_vars_chr %>%
      purrr::map(~{
        innerloop_args_ls <- iteration_args_ls
        innerloop_args_ls$key_vars_chr <- .x
        data_tb %>%
          serious::transform_to_tsibble(
            date_tfmn_fn = date_tfmn_fn, ###
            focused_args_ls = focused_args_ls,
            focused_fn = transform_to_focused_tsb,
            index_1L_chr = index_1L_chr,
            key_vars_chr = .x, ###
            metrics_chr = c(referrals_var_1L_chr, appointments_var_1L_chr, cancellations_var_1L_chr, cost_var_1L_chr),
            type_1L_chr = "focused"
          )
      }) %>%
      stats::setNames(key_vars_chr)
  }) %>% stats::setNames(names(selected_ls))
  return(totals_dss_ls)
}
make_key_vars <- function(add_chr = character(0),
                          drop_chr = character(0),
                          sort_1L_lgl = FALSE){
  key_vars_chr <- c("Role", "Age",  "Sex", "Categorisation", "Para",
                    "Aesthetic", "Individual", "Winter", "Referrer",
                    "Service", "ProviderState", "ProviderID", "Severity", "Tenure", add_chr) %>% setdiff(drop_chr)
  if(sort_1L_lgl)
    key_vars_chr <- sort(key_vars_chr)
  return(key_vars_chr)
}
make_imputed_dataset <- function(X_Ready4useDyad,
                                 Y_Ready4useDyad){

  imputed_1_xx <- mice::mice(Y_Ready4useDyad@ds_tb %>% dplyr::mutate(dplyr::across(c(Role, Age, Sex, Categorisation, Referrer, Service, ProviderState, ProviderID, Severity), ~ as.factor(.x))), method = "rf", m = 1, maxit = 1)
  Z_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb",
                               mice::complete(imputed_1_xx) %>% tibble::as_tibble() %>% dplyr::mutate(dplyr::across(c(Role, Age, Sex, Categorisation, Referrer, Service, ProviderState, ProviderID, Severity), ~ as.character(.x))))
  Z_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                               ready4use::add_from_lup_prototype(X_Ready4useDyad@ds_tb,
                                                                 lup_prototype_tb = Z_Ready4useDyad@ds_tb, match_var_nm_1L_chr = "UID",
                                                                 type_1L_chr = c("self"), vars_chr = c("Role", "Age", "Sex", "Categorisation", "Referrer")) %>%  #, Service, ProviderState, ProviderID, Severity
                                 dplyr::arrange(UID, Date))
  imputed_2_xx <- mice::mice(Z_Ready4useDyad@ds_tb %>% dplyr::mutate(dplyr::across(c(ProviderID), ~ as.factor(.x))), method = "rf", m = 1, maxit = 1)
  Z_Ready4useDyad <- renewSlot(Z_Ready4useDyad, "ds_tb",
                               mice::complete(imputed_2_xx) %>% tibble::as_tibble() %>% dplyr::mutate(dplyr::across(c(ProviderID), ~ as.character(.x))))
  providers_lup <- X_Ready4useDyad@ds_tb %>% dplyr::select(ProviderID, ProviderState) %>% na.omit() %>% dplyr::distinct()
  Z_Ready4useDyad <- renewSlot(Z_Ready4useDyad, "ds_tb",
                               Z_Ready4useDyad@ds_tb %>% dplyr::mutate(ProviderState = dplyr::case_when(is.na(ProviderState) ~ purrr::map_chr(ProviderID, ~ ready4::get_from_lup_obj(providers_lup,
                                                                                                                                                                                     match_value_xx = .x,
                                                                                                                                                                                     match_var_nm_1L_chr = "ProviderID",
                                                                                                                                                                                     target_var_nm_1L_chr = "ProviderState")),
                                                                                                        TRUE ~ ProviderState)))
  severity_lup <- add_severity(Z_Ready4useDyad@ds_tb, severity_args_ls = severity_args_ls) %>% dplyr::select(UID, Date, Severity, Severity_7_to_12_plus_Disc) %>% na.omit() %>% dplyr::distinct()
  Z_Ready4useDyad <- renewSlot(Z_Ready4useDyad, "ds_tb",
                               Z_Ready4useDyad@ds_tb %>%
                                 dplyr::mutate(Severity = dplyr::case_when(is.na(Severity) ~ Z_Ready4useDyad@ds_tb %>% dplyr::select(UID, Date, Severity) %>% purrr::pmap_chr( ~ {
                                   filtered_tb <- severity_lup %>%  dplyr::filter(UID == ..1) %>% dplyr::filter(Date>=..2)
                                   filtered_tb$Severity[1]
                                 }),
                                 TRUE ~ Severity)) %>%
                                 dplyr::mutate(Severity_7_to_12_plus_Disc = dplyr::case_when(is.na(Severity_7_to_12_plus_Disc) ~ Z_Ready4useDyad@ds_tb %>% dplyr::select(UID, Date, Severity_7_to_12_plus_Disc) %>% purrr::pmap_chr( ~ {
                                   filtered_tb <- severity_lup %>%  dplyr::filter(UID == ..1) %>% dplyr::filter(Date>=..2)
                                   filtered_tb$Severity_7_to_12_plus_Disc[1]
                                 }),
                                 TRUE ~ Severity_7_to_12_plus_Disc)))
  return(Z_Ready4useDyad)
}
make_linked_ds <- function (datasets_ls = NULL,
                            disciplines_1L_lgl = TRUE,
                            end_date_dtm = lubridate::ymd("2024-06-30"),
                            exclude_chr = c("Cost", "Duration"),
                            imputed_uid_pfx_chr = "UNK",
                            keep_all_1L_lgl = FALSE,
                            missing_1L_chr = "0",
                            path_1L_chr = character(0),
                            price_indices_dbl = numeric(0),
                            price_ref_1L_int = 1L,
                            provider_id_1L_chr = "ProviderID",
                            provider_location_1L_chr = "ProviderState",
                            referrals_cols_int = 4:7,
                            separation_after_dbl = 3,
                            sessions_moderate_int = c(4, 15),
                            severity_args_ls = NULL,
                            severity_var_1L_chr = "Severity",
                            sheets_int = 1:5,
                            uid_pfx_1L_chr = "CID",
                            uid_vars_chr = c("MedlinksID",
                                             "AISID"),
                            unit_1L_chr = "month",
                            var_ctg_chr = character(0),
                            what_1L_chr = c("table", "dyad")) {
  what_1L_chr <- match.arg(what_1L_chr)
  if (is.null(datasets_ls)) {
    datasets_ls <- get_raw_data(path_1L_chr = path_1L_chr,
                                referrals_cols_int = referrals_cols_int, sheets_int = sheets_int)
  }
  datasets_ls <- update_ingested_data(datasets_ls, categories_chr = c("Individual Sports",
                                                                      "Aesthetic Sports", "Winter Sports"), exclude_chr = exclude_chr,
                                      imputed_uid_pfx_chr = imputed_uid_pfx_chr, missing_1L_chr = missing_1L_chr,
                                      provider_id_1L_chr = provider_id_1L_chr, provider_location_1L_chr = provider_location_1L_chr,
                                      uid_vars_chr = uid_vars_chr)
  data_tb <- dplyr::bind_rows(datasets_ls$appointments, datasets_ls$cancellations,
                              datasets_ls$referrals) %>% dplyr::arrange(Date)
  data_tb <- serious::add_temporal_vars(data_tb, date_var_1L_chr = "Date",
                                        fiscal_start_1L_int = 7L)
  data_tb <- serious::add_new_uid(data_tb, drop_old_uids_1L_lgl = T,
                                  arrange_by_1L_chr = "Date", imputed_uid_pfx_chr = imputed_uid_pfx_chr,
                                  recode_1L_lgl = T, uid_pfx_1L_chr = uid_pfx_1L_chr, uid_vars_chr = uid_vars_chr)
  data_tb <- data_tb %>% serious::add_tenure(date_var_1L_chr = "Date",
                                             tenure_var_1L_chr = "Tenure", uid_var_1L_chr = "UID",
                                             unit_1L_chr = "year")
  if (is.null(severity_args_ls)) {
    severity_args_ls <- make_severity_args_ls(disciplines_ls = list(disciplines_1L_lgl),
                                              sessions_ls = list(sessions_moderate_int), names_chr = character(0),
                                              severity_var_1L_chr = severity_var_1L_chr)
  }
  severity_vars_chr <- names(severity_args_ls$sessions_ls)
  data_tb <- data_tb %>% add_severity(provider_var_1L_chr = provider_id_1L_chr,
                                      severity_args_ls = severity_args_ls, severity_var_1L_chr = severity_var_1L_chr)
  data_tb <- data_tb %>% dplyr::mutate(Sex = dplyr::case_when(Sex ==
                                                                "Non-Binary" ~ NA_character_, TRUE ~ Sex), `Referrer Role` = dplyr::case_when(`Referrer Role` ==
                                                                                                                                                "." ~ NA_character_, TRUE ~ `Referrer Role`)) %>% dplyr::mutate(Age = dplyr::case_when(Age ==
                                                                                                                                                                                                                                         "≥35 years" ~ "35 years and over", Age == "<16 years" ~
                                                                                                                                                                                                                                         "0-15 years", TRUE ~ Age))
  data_tb <- add_imputed_costs(data_tb, arrange_by_1L_chr = "Date",
                               provider_id_1L_chr = provider_id_1L_chr)
  data_tb <- data_tb %>% serious::update_for_price_year(price_indices_dbl = price_indices_dbl,
                                                        price_ref_1L_int = price_ref_1L_int)
  data_tb <- data_tb %>% dplyr::rename(Referrer = `Referrer Role`,
                                       Aesthetic = `Aesthetic Sports`, Individual = `Individual Sports`,
                                       Winter = `Winter Sports`)
  data_tb <- data_tb %>% ready4use::add_from_lup_prototype(match_var_nm_1L_chr = "UID",
                                                           method_1L_chr = "sample", vars_chr = c("Referrer", "Role",
                                                                                                  "Sex", "Age", "Categorisation", "Para", "Aesthetic",
                                                                                                  "Individual", "Winter"), type_1L_chr = "self")
  data_tb <- data_tb %>% dplyr::select(UID, Date, Referrer,
                                       Tenure, Role, Sex, Age, Categorisation, Para, Aesthetic,
                                       Individual, Winter, !!!rlang::syms(severity_vars_chr),
                                       Service, !!rlang::sym(provider_id_1L_chr), !!rlang::sym(provider_location_1L_chr),
                                       Activity, Appointments, Cancellations, Referrals, Cost,
                                       Weekday, Week, Quarter, Year, FiscalQuarter, FiscalYear,
                                       dplyr::everything()) %>% dplyr::arrange(UID, Date)
  data_tb <- data_tb %>% dplyr::mutate(Date = Date %>% format() %>%
                                         stringr::str_sub(end = 10) %>% lubridate::ymd())
  if (!keep_all_1L_lgl) {
    data_tb <- data_tb %>% dplyr::select(-c("Para/Able",
                                            "Annual appointments", "Annual DE Psychology Appointments",
                                            "Annual Dietetics Appointments", "Annual Psychiatry Appointments",
                                            "Annual Psychology Appointments", "Annual Disciplines",
                                            "Annual Providers"))
  }
  if (what_1L_chr == "dyad") {
    X <- ready4use::Ready4useDyad(ds_tb = data_tb)
    if (identical(var_ctg_chr, character(0))) {
      var_ctg_chr <- c("Identifier", "Temporal", rep("Healthcare",
                                                     2), rep("Demographic", 3), rep("Sporting", 5),
                       rep("Clinical", length(severity_vars_chr)), rep("Healthcare",
                                                                       2), "Spatial", rep("Healthcare", 5), rep("Temporal",
                                                                                                                10))
    }
    X <- ready4use::add_dictionary(X, var_ctg_chr = var_ctg_chr)
    X <- X %>% serious::add_cumulatives(metrics_chr = c("Appointments",
                                                        "Cancellations", "Referrals", "Cost"), arrange_by_1L_chr = "Date",
                                        group_by_1L_chr = "UID")
    X <- X %>% serious::add_episodes(separation_after_dbl = separation_after_dbl,
                                     end_date_dtm = end_date_dtm, unit_1L_chr = unit_1L_chr)
    episodes_vars_ls <- serious::make_episodes_vars(separation_after_dbl = separation_after_dbl,
                                                    flatten_1L_lgl = F)
    X@ds_tb <- X@ds_tb %>% dplyr::arrange(UID, Date) %>%
      dplyr::select(UID, Date, Referrer, Tenure, Role,
                    Sex, Age, Categorisation, Para, Aesthetic, Individual,
                    Winter, !!!rlang::syms(severity_vars_chr), Service,
                    !!rlang::sym(provider_id_1L_chr), !!rlang::sym(provider_location_1L_chr),
                    Activity, !!!rlang::syms(episodes_vars_ls %>%
                                               purrr::map_chr(~.x[2])), Appointments, Cancellations,
                    Referrals, Cost, !!!rlang::syms(episodes_vars_ls %>%
                                                      purrr::map_chr(~.x[3])), !!!rlang::syms(episodes_vars_ls %>%
                                                                                                purrr::map_chr(~.x[1])), paste0("Cumulative",
                                                                                                                                c(episodes_vars_ls %>% purrr::map_chr(~.x[2]),
                                                                                                                                  "Appointments", "Cancellations", "Referrals",
                                                                                                                                  "Cost", episodes_vars_ls %>% purrr::map_chr(~.x[3]))),
                    dplyr::everything())
    data_xx <- X
  }
  else {
    data_xx <- data_tb
  }
  return(data_xx)
}
make_mdl_outcomes_ds_ls <- function(datasets_ls, outcomes_chr, predictors_1L_chr) { #ath_ols_loop_Data_Outcome
  dataset_ls <- lapply(datasets_ls, function(ds_tb) {
    # Run regressions over each outcome
    outcome_ls <- lapply(outcomes_chr, function(outcome_1L_chr) {
      # Construct the formula
      formula_fml <- as.formula(paste(outcome_1L_chr, "~", predictors_1L_chr))

      # Run the regression and store the result
      estimatr::lm_robust(formula_fml, data = ds_tb, se_type = "HC1")
    }) %>% stats::setNames(outcomes_chr)
    outcome_ls
  }) %>% setNames(names(datasets_ls))

  # Return list
  return(dataset_ls)
}
make_mdl_predictors_ds_ls <- function(ds_tb,
                                      # dataset_1L_chr,
                                      outcomes_chr, predictors_chr) { #ath_ols_loop_Outcome_Predictor
  dataset_ls <- lapply(outcomes_chr, function(outcome_1L_chr) {
    outcome_ls <- lapply(predictors_chr, function(predictor) {
      formula <- as.formula(paste(outcome_1L_chr, "~", predictor))

      estimatr::lm_robust(formula, data = ds_tb, se_type = "HC1")
    }) %>% setNames(predictors_chr)
    outcome_ls
  }) %>% setNames(outcomes_chr)

  # Return list
  return(dataset_ls)
}
make_mdl_smry_tbl <- function(model_mdl, add_glance_1L_lgl = TRUE, statistic_1L_chr = "Coef (SE)", labels_ls = NULL) { #ath_ols_format
  table_xx <- model_mdl %>%
    gtsummary::tbl_regression(label = labels_ls) %>%  # Pass custom labels here
    gtsummary::add_significance_stars(             # Modify p-value stars & show only coef (SE)
      pattern = "{estimate} ({std.error}){stars}",
      hide_ci = TRUE, hide_se = TRUE,
      thresholds = c(0.01, 0.05, 0.1)
    ) %>%
    gtsummary::modify_header(                      # Modify column name
      label ~ "**Characteristic**",
      estimate ~ paste0("**", statistic_1L_chr, "**")
    )
  if(add_glance_1L_lgl){
    table_xx <- table_xx   %>%
      gtsummary::add_glance_table(include = c("nobs", "adj.r.squared")) # Add number of observations & adj r squared
  }
  table_xx <- table_xx   %>%
    gtsummary::modify_footnote(gtsummary::everything() ~ NA, abbreviation = TRUE)
  return(table_xx)
}
make_merged_continuous_smry <- function(datasets_ls, outcomes_chr, vars_chr, method_1L_chr = "mean",
                                        labels_ls = NULL, tab_spanner_chr = NULL, pval_1L_lgl=TRUE, pval_method_1L_chr=NULL) { #ath_tbl_cont_merge_perOutcome
  merged_tables_ls <- lapply(outcomes_chr, function(outcome_1L_chr) {
    tables_ls <- lapply(datasets_ls, function(ds_tb) {
      make_continuous_smry(data_tb = ds_tb, outcome_1L_chr = rlang::sym(outcome_1L_chr),
                           vars_chr = vars_chr, method_1L_chr = method_1L_chr, pval_1L_lgl = pval_1L_lgl , pval_method_1L_chr = pval_method_1L_chr, labels_ls = labels_ls)
    })
    gtsummary::tbl_merge(tbls = tables_ls, tab_spanner = tab_spanner_chr)

  }) %>% stats::setNames(outcomes_chr)
  return(merged_tables_ls)
}
make_merged_mdl_tbl <- function (models_ls, statistics_ls = NULL, tab_spanner_chr = NULL, labels_ls = NULL)
{
  tables_ls <- lapply(seq_along(models_ls), function(i) {
    make_mdl_smry_tbl(model_mdl = models_ls[[i]], statistic_1L_chr = statistics_ls[i],
                      labels_ls = labels_ls)
  })
  merged_table_xx <- gtsummary::tbl_merge(tbls = tables_ls, tab_spanner = tab_spanner_chr)
  return(merged_table_xx)
}
make_modelling_dss <- function(data_tb,
                               activity_1L_chr = "Activity",
                               athlete_roles_chr = c("Athlete", "AlumniAthlete"),
                               appointments_var_1L_chr = "Appointments",
                               cancellations_var_1L_chr = "Cancellations",
                               clinical_team_1L_chr = "Clinical Team",
                               clinician_1L_chr = "Clinician",
                               clinician_discipline_1L_chr = "Service",
                               components_chr = c("Year","Quarter", "Week"),
                               cost_var_1L_chr = "Cost",
                               days_1L_chr = "Weekday",
                               duration_1L_chr = "Duration",
                               exclude_chr = "Group",#character(0),
                               fns_ls = NULL,
                               group_1L_chr = character(0),
                               index_1L_chr = "Date", # rename as index
                               key_vars_chr = character(0),
                               periods_chr = c("sub","daily", "weekly", "monthly", "quarterly", "yearly"),
                               referrals_var_1L_chr = "Referrals",
                               referrers_1L_chr = "Referrer Role",
                               severity_1L_chr = "Severity",
                               team_disciplines_1L_chr = "Disciplines",
                               uid_var_1L_chr = "UID"){
  ### VALIDATION REQ:
  ### No var names should have "__"
  if(identical(key_vars_chr, character(0))){
    key_vars_chr <- get_key_vars(data_tb,
                                 activity_1L_chr = activity_1L_chr,
                                 athlete_roles_chr = athlete_roles_chr,
                                 appointments_var_1L_chr = appointments_var_1L_chr,
                                 cancellations_var_1L_chr = cancellations_var_1L_chr,
                                 clinical_team_1L_chr = clinical_team_1L_chr,
                                 clinician_1L_chr = clinician_1L_chr,
                                 clinician_discipline_1L_chr =clinician_discipline_1L_chr,
                                 components_chr = components_chr,
                                 cost_var_1L_chr = cost_var_1L_chr,
                                 days_1L_chr = days_1L_chr,
                                 duration_1L_chr = duration_1L_chr,
                                 exclude_chr = exclude_chr,#character(0),
                                 group_1L_chr = group_1L_chr,
                                 index_1L_chr = index_1L_chr,
                                 referrals_var_1L_chr = referrals_var_1L_chr,
                                 referrers_1L_chr = referrers_1L_chr,
                                 severity_1L_chr = severity_1L_chr,
                                 team_disciplines_1L_chr = team_disciplines_1L_chr,
                                 uid_var_1L_chr = uid_var_1L_chr)
  }
  key_dss_ls <- make_keys_dss(data_tb,
                              key_vars_chr = key_vars_chr,
                              activity_1L_chr = activity_1L_chr,
                              athlete_roles_chr = athlete_roles_chr,
                              appointments_var_1L_chr = appointments_var_1L_chr,
                              cancellations_var_1L_chr = cancellations_var_1L_chr,
                              clinical_team_1L_chr = clinical_team_1L_chr,
                              clinician_1L_chr = clinician_1L_chr,
                              clinician_discipline_1L_chr = clinician_discipline_1L_chr,
                              components_chr = components_chr,
                              cost_var_1L_chr = cost_var_1L_chr,
                              days_1L_chr = days_1L_chr,
                              duration_1L_chr = duration_1L_chr,
                              exclude_chr = exclude_chr,#character(0),
                              fns_ls = fns_ls,
                              group_1L_chr = group_1L_chr,
                              index_1L_chr = index_1L_chr,
                              periods_chr = periods_chr,
                              referrals_var_1L_chr = referrals_var_1L_chr,
                              referrers_1L_chr = referrers_1L_chr,
                              severity_1L_chr = severity_1L_chr,
                              team_disciplines_1L_chr = team_disciplines_1L_chr,
                              uid_var_1L_chr = uid_var_1L_chr)
  totals_dss_ls <- make_totals_dss(data_tb,
                                   activity_1L_chr = activity_1L_chr,
                                   athlete_roles_chr = athlete_roles_chr,
                                   appointments_var_1L_chr = appointments_var_1L_chr,
                                   cancellations_var_1L_chr = cancellations_var_1L_chr,
                                   clinical_team_1L_chr = clinical_team_1L_chr,
                                   clinician_1L_chr = clinician_1L_chr,
                                   clinician_discipline_1L_chr = clinician_discipline_1L_chr,
                                   components_chr = components_chr,
                                   cost_var_1L_chr = cost_var_1L_chr,
                                   days_1L_chr = days_1L_chr,
                                   duration_1L_chr = duration_1L_chr,
                                   exclude_chr = exclude_chr,#character(0),
                                   fns_ls = fns_ls,
                                   group_1L_chr = group_1L_chr,
                                   index_1L_chr = index_1L_chr,
                                   periods_chr = periods_chr,
                                   referrals_var_1L_chr = referrals_var_1L_chr,
                                   referrers_1L_chr = referrers_1L_chr,
                                   severity_1L_chr = severity_1L_chr,
                                   team_disciplines_1L_chr = team_disciplines_1L_chr,
                                   uid_var_1L_chr = uid_var_1L_chr)
  wide_dss_ls <- make_wide_dss(data_tb,
                               activity_1L_chr = activity_1L_chr,
                               athlete_roles_chr = athlete_roles_chr,
                               appointments_var_1L_chr = appointments_var_1L_chr,
                               cancellations_var_1L_chr = cancellations_var_1L_chr,
                               clinical_team_1L_chr = clinical_team_1L_chr,
                               clinician_1L_chr = clinician_1L_chr,
                               clinician_discipline_1L_chr = clinician_discipline_1L_chr,
                               components_chr = components_chr,
                               cost_var_1L_chr = cost_var_1L_chr,
                               days_1L_chr = days_1L_chr,
                               duration_1L_chr = duration_1L_chr,
                               exclude_chr = exclude_chr,#character(0),
                               fns_ls = fns_ls,
                               group_1L_chr = group_1L_chr,
                               index_1L_chr = index_1L_chr,
                               key_vars_chr = character(0), ##### ???
                               periods_chr = periods_chr,
                               referrals_var_1L_chr = referrals_var_1L_chr,
                               referrers_1L_chr = referrers_1L_chr,
                               severity_1L_chr = severity_1L_chr,
                               team_disciplines_1L_chr = team_disciplines_1L_chr,
                               uid_var_1L_chr = uid_var_1L_chr)
  dss_lss <- list(key_dss_ls = key_dss_ls,
                  totals_dss_ls = totals_dss_ls,
                  wide_dss_ls = wide_dss_ls)
  return(dss_lss)
}
make_nested_boxplot_ls <- function (datasets_ls, labels_ls = NULL, outcomes_ls, predictors_chr)
{
  boxplot_ls_ls <- list(all = make_outcomes_boxplots(data_tb = datasets_ls$all_tb,
                                                     outcomes_chr = outcomes_ls$all_chr,#outcome_all,
                                                     predictors_chr = predictors_chr,
                                                     labels_ls = labels_ls),
                        year1 = make_outcomes_boxplots(data_tb = datasets_ls$year_1_tb,
                                                       outcomes_chr = outcomes_ls$year_1_chr,#outcome_1y,
                                                       predictors_chr = predictors_chr,
                                                       labels_ls = labels_ls),
                        year2 = make_outcomes_boxplots(data_tb = datasets_ls$year_2_tb,
                                                       outcomes_chr = outcomes_ls$year_2_chr,#outcome_2y,
                                                       predictors_chr = predictors_chr,
                                                       labels_ls = labels_ls))
  return(boxplot_ls_ls)
}
make_outcomes_boxplots <- function (outcomes_chr, predictors_chr, data_tb, labels_ls = NULL, palette_1L_chr = "lancet")
{
  scale_fill_fn <- ready4use::get_journal_palette_fn("fill",  what_1L_chr = palette_1L_chr)
  boxplots_ls <- lapply(outcomes_chr, function(outcome_1L_chr) {
    lapply(predictors_chr, function(predictor_1L_chr) {
      data_tb %>% ggplot2::ggplot(ggplot2::aes(x = .data[[predictor_1L_chr]],
                                               y = .data[[outcome_1L_chr]], fill = .data[[predictor_1L_chr]])) +
        ggplot2::geom_boxplot(varwidth = TRUE, alpha = 0.8) +
        ggplot2::theme_classic() + scale_fill_fn() +
        ggplot2::theme(legend.position = "none", panel.background = element_blank(),
                       panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
                       panel.grid.minor.x = element_blank())       +
        ggplot2::scale_y_continuous(labels = scales::comma, breaks = scales::breaks_pretty(6))+
        ggplot2::labs(x = if (!is.null(labels_ls)) labels_ls[[predictor_1L_chr]] else predictor_1L_chr,
                      y = if (!is.null(labels_ls)) labels_ls[[outcome_1L_chr]] else outcome_1L_chr,
                      fill = if (!is.null(labels_ls)) labels_ls[[predictor_1L_chr]] else predictor_1L_chr)
    }) %>% setNames(predictors_chr)
  }) %>% setNames(outcomes_chr)
  return(boxplots_ls)
}
make_processed_providers_dss_ls <- function(data_tb) {
  # Step 1: Collapse to provider outcome/service use
  P_outcome_tb <- data_tb %>%
    dplyr::group_by(ProviderID, ProviderState) %>%
    dplyr::summarise(
      dplyr::across(c(Appointments, Cost, Referrals, Cancellations,
                      Episodes, Episodes_6, Separations, Separations_6), # Cumulative outcomes/uses
                    ~ sum(.x, na.rm = FALSE)),
      dplyr::across(c(Service, ServDEP, ServDiet, ServPsych, ServPsyco), # Type of services
                    ~ max(.x, na.rm = FALSE)),
      DateStart = min(Date, na.rm = TRUE),
      DateEnd = max(Date, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Step 2: Collapse to provider-client level
  P_individual_tb <- data_tb %>%
    dplyr::group_by(ProviderID, ProviderState, UID) %>%
    dplyr::summarise(
      dplyr::across(c(Tenure_years, Active, Active_6,
                      RefSelf, RoleAth, SexFem, Age35o, Age20l, Para, Aesthetic, Individual, Winter,
                      RefSelf_M, RoleAth_M, SexFem_M, Age35o_M, Age20l_M, Para_M, Aesthetic_M, Individual_M, Winter_M),
                    ~ max(.x, na.rm = FALSE))
    )

  # Step 3: Collapse to provider level
  P_char_tb <- P_individual_tb %>%
    dplyr::group_by(ProviderID, ProviderState) %>%
    dplyr::summarise(
      NClients = dplyr::n_distinct(UID),
      dplyr::across(c(Tenure_years, Active, Active_6,
                      RefSelf, RoleAth, SexFem, Age35o, Age20l, Para, Aesthetic, Individual, Winter,
                      RefSelf_M, RoleAth_M, SexFem_M, Age35o_M, Age20l_M, Para_M, Aesthetic_M, Individual_M, Winter_M),
                    ~ mean(.x, na.rm = FALSE))
    )

  # Step 4: Merge with provider outcome
  P_all_tb <- P_char_tb %>%
    dplyr::left_join(P_outcome_tb, by = c("ProviderID", "ProviderState")) %>%
    dplyr::filter(!is.na(ProviderID)) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble()

  # Step 5: Define service type
  P_all_tb <- P_all_tb %>%
    dplyr::mutate(Service = dplyr::if_else(ServPsyco == 1, "Psychology", Service),
                  Service = dplyr::if_else(ServDEP == 1, "DE Psychology", Service),
                  Service = dplyr::if_else(ServDiet == 1, "Dietetics", Service),
                  Service = dplyr::if_else(ServPsych == 1, "Psychiatry", Service)) %>%
    dplyr::mutate(Service2 = dplyr::if_else(Service == "DE Psychology", "Psychology", Service))

  # Step 6: Convert to factor and define reference category
  P_all_tb <- P_all_tb %>%
    dplyr::mutate(Service = stats::relevel(factor(Service), ref = "Psychology"),
                  Service2 = stats::relevel(factor(Service2), ref = "Psychology"),
                  ProviderState = stats::relevel(factor(ProviderState), ref = "ACT"))


  # Step 7: Calculate perClients service use
  P_all_tb <- P_all_tb %>%
    dplyr::mutate(Appointments_perClients = Appointments/NClients,
                  Cost_perClients = Cost/NClients,
                  Episodes_perClients = Episodes/NClients)

  return(P_all_tb)
}
make_regression_tbs_ls <- function (models_ls_ls, outcomes_chr, tab_spanner_chr, dataset_1L_chr, labels_ls = NULL)
{
  models_ls <- lapply(outcomes_chr, function(outcome_1L_chr) {
    list(models_ls_ls$all_c_ls[[paste0(dataset_1L_chr, "_tb")]][[outcome_1L_chr]],
         models_ls_ls$spl_c_ls[[paste0(dataset_1L_chr, "_1y_tb")]][[paste0("Year1", outcome_1L_chr)]],
         models_ls_ls$spl_c_ls[[paste0(dataset_1L_chr, "_2y_tb")]][[paste0("Year1", outcome_1L_chr)]],
         models_ls_ls$spl_c_ls[[paste0(dataset_1L_chr, "_2y_tb")]][[paste0("Year2", outcome_1L_chr)]])
  }) %>% setNames(outcomes_chr)
  model_names_ls <- lapply(outcomes_chr, function(outcome_1L_chr) {
    c(paste("Total", outcome_1L_chr), paste("Year 1", outcome_1L_chr),
      paste("Year 1", outcome_1L_chr), paste("Year 2",
                                             outcome_1L_chr))
  }) %>% setNames(outcomes_chr)
  regression_tbls_ls <- lapply(outcomes_chr, function(outcome_1L_chr) {
    make_merged_mdl_tbl(models_ls = models_ls[[outcome_1L_chr]],
                        statistics_ls = model_names_ls[[outcome_1L_chr]],
                        labels_ls = labels_ls, tab_spanner_chr = tab_spanner_chr)
  }) %>% setNames(outcomes_chr)
  return(regression_tbls_ls)
}

make_rename_lup <- function(){
  lup_ready4show_correspondences <- ready4show::ready4show_correspondences() %>%
    ready4show::renew.ready4show_correspondences(old_nms_chr = c("HP Staff (excluding coaches)", "female", "male", "0-15 years","16-19 years",
                                                                 "20-24 years", "25-29 years",
                                                                 "30-34 years",
                                                                 "35 years and over",
                                                                 "Other HP staff member", "Podium Potential and above"),
                                                 new_nms_chr = c("Other HP", "Female", "Male", "0-15","16-19",
                                                                 "20-24", "25-29",
                                                                 "30-34",
                                                                 "35 + ",
                                                                 "Other HP", "Podium Potential+"))
  return(lup_ready4show_correspondences)

}
make_service_use_vars <- function(X_Ready4useDyad,
                                  active_base_1L_chr = "Active",
                                  patterns_ls = list(c("[[:space:]]", "")),
                                  prefix_1L_chr = "Cumulative",
                                  separation_after_dbl = c(3,6),
                                  service_var_1L_chr = "Service",
                                  tenure_var_1L_chr = "Tenure"){
  service_use_chr <- c(tenure_var_1L_chr,
                       serious::make_metric_vars("eoc",
                                                 separation_after_dbl = separation_after_dbl)[startsWith(serious::make_metric_vars("eoc",
                                                                                                                                   separation_after_dbl = separation_after_dbl),active_base_1L_chr)],
                       serious::make_cumulatives(separation_after_dbl = separation_after_dbl),
                       paste0(prefix_1L_chr, c(unique(X_Ready4useDyad@ds_tb %>% dplyr::pull(!!rlang::sym(service_var_1L_chr))))))
  if(!is.null(patterns_ls)){
    service_use_chr  <- purrr::reduce(patterns_ls,
                                      .init = service_use_chr,
                                      ~ {
                                        .x %>% stringr::str_replace_all(.y[1], .y[2])
                                      })
  }
  return(service_use_chr)
}
make_severity_args_ls <- function(disciplines_ls,
                                  sessions_ls,
                                  names_chr = character(0),
                                  severity_var_1L_chr = "Severity"){
  severity_args_ls <- list(disciplines_ls = disciplines_ls,
                           sessions_ls = sessions_ls)
  if(identical(names_chr, character(0))){
    names_chr <- make_severity_vars(severity_args_ls,
                                    severity_var_1L_chr = severity_var_1L_chr)
  }
  severity_args_ls <- severity_args_ls %>% purrr::map(~.x %>% stats::setNames(names_chr))
  return(severity_args_ls)
}
make_severity_vars <- function(severity_args_ls,
                               severity_var_1L_chr = "Severity"){
  severity_vars_chr <- 1:1:length(severity_args_ls$sessions_ls) %>% purrr::map_chr(~paste0(severity_var_1L_chr,ifelse(.x==1,"",paste0("_",LETTERS[.x-1]))))
  return(severity_vars_chr)
}
make_sngl_predictor_dss_ls <- function(ds_tb, outcomes_chr, predictors_ls) { #ath_ols_loop_Predictor_Outcome
  dataset_ls <- lapply(predictors_ls, function(predictor_1L_chr) {
    outcome_ls <- lapply(outcomes_chr, function(outcome_1L_chr) {
      formula_fml <- as.formula(paste(outcome_1L_chr, "~", predictor_1L_chr))

      estimatr::lm_robust(formula_fml, data = ds_tb, se_type = "HC1")
    }) %>% setNames(outcomes_chr)
    outcome_ls
  }) %>% setNames(names(predictors_ls))

  # Return list
  return(dataset_ls)
}

make_sports_categories <- function(type_1L_int = c("current","deprecated"),
                                   which_int = integer(0)){
  type_1L_int <- match.arg(type_1L_int)
  if(type_1L_int == "current"){
    categories_chr <- c("High burden of injury (TBA)",  "Individual or other", "Criteria-based officiating (aesthetic)", "Winter",
                        "Endurance / long distance",  "Middle distance / power",  "Speed / strength", "Precision / skill-dependent", "Combat / weight-making")
  }
  if(type_1L_int == "deprecated"){
    categories_chr <- c("Risky", "Subjective", "Team", "Type", "Weighed", "Winter")
  }
  if(!identical(which_int, integer(0))){
    categories_chr <- categories_chr[which_int]
  }
  return(categories_chr)
}
make_sports_tb <- function(datasets_ls,
                           categories_chr = make_sports_categories("deprecated")) {
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
                               categories_chr = make_sports_categories(),
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
make_stacked_mdl_tbl <- function (models_ls, statistics_ls = NULL, labels_ls = NULL)
{
  tables_ls <- lapply(seq_along(models_ls), function(i) {
    make_mdl_smry_tbl(model_mdl = models_ls[[i]], add_glance_1L_lgl = TRUE,
                      statistic_1L_chr = statistics_ls[i], labels_ls = labels_ls)
  })
  merged_table_xx <- gtsummary::tbl_stack(tbls = tables_ls)
  return(merged_table_xx)
}
make_tabular_summary <- function(data_tb, vars_chr, labels_ls = NULL, method_1L_chr = "mean", digits_1L_int = 2, missing_1L_chr = "no") { #ath_tbl_summary
  statistic_continuous <- switch(method_1L_chr,
                                 "mean" = "{mean} ({sd})",
                                 "median" = "{median} ({p25} - {p75})",
                                 stop("Invalid method. Choose 'mean' or 'median'."))
  table_xx <- data_tb %>%
    dplyr::select(all_of(vars_chr)) %>%
    gtsummary::tbl_summary(type = list(where(is.numeric) ~ "continuous",
                                       where(is.factor) ~ "categorical"),
                           statistic = list(gtsummary::all_continuous() ~ statistic_continuous, gtsummary::all_categorical() ~ "{n} ({p}%)"),
                           label = labels_ls,
                           digits = list(gtsummary::all_continuous() ~ digits_1L_int,
                                         gtsummary::all_categorical() ~ 0),
                           missing = missing_1L_chr)

  return(table_xx)
}
make_tabular_merge <- function(datasets_ls, vars_chr, labels_ls = NULL, tab_spanner_chr = NULL, method_1L_chr = "mean", digits_1L_int=2) { #ath_tbl_sum_merge
  # Call function: loop after different dataset
  tables_ls <- lapply(datasets_ls, function(dataset_tb) {
    tables_ls <- make_tabular_summary(data_tb = dataset_tb,
                                      vars_chr = vars_chr, labels_ls = labels_ls,
                                      method_1L_chr = method_1L_chr, digits_1L_int=digits_1L_int)
  }) %>% stats::setNames(names(datasets_ls))

  # Create a merged table
  merged_table <- gtsummary::tbl_merge(tbls = tables_ls, tab_spanner = tab_spanner_chr)

  # Return merged table
  return(merged_table)
}
make_totals_dss <- function(data_tb,
                            activity_1L_chr = "Activity",
                            athlete_roles_chr = c("Athlete", "AlumniAthlete"),
                            appointments_var_1L_chr = "Appointments",
                            cancellations_var_1L_chr = "Cancellations",
                            clinical_team_1L_chr = "Clinical Team",
                            clinician_1L_chr = "Clinician",
                            clinician_discipline_1L_chr = "Service",
                            components_chr = c("Year","Quarter", "Week"),
                            cost_var_1L_chr = "Cost",
                            days_1L_chr = "Weekday",
                            duration_1L_chr = "Duration",
                            exclude_chr = "Group",#character(0),
                            fns_ls = NULL,
                            group_1L_chr = character(0),
                            index_1L_chr = "Date", # rename as index
                            #key_vars_chr = character(0),
                            periods_chr = c("sub","daily", "weekly", "monthly", "quarterly", "yearly"),
                            referrals_var_1L_chr = "Referrals",
                            referrers_1L_chr = "Referrer Role",
                            severity_1L_chr = "Severity",
                            team_disciplines_1L_chr = "Disciplines",
                            uid_var_1L_chr = "UID"){
  if(is.null(fns_ls)){
    fns_ls <- serious::make_temporal_fns(daily_fn = identity)
    # list(sub = lubridate::ymd_hms, daily = identity, weekly = tsibble::yearweek,
    #              monthly = tsibble::yearmonth, quarterly = tsibble::yearquarter, yearly = lubridate::year)
  }
  selected_ls <- fns_ls %>% purrr::keep_at(periods_chr)
  totals_dss_ls <- selected_ls %>% purrr::map(~{
    date_tfmn_fn <- .x
    focused_args_ls <- make_focused_args(activity_1L_chr = activity_1L_chr,
                                         athlete_roles_chr = athlete_roles_chr,
                                         appointments_var_1L_chr = appointments_var_1L_chr,
                                         cancellations_var_1L_chr = cancellations_var_1L_chr,
                                         clinical_team_1L_chr = clinical_team_1L_chr,
                                         clinician_1L_chr = clinician_1L_chr,
                                         clinician_discipline_1L_chr = clinician_discipline_1L_chr,
                                         components_chr = components_chr,
                                         cost_var_1L_chr = cost_var_1L_chr,
                                         date_tfmn_fn = date_tfmn_fn,
                                         days_1L_chr = days_1L_chr,
                                         duration_1L_chr = duration_1L_chr,
                                         exclude_chr = exclude_chr,
                                         group_1L_chr = group_1L_chr,
                                         index_1L_chr = index_1L_chr,
                                         is_wide_1L_lgl = F,
                                         # key_vars_chr = character(0),
                                         metrics_chr = c(referrals_var_1L_chr, appointments_var_1L_chr, cancellations_var_1L_chr, cost_var_1L_chr),
                                         referrals_var_1L_chr = referrals_var_1L_chr,
                                         referrers_1L_chr = referrers_1L_chr,
                                         severity_1L_chr = severity_1L_chr,
                                         team_disciplines_1L_chr = team_disciplines_1L_chr,
                                         uid_var_1L_chr = uid_var_1L_chr,
                                         what_1L_chr = "totals")
    data_tb %>%
      serious::transform_to_tsibble(date_tfmn_fn = date_tfmn_fn,
                                    focused_args_ls = focused_args_ls,
                                    focused_fn = transform_to_focused_tsb,
                                    index_1L_chr = index_1L_chr,
                                    key_vars_chr = character(0),
                                    metrics_chr = c(referrals_var_1L_chr, appointments_var_1L_chr, cancellations_var_1L_chr, cost_var_1L_chr),
                                    type_1L_chr = "focused")
  }) %>% stats::setNames(names(selected_ls))
  return(totals_dss_ls)
}
make_wide_dss <- function(data_tb,
                          activity_1L_chr = "Activity",
                          athlete_roles_chr = c("Athlete", "AlumniAthlete"),
                          appointments_var_1L_chr = "Appointments",
                          cancellations_var_1L_chr = "Cancellations",
                          clinical_team_1L_chr = "Clinical Team",
                          clinician_1L_chr = "Clinician",
                          clinician_discipline_1L_chr = "Service",
                          components_chr = c("Year","Quarter", "Week"),
                          cost_var_1L_chr = "Cost",
                          days_1L_chr = "Weekday",
                          duration_1L_chr = "Duration",
                          exclude_chr = "Group",#character(0),
                          fns_ls = NULL,
                          group_1L_chr = character(0),
                          index_1L_chr = "Date", # rename as index
                          key_vars_chr = character(0),
                          periods_chr = c("sub","daily", "weekly", "monthly", "quarterly", "yearly"),
                          referrals_var_1L_chr = "Referrals",
                          referrers_1L_chr = "Referrer Role",
                          severity_1L_chr = "Severity",
                          team_disciplines_1L_chr = "Disciplines",
                          uid_var_1L_chr = "UID"){
  ### VALIDATION REQ:
  ### No var names should have "__"
  if(is.null(fns_ls)){
    fns_ls <- serious::make_temporal_fns(daily_fn = identity)
    # list(sub = lubridate::ymd_hms, daily = identity, weekly = tsibble::yearweek,
    #              monthly = tsibble::yearmonth, quarterly = tsibble::yearquarter, yearly = lubridate::year)
  }
  selected_ls <- fns_ls %>% purrr::keep_at(periods_chr)
  wide_dss_ls <- selected_ls %>% purrr::map(~{
    date_tfmn_fn <- .x
    focused_args_ls <- make_focused_args(activity_1L_chr = activity_1L_chr,
                                         athlete_roles_chr = athlete_roles_chr,
                                         appointments_var_1L_chr = appointments_var_1L_chr,
                                         cancellations_var_1L_chr = cancellations_var_1L_chr,
                                         clinical_team_1L_chr = clinical_team_1L_chr,
                                         clinician_1L_chr = clinician_1L_chr,
                                         clinician_discipline_1L_chr = clinician_discipline_1L_chr,
                                         components_chr = components_chr,
                                         cost_var_1L_chr = cost_var_1L_chr,
                                         date_tfmn_fn = date_tfmn_fn,
                                         days_1L_chr = days_1L_chr,
                                         duration_1L_chr = duration_1L_chr,
                                         exclude_chr = exclude_chr,
                                         group_1L_chr = group_1L_chr,
                                         index_1L_chr = index_1L_chr,
                                         is_wide_1L_lgl = T,
                                         key_vars_chr = key_vars_chr,
                                         metrics_chr = c(referrals_var_1L_chr, appointments_var_1L_chr, cancellations_var_1L_chr, cost_var_1L_chr),
                                         referrals_var_1L_chr = referrals_var_1L_chr,
                                         referrers_1L_chr = referrers_1L_chr,
                                         severity_1L_chr = severity_1L_chr,
                                         team_disciplines_1L_chr = team_disciplines_1L_chr,
                                         uid_var_1L_chr = uid_var_1L_chr,
                                         what_1L_chr = "all")
    data_tb %>% transform_to_prep(
      activity_1L_chr = activity_1L_chr,
      appointments_var_1L_chr = appointments_var_1L_chr,
      cancellations_var_1L_chr = cancellations_var_1L_chr,
      clinical_team_1L_chr = clinical_team_1L_chr,
      clinician_1L_chr = clinician_1L_chr,
      clinician_discipline_1L_chr = clinician_discipline_1L_chr,
      components_chr = components_chr,
      cost_var_1L_chr = cost_var_1L_chr,
      days_1L_chr = days_1L_chr,
      duration_1L_chr = duration_1L_chr,
      exclude_chr = exclude_chr,
      group_1L_chr = group_1L_chr,
      index_1L_chr =  index_1L_chr,
      referrals_var_1L_chr = referrals_var_1L_chr,
      referrers_1L_chr = referrers_1L_chr,
      severity_1L_chr = severity_1L_chr,
      team_disciplines_1L_chr =  team_disciplines_1L_chr,
      uid_var_1L_chr = uid_var_1L_chr,
      what_1L_chr = "wide") %>%
      serious::transform_to_tsibble(date_tfmn_fn = date_tfmn_fn,
                                    index_1L_chr = index_1L_chr,
                                    focused_args_ls = focused_args_ls,
                                    focused_fn = transform_to_focused_tsb,
                                    key_vars_chr = key_vars_chr,
                                    metrics_chr = c(referrals_var_1L_chr, appointments_var_1L_chr, cancellations_var_1L_chr, cost_var_1L_chr),
                                    type_1L_chr = "focused")}) %>% stats::setNames(names(selected_ls))
  return(wide_dss_ls)
}
