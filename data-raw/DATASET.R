library(ready4)
library(ready4use)
library(ready4fun)
# library(ready4class)
# library(ready4show)
# MANUAL STEP. Write all your functions to R files in the new "fns" directory.
# fns_env_ls <- ready4fun::read_fns(c("data-raw/fns/","data-raw/mthds/"),
#                                   fns_env = new.env(parent = globalenv()))
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Analyse Health Data For Athletes and Sports Support Staff",
                                 pkg_desc_1L_chr = "Tools for undertaking analysis of sports datasets for use in computational health economic models developed with the ready4 framework (https://ready4-dev.github.io/ready4/).
                            This development version of the athletic package has been made available as part of the process of testing and documenting the package.
                            If you have any questions, please contact the authors (matthew.hamilton2@monash.edu).",
                                 authors_prsn = c(utils::person(given = "Matthew",family = "Hamilton",email = "matthew.hamilton1@monash.edu", role = c("aut", "cre"), comment = c(ORCID = "0000-0001-7407-9194")),
                                                  utils::person(given = "Fikru",family = "Rizal", email = "fikru.rizal@monash.edu", role = c("aut"), comment = c(ORCID = "0000-0002-0064-3255")),
                                                  utils::person("Monash University", role = c("cph"))
                                                  ),
                                 urls_chr = c("https://muheg.github.io/athletic/",
                                              "https://github.com/MUHEG/athletic",
                                              "https://ready4-dev.github.io/ready4/")) %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(suggests_chr = c("knitr","knitrBootstrap","rmarkdown"),
                                                                       #imports_chr = c(),
                                                                       depends_chr = c("ready4")
  ),
  build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
  check_type_1L_chr = "ready4",
  copyright_holders_chr = "Monash University",
  custom_dmt_ls = ready4fun::make_custom_dmt_ls(user_manual_fns_chr = c("add_counts",
                                                                        "add_imputed_costs",
                                                                        "add_severity",
                                                                        "add_sports_data",
                                                                        "get_raw_data",
                                                                        "make_linked_ds",
                                                                        "make_service_use_vars",
                                                                        "make_severity_args_ls")),##
  dev_pkgs_chr = c(#"cmdstanr",
    #"ready4",#"ready4fun",
    "ready4use",
    "serious",
    #,"ready4show"#,
   "youthvars"  #,"scorz",
    #"specific"
  ),
  lifecycle_stage_1L_chr = "experimental",
  path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/athletic-logo/default.png",
  piggyback_to_1L_chr = "ready4-dev/ready4",
  ready4_type_1L_chr = "modelling",
  zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11043502.svg)](https://doi.org/10.5281/zenodo.11043502)"#
  )
y <- ready4class::ready4class_constructor()
z <- ready4pack::make_pt_ready4pack_manifest(x, constructor_r3 = y) %>%
  ready4pack::ready4pack_manifest()
z <- ready4::author(z)
#ready4::write_extra_pkgs_to_actions(path_to_dir_1L_chr = ".github/workflows", consent_1L_chr = "Y")
ready4::write_to_edit_workflow("pkgdown.yaml", consent_1L_chr = "Y") # In other packages, run for "test-coverage.yaml" as well.
write_to_tidy_pkg(z$x_ready4fun_manifest,
                  build_vignettes_1L_lgl = TRUE,
                  clean_license_1L_lgl = TRUE,
                  consent_1L_chr = "Y",
                  examples_chr = character(0),
                  project_1L_chr = "Framework",
                  suggest_chr = "pkgload")
# readLines("_pkgdown.yml") %>%
#   stringr::str_replace_all("  - text: Model", "  - text: Framework & Model") %>%
#   writeLines(con = "_pkgdown.yml")
write_citation_fl(z$x_ready4fun_manifest)
# devtools::build_vignettes()
