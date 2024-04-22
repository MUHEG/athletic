library(ready4)
library(ready4use)
library(ready4fun)
X <- Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                    gh_tag_1L_chr = "Documentation_0.0")
Y <- ingest(X)
# Only needed occationally....
# Z <- Ready4useRepos(dv_nm_1L_chr = "ready4fw",
#                     dv_server_1L_chr = "dataverse.harvard.edu",
#                     dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/RIQTKK") %>%
#   ingest()
Y <- renewSlot(Y,
               new_val_xx = Ready4useIngest(objects_ls = list(
                 #classes_bup_lup =  Y@b_Ready4useIngest@objects_ls$bup_classes_lup
                 #classes_lup = Z@b_Ready4useIngest@objects_ls$framework_metadata_ls$classes_lup
                 #abbreviations_lup = abbreviations_lup,
                 #libraries_tb = libraries_tb
                 #exclude_chr = c("aus_09_synth","dce_sa_cards","fakefolk","rebuild")
                 #libraries_ls = libraries_ls
                 #methods_tb = methods_tb
                 #modules_tb = modules_tb
                 treat_as_words_chr = c(Y@b_Ready4useIngest@objects_ls$treat_as_words_chr, "neuropsychological") %>% sort()
               )),
               slot_nm_1L_chr = "b_Ready4useIngest")
Y <- share(Y,
           type_1L_chr = "prefer_gh")
