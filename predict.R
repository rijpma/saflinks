rm(list = ls())

library("capelinker")
library("data.table")
library("xgboost")
library("stringdist")
library("stringi")

# set stringdist cores
options(sd_num_thread = 8)
# set xgboost cores?
# set data.table cores?
getDTthreads()
setDTthreads(8)


# cnd = fread("~/data/cape/saf/saf_couples_candidates_full.csv", na.strings = "")
cnd = fread("saf_couples_full_candidates.csv", na.strings = "")

cnd[, mfirstdist := stringdist::stringdist(firstnames_ego_husb, firstnames_spouse_husb, method = "jw")]
cnd[, mlastdist := stringdist::stringdist(surname_ego_husb, surname_spouse_husb, method = "jw")]

cnd[, wfirstdist := stringdist::stringdist(firstnames_ego_wife, firstnames_spouse_wife, method = "jw")]
cnd[, wlastdist := stringdist::stringdist(surname_ego_wife, surname_spouse_wife, method = "jw")]

cnd[, minitialsdist_osa := 1 - stringdist::stringsim(initials_ego_husb, initials_spouse_husb, method = "osa")]
cnd[, winitialsdist_osa := 1 - stringdist::stringsim(initials_ego_husb, initials_spouse_husb, method = "osa")]

# cnd[, maryear_initialsdist_osa := 1 - stringdist::stringsim(as.character(married_year_ego_husb), as.character(married_year_ego_wife), method = "osa")]
# cnd[, list(married_year_ego_husb, married_year_ego_wife)]

cnd[, `(Intercept)` := 1]

cnd[,
    pred := predict(
        object = pretrained_models$m_boost_saf$model,
        newdata = xgboost::xgb.DMatrix(as.matrix(.SD))), 
    .SDcols = pretrained_models$m_boost_saf$model$feature_names]

cnd[, r := rank(-pred), by = couple_id_from]

cnd[, lapply(5:9 / 10, function(i) sum(r == 1 & pred > i))]
cnd[, lapply(5:9 / 10, function(i) sum(r == 1 & pred > i) / uniqueN(couple_id_from))]

out = cnd[r == 1 & pred > 0.5, list(couple_id_from, couple_id_to, pred)]
out[, individual_id_from := stringi::stri_replace_last_regex(couple_id_from, "_mar_\\d", "")]
out[, individual_id_to := stringi::stri_replace_last_regex(couple_id_to, "_mar_\\d", "")]
out[, nth_marriage_from := stringi::stri_extract_last_regex(couple_id_from, "\\d$")]
out[, nth_marriage_to := stringi::stri_extract_last_regex(couple_id_to, "\\d$")]

fwrite(out, "spousallinks_full_2021jan6.csv") # by which I mean 2022
# fwrite(out, "spousallinks_full_2021dec23.csv")
# fwrite(out, "~/data/cape/saf/spousallinks_full_2021dec23.csv")
