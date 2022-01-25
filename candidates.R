rm(list = ls())

library("capelinker")
library("data.table")
library("stringdist")

# saf_long = data.table::fread("~/data/cape/saf/saf4spouselinkage_full_clean.csv", na.strings = "")
saf_long = data.table::fread("saf4spouselinkage_full_clean.csv", na.strings = "")

# create candidates in blocks to prevent mem limit, even with sparse matrices
# with lots of mem you get: Cholmod error 'problem too large' at
# file ../Core/cholmod_dense.c, line 105 Calls: candidates ... apply ->
# as.matrix -> as.matrix.Matrix -> as -> asMethod Execution halted

chunksize = 4e3

men = saf_long[gender == 1, unique(couple_id)] # careful, gender == 3 also exists
men_split = split(men, f = ceiling(seq_along(men) / chunksize))

fill = list()
for (i in 1:length(men_split)){
    gc()

    tolink = saf_long[couple_id %chin% men_split[[i]]]

    cat(i, "-- creating candidates for ", nrow(tolink), "candidates\n")

    fill$fromhusb[[i]] = capelinker::candidates(
        dat_from = tolink[
            !is.na(surname) & gender == 1, # from is men
            list(couple_id, 
                surname_ego_husb = surname, 
                firstnames_ego_husb = firstnames, 
                initials_ego_husb = ego_initials, 
                surname_spouse_wife = spouse_surname_clean, 
                firstnames_spouse_wife = spouse_firstnames_clean, 
                initials_spouse_wife = spouse_initials, 
                married_year_ego_husb = married_year,
                startyear_ego_husb = sy,
                marriage_ego_husb = marriage)],
        dat_to = saf_long[
            # !is.na(spouse_surname_clean) & gender == 2, # to is women, gender == 3 still needs cleaning
            !is.na(spouse_surname_clean) & gender > 1, # to is women, gender == 3 still needs cleaning
            list(couple_id, 
                surname_ego_wife = surname, 
                firstnames_ego_wife = firstnames, 
                initials_ego_wife = ego_initials, 
                surname_spouse_husb = spouse_surname_clean, 
                firstnames_spouse_husb = spouse_firstnames_clean, 
                initials_spouse_husb = spouse_initials, 
                married_year_ego_wife = sy,
                startyear_ego_wife = startyear,
                marriage_ego_wife = marriage)],
        idvariable_from = "couple_id",
        idvariable_to = "couple_id",
        blockvariable_from = "surname_ego_husb",
        blockvariable_to = "surname_spouse_husb",
        blocktype = "bigram distance",
        linktype = "one:one",
        maxdist = 0.2)
    cat(i, "-- created", fill$fromhusb[[i]][, .N], "candidates from husb\n")

    fill$fromwife[[i]] = capelinker::candidates(
        dat_from = tolink[
            !is.na(surname) & gender == 1, # from is men
            list(couple_id, 
                surname_ego_husb = surname, 
                firstnames_ego_husb = firstnames, 
                initials_ego_husb = ego_initials, 
                surname_spouse_wife = spouse_surname_clean, 
                firstnames_spouse_wife = spouse_firstnames_clean, 
                initials_spouse_wife = spouse_initials, 
                married_year_ego_husb = married_year,
                startyear_ego_husb = sy,
                marriage_ego_husb = marriage)],
        dat_to = saf_long[
            # !is.na(spouse_surname_clean) & gender == 2, # to is women, gender == 3 still needs cleaning
            !is.na(spouse_surname_clean) & gender > 1, # to is women, gender == 3 still needs cleaning
            list(couple_id, 
                surname_ego_wife = surname, 
                firstnames_ego_wife = firstnames, 
                initials_ego_wife = ego_initials, 
                surname_spouse_husb = spouse_surname_clean, 
                firstnames_spouse_husb = spouse_firstnames_clean, 
                initials_spouse_husb = spouse_initials, 
                married_year_ego_wife = sy,
                startyear_ego_wife = startyear,
                marriage_ego_wife = marriage)],
        idvariable_from = "couple_id",
        idvariable_to = "couple_id",
        blockvariable_from = "surname_spouse_wife",
        blockvariable_to = "surname_ego_wife",
        blocktype = "bigram distance",
        linktype = "one:one",
        maxdist = 0.2)
    cat(i, "-- created", fill$fromwife[[i]][, .N], "candidates from wife\n\n")
}

gc()
fill = lapply(fill, rbindlist)
gc()
cnd = rbindlist(fill, id = "linkvariable")
gc()
cnd = cnd[!duplicated(paste(couple_id_from, couple_id_to, sep = "_"))]


gc()
fwrite(cnd, "saf_couples_full_candidates.csv")
# fwrite(cnd, "~/data/cape/saf/saf_couples_full_candidates.csv")
