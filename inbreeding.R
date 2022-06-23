# calculate inbreeding coefficients

rm(list = ls())

library("data.table")
library("pedigree")

parentchild = fread("~/data/cape/saf/parentchild.csv", na.strings = "")
spouses = fread("~/data/cape/saf/spousallinks_full_2021jan6.csv", na.strings = "")
saf_long = fread("~/data/cape/saf/saf4spouselinkage_full_clean.csv", na.strings = "")

xx = merge(
    parentchild,
    spouses[nth_marriage_from == 1],
    by.x = "parent",
    by.y = "individual_id_from",
    all.x = TRUE, all.y = FALSE,
    allow.cartesian = TRUE)
# bv individuals are duplicated in x, really you should filter those out to
# get only the actual parents of the children 
# for now we keep it simple and say marriage = 1

xx = xx[!is.na(individual_id_to), list(child, parent, individual_id_to)]
xx = merge(
    xx,
    saf_long[, list(family_id, individual_id, gender, gen)],
    by.x = "child",
    by.y = "individual_id")
xx[gender == 3, gender := 0]

# all fathers should appear as child (with missing parents if need by)
fathers_not_in_child = setdiff(xx$parent, xx$child)
xx = rbindlist(list(xx, data.table(child = fathers_not_in_child, gen = 1)), fill = TRUE)

mothers_not_in_child = setdiff(xx$individual_id_to, xx$child)
xx = rbindlist(list(xx, data.table(child = mothers_not_in_child, gen = 1)), fill = TRUE)

# order the dataset
xx = unique(xx)
xx = xx[order(gen)]
pedorder = pedigree::orderPed(xx[, list(child, parent, individual_id_to)])
xx[, ord := pedorder]
xx = xx[order(ord)]

# calc F coefficient
xx[ord > -1, F := pedigree::calcInbreeding(.SD), .SDcols = c("child", "parent", "individual_id_to")]

fwrite(
    xx[, list(individual_id = child, F)],
    "~/data/cape/saf/inbreeding.csv")

# look at results
summary(xx$F)
xx[, list(.N, mean(F, na.rm = TRUE)), by = gen][order(gen)]
xx[gen > 1, mean(F, na.rm = TRUE)]
xx[gen > 2, mean(F, na.rm = TRUE)]
xx[gen > 3, mean(F, na.rm = TRUE)]
xx[gen > 4, mean(F, na.rm = TRUE)]
table(round(xx$F, 2))

hist(xx$F)
