library("data.table")
library("readxl")
library("stringi")
library("readstata13")

mypar = function(...){
    par(..., 
        bty = "l", 
        mar = c(3, 3, 2, 1), 
        mgp = c(1.7, .5, 0), 
        tck=-.01,
        font.main = 1)
}

# saf = readstata13::read.dta13("~/data/cape/saf/SAF match sample for spouse linkage_with years.dta")
# setDT(saf)

saf_long = fread("~/data/cape/saf/saf4spouselinkage_full_clean.csv", na.strings = "")
saf_long_old = fread("~/data/cape/saf/saf4spouselinkage_clean.csv", na.strings = "")
links = fread("~/data/cape/saf/spousallinks_full_2021jan6.csv", na.strings = "")
links_old = fread("~/data/cape/saf/spousallinks_2021aug05.csv", na.strings = "")

saf_long = merge(saf_long, 
    links[, list(couple_id = couple_id_from, individual_id_wife = individual_id_to, pred = pred)],
    by = "couple_id",
    all.x = TRUE)
saf_long_old = merge(saf_long_old, 
    links_old[, list(couple_id = couple_id_from, individual_id_wife = individual_id_to, pred = pred)],
    by = "couple_id",
    all.x = TRUE)

# linkage example
out = saf_long[couple_id %in% c("koortsa1b2c7d2e2_mar_1", "abbotta1_mar_1"), 
    list(couple_id, startyear, deathyear, firstnames, surname, married_year, spouse_firstnames_clean, spouse_surname_clean)]
out = transpose(out, keep.names = "couple_id", make.names = "couple_id")
out = knitr::kable(out, format = "latex")
writeLines(out, "~/repos/saf/example.tex")

# overall rates
out = list(
    `total links` = saf_long[, sum(!is.na(individual_id_wife))],
    `share of husbands' marriages linked` = saf_long[gender == 1, mean(!is.na(individual_id_wife))],
    `share of husbands linked at least once` = saf_long[gender == 1, uniqueN(individual_id[!is.na(individual_id_wife)]) / uniqueN(individual_id)],
    `share of wives' marriages linked` = saf_long[, sum(!is.na(individual_id_wife)) / sum(gender > 1)],
    `share of wives linked at least once` = saf_long[, uniqueN(individual_id_wife) / uniqueN(individual_id[gender == 2])]
)
out_period = list(
    `total links` = saf_long[between(startyear, 1700, 1850), sum(!is.na(individual_id_wife))],
    `share of husbands' marriages linked` = saf_long[between(startyear, 1700, 1850) & gender == 1, mean(!is.na(individual_id_wife))],
    `share of husbands linked at least once` = saf_long[between(startyear, 1700, 1850)& gender == 1, uniqueN(individual_id[!is.na(individual_id_wife)]) / uniqueN(individual_id)],
    `share of wives' marriages linked` = saf_long[between(startyear, 1700, 1850), sum(!is.na(individual_id_wife)) / sum(gender > 1)],
    `share of wives linked at least once` = saf_long[between(startyear, 1700, 1850), uniqueN(individual_id_wife) / uniqueN(individual_id[gender == 2])]
)
out = cbind(all = out, `1700-1850` = out_period)
out = xtable::xtable(out, digits = 2, caption = "Linkage rate in full and subsetted SAF data")
print(out, file = "~/repos/saf/rates.tex")

toplot = saf_long[gender == 1, mean(!is.na(individual_id_wife)), by = list(decade = floor(startyear / 10) * 10)]
toplot_old = saf_long_old[gender == 1, mean(!is.na(individual_id_wife)), by = list(decade = floor(startyear / 10) * 10)]
knitr::kable(merge(toplot, toplot_old, by = "decade"), digits = 2)
pdf("~/repos/saf/saf_linkrates_compared.pdf", height = 6)
mypar()
plot(toplot[order(decade)], type = "b", pch = 19, col = 2,
    xlab = "Start/birth year", 
    ylab = "share of husbands linked", ylim = c(0, 0.7))
lines(toplot_old[order(decade)], type = "b", pch = 19, col = 1)
abline(v = c(1750, 1844), col = "gray")
legend("topright", fill = 1:2, legend = c("old", "new"))
dev.off()

pdf("~/repos/saf/new_saf_linkrates.pdf", height = 6)
mypar()
plot(toplot[order(decade)][decade > 1700], type = "b", pch = 19, col = 2,
    xlab = "Start/birth year", 
    ylab = "share of husbands linked", ylim = c(0, 0.7))
abline(v = c(1750, 1844), col = "gray")
dev.off()

# compare with:
# male-female ratio weddings
toplot = saf_long[!is.na(gender), sum(gender == 1) / sum(gender == 2), by = list(decade = floor(startyear / 10) * 10)]
toplot[decade <= 1700, V1 := NA]
pdf("~/repos/saf/saf_genderbalance_weddings.pdf", height = 6)
mypar()
plot(toplot[order(decade)], type = "b", pch = 19, col = 2,
    xlab = "Start/birth year", 
    ylab = "male:female ego in weddings")
dev.off()

# share missing spouse names
toplot = saf_long[!is.na(sy), 
    list(spouse_firstnames_miss = mean(is.na(spouse_firstnames_clean)),
        spouse_surname_miss = mean(is.na(spouse_surname_clean)),
        spouse_either_miss = mean(is.na(spouse_surname_clean) | is.na(spouse_firstnames_clean)),
        spouse_both_miss = mean(is.na(spouse_surname_clean) & is.na(spouse_firstnames_clean))
    ),
    by = list(decade = floor(startyear / 10) * 10)]
toplot[decade <= 1700, (2:5) := NA]
setorder(toplot, "decade")
pdf("~/repos/saf/saf_spouse_missing.pdf", height = 6)
mypar()
plot(spouse_either_miss ~ decade, data = toplot, type = "b", pch = 19, col = 1,
    xlab = "Start/birth year", 
    ylab = "share missing spouse info")
lines(spouse_surname_miss ~ decade, data = toplot, type = "b", pch = 19, col = 2)
lines(spouse_firstnames_miss ~ decade, data = toplot, type = "b", pch = 19, col = 3)
legend("topleft", fill = 1:3, legend = c("either", "surname", "firstname"))
dev.off()

# what if we recalculate the rates for spouse names present only?
toplot1 = saf_long[gender == 1, mean(!is.na(individual_id_wife)), by = list(decade = floor(startyear / 10) * 10)]
toplot2 = saf_long[gender == 1 & !is.na(spouse_firstnames_clean) & !is.na(spouse_surname_clean), mean(!is.na(individual_id_wife)), by = list(decade = floor(startyear / 10) * 10)]
mypar()
plot(toplot1[order(decade)], type = 'b')
lines(toplot2[order(decade)], type = 'b', col = 2)
# little difference

# male-female ratio in children
saf = read.dta13("~/data/cape/saf/SAF FULL selection for spouse linkage.do")
setDT(saf)
# everything lowercase
saf = rapply(saf, tolower, classes = "character", how = "replace")

# missing values where necessary
saf[surname == "", surname := NA]
saf[firstnames == "", firstnames := NA]
saf[surname == "nn", surname := NA]
saf[firstnames == "nn", firstnames := NA]

# overall sex ratio
saf[, sum(gender == 1) / sum(gender > 1)]
saf_long[, sum(gender == 1) / sum(gender > 1)]

toplot = saf[, list(men = sum(gender == 1), women = sum(gender == 2)), by = list(decade = floor(sy / 10) * 10)]
toplot[decade >= 1700 & decade < 2000, ratio := men / women]
pdf("~/repos/saf/genderbalance_total.pdf", height = 6)
mypar()
plot(ratio ~ decade, data = toplot[order(decade)], type = 'b', pch = 19,
    xlab = "Start/birth year", 
    ylab = "male:female ratio all indivuals")
dev.off()

excessmales = saf[, list(excessmales = sum(gender == 1) - sum(gender == 2), sy = min(sy)), by = family_id]
toplot = excessmales[, mean(excessmales), by = list(decade = floor(sy / 10) * 10)]
toplot[!between(decade, 1700, 2000), V1 := NA]
pdf("~/repos/saf/excessmales_perfamily.pdf", height = 6)
mypar()
plot(toplot[order(decade)], type = 'b', pch = 19,
    xlab = "Start/birth year", 
    ylab = "mean sibship excess males")
dev.off()

    


toplot = saf_long[gender == 1, sum(!is.na(individual_id_wife)), by = list(decade = floor(startyear / 10) * 10)]
toplot_old = saf_long_old[gender == 1, sum(!is.na(individual_id_wife)), by = list(decade = floor(startyear / 10) * 10)]

mypar()
plot(toplot[order(decade)], type = "b", pch = 19, col = 2,
    xlab = "Start/birth year", 
    ylab = "number of husbands linked")
lines(toplot_old[order(decade)], type = "b", pch = 19, col = 1)
abline(v = c(1750, 1844), col = "gray")

toplot = saf_long[, .N, by = list(decade = floor(startyear / 10) * 10)]
toplot_old = saf_long_old[, .N, by = list(decade = floor(startyear / 10) * 10)]
plot(toplot[order(decade)], type = "b", pch = 19, col = 2,
    xlab = "Start/birth year", 
    ylab = "total number of marriags")
lines(toplot_old[order(decade)], type = "b", pch = 19, col = 1)
dev.off()
