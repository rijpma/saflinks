# make graph db for easy familiy relation querying

library("data.table")
library("stringi")

uri = function(base, string = ""){
    stri_join("<", base, string, ">")
}
literal = function(string, datatype = "http://www.w3.org/2001/XMLSchema#string"){
    stri_join("\"", string, "\"^^", uri(datatype))
}

saf_long = fread("~/data/cape/saf/saf4spouselinkage_full_clean.csv", na.strings = "")
links = fread("~/data/cape/saf/spousallinks_full_2021jan6.csv", na.strings = "")


# saf_long[, indid := stri_sub(couple_id_from, 1, -7)]
# saf_long[, indid_gen := stri_extract_last_regex(individual_id, "[a-z]\\d.*")]
# saf_long[, indid_fam := stri_extract_last_regex(individual_id, "[a-z]\\d.*")]
# saf_long[, stri_split_regex(indid_gen, "(?<=[0-9])", simplify = TRUE), by = couple_id_from]

# move _dup[0-9] to the front where it does no harm
# though note that some are proper duplicates
# abriea1b1

saf_long[, dup := stri_extract_last_regex(individual_id, "_dup_[0-9]+")]
saf_long[!is.na(dup), individual_id := stri_replace_all_fixed(individual_id, dup, "")]
saf_long[!is.na(dup), individual_id := stri_join(dup, "_", individual_id)]

links[, dup := stri_extract_last_regex(individual_id_from, "_dup_[0-9]+")]
links[!is.na(dup), individual_id_from := stri_replace_all_fixed(individual_id_from, dup, "")]
links[!is.na(dup), individual_id_from := stri_join(dup, "_", individual_id_from)]

links[, dup := stri_extract_last_regex(individual_id_to, "_dup_[0-9]+")]
links[!is.na(dup), individual_id_to := stri_replace_all_fixed(individual_id_to, dup, "")]
links[!is.na(dup), individual_id_to := stri_join(dup, "_", individual_id_to)]


# count tree lengths
saf_long[, max(stri_count_regex(individual_id, "[a-z][1-9]+"))]
# chains are max 13g

ptrn = "[a-z][0-9?]{1,3}$" # lower case followed by question mark or three digits
rellist = list()
rellist[[1]] = saf_long[, list(child = individual_id, parent = stri_replace_last_regex(individual_id, ptrn, ""))]
for (i in 2:13){
    rellist[[i]] = rellist[[i - 1]][, list(child = child, parent = stri_replace_last_regex(child, ptrn, ""))]
}
x = rbindlist(rellist)
# what do those _dup[123] mean?
# should be none
x[parent == child]

# if it doesn't end on a number/question mark, we've gone to far, so drop
x = x[!stri_detect_regex(parent, "[a-z]$")]

# parent-child simple and too big for rdf, so simple pair csv
fwrite(unique(x), "~/data/cape/saf/parentchild.csv")

sibs = merge(saf_long[, list(individual_id, family_id)],
      saf_long[, list(individual_id, family_id)],
    by = "family_id",
    allow.cartesian = TRUE)
fwrite(unique(sibs), "~/data/cape/saf/siblings.csv")

cape = "https://www.capepanel.org/resource/individual/"
rel = "http://purl.org/vocab/relationship#"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"
schema = "https://schema.org/"
xsdyear = "http://www.w3.org/2000/10/XMLSchema#gYear"

outlist = list(
    parents = x[, list(
        uri(cape, parent),
        uri(..rel, "parentOf"), # should be fatherOf
        uri(cape, child))],
    children = x[, list(
        uri(cape, child),
        uri(..rel, "childOf"), # should be dropped?
        uri(cape, parent))],
    labels = saf_long[, list(
        uri(cape, individual_id),
        uri(rdfs, "label"),
        literal(individual_id))],
    surnames = saf_long[, list(
        uri(cape, individual_id),
        uri(schema, "familyName"),
        literal(surname))],
    gender = saf_long[, list(
        uri(cape, individual_id),
        uri(schema, "gender"),
        fcase(gender == 1, uri(schema, "Male"),
            gender == 2, uri(schema, "Female"),
            gender == 3, literal("Unknown")))],
    firstnames = saf_long[, list(
        uri(cape, individual_id),
        uri(schema, "givenName"),
        literal(firstnames))],
    startyear = saf_long[, list(
        uri(cape, individual_id),
        uri(cape, "startyear"),
        literal(startyear, xsdyear))],
    deathyear = saf_long[, list(
        uri(cape, individual_id),
        uri(cape, "deathyear"),
        literal(deathyear, xsdyear))],
    spousename = saf_long[, list(
        uri(cape, individual_id),
        uri(cape, "spouseName"),
        literal(stri_join(spouse_firstnames_clean, " ", spouse_surname_clean)))],
    spouses1 = links[, list(
        uri(cape, individual_id_from),
        uri(..rel, "spouseOf"),
        uri(cape, individual_id_to))],
    spouses2 = links[, list(
        uri(cape, individual_id_to),
        uri(..rel, "spouseOf"),
        uri(cape, individual_id_from))],
    siblings1 = sibs[individual_id.x != individual_id.y, list(
        uri(cape, individual_id.x),
        uri(..rel, "siblingOf"),
        uri(cape, individual_id.y))],
    siblings2 = sibs[individual_id.x != individual_id.y, list(
        uri(cape, individual_id.y),
        uri(..rel, "siblingOf"),
        uri(cape, individual_id.x))]
)
out = rbindlist(outlist)
out = unique(out)
out = na.omit(out)
# no spaces in uris
out[, V1 := stri_replace_all_fixed(V1, " ", "_")]
out[!stri_detect_fixed(V3, "^^"), V3 := stri_replace_all_fixed(V3, " ", "_")]

fwrite(out,  
    file = "~/data/cape/saf/saf_relations.nt.gz", 
    compress = "gzip",
    sep = " ", 
    eol = " .\n",
    quote = FALSE,
    col.names = FALSE)


rbind(
    sibs[, list(individual_id = individual_id.x, sibling = individual_id.y)],
    sibs[, list(individual_id = individual_id.y, sibling = individual_id.x)])
rbind(
    links[, list(individual_id = individual_id_from, nth_marriage = nth_marriage_from, spouse = individual_id_to, nth_marriage_to)],
    links[, list(individual_id = individual_id_to, nth_marriage = nth_marriage_to, spouse = individual_id_from, nth_marriage_from)]
)
x[, list(individual_id = child, father = parent)]
x[, list(individual_id = parent, child = child)]

# ok now you get hush-father1-grandfather1 
#            and wife-father2-grandfather1
# so is 100% paternal line? sure why not
# but we also want combination of maternal line?
# so  husb-father1-grandfather1
# and wife-mother1-grandfather

# add fullname labels?
# startyear endyear for individuals?
# correct regex for a1b2c?d1
# correct regex for 

# ego marries:
# FaBrDa [base query] <> FaBrSo
# FaSiDa [base query plus one parent/spouse] <> MoBrSo
# MoBrDa [base query plus the other parent/spouse] <> FaSiSo
# MoSiDa [base query plus both parent/spouse] <> MoSiSo
# So note the complete symmetry, you double count in all, can restrict by saying ego = male


# are you double counting the 