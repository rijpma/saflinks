# make a list of all cousin pairs in SAF

library("data.table")

father_child = fread("~/data/cape/saf/parentchild.csv")
links = fread("~/data/cape/saf/spousallinks_full_2021jan6.csv", na.strings = "")
siblings = fread("~/data/cape/saf/siblings.csv")

parents_child = merge(
    unique(father_child),
    links,
    by.x = "parent",
    by.y = "individual_id_from")
grandparents = merge(
    parents_child[, list(child, father = parent, mother = individual_id_to)],
    father_child[, list(child, paternal_grandfather = parent)],
    by.x = "father",
    by.y = "child")
grandparents = merge(
    grandparents,
    father_child[, list(maternal_grandfather = parent, child)],
    by.x = "mother",
    by.y = "child")
grandparents

# merge on common grandfather for each type of marriage
msd = merge(
    grandparents[, list(child, shared_grandfather = maternal_grandfather)],
    grandparents[, list(child, shared_grandfather = maternal_grandfather)],
    by = "shared_grandfather",
    allow.cartesian = TRUE)
fbd = merge(
    grandparents[, list(child, shared_grandfather = paternal_grandfather)],
    grandparents[, list(child, shared_grandfather = paternal_grandfather)],
    by = "shared_grandfather",
    allow.cartesian = TRUE)
mbd = merge(
    grandparents[, list(child, shared_grandfather = maternal_grandfather)],
    grandparents[, list(child, shared_grandfather = paternal_grandfather)],
    by = "shared_grandfather",
    allow.cartesian = TRUE)
fsd = merge(
    grandparents[, list(child, shared_grandfather = paternal_grandfather)],
    grandparents[, list(child, shared_grandfather = maternal_grandfather)],
    by = "shared_grandfather",
    allow.cartesian = TRUE)
cousins = rbindlist(
    list(msd = unique(msd), fbd = unique(fbd), mbd = unique(mbd), fsd = unique(fsd)),
    idcol = "type")

# filter out siblings
# also check the sparqls for that?
cousins[, siblings := paste(child.x, child.y) %chin% paste(siblings$individual_id.x, siblings$individual_id.y)]
cousins = cousins[siblings == FALSE]

fwrite(cousins, "~/data/cape/saf/cousins.csv")
