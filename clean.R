rm(list = ls())

library("data.table")
library("readstata13")
library("stringdist")
library("stringi")
library("capelinker")

saf = read.dta13("~/data/cape/saf/SAF FULL selection for spouse linkage.do")
setDT(saf)

# everything lowercase
saf = rapply(saf, tolower, classes = "character", how = "replace")

# missing values where necessary
saf[surname == "", surname := NA]
saf[firstnames == "", firstnames := NA]
saf[surname == "nn", surname := NA]
saf[firstnames == "nn", firstnames := NA]

toplot = saf[, list(men = sum(gender == 1), women = sum(gender == 2)), by = list(decade = floor(sy / 10) * 10)]
toplot[decade >= 1700 & decade < 2000, ratio := men / women]
pdf("~/repos/saf/genderbalance_total.pdf", height = 6)
plot(ratio ~ decade, data = toplot[order(decade)], type = 'b', pch = 19,
    xlab = "Start/birth year", 
    ylab = "male:female ratio all indivuals")
dev.off()

excessmales = saf[, list(excessmales = sum(gender == 1) - sum(gender == 2), sy = min(sy)), by = family_id]
toplot = excessmales[, mean(excessmales), by = list(decade = floor(sy / 10) * 10)]
toplot[!between(decade, 1700, 2000), V1 := NA]
pdf("~/repos/saf/excessmales_perfamily.pdf", height = 6)
plot(toplot[order(decade)], type = 'b', pch = 19)
    xlab = "Start/birth year", 
    ylab = "mean sibship excess males")
dev.off()

    
# check one character firstnames (all initials)
# saf[nchar(firstnames) == 1, unique(firstnames)]
saf[, surname := stri_trans_general(surname, "Latin-ASCII")]
saf[stri_detect_fixed(surname, "("), surname := stri_replace_all_regex(surname, "\\(.*\\)", "")]

# long format where each marriage/spouse of an individual gets its own row
spouses = melt(
    saf, 
    id = "individual_id",
    measure = patterns(
        dom = "dom\\d", 
        married_year = "married\\d_year", 
        spouse_dod = "spouse\\d_dod",
        spouse_dob = "spouse\\d_dob",
        spouse_initials = "spouse\\d_initials",
        spouse_surname = "spouse\\d_surname$", 
        spouse_surname_clean = "spouse\\d_surname_clean", 
        spouse_firstnames = "spouse\\d_first_names$",
        spouse_firstnames_clean = "spouse\\d_first_names_clean",
        spouse_daughter_of = "spouse\\d_daughter_of",
        spouse_son_of = "spouse\\d_son_of",
        spouse_occupation = "spouse\\d_occupation"),
    variable.name = "marriage")


spouses[spouse_surname == "", spouse_surname := NA]
spouses[spouse_surname_clean == "", spouse_surname_clean := NA]
spouses[spouse_initials == "", spouse_initials := NA]
spouses[spouse_firstnames == "", spouse_firstnames := NA]
spouses[spouse_firstnames_clean == "", spouse_firstnames_clean := NA]
# spouses[spouse_death_year == "", spouse_death_year := NA]
spouses[spouse_daughter_of == "", spouse_daughter_of := NA]
spouses[spouse_son_of == "", spouse_son_of := NA]

spouses[spouse_surname == "nn", spouse_surname := NA]
spouses[spouse_surname_clean == "nn", spouse_surname_clean := NA]
spouses[spouse_initials == "nn", spouse_initials := NA]
spouses[spouse_firstnames == "nn", spouse_firstnames := NA]
spouses[spouse_firstnames_clean == "nn", spouse_firstnames_clean := NA]
# spouses[spouse_death_year == "nn", spouse_death_year := NA]
spouses[spouse_daughter_of == "nn", spouse_daughter_of := NA]
spouses[spouse_son_of == "nn", spouse_son_of := NA]

# number married at least once
spouses[marriage == 1, sum(!(is.na(spouse_surname) & is.na(spouse_firstnames) & is.na(spouse_initials) & is.na(dom)))]

# drop no-couples
spouses = spouses[!(is.na(spouse_surname) & is.na(spouse_firstnames) & is.na(spouse_initials) & is.na(dom))]

# merge rest of saf back in 
saf_long = merge(
    x = saf[, .SD, .SDcols = -patterns("\\d")],
    y = spouses,
    by = "individual_id",
    all.x = FALSE, # so lose singles!
    all.y = TRUE)

# couple id
saf_long[, couple_id := paste0(individual_id, "_mar_", marriage)] # individuals no longer unique

# some NA surnames 1.5%, drop
# nchar =1, some initials, clean below
saf_long[spouse_firstnames_clean == "-", spouse_firstnames_clean := NA]

saf_long[, spouse_surname_clean := stri_trim_both(spouse_surname_clean)]
saf_long[, spouse_firstnames_clean := stri_trim_both(spouse_firstnames_clean)]
saf_long[, spouse_surname_clean := stri_replace_all_fixed(spouse_surname_clean, ".", "")]

# encoding issue on accented chars (not necessary for surname)
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "\u00A0", "y")] # ÿ ?
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "÷", "o")] # ö
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "∩", "i")] # ï
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "≥", ".")] # ?
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, " ╓", "")] # trailing stuff
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "╔", "e")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "▄", "u")] # u
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "æ", "")] # '
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "ƒ", "y")] # ÿ
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "ⁿ", "u")] # ü
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "γ", "a")] # à
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "δ", "e")] # ë
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "θ", "e")] # é
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "π", "a")] # à
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "σ", "a")] # or a?
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "φ", "e")] # also é
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, ":", "l")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, ";", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "@", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "┼", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "*", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "_", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "τ", "c")] # ç
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "}", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "{", "p")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "ω", "e")] # ë
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "δ", "e")] # ë
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "²", "")] # ?
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "α", "")] # ?
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "≈", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "ß", "a")] # ?
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "─", "-")] # ?

# some placenames in data
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "souther-rhodesia", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "oos-londen", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "beaufort-wes", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "griekwaland-wes", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "somerset-oos", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "beaufort-wes", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "victoria-wes", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "somerset-wes", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "graaff-reinet", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "nieu-duitsland", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "onder-bokkeveld", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "noord-brabant", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "aliwal-noord", "")]
saf_long[, spouse_firstnames_clean := stri_replace_all_fixed(spouse_firstnames_clean, "barkly-oos", "")]
# few - in weird places left

saf_long[, spouse_firstnames_clean := stri_trans_general(spouse_firstnames_clean, "Latin-ASCII")]

# correct initials
# saf_long[stri_detect_regex(spouse_surname_clean, "^[a-z] "), list(spouse_initials, spouse_firstnames_clean, spouse_surname_clean)]
saf_long[spouse_surname_clean == "m ller moeller", spouse_initials := NA]
saf_long[spouse_surname_clean == "w lfling von welfling", spouse_initials := NA]
saf_long[spouse_surname_clean == "k nigkr mer", spouse_initials := NA]
saf_long[spouse_surname_clean == "r scher rosscher", spouse_initials := NA]
saf_long[spouse_surname_clean == "d ncker dunkerdt", spouse_initials := NA]
saf_long[spouse_surname_clean == "b hme beeme", spouse_initials := NA]
saf_long[spouse_surname_clean == "l e rahske", spouse_initials := "l.e."]
saf_long[spouse_surname_clean == "l e smit", spouse_initials := "l.e."]

saf_long[spouse_surname_clean == "m ller moeller", spouse_surname_clean := "muller"]
saf_long[spouse_surname_clean == "w lfling von welfling", spouse_surname_clean := "wulfling"]
saf_long[spouse_surname_clean == "k nigkr mer", spouse_surname_clean := "kunigkrumer"]
saf_long[spouse_surname_clean == "r scher rosscher", spouse_surname_clean := "ruscher"]
saf_long[spouse_surname_clean == "d ncker dunkerdt", spouse_surname_clean := "duncker"]
saf_long[spouse_surname_clean == "b hme beeme", spouse_surname_clean := "buhme"]

saf_long[spouse_surname_clean == "c schl ter", spouse_surname_clean := "c schluter"]
saf_long[spouse_surname_clean == "p meiring ng", spouse_surname_clean := "p meiringung"]
saf_long[spouse_surname_clean == "h hirshowitz hersowitz", spouse_surname_clean := "h hirshowitz"]
saf_long[spouse_surname_clean == "c swart swart", spouse_surname_clean := "c swart"]
saf_long[spouse_surname_clean == "s kr ger", spouse_surname_clean := "s kruger"]

# fix initials in surnames
# initials already present in initials, so just cut whatever's in surname
# max = two initials, so just do twice
saf_long[stri_detect_regex(spouse_surname_clean, "^[a-z] "), 
    spouse_surname_clean := stri_replace_first_regex(spouse_surname_clean, "^[a-z] ", "")]
saf_long[stri_detect_regex(spouse_surname_clean, "^[a-z] "), 
    spouse_surname_clean := stri_replace_first_regex(spouse_surname_clean, "^[a-z] ", "")]
saf_long[stri_detect_regex(spouse_surname_clean, "^[a-z] "), list(spouse_initials, spouse_firstnames_clean, spouse_surname_clean)]
# maybe you want to fill empty firstnames with those initials?

# try to get rid of duplicated words, lot of manual work
# firstnames
saf_long[, firstnames_split := stri_split_boundaries(firstnames, type = "word")]
saf_long[, dupname := lapply(firstnames_split, function(x) any(duplicated(x[!x %in% c(" ", ",", ".")])))]
saf_long[firstnames == "willem van eyck van", firstnames := "willem"]
# saf_long[dupname == TRUE][1:2, sapply(firstnames_split, function(x) paste0(unique(x[!x == " "]), collapse = " "))]
saf_long[dupname == TRUE, firstnames := sapply(firstnames_split, function(x) paste0(unique(x[!x == " "]), collapse = " "))]
# saf_long[dupname == TRUE, list(firstnames)]
saf_long[, firstnames_split := NULL]

# still none in surname

# spouse_firstnames
saf_long[, spouse_firstnames_clean_split := NULL]
saf_long[, spouse_firstnames_clean_split := stri_split_boundaries(spouse_firstnames_clean, type = "word")]
saf_long[, dupname := lapply(spouse_firstnames_clean_split, function(x) any(duplicated(x[!x %in% c(" ", ",", ".")])))]

saf_long[spouse_firstnames_clean == "director of natal bank as well as durban tramway co was knighted in aug",
    spouse_firstnames_clean := NA]
saf_long[spouse_firstnames_clean == "lydia margaret mcintyre v ceres twee seuns uit eerste huwelik gebore en twee dogters uit tweede huwelik", 
    spouse_surname_clean := "mcintyre"]
saf_long[spouse_firstnames_clean == "lydia margaret mcintyre v ceres twee seuns uit eerste huwelik gebore en twee dogters uit tweede huwelik", 
    spouse_firstnames_clean := "lydia margaret"]
saf_long[spouse_firstnames_clean == "jacoba maria charlottejacoba maria zarlotta",
    spouse_firstnames_clean := "jacoba maria charlotte"]
saf_long[spouse_firstnames_clean == "martha hester petronellamarthina hester petronella",
    spouse_firstnames_clean := "martha hester petronella"]
saf_long[spouse_firstnames_clean == "dirkie maria susannadirkje maria sophia",
    spouse_firstnames_clean := "dirkie maria susanna"]
saf_long[spouse_firstnames_clean == "johannes cornelius smith johannes carolis smit",
    spouse_firstnames_clean := "johannes cornelius smith"]
saf_long[spouse_firstnames_clean == "johanna susarahsara johanna",
    spouse_firstnames_clean := "johanna susarah"]
saf_long[spouse_firstnames_clean == "onderkoopman op die skip leyden wat op reis na batavia was",
    spouse_surname_clean := "nolthenius"]
saf_long[spouse_firstnames_clean == "onderkoopman op die skip leyden wat op reis na batavia was",
    spouse_firstnames_clean := "daniel"]
saf_long[spouse_firstnames_clean == "david johannes  daniel johannes  david jacobus",
    spouse_firstnames_clean := "david johannes daniel"]
saf_long[spouse_firstnames_clean == "frederika wilhelmina frederika",
    spouse_firstnames_clean := "frederika wilhelmina"]
# saf_long[spouse_firstnames_clean == "henrietta elizabeth gerdina johanna van arensalt van der kooi"]
saf_long[spouse_firstnames_clean == "janetta wilhelmina geertruidajeanetha wilhelmina gertruida",
    spouse_firstnames_clean := "janetta wilhelmina geertruida"]
saf_long[spouse_firstnames_clean == "johannes hendricus johannes",
    spouse_firstnames_clean := "johannes hendricus"]
saf_long[spouse_firstnames_clean == "wouter de wet de waal",
    spouse_firstnames_clean :=  "wouter de wet"]
saf_long[spouse_firstnames_clean == "mary obrien die egpaar het  seun en  dogters gehad",
    spouse_firstnames_clean :=  "mary obrien"]
saf_long[spouse_firstnames_clean == "theunissina gertina nicolasinatheunsina nicolasina gertina",
    spouse_firstnames_clean :=  "theunissina gertina nicolasina"]
saf_long[spouse_firstnames_clean == "marion marion",
    spouse_firstnames_clean :=  "marion"]
saf_long[spouse_firstnames_clean == "cornelia jacoba frederikacornelia fredrika jacoba",
    spouse_firstnames_clean :=  "cornelia jacoba frederika"]
saf_long[spouse_firstnames_clean == "johanna elizabeth elizabeth",
    spouse_firstnames_clean :=  "johanna elizabeth"]
saf_long[spouse_firstnames_clean == "daniel jacobus  daniel pieter",
    spouse_firstnames_clean :=  "daniel jacobus"]
saf_long[spouse_firstnames_clean == "lourens lourens adrianis",
    spouse_firstnames_clean :=  "lourens adrianis"]
saf_long[spouse_firstnames_clean == "christina wilhelmina van rheede van oudtshoorn",
    spouse_surname_clean := "van rheede van oudtshoorn"]
saf_long[spouse_firstnames_clean == "christina wilhelmina van rheede van oudtshoorn",
    spouse_firstnames_clean :=  "christina wilhelmina"]
saf_long[spouse_firstnames_clean == "nicolaas de wet de la rey botha",
    spouse_firstnames_clean := "nicolaas"]
saf_long[spouse_firstnames_clean == "johanna johanna maria catharina",
    spouse_firstnames_clean := "johanna maria catharina"]
saf_long[spouse_firstnames_clean == "jacobus jacobus",
    spouse_firstnames_clean := "jacobus"]
saf_long[spouse_firstnames_clean == "nieu-suid-wallis",
    spouse_firstnames_clean := "elsie magdeline"]
saf_long[spouse_firstnames_clean == "cornelia magrieta cornelia",
    spouse_firstnames_clean := "cornelia magrieta"]
saf_long[spouse_firstnames_clean == "soldelina van rheede van saasveld",
    spouse_firstnames_clean := "soldelina"]
saf_long[spouse_firstnames_clean == "mary-rose murray-macgregor",
    spouse_surname_clean := "murray-macgregor"]
saf_long[spouse_firstnames_clean == "mary-rose murray-macgregor",
    spouse_firstnames_clean := "mary-rose"]
saf_long[spouse_firstnames_clean == "hendrik johannesjohannes hendrik",
    spouse_firstnames_clean := "hendrik johannes"]
saf_long[spouse_firstnames_clean == "john st john",
    spouse_firstnames_clean := "john"]
saf_long[spouse_firstnames_clean == "duits-oos-afrika anna catharina",
    spouse_firstnames_clean := "anna catharina"]
saf_long[spouse_firstnames_clean == "te couple lived in pietermaritzburg in",
    spouse_surname_clean := "mccrystal"]
saf_long[spouse_firstnames_clean == "te couple lived in pietermaritzburg in",
    spouse_firstnames_clean := "henry patrick"]
saf_long[spouse_firstnames_clean == "suzanne magrieta maria maria",
    spouse_firstnames_clean := "suzanne magrieta maria"]
saf_long[spouse_firstnames_clean == "frans johannes johannes",
    spouse_firstnames_clean := "frans johannes"]
saf_long[spouse_firstnames_clean == "elizabeth johanna van rheede van oudtshoorn",
    spouse_surname_clean := "van rheede van oudtshoorn"]
saf_long[spouse_firstnames_clean == "elizabeth johanna van rheede van oudtshoorn",
    spouse_firstnames_clean := "elizabeth johanna"]
saf_long[spouse_firstnames_clean == "- - julia blanche",
    spouse_firstnames_clean := "julia blanche"]
saf_long[spouse_firstnames_clean == "samuel hendrik hendrik",
    spouse_firstnames_clean := "samuel hendrik"]
saf_long[spouse_firstnames_clean == "maria elizabeth maria",
    spouse_firstnames_clean := "maria elizabeth"]
saf_long[spouse_firstnames_clean == "willem frederik henning henning",
    spouse_surname_clean := "henning"]
saf_long[spouse_firstnames_clean == "willem frederik henning henning",
    spouse_firstnames_clean := "willem frederik"]
saf_long[spouse_firstnames_clean == "- - sylvia",
    spouse_firstnames_clean := "sylvia"]
saf_long[spouse_firstnames_clean == "- - buchner adriaan",
    spouse_firstnames_clean := "buchner adriaan"]
saf_long[spouse_firstnames_clean == "- - joseph james",
    spouse_firstnames_clean := "joseph james"]
saf_long[spouse_firstnames_clean == "- - martha maria",
    spouse_firstnames_clean := "martha maria"]
saf_long[spouse_firstnames_clean == "- - hugh mckinnell",
    spouse_surname_clean := "mckinnell"]
saf_long[spouse_firstnames_clean == "- - hugh mckinnell",
    spouse_firstnames_clean := "hugh mckinnell"]
saf_long[spouse_firstnames_clean == "vereeniging - - jacobus",
    spouse_firstnames_clean := "jacobus"]
saf_long[spouse_firstnames_clean == "- - susanna jacoba",
    spouse_firstnames_clean := "susanna jacoba"]
saf_long[spouse_firstnames_clean == "for a short while to a chorus girl",
    spouse_firstnames_clean := NA]
saf_long[spouse_firstnames_clean == "ashley ashley-cooper",
    spouse_surname_clean := "cooper"]
saf_long[spouse_firstnames_clean == "ashley ashley-cooper",
    spouse_firstnames_clean := "ashley"]
saf_long[spouse_firstnames_clean == "henrietta elizabeth gerdina johanna van arensalt van der kooi",
    spouse_firstnames_clean := "henrietta elizabeth gerdina johanna "]
saf_long[spouse_firstnames_clean == "adriana johanna venterxx elizabeth johanna",
    spouse_firstnames_clean := "adriana johanna venterxx elizabeth"]
saf_long[spouse_firstnames_clean == "jacobus johannes francoisjosias jacobus francois",
    spouse_firstnames_clean := "jacobus johannes francois"]
saf_long[spouse_firstnames_clean == "abraham johannes johannes jacobus",
    spouse_firstnames_clean := "abraham johannes jacobus"]
saf_long[spouse_firstnames_clean == "frederik hendrik willem willem gerhardus",
    spouse_firstnames_clean := "frederik hendrik willem gerhardus"]


# spouse surname
# saf_long[, spouse_surname_clean_split := stri_split_boundaries(spouse_surname_clean, type = "word")]
# saf_long[, dupname := lapply(spouse_surname_clean_split, function(x) any(duplicated(x[!x %in% c(" ", ",", ".")])))]
# saf_long[dupname == TRUE, cat(unique(spouse_surname_clean), sep = "\n")]

# saf_long[spouse_surname_clean == "van heyde van josowe", spouse_surname_clean := "van heyde van josowe"]
saf_long[spouse_surname_clean == "de wit de wet", spouse_surname_clean := "de wit"]
saf_long[spouse_surname_clean == "meyer de meyer", spouse_surname_clean := "meyer"]
saf_long[spouse_surname_clean == "van staden van staaden", spouse_surname_clean := "van staden"]
saf_long[spouse_surname_clean == "van staden d v van staden", spouse_surname_clean := "van staden"]
saf_long[spouse_surname_clean == "van reenen van renen", spouse_surname_clean := "van reenen"]
saf_long[spouse_surname_clean == "van stade van staden", spouse_surname_clean := "van staden"]
saf_long[spouse_surname_clean == "van staden van stade", spouse_surname_clean := "van staden"]
# saf_long[spouse_surname_clean == "van rheede van oudtshoorn", spouse_surname_clean := "van rheede van oudtshoorn"]
saf_long[spouse_surname_clean == "van wyngaard van wyngaarden", spouse_surname_clean := "van wyngaard"]
saf_long[spouse_surname_clean == "van der westhuizen van der westhuyzen", spouse_surname_clean := "van der westhuizen"]
saf_long[spouse_surname_clean == "van der wat van der watt", spouse_surname_clean := "van der wat"]
saf_long[spouse_surname_clean == "van greunen von grune van greunig", spouse_surname_clean := "van greunen"]
saf_long[spouse_surname_clean == "vos de vos", spouse_surname_clean := "vos"]
saf_long[spouse_surname_clean == "pieterse w pieterse", spouse_surname_clean := "pieterse"]
saf_long[spouse_surname_clean == "van rensburg van renseburg", spouse_surname_clean := "van rensburg"]
saf_long[spouse_surname_clean == "gey gey van pittius", spouse_surname_clean := "gey van pittius"]
saf_long[spouse_surname_clean == "van locherenberg van loggenberg", spouse_surname_clean := "van locherenberg"]
saf_long[spouse_surname_clean == "van loggenberg van locherenberg", spouse_surname_clean := "van loggenberg"]
saf_long[spouse_surname_clean == "de wit de witt", spouse_surname_clean := "de wit"]
saf_long[spouse_surname_clean == "mc callum mc cullum", spouse_surname_clean := "mc callum"]
# saf_long[spouse_surname_clean == "van reede van oudtshoorn", spouse_surname_clean := "van reede van oudtshoorn"]
saf_long[spouse_surname_clean == "van lille van lill", spouse_surname_clean := "van lille"]
saf_long[spouse_surname_clean == "van willinge van wielligh", spouse_surname_clean := "van willinge"]
saf_long[spouse_surname_clean == "de bruyn de bruin", spouse_surname_clean := "de bruyn"]
# saf_long[spouse_surname_clean == "van rheede van oudsthoorn", spouse_surname_clean := "van rheede van oudsthoorn"]
saf_long[spouse_surname_clean == "van den van der berg", spouse_surname_clean := "van den berg"]
saf_long[spouse_surname_clean == "cele nn nn nn brown", spouse_surname_clean := "brown"]
# saf_long[spouse_surname_clean == "de curmink de capelle", spouse_surname_clean := "de curmink de capelle"]
# saf_long[spouse_surname_clean == "van reede van oudshoorn", spouse_surname_clean := "van reede van oudshoorn"]
saf_long[spouse_surname_clean == "van heusden van huylsteen", spouse_surname_clean := "van huylsteen"]
# saf_long[spouse_surname_clean == "van der merwe van niekerk", spouse_surname_clean := "van der merwe van niekerk"]
saf_long[spouse_surname_clean == "le roes le roux", spouse_surname_clean := "le roux"]
saf_long[spouse_surname_clean == "van eeden van essen", spouse_surname_clean := "van eeden"]
# saf_long[spouse_surname_clean == "van rheede van oudshoorn", spouse_surname_clean := "van rheede van oudshoorn"]
saf_long[spouse_surname_clean == "van der schyff ven der schijff", spouse_surname_clean := "van der schyff"]
saf_long[spouse_surname_clean == "van biljon van boullion", spouse_surname_clean := "van biljon"]
saf_long[spouse_surname_clean == "van blerk van blerq", spouse_surname_clean := "van blerk"]
saf_long[spouse_surname_clean == "van niekerk van nieuwkerken", spouse_surname_clean := "van niekerk"]
# saf_long[spouse_surname_clean == "van lier van ryneveld", spouse_surname_clean := "van lier van ryneveld"]
saf_long[spouse_surname_clean == "van vreeden van heerden", spouse_surname_clean := "van vreeden"]
saf_long[spouse_surname_clean == "van der walt van", spouse_surname_clean := "van der walt"]
saf_long[spouse_surname_clean == "de graaff de graeff", spouse_surname_clean := "de graaff"]
saf_long[spouse_surname_clean == "pas van pas", spouse_surname_clean := "pas"]
saf_long[spouse_surname_clean == "van aardt van aarde", spouse_surname_clean := "van aardt"]
# saf_long[spouse_surname_clean == "van heyde van josowe", spouse_surname_clean := "van heyde van josowe"]
saf_long[spouse_surname_clean == "van wijk van wyk",
    spouse_surname_clean := "van wijk"]
# saf_long[spouse_surname_clean == "van rheede van oudtshoorn", spouse_surname_clean := "van rheede van oudtshoorn"]
saf_long[spouse_surname_clean == "gould van gould",
    spouse_surname_clean := "van gould"]
saf_long[spouse_surname_clean == "janse van rensburg janse van vuuren",
    spouse_surname_clean := "janse van rensburg janse van vuuren"]
saf_long[spouse_surname_clean == "boshoff boshoff",
    spouse_surname_clean := "boshoff"]
saf_long[spouse_surname_clean == "van riet van der riet",
    spouse_surname_clean := "van riet"]
saf_long[spouse_surname_clean == "roux le roux",
    spouse_surname_clean := "le roux"]
saf_long[spouse_surname_clean == "lawson manson lawson",
    spouse_surname_clean := "lawson"]
saf_long[spouse_surname_clean == "van vuuren van vuren",
    spouse_surname_clean := "van vuuren "]
saf_long[spouse_surname_clean == "van der nescht van der mescht",
    spouse_surname_clean := "van der nescht"]
saf_long[spouse_surname_clean == "van stryp van streep",
    spouse_surname_clean := "van stryp"]
saf_long[spouse_surname_clean == "van vuren van vuuren",
    spouse_surname_clean := "van vuren"]
saf_long[spouse_surname_clean == "van reede van oudtshoorn",
    spouse_surname_clean := "van reede van oudtshoorn"]
saf_long[spouse_surname_clean == "van neck van eck",
    spouse_surname_clean := "van neck"]
saf_long[spouse_surname_clean == "schormann schormann budewitz",
    spouse_surname_clean := "schormann budewitz"]
saf_long[spouse_surname_clean == "rowe rowe",
    spouse_surname_clean := "rowe"]
saf_long[spouse_surname_clean == "carroll s a nn a",
    spouse_surname_clean := "carroll"]
saf_long[spouse_surname_clean == "van rheede van oudsthoorn",
    spouse_surname_clean := "van rheede van oudsthoorn"]
saf_long[spouse_surname_clean == "de blanche ter blanche",
    spouse_surname_clean := "de blanche"]
saf_long[spouse_surname_clean == "de curmink de capelle",
    spouse_surname_clean := "de curmink de capelle"]
saf_long[spouse_surname_clean == "van reede van oudshoorn",
    spouse_surname_clean := "van reede van oudshoorn"]
saf_long[spouse_surname_clean == "snyman snyman",
    spouse_surname_clean := "snyman"]
saf_long[spouse_surname_clean == "van der merwe van niekerk",
    spouse_surname_clean := "van der merwe van niekerk"]
saf_long[spouse_surname_clean == "de jongh de jonge",
    spouse_surname_clean := "de jongh"]
saf_long[spouse_surname_clean == "van rheede van oudshoorn",
    spouse_surname_clean := "van rheede van oudshoorn"]
saf_long[spouse_surname_clean == "greeff de greeff",
    spouse_surname_clean := "greeff"]
saf_long[spouse_surname_clean == "van schalkwyk van schalkwijk",
    spouse_surname_clean := "van schalkwyk"]
saf_long[spouse_surname_clean == "van den berg van der berg",
    spouse_surname_clean := "van den berg"]
saf_long[spouse_surname_clean == "van lier van ryneveld",
    spouse_surname_clean := "van lier van ryneveld"]
saf_long[spouse_surname_clean == "van beest van andel",
    spouse_surname_clean := "van beest van andel"]
saf_long[spouse_surname_clean == "walters walters",
    spouse_surname_clean := "walters"]
saf_long[spouse_surname_clean == "du rand du raan",
    spouse_surname_clean := "du rand"]
saf_long[spouse_surname_clean == "smit j j smith",
    spouse_surname_clean := "smit"]


# and then redo with some sort of fuzzy one
# surnames first

saf_long[spouse_surname_clean == "esterhuizen esterhuyse", spouse_surname_clean := "esterhuizen"]
saf_long[spouse_surname_clean == "odendal odendaal", spouse_surname_clean := "odendal"]
saf_long[spouse_surname_clean == "benadie benade", spouse_surname_clean := "benadie"]
saf_long[spouse_surname_clean == "rood rode rhoode rohde", spouse_surname_clean := "rood"]
saf_long[spouse_surname_clean == "coetzee coetsee", spouse_surname_clean := "coetzee"]
saf_long[spouse_surname_clean == "esterhuizen esterhuis", spouse_surname_clean := "esterhuizen"]
saf_long[spouse_surname_clean == "gildenhuyzen gildenhuys", spouse_surname_clean := "gildenhuyzen"]
saf_long[spouse_surname_clean == "claasen klaasen", spouse_surname_clean := "claasen"]
saf_long[spouse_surname_clean == "louwrens lourens", spouse_surname_clean := "louwrens"]
saf_long[spouse_surname_clean == "kleinsmith kleynsmith", spouse_surname_clean := "kleinsmith"]
saf_long[spouse_surname_clean == "geldenhuyzen gildenhuysen", spouse_surname_clean := "geldenhuyzen"]
saf_long[spouse_surname_clean == "claasen claassen", spouse_surname_clean := "claasen"]
saf_long[spouse_surname_clean == "gildenhaus gildenhausen gildenhuyzen", spouse_surname_clean := "gildenhaus"]
saf_long[spouse_surname_clean == "soesman soszmann", spouse_surname_clean := "soesman"]
saf_long[spouse_surname_clean == "thyssen theysen theyssen", spouse_surname_clean := "thyssen"]
saf_long[spouse_surname_clean == "jacobse jacobussen", spouse_surname_clean := "jacobse"]
saf_long[spouse_surname_clean == "knoetze knoedzen", spouse_surname_clean := "knoetze"]
saf_long[spouse_surname_clean == "melvill mellville", spouse_surname_clean := "melvill"]
saf_long[spouse_surname_clean == "cornelisse cornelissen", spouse_surname_clean := "cornelisse"]
saf_long[spouse_surname_clean == "van der wat watt", spouse_surname_clean := "van der wat"]
saf_long[spouse_surname_clean == "sass sals", spouse_surname_clean := "sass"]
saf_long[spouse_surname_clean == "schoultz scholtz", spouse_surname_clean := "schoultz"]
saf_long[spouse_surname_clean == "lamprecht lambrechts", spouse_surname_clean := "lamprecht"]
saf_long[spouse_surname_clean == "terblanche terblans", spouse_surname_clean := "terblanche"]
saf_long[spouse_surname_clean == "jarling jerling", spouse_surname_clean := "jarling"]
saf_long[spouse_surname_clean == "coleskie colesky", spouse_surname_clean := "coleskie"]
saf_long[spouse_surname_clean == "van der westhuizen westhuysen", spouse_surname_clean := "van der westhuizen"]
saf_long[spouse_surname_clean == "nieuwenhuizen nieuwenhuyse", spouse_surname_clean := "nieuwenhuizen"]
saf_long[spouse_surname_clean == "spruyt spruit", spouse_surname_clean := "spruyt"]
saf_long[spouse_surname_clean == "olwagen olwage", spouse_surname_clean := "olwagen"]
saf_long[spouse_surname_clean == "genade gnade", spouse_surname_clean := "genade"]
saf_long[spouse_surname_clean == "elinckhuyzen ellinckhuizen", spouse_surname_clean := "elinckhuyzen"]
saf_long[spouse_surname_clean == "rose rous", spouse_surname_clean := "rose"]
saf_long[spouse_surname_clean == "grobler grobbelaar", spouse_surname_clean := "grobler"]
saf_long[spouse_surname_clean == "witsche wietsche", spouse_surname_clean := "witsche"]
saf_long[spouse_surname_clean == "feyt fiet", spouse_surname_clean := "feyt"]
saf_long[spouse_surname_clean == "agenbag aggenbach", spouse_surname_clean := "agenbag"]
saf_long[spouse_surname_clean == "jordaan jourdan", spouse_surname_clean := "jordaan"]
saf_long[spouse_surname_clean == "cranmer crammer", spouse_surname_clean := "cranmer"]
saf_long[spouse_surname_clean == "wilhelms wilhelmie", spouse_surname_clean := "wilhelms"]
saf_long[spouse_surname_clean == "scott schott", spouse_surname_clean := "scott"]
saf_long[spouse_surname_clean == "leeuwendal leeuwendaal", spouse_surname_clean := "leeuwendal"]
saf_long[spouse_surname_clean == "germishuys germishuis", spouse_surname_clean := "germishuys"]
saf_long[spouse_surname_clean == "hanson henson", spouse_surname_clean := "hanson"]
saf_long[spouse_surname_clean == "tipps tipp", spouse_surname_clean := "tipps"]
saf_long[spouse_surname_clean == "jooste joosten", spouse_surname_clean := "jooste"]
saf_long[spouse_surname_clean == "kuppe kupfer", spouse_surname_clean := "kuppe"]
saf_long[spouse_surname_clean == "buk buck", spouse_surname_clean := "buk"]
saf_long[spouse_surname_clean == "barrett barnet burnet", spouse_surname_clean := "barrett"]
saf_long[spouse_surname_clean == "morrison morisson", spouse_surname_clean := "morrison"]
saf_long[spouse_surname_clean == "benade benadie", spouse_surname_clean := "benade"]
saf_long[spouse_surname_clean == "vialls viallis", spouse_surname_clean := "vialls"]
saf_long[spouse_surname_clean == "winel wadel wedel", spouse_surname_clean := "winel"]
saf_long[spouse_surname_clean == "keuler keulder", spouse_surname_clean := "keuler"]
saf_long[spouse_surname_clean == "dielman thielemann", spouse_surname_clean := "dielman"]
saf_long[spouse_surname_clean == "bresler brasler", spouse_surname_clean := "bresler"]
saf_long[spouse_surname_clean == "horwarth howarth", spouse_surname_clean := "horwarth"]
saf_long[spouse_surname_clean == "moller moeller", spouse_surname_clean := "moller"]
saf_long[spouse_surname_clean == "debes delits debets", spouse_surname_clean := "debes"]
saf_long[spouse_surname_clean == "britz brits", spouse_surname_clean := "britz"]
saf_long[spouse_surname_clean == "jerling jarling", spouse_surname_clean := "jerling"]
saf_long[spouse_surname_clean == "claasens claassens", spouse_surname_clean := "claasens"]
saf_long[spouse_surname_clean == "sanders saunders", spouse_surname_clean := "sanders"]
saf_long[spouse_surname_clean == "nieuwenhuis nieuwenhuizen", spouse_surname_clean := "nieuwenhuis"]
saf_long[spouse_surname_clean == "bothma botma", spouse_surname_clean := "bothma"]
saf_long[spouse_surname_clean == "van der heyde heyden", spouse_surname_clean := "van der heyde"]
saf_long[spouse_surname_clean == "gous gouch gousch", spouse_surname_clean := "gous"]
saf_long[spouse_surname_clean == "smit smith", spouse_surname_clean := "smit"]
saf_long[spouse_surname_clean == "olwage olwagen", spouse_surname_clean := "olwage"]
saf_long[spouse_surname_clean == "heyman heymans", spouse_surname_clean := "heyman"]
saf_long[spouse_surname_clean == "gilkins gilkens", spouse_surname_clean := "gilkins"]
saf_long[spouse_surname_clean == "stimie stiemie", spouse_surname_clean := "stimie"]
saf_long[spouse_surname_clean == "grobbelaar grobler", spouse_surname_clean := "grobbelaar"]
saf_long[spouse_surname_clean == "scholtz schutz", spouse_surname_clean := "scholtz"]
saf_long[spouse_surname_clean == "propert propart", spouse_surname_clean := "propert"]
saf_long[spouse_surname_clean == "bowyer bower", spouse_surname_clean := "bowyer"]
saf_long[spouse_surname_clean == "muller mulder", spouse_surname_clean := "muller"]
saf_long[spouse_surname_clean == "claassens claasen", spouse_surname_clean := "claassens"]
saf_long[spouse_surname_clean == "celliers cilliers", spouse_surname_clean := "celliers"]
saf_long[spouse_surname_clean == "kraay van kraayenburg", spouse_surname_clean := "kraay"]
saf_long[spouse_surname_clean == "martens martins", spouse_surname_clean := "martens"]
saf_long[spouse_surname_clean == "duvenhage duvenage", spouse_surname_clean := "duvenhage"]
saf_long[spouse_surname_clean == "gerieke gericke", spouse_surname_clean := "gerieke"]
saf_long[spouse_surname_clean == "feagan fagan", spouse_surname_clean := "feagan"]
saf_long[spouse_surname_clean == "rog rogge", spouse_surname_clean := "rog"]
saf_long[spouse_surname_clean == "thomasse thomas", spouse_surname_clean := "thomasse"]
saf_long[spouse_surname_clean == "vorster voster", spouse_surname_clean := "vorster"]
saf_long[spouse_surname_clean == "nortier nortje", spouse_surname_clean := "nortier"]
saf_long[spouse_surname_clean == "rousseau rosseau", spouse_surname_clean := "rousseau"]
saf_long[spouse_surname_clean == "schultz scholtz", spouse_surname_clean := "schultz"]
saf_long[spouse_surname_clean == "van den hefer heever", spouse_surname_clean := "van den hefer"]
saf_long[spouse_surname_clean == "jerrom jerrome", spouse_surname_clean := "jerrom"]
saf_long[spouse_surname_clean == "reinders reynders", spouse_surname_clean := "reinders"]
saf_long[spouse_surname_clean == "stens steens", spouse_surname_clean := "stens"]
saf_long[spouse_surname_clean == "linstr m linston", spouse_surname_clean := "linstr"]
saf_long[spouse_surname_clean == "bastro backstroo", spouse_surname_clean := "bastro"]
saf_long[spouse_surname_clean == "lafenut lafenue", spouse_surname_clean := "lafenut"]
saf_long[spouse_surname_clean == "herbert herbst", spouse_surname_clean := "herbert"]
saf_long[spouse_surname_clean == "oosthuizen oosthuyzen", spouse_surname_clean := "oosthuizen"]
saf_long[spouse_surname_clean == "troskie trosche trotsche", spouse_surname_clean := "troskie"]
saf_long[spouse_surname_clean == "schmidt schmit", spouse_surname_clean := "schmidt"]
saf_long[spouse_surname_clean == "de nysschen nysschens", spouse_surname_clean := "de nysschen"]
saf_long[spouse_surname_clean == "weyers wyer", spouse_surname_clean := "weyers"]
saf_long[spouse_surname_clean == "cornelius cornelis", spouse_surname_clean := "cornelius"]
saf_long[spouse_surname_clean == "zappel sappel", spouse_surname_clean := "zappel"]
saf_long[spouse_surname_clean == "hofmeester hofmeister", spouse_surname_clean := "hofmeester"]
saf_long[spouse_surname_clean == "heydenrych heydenrich", spouse_surname_clean := "heydenrych"]
saf_long[spouse_surname_clean == "mulder muller", spouse_surname_clean := "mulder"]
saf_long[spouse_surname_clean == "lombard lombaard", spouse_surname_clean := "lombard"]
saf_long[spouse_surname_clean == "wagenaar wagener", spouse_surname_clean := "wagenaar"]
saf_long[spouse_surname_clean == "greifenstein greyvenstein", spouse_surname_clean := "greifenstein"]
saf_long[spouse_surname_clean == "janeke j neke", spouse_surname_clean := "janeke"]
saf_long[spouse_surname_clean == "van loggerenberg loggenberg", spouse_surname_clean := "van loggerenberg"]
saf_long[spouse_surname_clean == "steinberg steynberg steenberg", spouse_surname_clean := "steinberg"]
saf_long[spouse_surname_clean == "nortje nortier", spouse_surname_clean := "nortje"]
saf_long[spouse_surname_clean == "cilliers celliers", spouse_surname_clean := "cilliers"]
saf_long[spouse_surname_clean == "myburgh meyburg meiburg meiburgh", spouse_surname_clean := "myburgh"]
saf_long[spouse_surname_clean == "mienie minnie mennen", spouse_surname_clean := "mienie"]
saf_long[spouse_surname_clean == "greyvenstein greifenstein greyvensteyn", spouse_surname_clean := "greyvenstein"]
saf_long[spouse_surname_clean == "breed breedt", spouse_surname_clean := "breed"]
saf_long[spouse_surname_clean == "stols stolz", spouse_surname_clean := "stols"]
saf_long[spouse_surname_clean == "nel nell", spouse_surname_clean := "nel"]
saf_long[spouse_surname_clean == "gouws gous", spouse_surname_clean := "gouws"]
saf_long[spouse_surname_clean == "swart swarts", spouse_surname_clean := "swart"]
saf_long[spouse_surname_clean == "koorzen koorze koorts goosen", spouse_surname_clean := "koorzen"]
saf_long[spouse_surname_clean == "tinderholm tinnerholm", spouse_surname_clean := "tinderholm"]
saf_long[spouse_surname_clean == "baasden basden baarden", spouse_surname_clean := "baasden"]
saf_long[spouse_surname_clean == "lombaard lombard", spouse_surname_clean := "lombaard"]
saf_long[spouse_surname_clean == "renken renneke", spouse_surname_clean := "renken"]
saf_long[spouse_surname_clean == "weber weeber", spouse_surname_clean := "weber"]
saf_long[spouse_surname_clean == "ollewage oldenwagen", spouse_surname_clean := "ollewage"]
saf_long[spouse_surname_clean == "blom bloem", spouse_surname_clean := "blom"]
saf_long[spouse_surname_clean == "reickert riekert", spouse_surname_clean := "reickert"]
saf_long[spouse_surname_clean == "wege wegen", spouse_surname_clean := "wege"]
saf_long[spouse_surname_clean == "rossouw russouw", spouse_surname_clean := "rossouw"]
saf_long[spouse_surname_clean == "buschick buschiek", spouse_surname_clean := "buschick"]
saf_long[spouse_surname_clean == "la querenne guerenne", spouse_surname_clean := "la querenne"]
saf_long[spouse_surname_clean == "mathen mathew", spouse_surname_clean := "mathen"]
saf_long[spouse_surname_clean == "butger bottger", spouse_surname_clean := "butger"]
saf_long[spouse_surname_clean == "schoombie schoonbee", spouse_surname_clean := "schoombie"]
saf_long[spouse_surname_clean == "eduards edwards", spouse_surname_clean := "eduards"]
saf_long[spouse_surname_clean == "muller m ller", spouse_surname_clean := "muller"]
saf_long[spouse_surname_clean == "briene briese", spouse_surname_clean := "briene"]
saf_long[spouse_surname_clean == "marits maritz", spouse_surname_clean := "marits"]
saf_long[spouse_surname_clean == "holtshausen holzausen", spouse_surname_clean := "holtshausen"]
saf_long[spouse_surname_clean == "zaayman zaaiman", spouse_surname_clean := "zaayman"]
saf_long[spouse_surname_clean == "mar marais", spouse_surname_clean := "mar"]
saf_long[spouse_surname_clean == "bresler braesler", spouse_surname_clean := "bresler"]
saf_long[spouse_surname_clean == "herschelman herselmann hesselman", spouse_surname_clean := "herschelman"]
saf_long[spouse_surname_clean == "raath raats", spouse_surname_clean := "raath"]
saf_long[spouse_surname_clean == "drotschie drotskie", spouse_surname_clean := "drotschie"]
saf_long[spouse_surname_clean == "keck kock", spouse_surname_clean := "keck"]
saf_long[spouse_surname_clean == "van imweegen nimwegen", spouse_surname_clean := "van nimwegen"]
saf_long[spouse_surname_clean == "steynberg steenberg", spouse_surname_clean := "steynberg"]
saf_long[spouse_surname_clean == "meyer meijer", spouse_surname_clean := "meyer"]
saf_long[spouse_surname_clean == "bisschoff bishof", spouse_surname_clean := "bisschoff"]
saf_long[spouse_surname_clean == "buitendach buytendag", spouse_surname_clean := "buitendach"]
saf_long[spouse_surname_clean == "megel meegel", spouse_surname_clean := "megel"]
saf_long[spouse_surname_clean == "heyberg heiberg", spouse_surname_clean := "heyberg"]
saf_long[spouse_surname_clean == "gebhart giphart", spouse_surname_clean := "gebhart"]
saf_long[spouse_surname_clean == "pieters peters", spouse_surname_clean := "pieters"]
saf_long[spouse_surname_clean == "smits smuts", spouse_surname_clean := "smits"]
saf_long[spouse_surname_clean == "schreyn schreen", spouse_surname_clean := "schreyn"]
saf_long[spouse_surname_clean == "lanfersiete lanferniete", spouse_surname_clean := "lanfersiete"]
saf_long[spouse_surname_clean == "tendinger tentinger", spouse_surname_clean := "tendinger"]
saf_long[spouse_surname_clean == "jameson jamison", spouse_surname_clean := "jameson"]
saf_long[spouse_surname_clean == "smith smit", spouse_surname_clean := "smith"]
saf_long[spouse_surname_clean == "boersma boorsma", spouse_surname_clean := "boersma"]
saf_long[spouse_surname_clean == "hitsman hitzmann", spouse_surname_clean := "hitsman"]
saf_long[spouse_surname_clean == "noordman nortman", spouse_surname_clean := "noordman"]
saf_long[spouse_surname_clean == "wimmer wemmer", spouse_surname_clean := "wimmer"]
saf_long[spouse_surname_clean == "blake black", spouse_surname_clean := "blake"]
saf_long[spouse_surname_clean == "keer keur", spouse_surname_clean := "keer"]
saf_long[spouse_surname_clean == "huisamen huysamer", spouse_surname_clean := "huisamen"]
saf_long[spouse_surname_clean == "greybe grebe", spouse_surname_clean := "greybe"]
saf_long[spouse_surname_clean == "gray grey", spouse_surname_clean := "gray"]
saf_long[spouse_surname_clean == "lowies lewis", spouse_surname_clean := "lowies"]
saf_long[spouse_surname_clean == "crause crous", spouse_surname_clean := "crause"]
saf_long[spouse_surname_clean == "robinson robertson", spouse_surname_clean := "robinson"]
saf_long[spouse_surname_clean == "kock kok", spouse_surname_clean := "kock"]
saf_long[spouse_surname_clean == "leeb leep", spouse_surname_clean := "leeb"]
saf_long[spouse_surname_clean == "dilke delke", spouse_surname_clean := "dilke"]
saf_long[spouse_surname_clean == "onkruyt onkruydt", spouse_surname_clean := "onkruyt"]
saf_long[spouse_surname_clean == "haak hack", spouse_surname_clean := "haak"]
saf_long[spouse_surname_clean == "wansenburg wansenberg", spouse_surname_clean := "wansenburg"]
saf_long[spouse_surname_clean == "brits britz", spouse_surname_clean := "brits"]
saf_long[spouse_surname_clean == "coertze coetzee", spouse_surname_clean := "coetzee"]
saf_long[spouse_surname_clean == "renke reynecke", spouse_surname_clean := "renke"]
saf_long[spouse_surname_clean == "pottas potas", spouse_surname_clean := "pottas"]
saf_long[spouse_surname_clean == "fourie vourie", spouse_surname_clean := "fourie"]
saf_long[spouse_surname_clean == "fourie faurie", spouse_surname_clean := "fourie"]
saf_long[spouse_surname_clean == "booysen booyze", spouse_surname_clean := "booysen"]
saf_long[spouse_surname_clean == "lourens laurens", spouse_surname_clean := "lourens"]
saf_long[spouse_surname_clean == "bezuidenhout bezuidenhoud", spouse_surname_clean := "bezuidenhout"]
saf_long[spouse_surname_clean == "breytenbach breitenbach", spouse_surname_clean := "breytenbach"]
saf_long[spouse_surname_clean == "kleyngeld kleingeld", spouse_surname_clean := "kleyngeld"]
saf_long[spouse_surname_clean == "elard ellart", spouse_surname_clean := "elard"]
saf_long[spouse_surname_clean == "prien priem", spouse_surname_clean := "prien"]
saf_long[spouse_surname_clean == "bouwer bauer", spouse_surname_clean := "bouwer"]
saf_long[spouse_surname_clean == "coetsee coetzee", spouse_surname_clean := "coetzee"]
saf_long[spouse_surname_clean == "calitz gallitz", spouse_surname_clean := "calitz"]
saf_long[spouse_surname_clean == "greyffenberg greifenberg", spouse_surname_clean := "greyffenberg"]
saf_long[spouse_surname_clean == "steger steiger", spouse_surname_clean := "steger"]
saf_long[spouse_surname_clean == "gerrits gertsen", spouse_surname_clean := "gerrits"]
saf_long[spouse_surname_clean == "carstens karsten", spouse_surname_clean := "carstens"]
saf_long[spouse_surname_clean == "loubser laubscher", spouse_surname_clean := "loubser"]
saf_long[spouse_surname_clean == "robertson robinson", spouse_surname_clean := "robertson"]
saf_long[spouse_surname_clean == "claasen clasens", spouse_surname_clean := "claasen"]
saf_long[spouse_surname_clean == "prevot privo", spouse_surname_clean := "prevot"]
saf_long[spouse_surname_clean == "goosen speykerman spykerman", spouse_surname_clean := "goosen speykerman"]
saf_long[spouse_surname_clean == "claasz claasen", spouse_surname_clean := "claasz"]
saf_long[spouse_surname_clean == "fortman voortman", spouse_surname_clean := "fortman"]
saf_long[spouse_surname_clean == "coetzee cloete", spouse_surname_clean := "coetzee"]
saf_long[spouse_surname_clean == "drotske drotskie", spouse_surname_clean := "drotske"]
saf_long[spouse_surname_clean == "esterhuizen esterhuys", spouse_surname_clean := "esterhuizen"]
saf_long[spouse_surname_clean == "eygelaar eigelaar", spouse_surname_clean := "eygelaar"]
saf_long[spouse_surname_clean == "krause kruse", spouse_surname_clean := "krause"]
saf_long[spouse_surname_clean == "laurence lawrence", spouse_surname_clean := "laurence"]
saf_long[spouse_surname_clean == "hermanus hermans", spouse_surname_clean := "hermanus"]
saf_long[spouse_surname_clean == "peensch peens", spouse_surname_clean := "peensch"]
saf_long[spouse_surname_clean == "roos roosen", spouse_surname_clean := "roos"]
saf_long[spouse_surname_clean == "brunette burnett", spouse_surname_clean := "brunette"]
saf_long[spouse_surname_clean == "de bruin du bruyn", spouse_surname_clean := "de bruin"]
saf_long[spouse_surname_clean == "kriel kriek", spouse_surname_clean := "kriel"]
saf_long[spouse_surname_clean == "schimper schemper", spouse_surname_clean := "schimper"]
saf_long[spouse_surname_clean == "donovan duursen donsan", spouse_surname_clean := "donovan"]
saf_long[spouse_surname_clean == "elmoor elmore", spouse_surname_clean := "elmoor"]
saf_long[spouse_surname_clean == "reyniersen reiniers", spouse_surname_clean := "reyniersen"]
saf_long[spouse_surname_clean == "ttchen ttgen", spouse_surname_clean := "ttchen"]
saf_long[spouse_surname_clean == "pheyer pheiffer", spouse_surname_clean := "pheyer"]
saf_long[spouse_surname_clean == "heinenberg heyneberg", spouse_surname_clean := "heinenberg"]
saf_long[spouse_surname_clean == "ziemann zeemann", spouse_surname_clean := "ziemann"]

# basically has to be done manually
# saf_long[spouse_surname_clean == "van heyde van josowe", spouse_surname_clean := "van heyde van josowe"]
saf_long[spouse_surname_clean == "putter putters", spouse_surname_clean := "putter"]
saf_long[spouse_surname_clean == "matthijs matthys", spouse_surname_clean := "matthijs"]
saf_long[spouse_surname_clean == "achenbach aggenbach", spouse_surname_clean := "achenbach"]
saf_long[spouse_surname_clean == "gildenhuys geldenhuys", spouse_surname_clean := "gildenhuys"]
saf_long[spouse_surname_clean == "wools woolls", spouse_surname_clean := "wools"]
saf_long[spouse_surname_clean == "barti bartie", spouse_surname_clean := "barti"]
saf_long[spouse_surname_clean == "gotthart gotthard", spouse_surname_clean := "gotthart"]
saf_long[spouse_surname_clean == "du plessis w d", spouse_surname_clean := "du plessis"]
saf_long[spouse_surname_clean == "mathiesen mathysen", spouse_surname_clean := "mathiesen"]
saf_long[spouse_surname_clean == "brytenbach breytenbach", spouse_surname_clean := "brytenbach"]
saf_long[spouse_surname_clean == "howell howells", spouse_surname_clean := "howell"]
saf_long[spouse_surname_clean == "buitendag buitendacht", spouse_surname_clean := "buitendag"]
saf_long[spouse_surname_clean == "daensdons daaldons dansdons", spouse_surname_clean := "daensdons"]
saf_long[spouse_surname_clean == "seale searle", spouse_surname_clean := "seale"]
saf_long[spouse_surname_clean == "barnard burnard", spouse_surname_clean := "barnard"]
saf_long[spouse_surname_clean == "besselaar beeslaar", spouse_surname_clean := "besselaar"]
saf_long[spouse_surname_clean == "eales jales", spouse_surname_clean := "eales"]
saf_long[spouse_surname_clean == "eckhard eckard", spouse_surname_clean := "eckhard"]
saf_long[spouse_surname_clean == "alers ahlers", spouse_surname_clean := "alers"]
saf_long[spouse_surname_clean == "treurnicht trauernicht", spouse_surname_clean := "treurnicht"]
saf_long[spouse_surname_clean == "krugel kruger", spouse_surname_clean := "krugel"]
# saf_long[spouse_surname_clean == "van rheede van oudtshoorn" spouse_surname_clean := "van rheede van oudtshoorn"]
saf_long[spouse_surname_clean == "beukes boukes", spouse_surname_clean := "beukes"]
saf_long[spouse_surname_clean == "howarth horwarth", spouse_surname_clean := "howarth"]
saf_long[spouse_surname_clean == "lathom latham sharp", spouse_surname_clean := "lathom sharp"]
saf_long[spouse_surname_clean == "barnett bennett", spouse_surname_clean := "barnett"]
saf_long[spouse_surname_clean == "hurr hurd", spouse_surname_clean := "hurr"]
saf_long[spouse_surname_clean == "la cock lacock", spouse_surname_clean := "la cock"]
saf_long[spouse_surname_clean == "heuser huyser", spouse_surname_clean := "heuser"]
saf_long[spouse_surname_clean == "slabbert slabber", spouse_surname_clean := "slabbert"]
saf_long[spouse_surname_clean == "kotze kotzee", spouse_surname_clean := "kotze"]
saf_long[spouse_surname_clean == "craffert crafford", spouse_surname_clean := "craffert"]
saf_long[spouse_surname_clean == "crause krause", spouse_surname_clean := "crause"]
saf_long[spouse_surname_clean == "droskie drotskie", spouse_surname_clean := "droskie"]
saf_long[spouse_surname_clean == "trengrove trengove", spouse_surname_clean := "trengrove"]
saf_long[spouse_surname_clean == "carnie carney", spouse_surname_clean := "carnie"]
saf_long[spouse_surname_clean == "eysele yssel", spouse_surname_clean := "eysele"]
saf_long[spouse_surname_clean == "schr der schrade", spouse_surname_clean := "schrade"]
saf_long[spouse_surname_clean == "buitendach buitendag", spouse_surname_clean := "buitendach"]
saf_long[spouse_surname_clean == "blignault blignaut", spouse_surname_clean := "blignault"]
saf_long[spouse_surname_clean == "nieman niemand", spouse_surname_clean := "nieman"]
saf_long[spouse_surname_clean == "jorissen jorrison", spouse_surname_clean := "jorissen"]
saf_long[spouse_surname_clean == "boonzaier boonzaaier", spouse_surname_clean := "boonzaier"]
saf_long[spouse_surname_clean == "emslie elsmee", spouse_surname_clean := "emslie"]
saf_long[spouse_surname_clean == "janse van rensburg janse van vuuren", spouse_surname_clean := "janse van rensburg"]
saf_long[spouse_surname_clean == "dover doyer", spouse_surname_clean := "dover"]
saf_long[spouse_surname_clean == "snijman snyman", spouse_surname_clean := "snijman"]
saf_long[spouse_surname_clean == "van der westhuizen westhuyzen", spouse_surname_clean := "van der westhuizen"]
saf_long[spouse_surname_clean == "blignaut blignault", spouse_surname_clean := "blignaut"]
saf_long[spouse_surname_clean == "dafel davel", spouse_surname_clean := "dafel"]
saf_long[spouse_surname_clean == "gilmor gillmour", spouse_surname_clean := "gilmor"]
saf_long[spouse_surname_clean == "cilliers cilli", spouse_surname_clean := "cilliers"]
saf_long[spouse_surname_clean == "marais mar", spouse_surname_clean := "marais"]
saf_long[spouse_surname_clean == "tammadge talmage", spouse_surname_clean := "tammadge"]
saf_long[spouse_surname_clean == "melsome melson", spouse_surname_clean := "melsome"]
saf_long[spouse_surname_clean == "sanneberg sandenberg", spouse_surname_clean := "sanneberg"]
saf_long[spouse_surname_clean == "saayman zaayman", spouse_surname_clean := "saayman"]
saf_long[spouse_surname_clean == "etshells etchells", spouse_surname_clean := "etshells"]
saf_long[spouse_surname_clean == "coetzer coetzee", spouse_surname_clean := "coetzee"]
saf_long[spouse_surname_clean == "halgryn halgreen", spouse_surname_clean := "halgryn"]
saf_long[spouse_surname_clean == "mijnhardt mynhardt", spouse_surname_clean := "mijnhardt"]
saf_long[spouse_surname_clean == "sch n scheun", spouse_surname_clean := "scheun"]
saf_long[spouse_surname_clean == "martins maartens", spouse_surname_clean := "martins"]
saf_long[spouse_surname_clean == "bothma bothman", spouse_surname_clean := "bothma"]
saf_long[spouse_surname_clean == "long lang", spouse_surname_clean := "long"]
saf_long[spouse_surname_clean == "ngk troskie trotsky", spouse_surname_clean := "trotsky"]
saf_long[spouse_surname_clean == "kuipers kuypers kuyper kuiper", spouse_surname_clean := "kuipers"]
saf_long[spouse_surname_clean == "jurgens jurgins", spouse_surname_clean := "jurgens"]
saf_long[spouse_surname_clean == "pieterse pietersen", spouse_surname_clean := "pieterse"]
saf_long[spouse_surname_clean == "mare maree", spouse_surname_clean := "mare"]
saf_long[spouse_surname_clean == "stoltz stols", spouse_surname_clean := "stoltz"]
saf_long[spouse_surname_clean == "maree mare", spouse_surname_clean := "maree"]
saf_long[spouse_surname_clean == "teubes thebus tebis tibus", spouse_surname_clean := "teubes"]
saf_long[spouse_surname_clean == "bruwer bouwer", spouse_surname_clean := "bruwer"]
saf_long[spouse_surname_clean == "crouse crause", spouse_surname_clean := "crouse"]
saf_long[spouse_surname_clean == "claasen claassens claase", spouse_surname_clean := "claasen"]
saf_long[spouse_surname_clean == "cerfontein serfontein", spouse_surname_clean := "cerfontein"]
saf_long[spouse_surname_clean == "piejaater piater", spouse_surname_clean := "piejaater"]
# saf_long[spouse_surname_clean == "van reede van oudtshoorn", spouse_surname_clean := "van reede van oudtshoorn"]
saf_long[spouse_surname_clean == "leadlow laidlaw", spouse_surname_clean := "leadlow"]
saf_long[spouse_surname_clean == "bataillon battaliou", spouse_surname_clean := "bataillon"]
saf_long[spouse_surname_clean == "berth barthy", spouse_surname_clean := "berth"]
saf_long[spouse_surname_clean == "barnard barnardt", spouse_surname_clean := "barnard"]
saf_long[spouse_surname_clean == "weideman wydeman", spouse_surname_clean := "weideman"]
saf_long[spouse_surname_clean == "meyburg myburgh", spouse_surname_clean := "meyburg"]
# saf_long[spouse_surname_clean == "van rheede van oudsthoorn", spouse_surname_clean := "van rheede van oudsthoorn"]
saf_long[spouse_surname_clean == "orpen arpen", spouse_surname_clean := "orpen"]
saf_long[spouse_surname_clean == "lourens louwrens", spouse_surname_clean := "lourens"]
saf_long[spouse_surname_clean == "lauwrens lourens", spouse_surname_clean := "lauwrens"]
saf_long[spouse_surname_clean == "thompson thomson", spouse_surname_clean := "thompson"]
saf_long[spouse_surname_clean == "veldhuizen feldhuizen", spouse_surname_clean := "veldhuizen"]
saf_long[spouse_surname_clean == "holtzhuizen holtzhausen", spouse_surname_clean := "holtzhuizen"]
# saf_long[spouse_surname_clean == "de curmink de capelle", spouse_surname_clean := "de curmink de capelle"]
# saf_long[spouse_surname_clean == "van nesen hansen", spouse_surname_clean := "van nesen hansen"]
saf_long[spouse_surname_clean == "steynvaardt steenvaart", spouse_surname_clean := "steynvaardt"]
# saf_long[spouse_surname_clean == "van reede van oudshoorn", spouse_surname_clean := "van reede van oudshoorn"]
saf_long[spouse_surname_clean == "smal sa", spouse_surname_clean := "smal"]
saf_long[spouse_surname_clean == "swart swa", spouse_surname_clean := "swart"]
saf_long[spouse_surname_clean == "kriek kriel", spouse_surname_clean := "kriek"]
saf_long[spouse_surname_clean == "kerstel kastel", spouse_surname_clean := "kerstel"]
saf_long[spouse_surname_clean == "gelowey gilowey", spouse_surname_clean := "gelowey"]
saf_long[spouse_surname_clean == "phister phyfster", spouse_surname_clean := "phister"]
saf_long[spouse_surname_clean == "van der merwe van niekerk", spouse_surname_clean := "van der merwe"]
saf_long[spouse_surname_clean == "malet mallet", spouse_surname_clean := "malet"]
saf_long[spouse_surname_clean == "le kleu", spouse_surname_clean := "le kleu"]
saf_long[spouse_surname_clean == "liddiard lidiard liddard lediet", spouse_surname_clean := "liddiard"]
saf_long[spouse_surname_clean == "pinard pienaar", spouse_surname_clean := "pinard"]
saf_long[spouse_surname_clean == "homan human", spouse_surname_clean := "homan"]
saf_long[spouse_surname_clean == "de klerk van blerk", spouse_surname_clean := "de klerk"]
saf_long[spouse_surname_clean == "klaasen claasen", spouse_surname_clean := "klaasen"]
saf_long[spouse_surname_clean == "verwey verweij", spouse_surname_clean := "verwey"]
saf_long[spouse_surname_clean == "karstens carstens", spouse_surname_clean := "karstens"]
saf_long[spouse_surname_clean == "meyntjes meyntings menthing", spouse_surname_clean := "meyntjes"]
saf_long[spouse_surname_clean == "barenskie barensche", spouse_surname_clean := "barenskie"]
saf_long[spouse_surname_clean == "claas claasen", spouse_surname_clean := "claas"]
saf_long[spouse_surname_clean == "basson van ass", spouse_surname_clean := "basson"]
saf_long[spouse_surname_clean == "doege dooge", spouse_surname_clean := "doege"]
saf_long[spouse_surname_clean == "niewinhout nieuwoudt", spouse_surname_clean := "niewinhout"]
saf_long[spouse_surname_clean == "range ronge", spouse_surname_clean := "range"]
# saf_long[spouse_surname_clean == "van rheede van oudshoorn", spouse_surname_clean := "van rheede van oudshoorn"]
saf_long[spouse_surname_clean == "jordaan jourdaan", spouse_surname_clean := "jordaan"]
saf_long[spouse_surname_clean == "moller muller", spouse_surname_clean := "moller"]
saf_long[spouse_surname_clean == "olivier oliver", spouse_surname_clean := "olivier"]
saf_long[spouse_surname_clean == "claassen klaassen", spouse_surname_clean := "claassen"]
saf_long[spouse_surname_clean == "van lier van ryneveld", spouse_surname_clean := "van lier"]
saf_long[spouse_surname_clean == "van beest van andel", spouse_surname_clean := "van andel"]
saf_long[spouse_surname_clean == "oldewage olwage", spouse_surname_clean := "olwage"]
saf_long[spouse_surname_clean == "koch kock", spouse_surname_clean := "koch"]
saf_long[spouse_surname_clean == "wickham wichman", spouse_surname_clean := "wickham"]
saf_long[spouse_surname_clean == "miller muller", spouse_surname_clean := "miller"]
saf_long[spouse_surname_clean == "kachelhouer kachelhoffer", spouse_surname_clean := "kachelhouer"]
saf_long[spouse_surname_clean == "scherman sherman", spouse_surname_clean := "scherman"]
saf_long[spouse_surname_clean == "palvie palvi", spouse_surname_clean := "palvie"]
saf_long[spouse_surname_clean == "kien kiens kienst", spouse_surname_clean := "kien"]
saf_long[spouse_surname_clean == "roos rose", spouse_surname_clean := "roos"]
saf_long[spouse_surname_clean == "rube rubeck", spouse_surname_clean := "rube"]
saf_long[spouse_surname_clean == "bouwer bruwer", spouse_surname_clean := "bouwer"]
saf_long[spouse_surname_clean == "wagenaar wagner", spouse_surname_clean := "wagenaar"]
saf_long[spouse_surname_clean == "devres devers", spouse_surname_clean := "devres"]
saf_long[spouse_surname_clean == "besaang besaans", spouse_surname_clean := "besaang"]
saf_long[spouse_surname_clean == "spires spiers", spouse_surname_clean := "spires"]
saf_long[spouse_surname_clean == "lindenburg lindenberg", spouse_surname_clean := "lindenburg"]
saf_long[spouse_surname_clean == "schr n schroen", spouse_surname_clean := "schroen"]
saf_long[spouse_surname_clean == "thesnaar tesner", spouse_surname_clean := "thesnaar"]
saf_long[spouse_surname_clean == "tesner thesnaar", spouse_surname_clean := "thesnaar"]

saf_long[, spouse_surname_clean_split := stri_split_boundaries(spouse_surname_clean, type = "word")]
saf_long[, fuzdupname := lapply(spouse_surname_clean_split, function(x) any(stringdistmatrix(x[!x %in% c(" ", ",", ".") & length(x) > 1], method = "jw") < 0.2))]
# saf_long[fuzdupname == TRUE, list(spouse_surname_clean, spouse_surname_clean_split)]
saf_long[fuzdupname == TRUE, cat(unique(spouse_surname_clean), sep = "\n")]
# these seem ok

# surname dist here so we don't have to do it on candidates file
saf_long[, surnamedist := stringdist(surname, spouse_surname, method = "jw")]

# trim whitespaces
saf_long[, firstnames := stri_trim_both(firstnames)]
saf_long[, surname := stri_trim_both(surname)]
saf_long[, spouse_surname_clean := stri_trim_both(spouse_surname_clean)]
saf_long[, spouse_firstnames_clean := stri_trim_both(spouse_firstnames_clean)]

# made empty when removing some places
saf_long[spouse_firstnames_clean == "", spouse_firstnames_clean := NA]

saf_long[surname == "muller sien ook mulder", surname := "mulder"]
# saf_long[order(nchar(surname))], cat(unique(surname), sep = "\n")]
# looks ok

# maybe also some way to get all surnames and check whether there in firstnames?
    # stringdist::afind?
top500 = saf[, .N, by = surname][order(-N)][1:500]
surnamepattern = paste(top500$surname, collapse = "$|\\b")
surnamepattern = paste0("\\b", surnamepattern, "$")
saf_long[, surname_in_firstname := stringi::stri_detect_regex(spouse_firstnames_clean, surnamepattern)]
saf_long[surname_in_firstname == TRUE & is.na(spouse_surname_clean), spouse_surname_clean := stri_extract_last_regex(spouse_firstnames_clean, surnamepattern)]
saf_long[surname_in_firstname == TRUE & is.na(spouse_surname_clean), spouse_firstnames_clean := stri_replace_last_regex(spouse_firstnames_clean, surnamepattern, "")]
# leave alone when surname is present


# clean long surnames
# saf_long[order(nchar(firstnames)), cat(unique(firstnames), sep = "\n")]

saf_long[spouse_surname_clean == "breitschuh breedschoe", spouse_surname_clean := "breitschuh"]
saf_long[spouse_surname_clean == "sch nnberg schoennberg", spouse_surname_clean := "schonnberg"]
saf_long[spouse_surname_clean == "pretorius vier kinders", spouse_surname_clean := "pretorius"]
saf_long[spouse_surname_clean == "haussamer huisamen", spouse_surname_clean := "haussamer"]
saf_long[spouse_surname_clean == "hendriks heinrichs", spouse_surname_clean := "hendriks"]
saf_long[spouse_surname_clean == "bruinink bruyningh", spouse_surname_clean := "bruinink"]
saf_long[spouse_surname_clean == "mortimer martinson", spouse_surname_clean := "mortimer"]
saf_long[spouse_surname_clean == "giliomee guillamet", spouse_surname_clean := "guillamet"]
saf_long[spouse_surname_clean == "budler van der spuy", spouse_surname_clean := "budler"] # spui is spouse name
saf_long[spouse_surname_clean == "roodleder rothleder", spouse_surname_clean := "roodleder"]
saf_long[spouse_surname_clean == "dirk van der schyff", spouse_firstnames_clean := "dirk"]
saf_long[spouse_surname_clean == "dirk van der schyff", spouse_surname_clean := "van der schyff"]
saf_long[spouse_surname_clean == "daniel van zweeberg", spouse_firstnames_clean := "daniel"]
saf_long[spouse_surname_clean == "daniel van zweeberg", spouse_surname_clean := "van zweeberg"]
saf_long[spouse_surname_clean == "van rensburg pu cho", spouse_surname_clean := "van rensburg"]
# saf_long[spouse_surname_clean == "drc kennedy van dam", list(spouse_firstnames_clean, spouse_surname_clean)]
saf_long[spouse_surname_clean == "diskerson dickenson", spouse_surname_clean := "dickenson"]
saf_long[spouse_surname_clean == "combrinck commering", spouse_surname_clean := "combrinck"]
saf_long[spouse_surname_clean == "van biljon bouillion", spouse_surname_clean := "van biljon"]
saf_long[spouse_surname_clean == "rosettenstein l dsab", spouse_surname_clean := "rosettenstein"]
# saf_long[spouse_surname_clean == "van aswegen augustyn", list(spouse_firstnames_clean, spouse_surname_clean)]
# saf_long[spouse_surname_clean == "venter xx du plessis", list(spouse_firstnames_clean, spouse_surname_clean)]
saf_long[spouse_surname_clean == "pieter van schalkwyk", spouse_firstnames_clean := "pieter"]
saf_long[spouse_surname_clean == "pieter van schalkwyk", spouse_surname_clean := "van schalkwyk"]

saf_long[spouse_surname_clean == "van der westhuizen sa", spouse_surname_clean := "van der westhuizen"]
# saf_long[spouse_surname_clean == "roose gaarde bisschop", list(spouse_firstnames_clean, spouse_surname_clean)] # exists in surname too
saf_long[spouse_surname_clean == "nn p williams freeman", spouse_surname_clean := "williams freeman"]
# saf_long[spouse_surname_clean == "mersen senn van basel", list(spouse_firstnames_clean, spouse_surname_clean)]
# saf_long[spouse_surname_clean == "cronj xx van jaarsveld", list(spouse_firstnames_clean, spouse_surname_clean)]
saf_long[spouse_surname_clean == "janse van rensburg j g", spouse_surname_clean := "janse van rensburg"]

# clean long firstnames
# saf_long[order(-nchar(spouse_surname_clean))][1:1000][1000:1, cat(unique(spouse_surname_clean), sep = "\n")]
saf_long[spouse_firstnames_clean == "norah mccormick van engeland vier kinders is uit die huwelik gebore",
    spouse_surname_clean := "mccormick"]
saf_long[spouse_firstnames_clean == "norah mccormick van engeland vier kinders is uit die huwelik gebore",
    spouse_firstnames_clean := "norah"]
saf_long[spouse_firstnames_clean == "herman daniel bertus brauer en wilhelmine dorothea frederika sprong",
    spouse_firstnames_clean := NA]
saf_long[spouse_firstnames_clean == "kleindogter v genl louis botha twee kinders is uit sy eerste huwelik gebore",
    spouse_firstnames_clean := "botha"]
saf_long[spouse_firstnames_clean == "kleindogter v genl louis botha twee kinders is uit sy eerste huwelik gebore",
    spouse_firstnames_clean := NA]
saf_long[spouse_firstnames_clean == "wietzke van der vaart",
    spouse_surname_clean := "van der vaart"]
saf_long[spouse_firstnames_clean == "wietzke van der vaart",
    spouse_firstnames_clean := "wietzke"]

# check and clean long names
# saf_long[order(-nchar(spouse_firstnames_clean))][1:1000][1000:1, cat(unique(spouse_firstnames_clean), sep = "\n")]
saf_long[spouse_firstnames_clean == "johanna pieters v nymegen in nederland", spouse_surname_clean := "pieters"]
saf_long[spouse_firstnames_clean == "johanna pieters v nymegen in nederland", spouse_firstnames_clean := "johanna"]
saf_long[spouse_firstnames_clean == "potchefstroom maria magdalena jacomina", spouse_firstnames_clean := "maria magdalena jacomina"]
saf_long[spouse_firstnames_clean == "rachel christina van der spuy okennedy", spouse_surname_clean := "van der spuy"]
saf_long[spouse_firstnames_clean == "rachel christina van der spuy okennedy", spouse_firstnames_clean := "rachel christina"]
saf_long[spouse_firstnames_clean == "jeanetta wilhelmina catharina lombaard", spouse_surname_clean := "lombaard"]
saf_long[spouse_firstnames_clean == "jeanetta wilhelmina catharina lombaard", spouse_firstnames_clean := "jeanetta wilhelmina catharina"]
saf_long[spouse_firstnames_clean == "anna margaretha catharina van den berg", spouse_firstnames_clean := "anna margaretha catharina"]
saf_long[spouse_firstnames_clean == "herculessina petronella susanna fourie", spouse_firstnames_clean := "herculessina petronella susanna"]
saf_long[spouse_firstnames_clean == "potchefstroom cecilia cornelia magrieta", spouse_firstnames_clean := "cecilia cornelia magrieta"]
saf_long[spouse_firstnames_clean == "greylingstad johannes wilhelmus jacobus", spouse_firstnames_clean := "johannes wilhelmus jacobus"]
saf_long[spouse_firstnames_clean == "johannes hendrik stephanus van der walt", spouse_firstnames_clean := "johannes hendrik stephanus"]
saf_long[spouse_firstnames_clean == "suzanna margaretha maria bisschop boele", spouse_surname_clean := "bisschop boele"]
saf_long[spouse_firstnames_clean == "suzanna margaretha maria bisschop boele", spouse_firstnames_clean := "suzanna margaretha maria"]
saf_long[spouse_firstnames_clean == "wurttemberg en sara elisabeth nieman vd", spouse_surname_clean := "nieman"]
saf_long[spouse_firstnames_clean == "wurttemberg en sara elisabeth nieman vd", spouse_firstnames_clean := "sara elisabeth"]
saf_long[spouse_firstnames_clean == "carolina henrietta una bertha macdonald", spouse_surname_clean := "macdonald"]
saf_long[spouse_firstnames_clean == "carolina henrietta una bertha macdonald", spouse_firstnames_clean := "carolina henrietta una bertha"]
saf_long[spouse_firstnames_clean == "waarskynlik philippus andreas cornelius", spouse_firstnames_clean := "philippus andreas cornelius"]
saf_long[spouse_firstnames_clean == "uitenhage salomina catharina gerhardina", spouse_firstnames_clean := "salomina catharina gerhardina"]
saf_long[spouse_firstnames_clean == "clara elizabeth johanna von hoogenhough", spouse_firstnames_clean := "clara elizabeth johanna"]
saf_long[spouse_firstnames_clean == "woon oklipheuwelkraalo johannes jacobus", spouse_firstnames_clean := "johannes jacobus"]
saf_long[spouse_firstnames_clean == "ventersdorp honora mary teresa mcdonald", spouse_surname_clean := "mcdonald"]
saf_long[spouse_firstnames_clean == "ventersdorp honora mary teresa mcdonald", spouse_firstnames_clean := "honora mary teresa"]
saf_long[spouse_firstnames_clean == "king williams town  katharine josephine", spouse_firstnames_clean := "katharine josephine"]
# saf_long[spouse_firstnames_clean == "maria carolina andriesina stockenstroom"]
saf_long[spouse_firstnames_clean == "pietersburg janetta petronella francina", spouse_firstnames_clean := "janetta petronella francina"]
saf_long[spouse_firstnames_clean == "august frederick dange landsberg dastre", spouse_surname_clean := "landsberg dastre"]
saf_long[spouse_firstnames_clean == "august frederick dange landsberg dastre", spouse_firstnames_clean := "august frederick"]
saf_long[spouse_firstnames_clean == "johannes gerhardus marthinus oosthuizen", spouse_surname_clean := "oosthuizen"]
saf_long[spouse_firstnames_clean == "johannes gerhardus marthinus oosthuizen", spouse_firstnames_clean := "johannes gerhardus marthinus"]
saf_long[spouse_firstnames_clean == "mooije meijsjesfontein johanna hendrina", spouse_firstnames_clean := "johanna hendrina"]
saf_long[spouse_firstnames_clean == "anna magritha margaretha francina lubbe", spouse_firstnames_clean := "anna magritha margaretha francina"]
saf_long[spouse_firstnames_clean == "eastern cape luise wilhelmine ernestine", spouse_firstnames_clean := "luise wilhelmine ernestine"]
saf_long[spouse_firstnames_clean == "wilhelmina christina magdalena slabbert", spouse_surname_clean := "slabbert"]
saf_long[spouse_firstnames_clean == "wilhelmina christina magdalena slabbert", spouse_firstnames_clean := "wilhelmina christina magdalena"]
saf_long[spouse_firstnames_clean == "cornelia susanna gertruida van rensburg", spouse_surname_clean := "van rensburg"]
saf_long[spouse_firstnames_clean == "cornelia susanna gertruida van rensburg", spouse_firstnames_clean := "cornelia susanna gertruida"]
saf_long[spouse_firstnames_clean == "arie philippus cornelis johannes van eck", spouse_surname_clean := "van eck"]
saf_long[spouse_firstnames_clean == "arie philippus cornelis johannes van eck", spouse_firstnames_clean := "arie philippus cornelis johannes"]
saf_long[spouse_firstnames_clean == "koopman en sekretaris vd raad v justisie", spouse_surname_clean := "du grandpreez"]
saf_long[spouse_firstnames_clean == "koopman en sekretaris vd raad v justisie", spouse_firstnames_clean := "joseph"]
saf_long[spouse_firstnames_clean == "dorothea jacomina susanna maria lodewyks", spouse_surname_clean := "lodewyks"]
saf_long[spouse_firstnames_clean == "dorothea jacomina susanna maria lodewyks", spouse_firstnames_clean := "dorothea jacomina susanna maria"]
saf_long[spouse_firstnames_clean == "hartebeesfontein hermina elizabeth maria", spouse_firstnames_clean := "hermina elizabeth maria"]
# saf_long[spouse_firstnames_clean == "sevee napoleon mamoure tarby de la roche"]
saf_long[spouse_firstnames_clean == "colesberg met hul vader jacobus johannes", spouse_firstnames_clean := "jacobus johannes"]
saf_long[spouse_firstnames_clean == "klerksdorp anna aletta cecilia catharina", spouse_firstnames_clean := "anna aletta cecilia catharina"]
saf_long[spouse_firstnames_clean == "huibrecht maria elizabeth adriana visser", spouse_surname_clean := "visser"]
saf_long[spouse_firstnames_clean == "huibrecht maria elizabeth adriana visser", spouse_firstnames_clean := "huibrecht maria elizabeth adriana"]
saf_long[spouse_firstnames_clean == "maggel maria catharina els van der merwe", spouse_firstnames_clean := "maggel maria catharina"]
saf_long[spouse_firstnames_clean == "kroonstad catharina petronella christina", spouse_firstnames_clean := "catharina petronella christina"]
saf_long[spouse_firstnames_clean == "villiersdorp martha magdalena geertruyda", spouse_firstnames_clean := "martha magdalena geertruyda"]
saf_long[spouse_firstnames_clean == "kroonstad  isabella elizabeth margaretha", spouse_firstnames_clean := "isabella elizabeth margaretha"]
saf_long[spouse_firstnames_clean == "eastern cape friedrich heinrich christof", spouse_firstnames_clean := "friedrich heinrich christof"]
# saf_long[spouse_firstnames_clean == "johannes nicolaas christoffel terblanche"]
saf_long[spouse_firstnames_clean == "gertruida johanna catharina van der walt", spouse_firstnames_clean := "gertruida johanna catharina"]
# saf_long[spouse_firstnames_clean == "andries chritoffel van der byl neethling"]
saf_long[spouse_firstnames_clean == "cornelia margarieta ferdinand de villiers", spouse_firstnames_clean := "cornelia margarieta ferdinand"]
# saf_long[spouse_firstnames_clean == "annorina josephine wilhelmina de la roche"]
saf_long[spouse_firstnames_clean == "carolina hendrika susanna van heerden geb", spouse_firstnames_clean := "carolina hendrika susanna van heerden"]
# saf_long[spouse_firstnames_clean == "constantia petronella elisabeth van nuldt"]
saf_long[spouse_firstnames_clean == "rustenburg hendriena barendina petronella", spouse_firstnames_clean := "hendriena barendina petronella"]
saf_long[spouse_firstnames_clean == "waarskynlik mattheus petrus lucas hendrik", spouse_firstnames_clean := "mattheus petrus lucas hendrik"]
saf_long[spouse_firstnames_clean == "lutherse kerk heinrich august maximillian", spouse_firstnames_clean := "heinrich august maximillian"]
saf_long[spouse_firstnames_clean == "anna christina aletta catharinaoosthuizen", spouse_surname_clean := "oosthuizen"]
saf_long[spouse_firstnames_clean == "anna christina aletta catharinaoosthuizen", spouse_firstnames_clean := "anna christina aletta catharina"]
saf_long[spouse_firstnames_clean == "beaufort west elizabeth johanna catharina", spouse_firstnames_clean := "elizabeth johanna catharina"]
saf_long[spouse_firstnames_clean == "kroonstad elsie aletta cecilia petronella", spouse_firstnames_clean := "elsie aletta cecilia petronella"]
saf_long[spouse_firstnames_clean == "hofmeyr susanna jacomina elsje petronella", spouse_firstnames_clean := "susanna jacomina elsje petronella"]
saf_long[spouse_firstnames_clean == "re-married her first husband john derrick", spouse_firstnames_clean := "john derrick"]
# saf_long[spouse_firstnames_clean == "severe napoleon mamoure tarby de la roche"]
saf_long[spouse_firstnames_clean == "vlug met kinders tydens anglo-boereoorlog", spouse_firstnames_clean := NA]
saf_long[spouse_firstnames_clean == "bloemfontein catharina petronella susanna", spouse_firstnames_clean := "catharina petronella susanna"]
# saf_long[spouse_firstnames_clean == "ladismith kp susanna catharina petronella"]
saf_long[spouse_firstnames_clean == "johannesburg emmarentia augustina johanna", spouse_firstnames_clean := "emmarentia augustina johanna"]
# saf_long[spouse_firstnames_clean == "andries christoffel van der byl neethling"]
saf_long[spouse_firstnames_clean == "sibella aletta hendrica van der westhuizen", spouse_firstnames_clean := "sibella aletta hendrica"]
saf_long[spouse_firstnames_clean == "met toestemming v meester vd hooggeregshof", spouse_firstnames_clean := "cecilia maria elizabeth"]
saf_long[spouse_firstnames_clean == "boksburg-noord cecilia gertruida magdalena", spouse_firstnames_clean := "cecilia gertruida magdalena"]
saf_long[spouse_firstnames_clean == "leipoldtville christina johanna wilhelmina", spouse_firstnames_clean := "christina johanna wilhelmina"]
saf_long[spouse_firstnames_clean == "by hom aan die egpaar het twee seuns gehad", spouse_firstnames_clean := NA]
saf_long[spouse_firstnames_clean == "germiston magritha catherina johanna maria", spouse_firstnames_clean := "magritha catherina johanna maria"]
# saf_long[spouse_firstnames_clean == "steynsrus philippina maria charlotte roets"]
saf_long[spouse_firstnames_clean == "johannes stephanus potgieter v roodeheuwel", spouse_surname_clean := "potgieter"]
saf_long[spouse_firstnames_clean == "johannes stephanus potgieter v roodeheuwel", spouse_firstnames_clean := "johannes stephanus"]
saf_long[spouse_firstnames_clean == "philippolis  cornelia catharina margaretha", spouse_firstnames_clean := "cornelia catharina margaretha"]
saf_long[spouse_firstnames_clean == "johanna helena dorothea gertruida potgieter", spouse_firstnames_clean := "johanna helena dorothea gertruida"]
saf_long[spouse_firstnames_clean == "biezepan catharina levina engela petronella", spouse_firstnames_clean := "catharina levina engela petronella"]
saf_long[individual_id == "scotta32b4c9d9" & stri_detect_fixed(spouse_firstnames_clean, "isabella margaretha johanna")], spouse_firstnames_clean := "isabella margaretha johanna"]
saf_long[spouse_firstnames_clean == "jacoba johanna sophia christina van heerden", spouse_firstnames_clean := "jacoba johanna sophia christina"]
# saf_long[spouse_firstnames_clean == "marie wilhelmina elizabeth henriette schuyt"]
saf_long[spouse_firstnames_clean == "malmesbury wilhelmina michielina lambrechts", spouse_firstnames_clean := "wilhelmina michielina"]
saf_long[spouse_firstnames_clean == "philippolis feb  christina catharina dalina", spouse_firstnames_clean := "christina catharina dalina"]
saf_long[spouse_firstnames_clean == "philippolis apt  dalina christina catharina", spouse_firstnames_clean := "dalina christina catharina"]
saf_long[spouse_firstnames_clean == "maria christina magdalena van der westhuizen", spouse_firstnames_clean := "maria christina magdalena"]
saf_long[spouse_firstnames_clean == "christina jacoba cornelia janse van rensburg", spouse_surname_clean := "janse van rensburg"]
saf_long[spouse_firstnames_clean == "christina jacoba cornelia janse van rensburg", spouse_firstnames_clean := "christina jacoba cornelia"]
saf_long[spouse_firstnames_clean == "potchefstroom gertruida margaretha elizabeth", spouse_firstnames_clean := "gertruida margaretha elizabeth"]
saf_long[spouse_firstnames_clean == "helena petronella johanna van der westhuizen", spouse_firstnames_clean := "helena petronella johanna"]
saf_long[spouse_firstnames_clean == "maria petronella elizabeth antoinette mccabe", spouse_surname_clean := "mccabe"]
saf_long[spouse_firstnames_clean == "maria petronella elizabeth antoinette mccabe", spouse_firstnames_clean := "maria petronella elizabeth antoinette"]
# saf_long[spouse_firstnames_clean == "margaretha maria johanna lamberta oosthuizen"]
saf_long[spouse_firstnames_clean == "margaretha johanna hendrina sophia du plessis", spouse_firstnames_clean := "margaretha johanna hendrina sophia"]
saf_long[spouse_firstnames_clean == "besproeiings-ingenieur kimberley swartruggens", spouse_surname_clean := "swartruggens"]
saf_long[spouse_firstnames_clean == "besproeiings-ingenieur kimberley swartruggens", spouse_firstnames_clean := NA]
saf_long[spouse_firstnames_clean == "johanna christina carolina janse van rensburg", spouse_surname_clean := "janse van rensburg"]
saf_long[spouse_firstnames_clean == "johanna christina carolina janse van rensburg", spouse_firstnames_clean := "johanna christina carolina"]
saf_long[spouse_firstnames_clean == "erich martin heinrich sigel de merindol malan", spouse_surname_clean := "malan"]
saf_long[spouse_firstnames_clean == "erich martin heinrich sigel de merindol malan", spouse_firstnames_clean := "erich martin heinrich sigel de merindol"]
saf_long[spouse_firstnames_clean == "dordrecht cornelia susanna johanna wilhelmina", spouse_firstnames_clean := "cornelia susanna johanna wilhelmina"]
saf_long[spouse_firstnames_clean == "daniel jacobus gerhardus joubert v wellington", spouse_surname_clean := "joubert"]
saf_long[spouse_firstnames_clean == "daniel jacobus gerhardus joubert v wellington", spouse_firstnames_clean := "daniel jacobus gerhardus"]
saf_long[spouse_firstnames_clean == "petronella hendrina wilhelmina adriana grobler", spouse_surname_clean := "grobler"]
saf_long[spouse_firstnames_clean == "petronella hendrina wilhelmina adriana grobler", spouse_firstnames_clean := "petronella hendrina wilhelmina adriana"]
saf_long[spouse_firstnames_clean == "gerrit dirkse van schalkwyk en alida maria nel", spouse_surname_clean := "van schalkwyk"]
saf_long[spouse_firstnames_clean == "gerrit dirkse van schalkwyk en alida maria nel", spouse_son_of := "gerrit dirkse van schalkwyk en alida maria nel"]
saf_long[spouse_firstnames_clean == "gerrit dirkse van schalkwyk en alida maria nel", spouse_firstnames_clean := NA]
saf_long[spouse_firstnames_clean == "luise christine ook bekend as louisa christina", spouse_firstnames_clean := "louisa christina"]
saf_long[spouse_firstnames_clean == "woon okramersfonteino zacharyas jacobus petrus", spouse_firstnames_clean := "zacharyas jacobus petrus"]
saf_long[spouse_firstnames_clean == "antonie henriette baroness von hohenhausen und", spouse_firstnames_clean := "antonie henriette"]
# saf_long[spouse_firstnames_clean == "gustavus olaf wilder derrick howard van someren"]
# saf_long[spouse_firstnames_clean == "slagtersnekrebel wat begenadig en na nieuweveld"]
saf_long[spouse_firstnames_clean == "randfontein john cambell ernest victor mckenzie", spouse_surname_clean := "mckenzie"]
saf_long[spouse_firstnames_clean == "randfontein john cambell ernest victor mckenzie", spouse_firstnames_clean := "john cambell ernest victor"]
saf_long[spouse_firstnames_clean == "francina catharina hendrika paulina van niekerk", spouse_firstnames_clean := "francina catharina hendrika paulina"]
saf_long[spouse_firstnames_clean == "christina carolina elisabeth janse van rensburg", spouse_surname_clean := "janse van rensburg"]
saf_long[spouse_firstnames_clean == "christina carolina elisabeth janse van rensburg", spouse_firstnames_clean := "christina carolina elisabeth"]
saf_long[spouse_firstnames_clean == "martha johanna catharina petronella van heerden", spouse_firstnames_clean := "martha johanna catharina petronella"]
saf_long[spouse_firstnames_clean == "margaret mctyre drie kinders uit huwelik gebore", spouse_surname_clean := "mctyre"]
saf_long[spouse_firstnames_clean == "margaret mctyre drie kinders uit huwelik gebore", spouse_firstnames_clean := "margaret"]
saf_long[spouse_firstnames_clean == "wilhelmina christina johanna la grange wed fourie", spouse_surname_clean := "la grange"]
saf_long[spouse_firstnames_clean == "wilhelmina christina johanna la grange wed fourie", spouse_firstnames_clean := "wilhelmina christina johanna"]
saf_long[spouse_firstnames_clean == "nndie egpaar het drie seuns en vier dogters gehad", spouse_firstnames_clean := NA]
# saf_long[spouse_firstnames_clean == "maria theresia wilhelmine eleonora von buchenroder"]
saf_long[spouse_firstnames_clean == "emmerentiaemarensche petronella margarethamagrieta", spouse_firstnames_clean := "emmerentia petronella margaretha"]
saf_long[spouse_firstnames_clean == "machteld aletta van heerden en hester helena burger", spouse_daughter_of := "machteld aletta van heerden en hester helena burger"]
saf_long[spouse_firstnames_clean == "machteld aletta van heerden en hester helena burger", spouse_firstnames_clean := NA]
saf_long[spouse_firstnames_clean == "frederika maria jacoba eygelaar v ovliegersfonteino", spouse_surname_clean := "eygelaar"]
saf_long[spouse_firstnames_clean == "frederika maria jacoba eygelaar v ovliegersfonteino", spouse_firstnames_clean := "frederika maria jacoba"]
saf_long[spouse_firstnames_clean == "charlotte maria benedikte eleonore adelheid prinses", spouse_firstnames_clean := "charlotte maria benedikte eleonore"]
saf_long[spouse_firstnames_clean == "italiaanse vrou wat met  kinders terugkeer na italie", spouse_firstnames_clean := NA]
saf_long[spouse_firstnames_clean == "potchefstroom christina elizabeth susanna wilhelmina", spouse_firstnames_clean := "christina elizabeth susanna wilhelmina"]
saf_long[spouse_firstnames_clean == "gladys ellen odonoghue twee seuns uit huwelik gebore", spouse_surname_clean := "o donoghue"]
saf_long[spouse_firstnames_clean == "gladys ellen odonoghue twee seuns uit huwelik gebore", spouse_firstnames_clean := "gladys ellen"]
saf_long[spouse_firstnames_clean == "potchefstroom henrietta cornelia christina wilhelmina", spouse_firstnames_clean := "henrietta cornelia christina wilhelmina"]
saf_long[spouse_firstnames_clean == "weer met sy eerste eggenote jaconina hendrina gysbertha", spouse_firstnames_clean := "jaconina hendrina gysbertha"]
saf_long[spouse_firstnames_clean == "sabina mcewan die egpaar he teen seun en drie dogters gehad", spouse_surname_clean := "mcewan"]
saf_long[spouse_firstnames_clean == "sabina mcewan die egpaar he teen seun en drie dogters gehad", spouse_firstnames_clean := "sabina"]
saf_long[stri_startswith_fixed(spouse_firstnames_clean, "vermoedelik suster"), spouse_firstnames_clean := NA]
saf_long[spouse_firstnames_clean == "emerentia hendrika antoinette magdalena cornelia janse van rensburg", spouse_surname_clean := "janse van rensburg"]
saf_long[spouse_firstnames_clean == "emerentia hendrika antoinette magdalena cornelia janse van rensburg", spouse_firstnames_clean := "emerentia hendrika antoinette magdalena cornelia"]

# fix initials spread over two fields
# first some manual fixes
saf_long[stri_endswith_fixed(spouse_initials, "m.") & spouse_surname_clean == "ller", 
    `:=`(spouse_initials = stri_replace_last_fixed(spouse_initials, "m.", ""), spouse_surname_clean = "muller")]
saf_long[stri_endswith_fixed(spouse_initials, "k.r.") & spouse_surname_clean == "ger", 
    `:=`(spouse_initials = stri_replace_last_fixed(spouse_initials, "k.r.", ""), spouse_surname_clean = "kruger")]
saf_long[stri_endswith_fixed(spouse_initials, "b.r.") & spouse_surname_clean == "ssow", 
    `:=`(spouse_initials = stri_replace_last_fixed(spouse_initials, "b.r.", ""), spouse_surname_clean = "brussow")]
saf_long[stri_endswith_fixed(spouse_initials, "j.") & spouse_surname_clean == "van rensburg", 
    `:=`(spouse_initials = stri_replace_last_fixed(spouse_initials, "j.", ""), spouse_surname_clean = "janse van rensburg")]
saf_long[stri_endswith_fixed(spouse_initials, "l.") & spouse_surname_clean == "tter", 
    `:=`(spouse_initials = stri_replace_last_fixed(spouse_initials, "l.", ""), spouse_surname_clean = "lotter")]
saf_long[stri_endswith_fixed(spouse_initials, "d.e.") & spouse_surname_clean == "la rey", 
    `:=`(spouse_initials = stri_replace_last_fixed(spouse_initials, "d.e.", ""), spouse_surname_clean = "de la rey")]
saf_long[stri_endswith_fixed(spouse_initials, "d.e.") & spouse_surname_clean == "la harpe", 
    `:=`(spouse_initials = stri_replace_last_fixed(spouse_initials, "d.e.", ""), spouse_surname_clean = "de la harpe")]
saf_long[stri_endswith_fixed(spouse_initials, "n.") & spouse_surname_clean == "thling", 
    `:=`(spouse_initials = stri_replace_last_fixed(spouse_initials, "n.", ""), spouse_surname_clean = "nothling")]
saf_long[stri_endswith_fixed(spouse_initials, "f.") & spouse_surname_clean == "lscher", 
    `:=`(spouse_initials = stri_replace_last_fixed(spouse_initials, "f.", ""), spouse_surname_clean = "folscher")]
saf_long[stri_endswith_fixed(spouse_initials, "b.") & spouse_surname_clean == "hmer", 
    `:=`(spouse_initials = stri_replace_last_fixed(spouse_initials, "b.", ""), spouse_surname_clean = "bohmer")]
saf_long[stri_endswith_fixed(spouse_initials, "k.") & spouse_surname_clean == "hn", 
    `:=`(spouse_initials = stri_replace_last_fixed(spouse_initials, "k.", ""), spouse_surname_clean = "kuhn")]
# there are more like this?
# pke              k.
saf_long[spouse_initials == "", spouse_initials := NA]

# paste the two fields wherever last initials is not intial of last firstname
saf_long[, spouse_initials_temp := initials(spouse_firstnames_clean, return_NA_on_empty = FALSE)]
saf_long[stri_sub(spouse_initials_temp, from = -1, to = -1) != stri_sub(spouse_initials, from = -2, to = -2),
    spouse_initials_temp := stri_join(spouse_initials_temp, stri_replace_all_regex(spouse_initials, "[^a-z]", ""))]
saf_long[, spouse_initials := spouse_initials_temp]
saf_long[spouse_initials == "", spouse_initials := NA]
saf_long[, ego_initials := initials(firstnames, return_NA_on_empty = TRUE)]

saf_long[stri_startswith_fixed(spouse_firstnames_clean, "-")]

capelinker::preflight(
    dat = saf_long[, list(
        mlast = surname,
        mfirst = firstnames,
        minitials = ego_initials,
        wlast = spouse_surname_clean,
        wfirst = spouse_firstnames_clean,
        winitials = spouse_initials,
        year = as.numeric(married_year))], 
    modstring = "m_boost_stel_rein")

fwrite(saf_long, "~/data/cape/saf/saf4spouselinkage_full_clean.csv")
