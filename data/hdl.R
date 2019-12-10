setwd(dirname(parent.frame(2)$ofile))
pkgs = c("dplyr", "reshape2", "stringr", "tidyr", "tibble",
         "Metabase", "readxl","readr", "zeallot")
for(pkg in pkgs){
    library(pkg, character.only = TRUE, warn.conflicts = FALSE, 
            quietly = TRUE, verbose = FALSE)
}

################################################################################
##########                      L I P I D O M E                       ##########
################################################################################

## -------- load data ----------------------------------------------------------
## Note: set the working directory to the location of this script before running it.
pos = import_wcmc_excel(
    file = "../raw_data/MX388497_Zhu_CSH_Submit.xlsx",
    sheet = "Positive Submit",
    conc_range = "K8:DB1742",
    sample_range = "J1:DB7",
    feature_range = "A7:J1742",
    InChIKey = "InChI Key",
    experiment_type = "Lipidomcis"
)
neg = import_wcmc_excel(
    file = "../raw_data/MX388497_Zhu_CSH_Submit.xlsx",
    sheet = "Negative Submit",
    conc_range = "K8:DD610",
    sample_range = "J1:DD7",
    feature_range = "A7:J610",
    InChIKey = "InChI Key",
    experiment_type = "Lipidomics"
)
## -------- cleaning -----------------------------------------------------------
pos = subset_features(pos, !is.na(pos$feature_data$InChIKey))
neg = subset_features(neg, !is.na(neg$feature_data$InChIKey))
## QC
pos = collapse_QC(pos, qc_names = paste0("Biorec00", 1:8))
neg = collapse_QC(neg, qc_names = paste0("Biorec00", 1:9))
## remove blank samples
pos = subset_samples(pos, !grepl("^MtdBlnk", sampleNames(pos)))
neg = subset_samples(neg, !grepl("^MtdBlnk", sampleNames(neg)))

pos$feature_data$ESI = "pos"
neg$feature_data$ESI = "neg"

featureNames(pos) = paste0("pos", 1:nfeatures(pos))
featureNames(neg) = paste0("neg", 1:nfeatures(neg))
## merge
lpd = LipidomicsSet(
    conc_table = conc_table(rbind(pos$conc_table, neg$conc_table)),
    sample_table = pos$sample_table,
    feature_data = feature_data(rbind(pos$feature_data, neg$feature_data)),
    experiment_data = pos$experiment_data
)
## fill up NAs
lpd = subset_features(
    lpd, apply(conc_table(lpd), 1, function(x) sum(is.na(x)) < 10) )
lpd = transform_by_feature(
    lpd, function(x) ifelse(is.na(x), min(x, na.rm = TRUE)/2, x)
)
## calibration
standards = read.csv("../raw_data/wcmc_lipidomics_standards.csv")
feature_data(lpd)$class = assign_lipid_class(feature_data(lpd)$Annotation)
experiment_data(lpd)$institute = "West Coast Metabolomics Center"
experiment_data(lpd)$sample_volumn_ul = 20
experiment_data(lpd)$internal_standards = standards
# remove PI (phosphatidyl inositol), CUDA, and AC (acyl-carnitines)
lpd = subset_features(
    lpd, !lpd$feature_data$class %in% c("CUDA", "PI", "AC"))
PI_featNames = featureNames(lpd)[!lpd$feature_data$class %in% standards$class]
lpd = calibrate_lipidomics_wcmc(lpd, cid = "InChIKey", 
                                class = "class", ESI = "ESI")

## filter negative and positive features
lpd = filter_by_cv(lpd, cv = "qc_cv", cid = "InChIKey")
lpd$feature_data$Annotation = lipid_name_formater(lpd$feature_data$Annotation)
lpd$feature_data$Annotation = make.unique(lpd$feature_data$Annotation)
featureNames(lpd) = lpd$feature_data$Annotation
sampleNames(lpd) = gsub("Ghana ", "ghana_", sampleNames(lpd))

################################################################################
##########                      Clinical Values                       ##########
################################################################################
## read additional sample data
cli_data = read_csv("../raw_data/dyadg_hdl_covdat_20191210.csv") %>%
    mutate(
        flipgroup = factor(flipgroup),
        sample_id = paste0("ghana_", wid),
        sex_updated = factor(sex_updated)
    ) %>%
    as.data.frame %>%
    column_to_rownames("sample_id")

# # Data no longer needed
# cli_data2 = read_csv("../raw_data/ghana_anthro12m_20190813.csv") %>%
#     as.data.frame %>%
#     column_to_rownames("id")
# cli_data2 = cli_data2[rownames(cli_data),]
# 
# cli_data = cbind(cli_data, cli_data2)

vars = c('wid', 'flipgroup', 'ppregbmi', 'ws2WomanAge', 'totschyrs', 'momht',
         'under5', 'hfia', 'primip', 'gaatdel', 'asset1', 'housing1', 'malaria',
         'mbhb', 'sex_updated', "logmbagp", "logmbcrp",
         'anyepisodes_6to18', 'feverepisodes_6to18', 'looseepisodes_6to18', 
         'ariepisodes_6to18', 'poorappetiteepisodes_6to18', 'ffqmeat7',
         'ffqeggs7', 'ffqfish7', 'ffqdairy7', 'ffqfat7', 'ffqpalm7', 'asf7sum',
         'flesh7sum', 'fat_all7')
pdata = cli_data[,vars]
edata = cli_data[,!(colnames(cli_data) %in% vars)]
edata["ghana_2421", "hcz18"] = NA
edata["ghana_2421", "hcz12"] = NA
edata = edata[, c(
    which(!grepl("[a-z]{2}z1[28]{1}", colnames(edata))),
    which(grepl("[a-z]{2}z1[28]{1}", colnames(edata)))
)] %>%
    rownames_to_column("id") %>%
    mutate(
        `waz18-12` = waz18 - waz12,
        `laz18-12` = laz18 - laz12,
        `wlz18-12` = wlz18 - wlz12,
        `hcz18-12` = hcz18 - hcz12
    ) %>%
    column_to_rownames("id") %>% t

cli = MultxSet(
    conc_table = conc_table(edata),
    sample_table = sample_table(pdata)
)

lpd = subset_samples(lpd, sampleNames(cli))
lpd$sample_table = sample_table(cbind(lpd$sample_table, pdata))

################################################################################
##########                   C H O L   E F F L U X                    ##########
################################################################################

chol_efflux = read_excel(
    "../raw_data/20190319 Ghana Study HDL 100ug J774 Cholesterol Efflux.xlsx",
    sheet = "Final Results",
    range = "A3:B83"
) %>%
    mutate(`Subject ID` = str_c("ghana_", `Subject ID`)) %>%
    as.data.frame %>% column_to_rownames("Subject ID")
colnames(chol_efflux) = "chol_efflux"
chol_efflux = chol_efflux[sampleNames(cli),,drop = FALSE]
fct = MultxSet(
    conc_table = conc_table(t(chol_efflux)),
    sample_table = sample_table(pdata),
    feature_data = feature_data(data.frame(
        assay = "% Cholesterol Efflux",
        unit = "%",
        row.names = "chol_efflux"
    ))
)

################################################################################
##########                           S E C                            ##########
################################################################################

files = list.files("../raw_data/ghana sec/", full.names = TRUE)
sec_data = lapply(files, function(f){
    data = read.table(
        f, header = T, skip = 2, sep = "\t",
        stringsAsFactors = FALSE
    )
    
    uv = data[,1:2] %>% drop_na()
    fr = data[,3:4] %>% drop_na()
    bl = data[5:6] %>% drop_na()
    
    colnames(fr) = c("ml", "fraction")
    colnames(bl) = c("ml", "mAU")
    
    fr$fraction = trimws(fr$fraction, which = "left")
    
    return(list(
        uv = uv,
        fr = fr,
        bl = bl
    ))
})
names(sec_data) = gsub(".+ghana-\\d*-(\\d{4})\\.asc", "ghana_\\1", files)

auc_data = sapply(sec_data, function(sample){
    c(uv, fr, bl) %<-% sample
    uv$mAU = uv$mAU - bl$mAU
    fr = filter(fr, fraction %in% as.character(1:8))
    uv$fraction = cut(uv$ml, breaks = fr$ml, labels = as.character(fr$fraction[-nrow(fr)]))
    
    auc = function(ml, mAU) {
        sum(sapply(seq_len(length(ml) - 1), function(i){
            (mAU[i] + mAU[i + 1]) * (ml[i + 1] - ml[i]) / 2
        }))
    }
    
    uv = filter(uv, !is.na(fraction)) %>%
        group_by(fraction) %>%
        arrange(ml) %>%
        summarize(
            auc = auc(ml, mAU)
        )
    auc = uv$auc
    names(auc) = uv$fraction
    return(auc)
})
colnames(auc_data) = names(sec_data)
rownames(auc_data) = paste0("F", rownames(auc_data))
auc_data = auc_data[, sampleNames(lpd)]
sec = MultxSet(
    conc_table = conc_table(as.matrix(auc_data)),
    sample_table = lpd$sample_table
)
chr = sec_data

################################################################################
##########                 G L Y C O P R O T E O M E                  ##########
################################################################################
file = "../raw_data/20190529 Ghana HDL data 20190716.xlsx"
glc_data = read_excel(
    file, sheet = 2, col_names = F, skip = 2
) %>% as.data.frame %>% 
    column_to_rownames("...1")
glc_data = glc_data[-1,]
pdata = read_excel(
    file, sheet = 2, col_names = T, n_max = 1
) %>%
    select(-`Sample Code`) %>%
    t %>% as.data.frame
rownames(pdata)[85:98] = paste0("SS-pool-", 1:14)
colnames(glc_data) = rownames(pdata)

colnames(pdata) = "Sequence Name"
pdata = pdata %>%
    rownames_to_column("Sample Name") %>%
    mutate('Sequence ID' = as.integer(str_extract(`Sequence Name`, "^[0-9]{3}"))) %>%
    arrange(`Sequence ID`)

batch = numeric(length = nrow(pdata))
for(i in seq_len(nrow(pdata))) {
    if(i == 1){
        b = 0
    } else if(grepl("^SS-pool", pdata$`Sample Name`[i - 1]) & 
              !grepl("^SS-pool", pdata$`Sample Name`[i])){
        b = b + 1
    }
    batch[i] = b
}
pdata$Batch = batch

pdata = filter(pdata, !grepl("^SS", `Sample Name`))
pdata = column_to_rownames(pdata, "Sample Name")

ss_data = glc_data %>% select(starts_with("SS"))
glc_data = glc_data[, rownames(pdata)]
    
## -------- glycoforms ---------------------------------------------------------
glc = glc_data[grepl("^[A-Z0-9]+_[0-9A-Za-z/]+_[0-9/]+", rownames(glc_data)),]
fdata = data.frame(
    gsub(" Results$", "", rownames(glc))
) %>% 
    tidyr::separate(
        col = 1,
        into = c("Protein", "Position", "Glycan", "z"),
        sep = "_"
    ) %>%
    mutate(
        pool_mean = ss_data[rownames(glc),] %>%
            select(starts_with("SS-pool")) %>%
            rowMeans(),
        pool_sd = ss_data[rownames(glc),] %>%
            select(starts_with("SS-pool")) %>%
            apply(1, function(row) sd(row))
    ) %>%
    mutate(
        pool_cv = pool_sd / pool_mean
    )

glycans = lapply(fdata$Glycan, function(glycan){
    if(grepl("/", glycan)){
        glycan = str_split(glycan, "/")[[1]]
        glycan = lapply(glycan, function(g){
            strsplit(g, "")[[1]]
        })
        Hex = ifelse(glycan[[1]][1] == glycan[[2]][1], glycan[[1]][1], NA)
        HexNAc = ifelse(glycan[[1]][2] == glycan[[2]][2], glycan[[1]][2], NA)
        Fuc = ifelse(glycan[[1]][3] == glycan[[2]][3], glycan[[1]][3], NA)
        Neu5Ac = ifelse(glycan[[1]][4] == glycan[[2]][4], glycan[[1]][4], NA)
    }else{
        glycan = strsplit(glycan, "")[[1]]
        Hex = glycan[1]
        HexNAc = glycan[2]
        Fuc = glycan[3]
        Neu5Ac = glycan[4]
    }
    res = as.integer(c(Hex, HexNAc, Fuc, Neu5Ac))
    names(res) = c("Hex", "HexNAc", "Fuc", "Neu5Ac")
    return(res)
})
glycans = do.call(rbind, glycans)
fdata = cbind(fdata, glycans)

rownames(fdata) = rownames(glc)
glc = GlycomicsSet(
    conc_table = conc_table(as.matrix(glc)),
    feature_data = feature_data(fdata),
    sample_table = sample_table(pdata)
)
sampleNames(glc) = paste0("ghana_", sampleNames(glc))
## -------- peptides -----------------------------------------------------------
pep = glc_data[!(
    grepl("^[A-Z0-9]+_[0-9A-Za-z/]+_[0-9/]+", rownames(glc_data)) |
        grepl("^ISTD", rownames(glc_data))
),]

proteins = NULL
sequences = NULL
zs = NULL
for(peptide in rownames(pep)){
    if(grepl("^[A-Z0-9]+_[0-9A-Za-z/]+_ungly+", peptide)){
        proteins = c(proteins, str_split_fixed(peptide, "_", n = 2)[1])
        sequences = c(sequences, NA)
        zs = c(zs, NA)
    } else if(grepl("^pep-", peptide)){
        proteins = c(proteins, gsub("^pep-([A-Z0-9]+)[-_]{1}.+", "\\1", peptide))
        sequences = c(sequences, gsub("^pep-[A-Z0-9]+[-_]{1}([A-Z]+).+", "\\1", peptide))
        if(grepl("^pep-[A-Z0-9]+[-_]{1}[A-Z]+_z[0-9]{1}.+", peptide)){
            zs = c(zs, gsub("^pep-[A-Z0-9]+[-_]{1}[A-Z]+_(z[0-9]{1}).+", "\\1", peptide))   
        } else {
            zs = c(zs, NA)
        }
    } else if(grepl("^QuantPep-", peptide)){
        proteins = c(proteins, gsub("^QuantPep-([A-Z0-9]+).+", "\\1", peptide))
        sequences = c(sequences, gsub("^QuantPep-[A-Z0-9]+_([A-Z]+).+", "\\1", peptide))
        zs = c(zs, NA)
    } else {
        proteins = c(proteins, gsub("^([A-Z0-9]+)[-_]{1}.+", "\\1", peptide))
        sequences = c(sequences, gsub("^[A-Z0-9]+[-_]{1}([A-Z]+).+", "\\1", peptide))
        zs = c(zs, gsub("^[A-Z0-9]+[-_]{1}[A-Z]+_(z[0-9]{1}).+", "\\1", peptide))
    }
}

fdata = data.frame(
    Protein = proteins,
    Sequence = sequences,
    z = zs,
    row.names = rownames(pep)
) %>%
    mutate(
        pool_mean = ss_data[rownames(pep),] %>%
            select(starts_with("SS-pool")) %>%
            rowMeans(),
        pool_sd = ss_data[rownames(pep),] %>%
            select(starts_with("SS-pool")) %>%
            apply(1, function(row) sd(row))
    ) %>%
    mutate(
        pool_cv = pool_sd / pool_mean
    )
rownames(fdata) = rownames(pep)
pep = ProteomicsSet(
    conc_table = conc_table(as.matrix(pep)),
    feature_data = feature_data(fdata),
    sample_table = sample_table(pdata)
)
sampleNames(pep) = paste0(paste0("ghana_", sampleNames(pep)))

## -------- calibraction curve -------------------------------------------------
curve_data = list(
    conc = read_excel(file, sheet = 1, range = "E4:I12", col_names = F) %>% as.data.frame,
    resp = read_excel(file, sheet = 1, range = "K4:O12", col_names = F) %>% as.data.frame
)
header = read_excel(file, sheet = 1, range = "E2:I2", col_names = F) %>% as.character()
peptide = read_excel(file, sheet = 1, range = "E3:I3", col_name = F) %>% as.character()

curve_data = lapply(curve_data, function(li) {
    colnames(li) = header
    return(li)
})

curve = sapply(seq_along(curve_data$conc), function(i){
    res = lm(curve_data$conc[,i] ~ curve_data$resp[,i])
    intercept = res$coefficients[1]
    slope = res$coefficients[2]
    r2 = summary(res)$r.squared
    result = c(intercept,  slope, r2)
    names(result) = c("intercept", "slope", "r2")
    return(result)
}) %>% t %>% as.data.frame %>%
    mutate(peptide = peptide)
rownames(curve) = colnames(curve_data$conc)

## -------- glc normalization --------------------------------------------------
glc = subset_features(glc, !(glc$feature_data$Protein %in% c("HPTR", "POLG")))
ss_data = ss_data[featureNames(glc),]

## -------- pep calibration ----------------------------------------------------
prt = subset_features(pep, sapply(curve$peptide, function(peptide) grep(peptide, featureNames(pep))))
for(i in seq_len(nfeatures(prt))) {
    prt$conc_table[i,] = prt$conc_table[i,]  * curve$slope[i] +  curve$intercept[i]
}
prt$experiment_data = list(
    unit = "g/l"
)

glc = list(
    protein = prt,
    peptide = pep,
    glycoforms = glc
)
glc = lapply(glc, function(li){
    li = subset_samples(li, sampleNames(lpd))
    li$sample_table = sample_table(cbind(
        li$sample_table,
        lpd$sample_table[,c("wid", "flipgroup","sex_updated")]
    ))
    return(li)
})


## -------- save ---------------------------------------------------------------
save(lpd, sec, chr, glc, cli, fct, file = "hdl.rda")

