pkgs = c("dplyr", "reshape2", "stringr", "tidyr", "tibble",
         "Metabase", "readxl","readr", "zeallot")
for(pkg in pkgs){
    library(pkg, character.only = TRUE, warn.conflicts = FALSE, 
            quietly = TRUE, verbose = FALSE)
}

setwd(dirname(parent.frame(2)$ofile))

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

## read additional sample data
group_data = read_csv("../raw_data/lipidomics_20180910.csv")[,1:17]
group_data = group_data %>%
    mutate(
        flipgroup = factor(flipgroup),
        sample_id = paste0("ghana_", wid),
        sex_updated = factor(sex_updated)
    ) %>%
    as.data.frame %>%
    column_to_rownames("sample_id")


lpd = subset_samples(lpd, rownames(group_data))
lpd$sample_table = sample_table(cbind(group_data, lpd$sample_table))

################################################################################
##########                   C H O L   E F F L U X                    ##########
################################################################################

chol_efflux = read_excel(
    "../raw_data/20190319 Ghana Study HDL 100ug J774 Cholesterol Efflux.xlsx",
    sheet = "Final Results",
    range = "A3:B81"
) %>%
    mutate(`Subject ID` = str_c("ghana_", `Subject ID`)) %>%
    as.data.frame %>% column_to_rownames("Subject ID")

lpd$sample_table$chol_efflux = chol_efflux[sampleNames(lpd),]

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
file = "../raw_data/20190529 Ghana HDL data.xlsx"
glc_data = read_excel(file, sheet = 2, col_names = T) %>%
    as.data.frame %>%
    column_to_rownames('...1')
colnames(glc_data) = paste0("ghana_", colnames(glc_data))
glc_data = glc_data[, sampleNames(lpd)]
## -------- glycoforms ---------------------------------------------------------
glc = glc_data[grepl("^[A-Z0-9]+_[0-9A-Za-z/]+_[0-9/]+", rownames(glc_data)),]
fdata = data.frame(
    gsub(" Results$", "", rownames(glc))
) %>% 
    tidyr::separate(
        col = 1,
        into = c("Protein", "Position", "Glycan", "z"),
        sep = "_"
    )
rownames(fdata) = rownames(glc)
glc = GlycomicsSet(
    conc_table = conc_table(as.matrix(glc)),
    feature_data = feature_data(fdata),
    sample_table = sample_table(lpd)
)

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
)
pep = ProteomicsSet(
    conc_table = conc_table(as.matrix(pep)),
    feature_data = feature_data(fdata)
)

## -------- calibraction curve -------------------------------------------------
curve_data = list(
    conc = read_excel(file, sheet = 1, range = "D2:H11") %>% as.data.frame,
    resp = read_excel(file, sheet = 1, range = "J2:N11") %>% as.data.frame
)

curve = sapply(seq_along(curve_data$conc), function(i){
    res = lm(curve_data$resp[,i] ~ curve_data$conc[,i])
    intercept = res$coefficients[1]
    slope = res$coefficients[2]
    r2 = summary(res)$r.squared
    result = c(intercept,  slope, r2)
    names(result) = c("intercept", "slope", "r2")
    return(result)
}) %>% t %>% as.data.frame
rownames(curve) = colnames(curve_data$conc)

glc = list(
    peptide = pep,
    glycoforms = glc,
    curve = list(
        curve_data = curve_data,
        curve_params = curve
    )
)

## -------- save ---------------------------------------------------------------
save(lpd, sec, chr, glc, file = "hdl.rda")

