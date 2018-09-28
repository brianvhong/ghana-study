library(reshape2);library(dplyr);library(stringr);library(Metabase)
library(data.table);library(tidyr);library(tibble);library(readxl)
rm(list=ls())

################################################################################
##########                      L I P I D O M E                       ##########
################################################################################

## -------- load data ----------------------------------------------------------
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Ghana_Study/Analysis/analysis")

pos = import_wcmc_excel(
    file = "raw_data/MX388497_Zhu_CSH_Submit.xlsx",
    sheet = "Positive Submit",
    conc_range = "K8:DB1742",
    sample_range = "J1:DB7",
    feature_range = "A7:J1742",
    InChIKey = "InChI Key",
    experiment_type = "Lipidomcis"
)
neg = import_wcmc_excel(
    file = "raw_data/MX388497_Zhu_CSH_Submit.xlsx",
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
standards = read.csv("raw_data/wcmc_lipidomics_standards.csv")
feature_data(lpd)$class = assign_lipid_class(feature_data(lpd)$Annotation)
experiment_data(lpd)$institute = "West Coast Metabolomics Center"
experiment_data(lpd)$sample_volumn_ul = 20
experiment_data(lpd)$internal_standards = standards
lpd = subset_features(
    lpd, !lpd$feature_data$class %in% c("CUDA", "PI", "AC"))
## using PC to calibrate PI. Not the best
PI_featNames = featureNames(lpd)[!lpd$feature_data$class %in% standards$class]
# lpd$feature_data$class[featureNames(lpd) %in% PI_featNames] = "PC"
lpd = calibrate_lipidomics_wcmc(lpd, cid = "InChIKey", 
                                class = "class", ESI = "ESI")
# lpd$feature_data$class[featureNames(lpd) %in% PI_featNames] = "PI"
## filter negative and positive features
lpd = filter_by_cv(lpd, cv = "qc_cv", cid = "InChIKey")
lpd$feature_data$Annotation = lipid_name_formater(lpd$feature_data$Annotation)
lpd$feature_data$Annotation = make.unique(lpd$feature_data$Annotation)
featureNames(lpd) = lpd$feature_data$Annotation
## -------- summarization ------------------------------------------------------
lpd_prop = transform_by_sample(lpd, function(x) x/sum(x))
lpd_class = summarize_features(lpd_prop, "class")
## calculate the molecular weight
data("wcmc_adduct")
molwt = as.numeric(rep(NA, nfeatures(lpd)))
for(i in 1:nfeatures(lpd)){
    species = str_split(lpd$feature_data$Species[i], "\\_")[[1]]
    species = gsub("\\[", "", species)
    species = gsub("\\]", "", species)
    species = gsub("\\+$", "", species)
    species = gsub("\\-$", "", species)
    species = species[species %in% rownames(wcmc_adduct)]
    species = species[which.min(1 * wcmc_adduct[species,]$Mult + wcmc_adduct[species,]$Mass)]
    if(length(species) == 0) next
    mz = as.numeric(str_split(lpd$feature_data$`m/z`[i], "\\_")[[1]])
    mz = mz[which.min(mz)]
    molwt[i] = mz2molwt(species, mz)
}
lpd$feature_data$molwt = molwt
## more summarization
lpd_mol = transform_by_sample(lpd, function(x) x/molwt)
lpd_eod = summarize_EOD(lpd_mol, name = "Annotation", class = "class")
lpd_acl = summarize_ACL(lpd_mol, name = "Annotation", class = "class")
lpd_ratio = summarize_lipid_ratios(lpd_mol, name = "Annotation", class = "class")
lpd_ratio = subset_features(lpd_ratio, c("surface/core", "CE/Cholesterol", "PC/LPC", "CE/TG"))
featureNames(lpd_eod) = paste0("EOD ",featureNames(lpd_eod))
featureNames(lpd_acl) = paste0("ACL ",featureNames(lpd_acl))
## -------- save ---------------------------------------------------------------
Lipidome = list(
    class = lpd_class,
    species = lpd,
    acl = lpd_acl,
    eod = lpd_eod,
    ratios = lpd_ratio
)
save(Lipidome, file = "Rdata/hdl.rda")

