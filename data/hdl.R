pkgs = c("dplyr", "reshape2", "stringr", "tidyr", "tibble",
         "Metabase", "readxl","readr")
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

## read additional sample data
group_data = read_csv("../raw_data/lipidomics_20180910.csv")[,1:17]
group_data = group_data %>%
    mutate(
        flipgroup = factor(flipgroup),
        sample_id = paste("Ghana", wid),
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
    mutate(`Subject ID` = str_c("Ghana ", `Subject ID`)) %>%
    as.data.frame %>% column_to_rownames("Subject ID")

lpd$sample_table$chol_efflux = chol_efflux[sampleNames(lpd),]

## -------- save ---------------------------------------------------------------
save(lpd, file = "hdl.rda")

