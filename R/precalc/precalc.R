## -------- load packages ------------------------------------------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble',"Metabase", "MatCorR")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
setwd(dirname(parent.frame(2)$ofile))

## -------- load data ----------------------------------------------------------
load("../../data/hdl.rda")

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
lpd_odd = summarize_odd_chain(lpd_mol, name = "Annotation", class="class")
lpd_ratio = summarize_lipid_ratios(lpd_mol, name = "Annotation", class = "class")
lpd_ratio = subset_features(lpd_ratio, c("surface/core", "CE/Cholesterol", "PC/LPC", "CE/TG"))
featureNames(lpd_eod) = paste0("EOD ",featureNames(lpd_eod))
featureNames(lpd_acl) = paste0("ACL ",featureNames(lpd_acl))

## Plasmalogen
lpd_plasmalogen = subset_features(lpd, grepl("p$", featureNames(lpd)))
lpd_plasmalogen = colSums(lpd_plasmalogen$conc_table)

pc_plasmalogen = subset_features(lpd, grepl("^PC", featureNames(lpd)) & grepl("p$", featureNames(lpd)))
pe_plasmalogen = subset_features(lpd, grepl("^PE", featureNames(lpd)) & grepl("p$", featureNames(lpd)))

pc_plasmalogen = colSums(pc_plasmalogen$conc_table)
pe_plasmalogen = colSums(pe_plasmalogen$conc_table)

lpd_ratio$conc_table = conc_table(
    rbind(lpd_ratio$conc_table, 
          "total Plasmalogen" = lpd_plasmalogen,
          "PC Plasmalogen" = pc_plasmalogen, 
          "PE Plasmalogen" = pe_plasmalogen))

## precursors
lpd_ratio$conc_table = conc_table(
    rbind(
        lpd_ratio$conc_table,
        "CE 20:3/18:3 ratio" = lpd$conc_table["CE 20:3",] / lpd$conc_table["CE 18:3",],
        "FA 20:3/18:3 ratio" = lpd$conc_table["FA 20:3.1",] / lpd$conc_table["FA 18:3",],
        "CE 20:4/18:2 ratio" = lpd$conc_table["CE 20:4",] / lpd$conc_table["CE 18:2",],
        "FA 20:4/18:2 ratio" = lpd$conc_table["FA 20:4",] / lpd$conc_table["FA 18:2",],
        "FA 24:1/24:0 ratio" = lpd$conc_table["FA 24:1",] / lpd$conc_table["FA 24:0",],
        "FA 20:2/20:1 ratio" = lpd$conc_table["FA 20:2",] / lpd$conc_table["FA 20:1",],
        "FA 20:1/20:0 ratio" = lpd$conc_table["FA 20:1",] / lpd$conc_table["FA 20:0",],
        "FA 20:2/18:2 ratio" = lpd$conc_table["FA 20:2",] / lpd$conc_table["FA 18:2",],
        "Cer % odd" = lpd_odd$conc_table["Cer",] / summarize_features(lpd_mol, "class")$conc_table["Cer",],
        "SM % odd" = lpd_odd$conc_table["SM",] / summarize_features(lpd_mol, "class")$conc_table["SM",],
        "PC % plasmalogen" = lpd_ratio$conc_table["PC Plasmalogen",] / lpd_class$conc_table["PC",],
        "PE % plasmalogen" = lpd_ratio$conc_table["PE Plasmalogen",] / lpd_class$conc_table["PE",],
        "PL % plasmalogen" = lpd_ratio$conc_table["total Plasmalogen",] / (lpd_class$conc_table["PC",] + lpd_class$conc_table["PE",])
    )
)

lpd = list(
    class = lpd_class,
    species = lpd_prop,
    acl = lpd_acl,
    eod = lpd_eod,
    "odd chain" = lpd_odd,
    ratios = lpd_ratio
)

## -------- linear model -------------------------------------------------------
design = model.matrix(data = as(lpd_class$sample_table, "data.frame"), 
                      ~flipgroup + 1)
lm_lpd = lapply(lpd, function(data){
        mSet_limma(data, design, coef = 2)
})

lm_sec = mSet_limma(sec, design, coef = 2)

## -------- correlation --------------------------------------------------------
## calculate correlation against anthropometric values.
X = t(lpd$class$sample_table[,c("waz18", "laz18", "wlz18", "hcz18", "momht", "chol_efflux")])
corr_atm = lapply(lpd, function(mset){
    MatCorPack(X, mset$conc_table, 
               methods = c("pearson", "spearman", "kendall"))
})

## -------- save out -----------------------------------------------------------

data = list(
    data = list(
        lpd = lpd,
        sec = sec
    ),
    lm = list(
        lpd = lm_lpd,
        sec = lm_sec
    ),
    corr = list(
        lpd = list(
            atm = corr_atm
        )
    ),
    chr = chr
)

save(data, file="../Rdata/lpd_precalc.rda")
