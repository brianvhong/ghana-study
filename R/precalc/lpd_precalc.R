## -------------------- load packages -----------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble',"limma")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## --------------------- load data -------------------------
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
lpd_ratio = summarize_lipid_ratios(lpd_mol, name = "Annotation", class = "class")
lpd_ratio = subset_features(lpd_ratio, c("surface/core", "CE/Cholesterol", "PC/LPC", "CE/TG"))
featureNames(lpd_eod) = paste0("EOD ",featureNames(lpd_eod))
featureNames(lpd_acl) = paste0("ACL ",featureNames(lpd_acl))
## ---------------------- save ----------------------------
lpd = list(
    class = lpd_class,
    species = lpd,
    acl = lpd_acl,
    eod = lpd_eod,
    ratios = lpd_ratio
)
save(lpd, file="../Rdata/lpd_precalc.Rdata")
