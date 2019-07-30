setwd(dirname(parent.frame(2)$ofile))
pkgs=c("dplyr", "reshape2", "Metabase", "MatCorR", "stringr")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

load("../../../../data/hdl.rda")
## -------- summarization ------------------------------------------------------
lpd_prop = transform_by_sample(lpd, function(x) x/sum(x))
lpd_class = summarize_features(lpd_prop, "class")

lpd_apoa1 = transform_by_feature(lpd, function(x) x/glc$protein$conc_table["pep-APOA1_LAEYHAK Results",])
lpd_class_apoa1 = summarize_features(lpd_apoa1, "class")

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
lpd_ratio = subset_features(lpd_ratio, c("surface/core", "ce/cholesterol", "pc/lpc", "tg/ce"))
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
    class_prop = lpd_class,
    species_prop = lpd_prop,
    class_apoa1 = lpd_class_apoa1,
    species_apoa1 = lpd_apoa1,
    acl = lpd_acl,
    eod = lpd_eod,
    "odd chain" = lpd_odd,
    ratios = lpd_ratio
)

## -------- glyccoproteome -----------------------------------------------------
glc_pep = glc$glycoforms
pep = glc$peptide
for(i in seq_len(nfeatures(glc_pep))) {
    quant_peptide = NULL
    protein = glc_pep$feature_data$Protein[i]
    if(protein == "ApoA1") {
        quant_peptide = pep$conc_table["pep-APOA1_LAEYHAK Results",]
    } else if (protein == "SAA"){
        quant_peptide = pep$conc_table[c(
            "SAA1_GPGGVWAAEAISDAR_z3 Results",
            "SAA2_GPGGAWAAEVISNAR_z3 Results",
            "pep-SAA1_FFGHGAEDSLADQAANEWGR_z3 Results"
        ),] %>% colSums
    } else {
        mset = subset_features(pep, pep$feature_data$Protein == protein)
        quant_peptide = mset$conc_table[which.max(rowMeans(mset$conc_table)),]
    }
    glc_pep$conc_table[i,] = glc_pep$conc_table[i,] / quant_peptide
}

glc_apoa1 = glc$glycoforms %>%
    transform_by_feature(function(row) row/glc$protein$conc_table["pep-APOA1_LAEYHAK Results",])

glc = list(
    protein = glc$protein,
    peptide = glc$peptide,
    glycan_pep = glc_pep,
    glycan_apoa1 = glc_apoa1
)

## -------- SEC ----------------------------------------------------------------
sec_hdl = sec %>%
    subset_features(c("F3", "F4", "F5")) %>%
    transform_by_sample(function(col) col / sum(col))
featureNames(sec_hdl) = c("lgHDL", 'mdHDL', "smHDL")
sec = list(
    fractions = sec,
    hdl = sec_hdl
)

## -------- linear model -------------------------------------------------------
design = model.matrix(data = as(cli$sample_table, "data.frame"), 
                      ~flipgroup + 1)
lm_lpd = lapply(lpd, function(data){
    mSet_limma(data, design, transform = log, coef = 2)
})
lm_glc = lapply(glc, function(data){
    mSet_limma(data, design, transform = log, coef = 2)
})
lm_sec = lapply(sec, function(data){
    mSet_limma(data, design, transform = log, coef = 2)
})

## -------- t test -------------------------------------------------------------
mSet_ttest = function(mset, var, transform = I, alternative = alternative){
    res = lapply(seq_len(nfeatures(mset)), function(i){
        x = transform(mset$conc_table[i,])
        tt = t.test(x ~ var, alternative = alternative)
        result = c(tt$estimate[1], log2(tt$estimate[2] / tt$estimate[1]), tt$statistic, tt$p.value)
        names(result) = c("baseMean", "estimate", "logFC", "pvalue")
        return(result)
    })
    res = do.call(rbind, res)
    rownames(res) = featureNames(mset)
    return(res)
}

alternatives = c("two.sided", "less", "greater")

lm_cli = lapply(alternatives, function(alt) mSet_ttest(cli, cli$sample_table$flipgroup, alternative = alt))
names(lm_cli) = alternatives
lm_fct = lapply(alternatives, function(alt) mSet_ttest(fct, fct$sample_table$flipgroup, alternative = alt))
names(lm_fct) = alternatives

## -------- correlation --------------------------------------------------------
## calculate correlation against anthropometric values.
lpd_cli = lapply(lpd, function(mset) {
    MatCor(cli$conc_table, mset$conc_table, "pearson")
})
lpd_fct = lapply(lpd, function(mset){
    MatCor(fct$conc_table, mset$conc_table, "pearson")
})
# lpd_sec = lapply(lpd, function(mset){
#     MatCor(sec$conc_table, mset$conc_table, "pearson")
# })
glc_cli = lapply(glc, function(mset){
    MatCor(cli$conc_table, mset$conc_table, "pearson")
})
glc_fct = lapply(glc, function(mset){
    MatCor(fct$conc_table, mset$conc_table, "pearson")
})
glc_sec = lapply(glc, function(mset){
    MatCor(sec$hdl$conc_table, mset$conc_table, "pearson")
})
# glc_sec = lapply(glc, function(mset){
#     MatCor(sec$conc_table, mset$conc_table, "pearson")
# })
fct_cli = MatCor(fct$conc_table, cli$conc_table, "pearson")
fct_sec = MatCor(fct$conc_table, sec$hdl$conc_table, "pearson")

## -------- save out -----------------------------------------------------------

data = list(
    data = list(
        lpd = lpd,
        glc = glc,
        sec = sec,
        cli = cli,
        fct = fct,
        chr = chr
    ),
    lm = list(
        lpd = lm_lpd,
        glc = lm_glc,
        cli = lm_cli,
        fct = lm_fct,
        sec = lm_sec
    ),
    corr = list(
        lpd = list(
            cli = lpd_cli,
            fct = lpd_fct
        ),
        glc = list(
            cli = glc_cli,
            fct = glc_fct,
            sec = glc_sec
        ),
        cli = list(
            fct = fct_cli
        ),
        fct = list(
            sec = fct_sec
        )
    ),
    chr = chr
)

save(data, file="data.rda")

