## -------------------- load packages -----------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble',"limma")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## --------------------- load data -------------------------
rm(list=ls())
setwd("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Ghana_Study/Analysis/R/precalc")
load("../../Rdata/hdl.Rdata")
edata = Lipidome$edata
pdata = Lipidome$pdata
fdata = Lipidome$fdata

## -------------------- edata list ------------------------
edata_conc = edata
rownames(edata_conc) = fdata$Annotation

edata_prop = sapply(edata, function(col){
    return(col/sum(col))
}) %>% as.data.frame
rownames(edata_prop) = fdata$Annotation


# Get the lipid class table
edata_class = sapply(edata, function(col){
    tapply(col, fdata$class, sum)
}) %>% as.data.frame

edata_class_prop = sapply(edata_class, function(col){
    return(col/sum(col))
}) %>% as.data.frame
rownames(edata_class_prop) = rownames(edata_class)

# Construct the data list
edata_list = list(
    class = list(
        "Concentration" = edata_class,
        "Proportion" = edata_class_prop
    ),
    species = list(
        "Concentration" = edata_conc,
        "Proportion" = edata_prop
    )
)

## ----------------------- limma ---------------------------
# limma modeling
# design = model.matrix(data=pdata, ~TX + Day + TX*Day + Subj + 1)
# runLimma = function(edata, design){
#     data = log2(edata+1)
#     fit = lmFit(data, design)
#     fit_ebayes = eBayes(fit)
#     fit_top = topTable(fit_ebayes, coef=23, number = nrow(edata), p.value=23, 
#                        sort.by='none')
#     return(fit_top)
# }
# limma_list = lapply(edata_list, function(data_sublist){
#     lapply(data_sublist, function(data) runLimma(data, design))
# })

## ---------------------- save ----------------------------
save(edata_list, pdata, fdata,
     file="../Rdata/lpd_precalc.Rdata")
