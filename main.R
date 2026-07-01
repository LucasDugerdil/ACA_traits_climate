#### Meta data ####
# Project: Pollen-based functional traits for the Arid Central Asia (ACA).
# Script: main.R
#
# Use:
# Change the Boolean variables into the 'Run the script' section (e.g., Import = T) to import, 
# reconstruct past environmental variables and drow the figure related to the article 
# Dugerdil et al. (in prep. for JoE)
# 
# Author: Lucas Dugerdil
# ORCID: 0000-0003-0266-564X
#
# Affiliations:
# 1) Univ. Lyon, ENS de Lyon, Université Lyon 1, CNRS, UMR 5276 LGL-TPE,
#    F-69364, Lyon, France
# 2) Université de Montpellier, CNRS, IRD, EPHE, UMR 5554 ISEM,
#    Montpellier, France
#
# Description:
# This script loads and cleans community-weighted mean (CWM) trait values devired from
# both vegetation plots and pollen surface samples from the Arid Central Asia (ACA).
# Then, the relationships between CWMs and climate parameters are tested for both datasets
# and compared. This script also assess spatial representativeness of WorldClim2.1 (Fick and Hijmans, 2017)
# and CHELSA (Karger et al., 2017).
# The methodological study to reconstruct CWM from pollen data is available from Dugerdil et al. (2025a, JBI)
# and the associated script may be found in https://github.com/LucasDugerdil/TraitPollen
#
# Related publication:
# Dugerdil, L. (in prep.). Modern pollen assemblages and vegetation plots record similar 
# community functional trait responses to climate. Journal of Ecology.
# 
# License:
# Creative Commons Attribution 4.0 International (CC BY 4.0)
#
# Citation:
# If you use this code, please cite:
# Lucas Dugerdil. (2025). LucasDugerdil/ACA_traits_climate: v1.0.0 (v1.0.0). 
# Zenodo. https://doi.org/10.5281/zenodo.17381815
#
# Created: 2024-08-01
# Last modified: 2025-12-27

#### Run the script ####
Import.data = T
Print.figures = T
Calculation = T
Calculation.past = T
Calculation.present = T
Fig.4 = T
Fig.5 = T
Fig.6 = T
Fig.7 = T
Fig.8 = T
Fig.A3 = T
Fig.A4 = T
Fig.A5 = T
Fig.A6 = T
Fig.A7 = T
Fig.A8 = T
Fig.A9 = T
Fig.A10 = T
Fig.A11 = T
Tab.A2 = T
Tab.A3 = T
Tab.A4 = T
Tab.A5 = T

# Parameters
Nb.perm.Moiran.I <- 3
# Nb.perm.Moiran.I <- 999

Trait <- c(11,12,5,7,13,4)
Clim <- c(2,3,16,15,18,20,22,26)
Seuil.pcover = 60

#### Import data ####
if(Import.data == T){
  #### Scripts ####
  source("Import/Script/functions.R")
  source("Import/Script/setup_packages.R")
  
  #### Spatial data ####
  ACA.bo <- readRDS("Import/Data/Spatial/ACA_borders.Rds")
  ACA.bo.co <- readRDS("Import/Data/Spatial/ACA_borders_country.Rds")
  ACA.bo.proj = fortify(ACA.bo)
  ACA.bo.co.proj = fortify(ACA.bo.co)
  
  #### Climate data ####
  MPS.ACA.Clim_chel <- as_tibble(readRDS("Import/Data/Pollen/Present/DBACA_Clim_chel.Rds"))
  MPS.ACA.Clim_wc <- as_tibble(readRDS("Import/Data/Pollen/Present/DBACA_Clim_wc.Rds"))
  MPS.ACA.Biom <- as_tibble(readRDS("Import/Data/Pollen/Present/DBACA_Biom.Rds"))
  
  MV.ACA.Clim_chel <- as_tibble(readRDS("Import/Data/Vegetation/DBACA_Clim_chel.Rds"))
  MV.ACA.Clim_wc <- as_tibble(readRDS("Import/Data/Vegetation/DBACA_Clim_wc.Rds"))
  MV.ACA.Biom <- as_tibble(readRDS("Import/Data/Vegetation/DBACA_Biom.Rds"))
  
  Psamp.var.chel <- as_tibble(readRDS("Import/Data/Climate/Chelsa_extract_ACA_df.Rds"))  
  Psamp.var.wc <- as_tibble(readRDS("Import/Data/Climate/WC_extract_ACA_df.Rds"))
  
  #### Traits list for the ACA ####
  MT.ACA.full <- readRDS("Import/Data/Traits/MT_ACA_full_gf.Rds")
  Tr.PT_ss_gf <- readRDS("Import/Data/Traits/Trait_pollen_fine_gf.Rds")
  Tr.MV_gf <- readRDS("Import/Data/Traits/Trait_vegetation_gf.Rds")
  MT.ACA.gf <- readRDS("Import/Data/Traits/Trait_ACA_gapfilled.Rds")
  MT.bool.ss <- readRDS("Import/Data/Traits/Table_corresp_plant_type_fine.Rds")
  MT.bool.sl <- readRDS("Import/Data/Traits/Table_corresp_plant_type_coarse.Rds")
  
  #### CWMs data ####
  MCWT.clim.PT_ss_gf.imp <- readRDS("Import/Data/CWM/CWT_ACA_PT_ss_gf_60p.Rds")
  MCWT.clim.PT_sl_gf.imp <- readRDS("Import/Data/CWM/CWT_ACA_PT_sl_gf_60p.Rds")
  MCWT.clim.MV_gf.imp <- readRDS("Import/Data/CWM/CWT_ACA_MV_gf_60p.Rds")
  Mat.R2.Pcov <- readRDS("Import/Data/CWM/CWT_Pcover.Rds")
  
  MCWT.clim.PT_ss_gf.imp <- MCWT.clim.PT_ss_gf.imp$MCWT
  MCWT.clim.PT_sl_gf.imp <- MCWT.clim.PT_sl_gf.imp$MCWT
  MCWT.clim.MV_gf.imp <- MCWT.clim.MV_gf.imp$MCWT
  
  #### Vegetation data ####
  header.ACA <- readRDS("Import/Data/Vegetation/sPlot.ACA.metadata.Rds")
  if(exists("MV.releve") == F){MV.releve <- readRDS("Import/Data/Vegetation/MV_ACA_plot_only_spermatophyte.Rds")}
  GrowthForm.ACA <- readRDS("Import/Data/Traits/TRY_GrowthForm_ACA.Rds")
  
  #### Pollen data (modern) ####
  MP_sl <- readRDS("Import/Data/Pollen/Present/ACASP_co.Rds")
  MP_ss <- readRDS("Import/Data/Pollen/Present/ACASP_fi.Rds")
  Table.Taxon <- readRDS("Import/Data/Pollen/Present/Table_match_pollen_type.Rds")
  
  #### TraCE-21K climate simulation ####
  Tra.arm1 <- readRDS("Import/Data/Climate_simulations/Sevan_TraCE21ky.Rds")
  Tra.arm2 <- readRDS("Import/Data/Climate_simulations/Shenkani_TraCE21ky.Rds")
  Tra.arm3 <- readRDS("Import/Data/Climate_simulations/Vanevan_TraCE21ky.Rds")
  Tra.arm4 <- readRDS("Import/Data/Climate_simulations/Zarishat_TraCE21ky.Rds")
  Tra.ge1 <- readRDS("Import/Data/Climate_simulations/Paravani_TraCE21ky.Rds")
  Tra.ge2 <- readRDS("Import/Data/Climate_simulations/Didachara_TraCE21ky.Rds")
  
  #### Past pollen sequences (Caucasus) ####
  Zar.MA <- readRDS("Import/Data/Pollen/Paleo/Zarishat_age.Rds")
  She.MA <- readRDS("Import/Data/Pollen/Paleo/Shenkani_age.Rds")
  Van.MA <- readRDS("Import/Data/Pollen/Paleo/Vanevan_age.Rds")
  Par.MA <- readRDS("Import/Data/Pollen/Paleo/Paravani_age.Rds")
  Did.MA <- readRDS("Import/Data/Pollen/Paleo/Didachara_age.Rds")
  Zar.MP_ss <- readRDS("Import/Data/Pollen/Paleo/Zarishat_pollen_fine.Rds")
  She.MP_ss <- readRDS("Import/Data/Pollen/Paleo/Shenkani_pollen_fine.Rds")
  Par.MP_ss <- readRDS("Import/Data/Pollen/Paleo/Paravani_pollen_fine.Rds")
  Did.MP_ss <- readRDS("Import/Data/Pollen/Paleo/Didachara_pollen_fine.Rds")
  Van.MP_ss <- readRDS("Import/Data/Pollen/Paleo/Vanevan_pollen_fine.Rds")
}

#### Calculations ####
if(Calculation == T){
  #### Calculations present ####
  if(Calculation.present == T){
    #### Extract clim ACA cells ####
    print("**** Extraction of the climate values per cells in ACA (both WorldClim2.1 and CHELSA). ****")
    
    MCWT.clim.PT_ss_gf <- MCWT.clim.PT_ss_gf.imp
    MCWT.clim.PT_sl_gf <- MCWT.clim.PT_sl_gf.imp
    MCWT.clim.MV_gf <- MCWT.clim.MV_gf.imp
    
    MCPV.MPS <- MC.extract.pca.prep(MPS.ACA.Clim_chel, MPS.ACA.Biom[c(5,6)], Psamp.var.chel)
    MCPV.MV <- MC.extract.pca.prep(MV.ACA.Clim_chel, MV.ACA.Biom[c(5,6)], Psamp.var.chel)
    MCPV.MPS.wc <- MC.extract.pca.prep(MPS.ACA.Clim_wc, MPS.ACA.Biom[c(5,6)], Psamp.var.wc)
    MCPV.MV.wc <- MC.extract.pca.prep(MV.ACA.Clim_wc, MV.ACA.Biom[c(5,6)], Psamp.var.wc)
    
    dir.create("Results/Climate", recursive = T, showWarnings = F)
    saveRDS(MCPV.MPS, "Results/Climate/MCPV_pollen_chel.Rds")
    saveRDS(MCPV.MV, "Results/Climate/MCPV_veget_chel.Rds")
    saveRDS(MCPV.MPS.wc, "Results/Climate/MCPV_pollen_wc.Rds")
    saveRDS(MCPV.MV.wc, "Results/Climate/MCPV_veget_wc.Rds")
    
    #### CWMs filtering-out ####
    print("**** Surface CWM filtering out. ****")
    Taiga.site <- MCWT.clim.MV_gf[which(MCWT.clim.MV_gf$Biome == "Boreal Forests/Taiga"),]$Site
    MV.releve.taig <- MV.releve[which(row.names(MV.releve) %in% Taiga.site),]
    MV.releve.taig <- MV.releve.taig[colMeans(MV.releve.taig) > 0]
    
    Mean.taiga <- data.frame(Mean = colMeans(MV.releve.taig))
    Mean.taiga$Nsup1 <- NA
    Mean.taiga$Nsup2 <- NA
    Mean.taiga$Nsup3 <- NA
    Mean.taiga$Nsup3 <- NA
    Mean.taiga$species <- row.names(Mean.taiga)
    for(i in 1:ncol(MV.releve.taig)){
      A = length(which(MV.releve.taig[i] >= 1))
      B = length(which(MV.releve.taig[i] >= 0.5))
      C = length(which(MV.releve.taig[i] >= 0.2))
      D = length(which(MV.releve.taig[i] >= 0.05))
      Mean.taiga$Nsup1[i] <- A
      Mean.taiga$Nsup2[i] <- B
      Mean.taiga$Nsup3[i] <- C
      Mean.taiga$Nsup4[i] <- D
    }
    
    MV.releve.taig.tree <- left_join(Mean.taiga, GrowthForm.ACA, by = "species")
    MV.releve.taig.tree <- MV.releve.taig.tree[MV.releve.taig.tree$GrowthForm != "Herb",]
    MV.releve.taig.tree <- MV.releve.taig.tree[MV.releve.taig.tree$GrowthForm != "Unknown",]
    MV.releve.taig.tree <- MV.releve.taig.tree[MV.releve.taig.tree$GrowthForm != "Other",]
    
    Mean.taiga.keep <- MV.releve.taig[MV.releve.taig.tree$species[!is.na(MV.releve.taig.tree$species)]]
    Mean.taiga.keep$havetree <- rowSums(Mean.taiga.keep)
    Mean.taiga.keep <- Mean.taiga.keep[Mean.taiga.keep$havetree>0,]
    Keep.taig <- row.names(Mean.taiga.keep)
    Remove.taig <- setdiff(row.names(MV.releve.taig), row.names(Mean.taiga.keep))
    
    Mean.taiga.rep <- Mean.taiga[Mean.taiga$Mean >= 0.015 & Mean.taiga$Nsup2 >= 30,]
    Mean.taiga <- dplyr::left_join(Mean.taiga, GrowthForm.ACA, by = "species")
    Mean.taiga <- Mean.taiga[order(Mean.taiga$GrowthForm),]
    
    MCWT.clim.MV_gf <- MCWT.clim.MV_gf[!MCWT.clim.MV_gf$Site %in% Remove.taig,]
    
    header.ACA.fo <- header.ACA[c(1,36)]
    names(header.ACA.fo)[1] <- "Site"
    MCWT.clim.MV.clean <- left_join(MCWT.clim.MV_gf, header.ACA.fo, by = "Site")
    Row.to.remove <- which(MCWT.clim.MV.clean$is.forest == T & MCWT.clim.MV.clean$Biome.no %in% c(8, 10, 13))
    Row.to.remove2 <- which(MCWT.clim.MV.clean$is.forest == F & MCWT.clim.MV.clean$Biome.no %in% c(4, 5, 6))
    Row.to.keep <- setdiff(seq(1:nrow(MCWT.clim.MV.clean)), c(Row.to.remove, Row.to.remove2))
    Site.to.keep <- MCWT.clim.MV.clean[Row.to.keep,"Site"]
    MCWT.clim.MV_gf <- MCWT.clim.MV_gf[MCWT.clim.MV_gf$Site %in% Site.to.keep,]
    
    #### Export clean data for plots ####
    dir.create("Results/CWM", recursive = T, showWarnings = F)
    saveRDS(MCWT.clim.PT_ss_gf, "Results/CWM/CWT_ACA_PT_ss_gf_clean.Rds")
    saveRDS(MCWT.clim.PT_sl_gf, "Results/CWM/CWT_ACA_PT_sl_gf_clean.Rds")
    saveRDS(MCWT.clim.MV_gf, "Results/CWM/CWT_ACA_MV_gf_clean.Rds")
    
    #### Clean data for plots ####
    LR.MCWT.clim.PT_ss_gf <- MCWT.clim.PT_ss_gf
    LR.MCWT.clim.PT_ss_gf$Type <- "Pollen"
    LR.MCWT.clim.MV_gf <- MCWT.clim.MV_gf
    LR.MCWT.clim.MV_gf$Type <- "Vegetation"
    names(LR.MCWT.clim.MV_gf) <- gsub("_wc", "", names(LR.MCWT.clim.MV_gf))
    names(LR.MCWT.clim.PT_ss_gf) <- gsub("_wc", "", names(LR.MCWT.clim.PT_ss_gf))
    
    names(MCWT.clim.MV_gf) <- gsub("_wc", "", names(MCWT.clim.MV_gf))
    names(MCWT.clim.PT_ss_gf) <- gsub("_wc", "", names(MCWT.clim.PT_ss_gf))
    names(MCWT.clim.PT_sl_gf) <- gsub("_wc", "", names(MCWT.clim.PT_sl_gf))
    dir.create("Figures/Article", recursive = T, showWarnings = F)
    dir.create("Figures/SI", recursive = T, showWarnings = F)
    
    #### Calculate correlations ####
    print("**** Calculate current CWM vs. climate correlation. ****")
    My_use.cor <- "pairwise.complete.obs"
    My_method.cor <- "pearson"
    
    MC.mv.gf <- Mat.corel.CWT.clim(MCWT.clim.MV_gf[Clim], MCWT.clim.MV_gf[Trait], 
                                   I.confiance = 0.95, Display.pval = "pch",
                                   Disp.R = "number", Label = F, Average = F,  Bar.pos = "n", 
                                   Return.slope = T, Print.result = T, 
                                   Use.cor = My_use.cor, Method.cor = My_method.cor)
    
    MC.ss.gf <- Mat.corel.CWT.clim(MCWT.clim.PT_ss_gf[Clim], MCWT.clim.PT_ss_gf[Trait],
                                   I.confiance = 0.95, return.pick = F, Return.slope = T, Print.result = T,
                                   Display.pval = "pch", 
                                   Disp.R = "number", Label = F, Average = F,  Bar.pos = "n",
                                   Use.cor = My_use.cor, Method.cor = My_method.cor,)
    
    MC.sl.gf <- Mat.corel.CWT.clim(MCWT.clim.PT_sl_gf[Clim], MCWT.clim.PT_sl_gf[Trait], 
                                   I.confiance = 0.95, Return.slope = T, Print.result = T,
                                   Display.pval = "pch",
                                   Disp.R = "number", Label = F, Average = F,  Bar.pos = "n",
                                   Use.cor = My_use.cor, Method.cor = My_method.cor)
    
    dir.create("Results/Matrix_correlation")
    saveRDS(MC.mv.gf, "Results/Matrix_correlation/MC_mv_gf.Rds")
    saveRDS(MC.ss.gf, "Results/Matrix_correlation/MC_ss_gf.Rds")
    saveRDS(MC.sl.gf, "Results/Matrix_correlation/MC_sl_gf.Rds")
    
    #### PCA pollen-types ####
    dir.create("Results/Table_LaTeX", recursive = T, showWarnings = F)
    dir.create("Results/Pollen", recursive = T, showWarnings = F)
    
    MP_sl <- cbind(MP_sl, MPS.ACA.Biom[c(5,6)])
    MP_ss <- cbind(MP_ss, MPS.ACA.Biom[c(5,6)])
    
    Pollen.PCA.sl <- PCA.bioclim(MP_sl, transp_OK = F, Cluster.core = "Biome", return.pick = F)
    Pollen.PCA.ss <- PCA.bioclim(MP_ss, transp_OK = F, Cluster.core = "Biome", return.pick = F)
    
    Pollen.PCA.sl.contrib <- Pollen.PCA.sl[,1]+Pollen.PCA.sl[,2]
    Pollen.PCA.sl.contrib <- Pollen.PCA.sl.contrib[order(Pollen.PCA.sl.contrib, decreasing = T)]
    Main.taxa.4 <- Pollen.PCA.sl.contrib[Pollen.PCA.sl.contrib > 1]
    Main.taxa.22 <- Pollen.PCA.sl.contrib[Pollen.PCA.sl.contrib > 0.01]
    
    saveRDS(Main.taxa.4, "Results/Pollen/4Main_taxa_PCA.Rds")
    saveRDS(Main.taxa.22, "Results/Pollen/22Main_taxa_PCA.Rds")
    
    #### Aggregate trait family / genus ####
    print("Let's aggregate pollen-type, bro !")
    
    Tr.PT_fam_gf <- Trait.aggregate.by.class(MT.ACA.gf[-c(2,3)])
    Tr.PT_gen_gf <- Trait.aggregate.by.class(MT.ACA.gf[-c(1,3)])
    
    #### Aggregation pollen-type ####
    names(MT.bool.sl)[names(MT.bool.sl) == "Capparidaceae"] <- "Capparaceae"
    MT.bool.sl[MT.bool.sl$family == "Capparaceae", names(MT.bool.sl) == "Capparaceae"] <- T
    MT.bool.sl[MT.bool.sl$family == "Xanthorrhoeaceae", names(MT.bool.sl) == "Xanthorrhoeaceae"] <- T
    MT.bool.ss[MT.bool.ss$family == "Capparaceae", names(MT.bool.ss) == "Capparaceae"] <- T
    names(MT.bool.ss)[names(MT.bool.ss) == "Capparidaceae"] <- "Capparaceae"
    
    Tr.PT_sl.g.full <- Trait.aggregate.by.type(MT.bool.sl, MT.ACA.gf, "PT_sl") # Normal sl
    Tr.PT_ss.g.full <- Trait.aggregate.by.type(MT.bool.ss, MT.ACA.gf, "PT_ss") # Normal sl
    
    Tr.PT_sl_gf.sd <- Tr.PT_sl.g.full$SD
    Tr.PT_ss_gf.sd <- Tr.PT_ss.g.full$SD
    
    Tr.PT_sl_gf <- Tr.PT_sl.g.full$Mean
    Tr.PT_ss_gf <- Tr.PT_ss.g.full$Mean
    
    #### Merge and melt ####
    MSD <- cbind(melt(Tr.PT_sl_gf.sd, id = "species"), type = "sl_gf")
    MSD2 <- cbind(melt(Tr.PT_ss_gf.sd, id = "species"), type = "ss_gf")
    names(MSD)[1] <- "species"
    names(MSD2)[1] <- "species"
    MSD <- rbind(MSD, MSD2)
    Trait.selected <- c("Height", "LeafArea", "LeafN", "SeedMass", "SLA", "SSD")
    MSD <- MSD[MSD$variable %in% Trait.selected,]
    MSD$variable <- factor(MSD$variable, levels = Trait.selected, ordered = T)
    MSD <- MSD[MSD$type %in% c("ss_gf", "sl_gf"),]
    
    MSD.maj4 <- MSD[MSD$species %in% names(Main.taxa.4),]
    MSD.maj4$type <- paste(MSD.maj4$type, "maj4", sep = "_")
    MSD.maj22 <- MSD[MSD$species %in% names(Main.taxa.22),]
    MSD.maj22$type <- paste(MSD.maj22$type, "maj22", sep = "_")
    MSD2 <- rbind(MSD, MSD.maj22, MSD.maj4)
  }
  
  #### Calculations past ####
  if(Calculation.past == T){
    #### Merging traits lists ####
    print("**** CWM reconstructions in the Caucasus for the Holocene. ****")
    MT.MP_ss.Zar <- Merge.local.global.trait(Pollen = Zar.MP_ss, Trait.local = MT.ACA.full, Trait.ACA.pol = list(ss = Tr.PT_ss_gf, ss_gf = Tr.PT_ss_gf), Trait.ACA.veg = list(veg_gf = Tr.MV_gf))
    MT.MP_ss.She <- Merge.local.global.trait(Pollen = She.MP_ss, Trait.local = MT.ACA.full, Trait.ACA.pol = list(ss = Tr.PT_ss_gf, ss_gf = Tr.PT_ss_gf), Trait.ACA.veg = list(veg_gf = Tr.MV_gf))
    MT.MP_ss.Van <- Merge.local.global.trait(Pollen = Van.MP_ss, Trait.local = MT.ACA.full, Trait.ACA.pol = list(ss = Tr.PT_ss_gf, ss_gf = Tr.PT_ss_gf), Trait.ACA.veg = list(veg_gf = Tr.MV_gf))
    MT.MP_ss.Did <- Merge.local.global.trait(Pollen = Did.MP_ss, Trait.local = MT.ACA.full, Trait.ACA.pol = list(ss = Tr.PT_ss_gf, ss_gf = Tr.PT_ss_gf), Trait.ACA.veg = list(veg_gf = Tr.MV_gf))
    MT.MP_ss.Par <- Merge.local.global.trait(Pollen = Par.MP_ss, Trait.local = MT.ACA.full, Trait.ACA.pol = list(ss = Tr.PT_ss_gf, ss_gf = Tr.PT_ss_gf), Trait.ACA.veg = list(veg_gf = Tr.MV_gf))
    
    #### CWM reconstructions (past) ####
    MCWT_ss.Zar <- CWT.calculation.2(MT = MT.MP_ss.Zar, MP = Zar.MP_ss, Mclim = Zar.MA, Accep.seuil = Seuil.pcover)
    MCWT_ss.She <- CWT.calculation.2(MT = MT.MP_ss.She, MP = She.MP_ss, Mclim = She.MA, Accep.seuil = Seuil.pcover)
    MCWT_ss.Van <- CWT.calculation.2(MT = MT.MP_ss.Van, MP = Van.MP_ss, Mclim = Van.MA, Accep.seuil = Seuil.pcover)
    MCWT_ss.Did <- CWT.calculation.2(MT = MT.MP_ss.Did, MP = Did.MP_ss, Mclim = Did.MA, Accep.seuil = Seuil.pcover)
    MCWT_ss.Par <- CWT.calculation.2(MT = MT.MP_ss.Par, MP = Par.MP_ss, Mclim = Par.MA, Accep.seuil = Seuil.pcover)
    
    List.CWM.arm <- list(MCWT_ss.Zar$MCWT,
                         MCWT_ss.She$MCWT,
                         MCWT_ss.Van$MCWT,
                         MCWT_ss.Did$MCWT,
                         MCWT_ss.Par$MCWT)
    
    #### Stacking parameters ####
    Lims.2k <- c(150, 13000); WD.len <- 250; Scaling <- F; Binning <- T; Anomaly <- T
    
    #### TraCE-21K stacking ####
    List.trace.arm <- list(Tra.arm1, Tra.arm2, Tra.arm3, Tra.arm4, Tra.ge1, Tra.ge2)
    
    Tra.arm <- Stacking.quantif(Imput.list = List.trace.arm, Anomaly = Anomaly, 
                                Scaling = Scaling, Binning = Binning, Limits = Lims.2k, Windows.length = WD.len, Plot.x = "Age", Type.name = "Climate model",
                                Keep.clim = c("TraCE_MAP", "TraCE_MAAT", "TraCE_Psum", "TraCE_Tsum", "TraCE_Pwin", "TraCE_Twin", "TraCE_Pspr", "TraCE_Tspr"),
                                New.param.names = c("MAP", "MAAT", "MPWAQ", "MTWAQ", "MPCOQ", "MTCOQ", "Pspr", "Tspr"))
    
    Tra.arm.1 <- Stacking.quantif(Imput.list = List.trace.arm, Scaling = Scaling, Binning = Binning, Anomaly = Anomaly, Limits = Lims.2k, Windows.length = WD.len, Plot.x = "Age", Type.name = "MTWAQ", Keep.clim = c("TraCE_Tsum"), New.param.names = c("(D) Temperature"))
    Tra.arm.2 <- Stacking.quantif(Imput.list = List.trace.arm, Scaling = Scaling, Binning = Binning, Anomaly = Anomaly, Limits = Lims.2k, Windows.length = WD.len, Plot.x = "Age", Type.name = "MPWAQ", Keep.clim = c("TraCE_Psum"), New.param.names = c("(C) Precipitation"))
    Tra.arm.3 <- Stacking.quantif(Imput.list = List.trace.arm, Scaling = Scaling, Binning = Binning, Anomaly = Anomaly, Limits = Lims.2k, Windows.length = WD.len, Plot.x = "Age", Type.name = "MTCOQ", Keep.clim = c("TraCE_Twin"), New.param.names = c("(D) Temperature"))
    Tra.arm.4 <- Stacking.quantif(Imput.list = List.trace.arm, Scaling = Scaling, Binning = Binning, Anomaly = Anomaly, Limits = Lims.2k, Windows.length = WD.len, Plot.x = "Age", Type.name = "MPCOQ", Keep.clim = c("TraCE_Pwin"), New.param.names = c("(C) Precipitation"))
    
    #### CWMs stacking ####
    CWM.arm.1 <- Stacking.quantif(Imput.list = List.CWM.arm, Scaling = Scaling, Binning = Binning, Limits = Lims.2k, Anomaly = Anomaly, Windows.length = WD.len, Plot.x = "Age", Type.name = "Leaf Area", Keep.clim = c("TRY_LeafArea"), New.param.names = c("(D) Temperature"))
    CWM.arm.2 <- Stacking.quantif(Imput.list = List.CWM.arm, Scaling = Scaling, Binning = Binning, Limits = Lims.2k, Anomaly = Anomaly, Windows.length = WD.len, Plot.x = "Age", Type.name = "Height", Keep.clim = c("TRY_Height"), New.param.names = c("(C) Precipitation"))
    CWM.arm.3 <- Stacking.quantif(Imput.list = List.CWM.arm, Scaling = Scaling, Binning = Binning, Limits = Lims.2k, Anomaly = Anomaly, Windows.length = WD.len, Plot.x = "Age", Type.name = "Seed mass", Keep.clim = c("TRY_SeedMass"), New.param.names = c("(C) Precipitation"))
    CWM.arm.4 <- Stacking.quantif(Imput.list = List.CWM.arm, Scaling = Scaling, Binning = Binning, Limits = Lims.2k, Anomaly = Anomaly, Windows.length = WD.len, Plot.x = "Age", Type.name = "SSD", Keep.clim = c("TRY_SSD"), New.param.names = c("(C) Precipitation"))
    CWM.arm.5 <- Stacking.quantif(Imput.list = List.CWM.arm, Scaling = Scaling, Binning = Binning, Limits = Lims.2k, Anomaly = Anomaly, Windows.length = WD.len, Plot.x = "Age", Type.name = "Leaf N", Keep.clim = c("TRY_LeafN"), New.param.names = c("(D) Temperature"))
    CWM.arm.6 <- Stacking.quantif(Imput.list = List.CWM.arm, Scaling = Scaling, Binning = Binning, Limits = Lims.2k, Anomaly = Anomaly, Windows.length = WD.len, Plot.x = "Age", Type.name = "SLA", Keep.clim = c("TRY_SLA"), New.param.names = c("(D) Temperature"))
    
    #### Export Results ####
    CWM.arm <- Stacking.quantif(Imput.list = List.CWM.arm, Scaling = Scaling, Binning = Binning, Limits = Lims.2k, Windows.length = WD.len, Bin.sd = T, Add.bin.count = T,
                                Plot.x = "Age", Type.name = "Pollen-CWM", Keep.clim = c("TRY_LeafArea", "TRY_SSD", "TRY_Height", "TRY_SeedMass", "TRY_LeafN", "TRY_SLA"), 
                                New.param.names = c("Leaf area", "Seed mass", "Plant height", "SSD", "Nleaf", "SLA"))
    
    dir.create("Results/Paleo/", showWarnings = F)
    saveRDS(CWM.arm, "Results/Paleo/MCWT_Caucasus_MP_ss_stack.Rds")
    saveRDS(Tra.arm, "Results/Paleo/TraCE_Caucasus_CWM_compar.Rds")
  }
}

#### Figures and Tables ####
if(Print.figures == T){
  #### Figure A3 ####
  if(Fig.A3 == T){
    print("**** Figure A3 plotting. ****")
    values.gf = c("Herb" = "#b5ab32ff", "Shrub" = "#aa373aff", "Other" = "grey90", "Unknown" = "grey90", "Tree" = "#0f6b31ff")
    ptaig <- ggplot()+ 
      geom_point(data = Mean.taiga, aes(x = Mean, y = Nsup2, color = GrowthForm), alpha = 0.8)+
      xlab("Mean taxa abundance")+ ylab("Nb of plot with taxa abundance > 50%")+
      scale_color_manual(values = values.gf, name = "Growth forms", drop = T)+
      annotate("text", x = 0.03, y = 0.5, label = paste("n_site = ", length(Taiga.site), "\nn_taxa = ", length(unique(Mean.taiga$species))), hjust = 0)+
      geom_text_repel(data = Mean.taiga.rep, aes(x = Mean, y = Nsup2, label = species), size = 3)+
      theme(axis.line = element_line(colour = "grey30"), 
            plot.background = element_blank(), panel.background = element_blank(),
            panel.grid = element_blank(), 
            legend.key = element_blank(),
            panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 1.5))
    
    W = 700 ; H = 600
    ggsave(filename = "Figures/SI/Figure_A3.pdf", ptaig, width = W*0.026458333, height = H*0.026458333, units = "cm")
  }
  
  #### Figure A4 ####
  if(Fig.A4 == T){
    print("**** Figure A4 plotting. ****")
    BioCl.pca.mps <- PCA.bioclim(MCPV.MPS, transp_OK = T, Scale.PCA = 7, 
                                 Cluster.core = "Biome", Shape = 16, Display.only.MPS = T,
                                 Legend.size = 8, Dot.size = 1.8, Size.MPS = 1.8, Dot.opac = 1,
                                 Groupes = list(Water = c("AI", "MAP", "MPWAQ", "MPCOQ"),
                                                Temperature = c("MAAT", "MTWAQ", "MTCOQ"),
                                                Altitude = c("Altitude", "Latitude", "Longitude")
                                 ),
                                 Variable.inactive = c("Altitude", "Latitude", "Longitude"),
                                 Site.name = "", Num.facet = "(C) Pollen", return.pick = T, Legend.position = "bottom")
    
    BioCl.pca.mv <- PCA.bioclim(MCPV.MV, transp_OK = T, Scale.PCA = 7,
                                Cluster.core = "Biome", Shape = 16, Dot.size = 1.8, Size.MPS = 1.6, Display.only.MPS = T, Dot.opac = 1,
                                Groupes = list(Water = c("AI", "MAP", "MPWAQ", "MPCOQ"),
                                               Temperature = c("MAAT", "MTWAQ", "MTCOQ"),
                                               Altitude = c("Altitude", "Latitude", "Longitude")),
                                Variable.inactive = c("Altitude", "Latitude", "Longitude"),
                                Site.name = "", Num.facet = "(B) Vegetation", return.pick = T, Legend.position = "none")
    
    BioCl.pca.lc <- PCA.bioclim(MCPV.MV[MCPV.MV$Type == "PV",], transp_OK = T, Scale.PCA = 7,
                                Cluster.core = "Biome", Dot.size = 1.8, 
                                Groupes = list(Water = c("AI", "MAP", "MPWAQ", "MPCOQ"),
                                               Temperature = c("MAAT", "MTWAQ", "MTCOQ")),
                                No.arrow = T, Hide.MPS = T,
                                Site.name = "", Num.facet = "(A) Land cover", return.pick = T, Legend.position = "none")
    
    full.pca.clim <- BioCl.pca.lc + BioCl.pca.mv + BioCl.pca.mps
    W = 1500; H = 600
    ggsave(filename = "Figures/SI/Figure_A4.pdf", full.pca.clim, width = W*0.026458333, height = H*0.026458333, units = "cm")
  }
  
  #### Figure A5 ####
  if(Fig.A5 == T){
    print("**** Figure A5 plotting. ****")
    
    Map.SI1 <- Map.biogeo.CWM(MCWT = MCWT.clim.MV_gf, Type1 = "ACA-vegetation", Show.trait.lab = F,
                              Strip.lab = F, Hex.size = 40, Show.diff = F, Vertical = T, Leg.pos = "none",
                              Select.trait = c("TRY_LeafArea", "TRY_LeafN", "TRY_SeedMass", "TRY_SLA", "TRY_SSD", "TRY_Height"))
    
    Map.SI2 <- Map.biogeo.CWM(MCWT = MCWT.clim.PT_ss_gf, Type1 = "ACA-fine", Show.trait.lab = F,
                              Strip.lab = F, Hex.size = 40, Show.diff = F, Vertical = T, Leg.pos = "bottom",
                              Select.trait = c("TRY_LeafArea", "TRY_LeafN", "TRY_SeedMass", "TRY_SLA", "TRY_SSD", "TRY_Height"))
    
    Map.SI3 <- Map.biogeo.CWM(MCWT = MCWT.clim.PT_sl_gf, Type1 = "ACA-coarse", 
                              Strip.lab = F, Hex.size = 40, Show.diff = F, Vertical = T, Leg.pos = "none",
                              Select.trait = c("TRY_LeafArea", "TRY_LeafN", "TRY_SeedMass", "TRY_SLA", "TRY_SSD", "TRY_Height"))
    
    
    MAP.CWM.SI <- wrap_plots(Map.SI1, Map.SI2, Map.SI3, widths = c(1/3, 1/3, 1/3)) 
    W = 1400 ; H = 2000 ; Save.plot = "Figures/SI/Figure_A5.pdf"
    ggsave(filename = Save.plot, MAP.CWM.SI, width = W*0.026458333, height = H*0.026458333, units = "cm")
  }
  
  #### Figure A6 ####
  if(Fig.A6 == T){
    print("**** Figure A6 plotting. ****")
    H = 700 ; W = 800 ; Save.matcor.CWM = "Figures/SI/Figure_A6.pdf"
    pdf(file = Save.matcor.CWM, width = W*0.01041666666667, height = H*0.01041666666667)
    par(mfrow = c(2,3))
    my_color <- colorRampPalette(c("royalblue", "royalblue", "grey95", "grey95", "darkorange", "darkorange"))(20)
    My_use.cor <- "pairwise.complete.obs"
    My_method.cor <- "pearson"
    Green.tiles <- list(c("Altitude", "Height"), c("Longitude", "LeafArea"))
    
    MC.mv.gf <- Mat.corel.CWT.clim(MCWT.clim.MV_gf[Clim], MCWT.clim.MV_gf[Trait], 
                                   I.confiance = 0.95, Display.pval = "pch", my_color = my_color, 
                                   Disp.R = "number", Label = F, Average = F,  Bar.pos = "n", 
                                   Good.tiles = Green.tiles, Return.slope = T, Print.result = F, 
                                   Use.cor = My_use.cor, Method.cor = My_method.cor)
    
    MC.ss.gf <- Mat.corel.CWT.clim(MCWT.clim.PT_ss_gf[Clim], MCWT.clim.PT_ss_gf[Trait],
                                   I.confiance = 0.95, return.pick = F, Return.slope = T, Print.result = F,
                                   my_color = my_color, Display.pval = "pch", 
                                   Disp.R = "number", Label = F, Average = F,  Bar.pos = "n",
                                   Use.cor = My_use.cor, Method.cor = My_method.cor,)
    
    MC.sl.gf <- Mat.corel.CWT.clim(MCWT.clim.PT_sl_gf[Clim], MCWT.clim.PT_sl_gf[Trait], 
                                   I.confiance = 0.95, Return.slope = T, Print.result = F,
                                   Display.pval = "pch", my_color = my_color,
                                   Disp.R = "number", Label = F, Average = F,  Bar.pos = "n",
                                   Use.cor = My_use.cor, Method.cor = My_method.cor)
    
    plot.new()
    Diff.mat <- round(MC.mv.gf$R2 - MC.ss.gf$R2, digits = 2)
    CP1 <- corrplot(Diff.mat, col = my_color,
                    tl.col="black", tl.srt=45, tl.cex = .7, method = "number", cl.align.text = "l", cl.pos = "n",
                    sig.level = 0.05, insig = "pch", pch.cex = 2)
    
    Diff.mat <- round(MC.mv.gf$R2 - MC.sl.gf$R2, digits = 2)
    CP2 <- corrplot(Diff.mat, col = my_color, tl.col="black", tl.srt=45,
                    tl.cex = .7, method = "number", cl.align.text = "l", cl.pos = "n",
                    sig.level = 0.05, insig = "pch", pch.cex = 2)
    dev.off()
  }
  
  #### Figure A7 ####
  if(Fig.A7 == T){
    print("**** Figure A7 plotting. ****")
    RL.CWT.clim.PT.f <- LRelation.CWT.clim(CWT = LR.MCWT.clim.PT_ss_gf,
                                           Select.Pclim = c("MPCOQ", "MTCOQ", "AI", "MAAT", "MAP"),
                                           Select.trait = c("TRY_SSD", "TRY_Height", "TRY_LeafArea", "TRY_SLA", "TRY_LeafN", "TRY_SeedMass"),
                                           Transform.Pclim = c("MPCOQ", "MAP", "AI"), Transformation.method = "sqrt",
                                           Select.eco = c("Type"), Pearson.r.pos = "bottomright", 
                                           Strip.lab = F, Bit.map = T, Add.n = T, Pearson.r = T,
                                           Leg.pos = "none", Add.linear = T, Alpha = .1, Trait.lim = c(-4,4))
    
    RL.CWT.clim.V.f <- LRelation.CWT.clim(CWT = LR.MCWT.clim.MV_gf,
                                          Transform.Pclim = c("MPCOQ", "MAP", "AI"), Transformation.method = "sqrt",
                                          Select.Pclim = c("MPCOQ", "MTCOQ", "AI", "MAAT", "MAP"),
                                          Select.trait = c("TRY_SSD", "TRY_Height", "TRY_LeafArea", "TRY_SLA", "TRY_LeafN", "TRY_SeedMass"),
                                          Select.eco = c("Type"),  Add.n = T, Pearson.r = T, Pearson.r.pos = "bottomright", 
                                          Leg.pos = "none", Add.linear = T, Alpha = .05, Strip.lab = F, Bit.map = T)
    
    RL.full.merge <- RL.CWT.clim.V.f/RL.CWT.clim.PT.f
    W = 1000 ; H = 2000 ; Save.plot.2 = "Figures/SI/Figure_A7.pdf"
    ggsave(RL.full.merge, file = Save.plot.2, width = W*0.026458333, height = H*0.026458333, units = "cm", useDingbats = TRUE)
  }
  
  #### Figure A8 ####
  if(Fig.A8 == T){
    print("**** Figure A8 plotting. ****")
    
    Col.vec <- c("sl_gf" = "grey10",
                 "sl_gf_maj22" = "grey30",
                 "sl_gf_maj4" = "grey60", "ss_gf" = "#335286ff",
                 "ss_gf_maj22" = "#3d77a1ff",
                 "ss_gf_maj4" = "#5fbfc2ff")
    
    p1 <- ggplot(MSD,  aes(x = variable, y = value, fill = type))+
      # ylim(0,1.3)+ xlab("Traits")+  ylab("z-score SD")+
      ylim(0,1.3)+ xlab("Traits")+  ylab(expression(SD~(italic(z)-scores)))+
      scale_fill_manual(values = Col.vec, name = "ACASP aggregation scheme", drop = T)+
      geom_boxplot(outlier.colour = "red", outlier.shape = NA, alpha = 0.7, notch = F, notchwidth = 0.7, varwidth = F, na.rm = T, show.legend = T)+
      theme(legend.position = c(0.8,0.8), panel.background = element_blank(),
            legend.key = element_blank(),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 10),
            plot.margin = unit(x = c(1, 2, 2, 0),units="mm"),
            axis.line = element_line(colour = "black"),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 10),
            plot.background = element_blank())
    
    p2 <- ggplot(MSD2,  aes(x = variable, y = value, fill = type))+
      ylim(0,1.3)+ ylab(NULL)+ xlab("Traits")+
      scale_fill_manual(values = Col.vec, drop = T)+
      geom_boxplot(outlier.colour = "red", outlier.shape = NA, alpha = 0.7, notch = F, notchwidth = 0.7, varwidth = F, na.rm = T, show.legend = T)+
      theme(legend.position = c(0.4,0.8), panel.background = element_blank(),
            legend.key = element_blank(),
            axis.title = element_text(size = 14),
            axis.text.y = element_blank(),
            axis.text = element_text(size = 10),
            plot.margin = unit(x = c(1, 2, 2, 0),units="mm"),
            axis.line = element_line(colour = "black"),
            plot.background = element_blank())
    
    p <- (p1 + p2) + plot_layout(widths = c(1/3, 2/3))
    W = 1100
    H = 500
    ggsave(filename = "Figures/SI/Figure_A8.pdf", p, width = W*0.026458333, height = H*0.026458333, units = "cm")
    
    
  }
  
  #### Figure A9 ####
  if(Fig.A9 == T){
    print("**** Figure A9 plotting. ****")
    H = W = 500
    Save.R2compPcov = "Figures/SI/Figure_A9.pdf"
    Mat.R2.Pcov <- melt(Mat.R2.Pcov, id = c("P_cover", "P_site_ACAV"))
    Mat.R2.Pcov$variable <- as.character(Mat.R2.Pcov$variable)
    Mat.R2.Pcov$variable[grep("_co", Mat.R2.Pcov$variable)] <- "ACAV vs. ACASP-co"
    Mat.R2.Pcov$variable[grep("_fi", Mat.R2.Pcov$variable)] <- "ACAV vs. ACASP-fi"
    coeff = 2
    
    p <- ggplot(Mat.R2.Pcov, aes(x = P_cover))+ 
      geom_vline(xintercept = 50, linewidth = .8, color = "grey60", linetype = "dashed")+
      xlab("P[cover] threshold (%)") +
      geom_line(aes(y = value, shape = variable), color = "darkorange") +
      geom_point(aes(y = value, shape = variable), color = "darkorange", size = 3) +
      geom_line(aes(y = P_site_ACAV / coeff), color = "royalblue") +
      geom_point(aes(y = P_site_ACAV / coeff), color = "royalblue") + 
      scale_x_continuous(n.breaks = 10)+
      scale_y_continuous(
        name = "R²",
        sec.axis = sec_axis(~.*coeff, name="Average %[sPlot-sites used in CWM]")) +
      
      theme(axis.line = element_line(colour = "grey30"), 
            plot.background = element_blank(), panel.background = element_blank(),
            panel.grid = element_blank(), legend.position = "top",
            axis.title.y = element_text(color = "darkorange"),
            legend.key = element_blank(), axis.title.y.right = element_text(color = "royalblue"),
            panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 1.5))
    
    ggsave(file = Save.R2compPcov, p, width = W*0.01041666666667, height = H*0.01041666666667)
  }
  
  #### Figure A10 ####
  if(Fig.A10 == T){
    print("**** Figure A10 plotting. ****")
    #### Trait select ####
    Trait <- c(11,12,5,7,13,4)
    Clim.chel <- c(2,3,15,16,17,19,21,23,25,27)
    Clim.wc <- c(2,3,15,16,17,18,20,22,24,26,28)
    Clim.both <- c(Clim.chel, Clim.wc)
    Clim <- Clim.chel
    Trait <- Clim.chel
    
    MR2.MV_gf <- MCWT.clim.MV_gf
    MR2.PT_ss_gf <- MCWT.clim.PT_ss_gf
    MR2.PT_sl_gf <- MCWT.clim.PT_sl_gf
    
    names(MR2.MV_gf) <- gsub("_chel", "", names(MR2.MV_gf))
    names(MR2.PT_ss_gf) <- gsub("_chel", "", names(MR2.PT_ss_gf))
    names(MR2.PT_sl_gf) <- gsub("_chel", "", names(MR2.PT_sl_gf))
    names(MR2.MV_gf) <- gsub("_wc", "", names(MR2.MV_gf))
    names(MR2.PT_ss_gf) <- gsub("_wc", "", names(MR2.PT_ss_gf))
    names(MR2.PT_sl_gf) <- gsub("_wc", "", names(MR2.PT_sl_gf))
    my_color <- colorRampPalette(c("royalblue", "royalblue", "grey95", "grey95", "darkorange", "darkorange"))(20)
    
    names(MR2.MV_gf) <- gsub("TRY_", "", names(MR2.MV_gf))
    names(MR2.PT_ss_gf) <- gsub("TRY_", "", names(MR2.PT_ss_gf))
    names(MR2.PT_sl_gf) <- gsub("TRY_", "", names(MR2.PT_sl_gf))
    
    #### Mat correl ####
    MC.mv.gf <- Mat.corel.CWT.clim(MR2.MV_gf[Trait], MR2.MV_gf[Trait], 
                                   I.confiance = 0.95,
                                   Display.pval = "pch", my_color = my_color, Display = "lower",
                                   Disp.R = "number", Label = F, Average = F,  Bar.pos = "n")
    
    MC.ss.gf <- Mat.corel.CWT.clim(MR2.PT_ss_gf[Trait], MR2.PT_ss_gf[Trait],
                                   I.confiance = 0.95, return.pick = F,
                                   my_color = my_color, Display.pval = "pch", Display = "lower",
                                   Disp.R = "number", Label = F, Average = F,  Bar.pos = "n")
    
    MC.sl.gf <- Mat.corel.CWT.clim(MR2.PT_sl_gf[Trait], MR2.PT_sl_gf[Trait], 
                                   I.confiance = 0.95,
                                   Display.pval = "pch", my_color = my_color, Display = "lower",
                                   Disp.R = "number", Label = F, Average = F,  Bar.pos = "n")
    
    #### R2 compar ####
    p1 <- R2.compar(MV = MC.mv.gf, MP = MC.ss.gf, Repel.outliers = F, shade.areas = T,
                    Xlab = "r (CWM-ACASP-fi)", Ylab = "r (CWM-ACAV)", Nb.labs = 0, Panel.lab = "(A)")
    p2 <- R2.compar(MV = MC.mv.gf, MP = MC.sl.gf, Leg.pos = "none", shade.areas = T,
                    Xlab = "r (CWM-ACASP-co)", Ylab = "r (CWM-ACAV)", Nb.labs = 6, Panel.lab = "(B)")
    
    pall <- p1 + p2
    H = 400 ; W = 700 ; Save.matcor.trait = "Figures/SI/Figure_A10.pdf"
    ggsave(file = Save.matcor.trait, pall, width = W*0.01041666666667, height = H*0.01041666666667)
  }
  
  #### Figure A11 ####
  if(Fig.A11 == T){
    print("**** Figure A11 plotting. ****")
    BP.MV <- MCWT.clim.MV_gf
    BP.PT <- MCWT.clim.PT_ss_gf
    names(BP.MV) <- gsub("_wc", "", names(MCWT.clim.MV_gf))
    names(BP.PT) <- gsub("_wc", "", names(MCWT.clim.PT_ss_gf))
    
    Biplot.clim.MV <- Biplot.bioclim(MC.area = BP.MV, MC.samp = BP.MV, 
                                     PClim1 = "MPWAQ", PClim2 = "Latitude",
                                     Leg.pos = "none", Show.density = F,
                                     Same.mat = T, Limites = c(0, 500, 25, 55))
    
    Biplot.clim.PT <- Biplot.bioclim(MC.area = BP.PT, MC.samp = BP.PT, 
                                     PClim1 = "MPWAQ", PClim2 = "Latitude",
                                     Leg.pos = "none", Show.density = F,
                                     Same.mat = T, Limites = c(0, 500, 25, 55))
    
    H = 800 ; W = 1300 ; Save.plot = "Figures/SI/Figure_A11.pdf"
    Biplot2 <- Biplot.clim.PT + Biplot.clim.MV 
    ggsave(Biplot2, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm", useDingbats = TRUE)
  }
  
  
  
  #### Figure 4 ####
  if(Fig.4 == T){
    print("**** Figure 4 plotting. ****")
    BioCl.pca.mps <- PCA.bioclim(MCPV.MPS.wc, transp_OK = T, Scale.PCA = 7,
                                 Cluster.core = "Biome", Shape = 16, Display.only.MPS = T,
                                 Legend.size = 8, Dot.size = 1.8, Size.MPS = 1.8, Dot.opac = 1,
                                 Groupes = list(Water = c("AI", "MAP", "MPWAQ", "MPCOQ"),
                                                Temperature = c("MAAT", "MTWAQ", "MTCOQ"),
                                                Altitude = c("Altitude", "Latitude", "Longitude")
                                 ),
                                 Variable.inactive = c("Altitude", "Latitude", "Longitude"),
                                 VIF = T, Display.VIF = T, Contrib = F,
                                 Site.name = "", Num.facet = "(C) Pollen", return.pick = T, Legend.position = "right")
    
    BioCl.pca.mv <- PCA.bioclim(MCPV.MV.wc, transp_OK = T, Scale.PCA = 7,
                                Cluster.core = "Biome", Shape = 16, Dot.size = 1.8, Size.MPS = 1.6, Display.only.MPS = T, Dot.opac = 1,
                                Contrib = F, return.pick = T, return.PCA.variable = F,
                                Groupes = list(Water = c("AI", "MAP", "MPWAQ", "MPCOQ"),
                                               Temperature = c("MAAT", "MTWAQ", "MTCOQ"),
                                               Altitude = c("Altitude", "Latitude", "Longitude")),
                                Variable.inactive = c("Altitude", "Latitude", "Longitude"),
                                Site.name = "", Num.facet = "(B) Vegetation", Legend.position = "none")
    
    BioCl.pca.lc <- PCA.bioclim(MCPV.MV.wc[MCPV.MV.wc$Type == "PV",], transp_OK = T, Scale.PCA = 7,
                                Contrib = F, return.pick = T, return.PCA.variable = F,
                                Cluster.core = "Biome", Dot.size = 1.8, 
                                Groupes = list(Water = c("AI", "MAP", "MPWAQ", "MPCOQ"),
                                               Temperature = c("MAAT", "MTWAQ", "MTCOQ")),
                                No.arrow = T, Hide.MPS = T,
                                Site.name = "", Num.facet = "(A) Land cover", Legend.position = "none")
    
    L <- "AB
          CD"
    
    full.pca.clim <- BioCl.pca.lc + BioCl.pca.mv + BioCl.pca.mps + guide_area() + plot_layout(design = L)
    W = 1500; H = 1200
    ggsave(filename = "Figures/Article/Figure_4.pdf", full.pca.clim, width = W*0.026458333, height = H*0.026458333, units = "cm")
  }
  
  #### Figure 5 ####
  if(Fig.5 == T){
    print("**** Figure 5 plotting. ****")
    BG.MV <- MCWT.clim.MV_gf
    BG.PT <- MCWT.clim.PT_ss_gf
    Keep.trait <- c("SLA", "SSD", "LeafArea", "Height", "SeedMass", "LeafN")
    Highlight.tiles <- list(c("Height", "SeedMass"), c("SLA", "LeafN"), c("SLA", "LeafArea"))
    
    A <- Mantel.plot(Keep.trait = Keep.trait, Npermutation = Nb.perm.Moiran.I, Moran.I = F, Nb.voisin = 10,
                     Mtrait = BG.MV, Display.info = F, Mantel.test = F, Moran.cross.I = T, Highlight.tiles = Highlight.tiles)
    
    B <- Mantel.plot(Keep.trait = Keep.trait, Npermutation = Nb.perm.Moiran.I, Moran.I = F,
                     Mtrait = BG.PT, Display.info = F, Mantel.test = F, Moran.cross.I = T, Nb.voisin = 10)
    
    Map <- Map.biogeo.CWM(MCWT = BG.PT, Type1 = "ACA-pollen-fine",
                          MCWT2 = BG.MV, Type2 = "ACA-vegetation",
                          Strip.lab = F, Hex.size = 40, Show.diff = F, Vertical = T, Leg.pos = "bottom",
                          Select.trait = c("TRY_LeafArea", "TRY_SSD", "TRY_Height"))
    
    MAP.CWM <- (A | B)/Map
    W = 1400 ; H = 2400 ; Save.plot = "Figures/Article/Figure_5.pdf"
    ggsave(filename = Save.plot, MAP.CWM, width = W*0.026458333, height = H*0.026458333, units = "cm")
    
  }
  
  #### Figure 6 ####
  if(Fig.6 == T){
    print("**** Figure 6 plotting. ****")
    
    RL.CWT.clim.PT <- LRelation.CWT.clim(CWT = LR.MCWT.clim.PT_ss_gf,
                                         Select.Pclim = c("MTCOQ", "MPCOQ", "MAP"),
                                         Transform.Pclim = c("MPCOQ", "MAP"), Transformation.method = "sqrt",
                                         Select.trait = c("TRY_SSD", "TRY_Height", "TRY_LeafArea"),
                                         Strip.lab = F, Add.n = F, Add.n.facet = T,
                                         Add.bootstrap = T, Nb.boot = 9999, 
                                         Select.eco = c("Type"),
                                         Leg.pos = "none", Add.linear = T, Alpha = .2, Trait.lim = c(-3,2),
                                         Bit.map = T, Pearson.r = T, Pearson.r.pos = "bottomright")
    
    RL.CWT.clim.V <- LRelation.CWT.clim(CWT = LR.MCWT.clim.MV_gf,
                                        Select.Pclim = c("MTCOQ", "MPCOQ", "MAP"),
                                        Transform.Pclim = c("MPCOQ", "MAP"), Transformation.method = "sqrt",
                                        Select.trait = c("TRY_SSD", "TRY_Height", "TRY_LeafArea"),
                                        Select.eco = c("Type"), Strip.lab = F, Bit.map = T, Pearson.r = T, Add.n = F, Add.n.facet = T,
                                        Add.bootstrap = T, Nb.boot = 9999, 
                                        Leg.pos = "none", Add.linear = T, Alpha = .05, Pearson.r.pos = "bottomright")
    
    RL.full <- RL.CWT.clim.V/RL.CWT.clim.PT
    W = 700 ; H = 1200 ; Save.plot = "Figures/Article/Figure_6.pdf"
    ggsave(RL.full, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm", useDingbats = TRUE)
  
    }
  
  #### Figure 7 ####
  if(Fig.7 == T){
    print("**** Figure 7 plotting. ****")
    My_use.cor <- "pairwise.complete.obs"
    My_method.cor <- "pearson"
    
    MC.mv.gf <- Mat.corel.CWT.clim(MCWT.clim.MV_gf[Clim], MCWT.clim.MV_gf[Trait], 
                                   I.confiance = 0.95, Display.pval = "pch", 
                                   Disp.R = "number", Label = F, Average = F,  Bar.pos = "n",  return.pick = F,
                                   Return.slope = F, Print.result = F, 
                                   Use.cor = My_use.cor, Method.cor = My_method.cor)
    
    MC.ss.gf <- Mat.corel.CWT.clim(MCWT.clim.PT_ss_gf[Clim], MCWT.clim.PT_ss_gf[Trait],
                                   I.confiance = 0.95, return.pick = F, Return.slope = T, Print.result = F, Display.pval = "pch", 
                                   Disp.R = "number", Label = F, Average = F,  Bar.pos = "n",
                                   Use.cor = My_use.cor, Method.cor = My_method.cor,)
    
    MC.sl.gf <- Mat.corel.CWT.clim(MCWT.clim.PT_sl_gf[Clim], MCWT.clim.PT_sl_gf[Trait], 
                                   I.confiance = 0.95, Return.slope = T, Print.result = F,
                                   Display.pval = "pch", return.pick = F,
                                   Disp.R = "number", Label = F, Average = F,  Bar.pos = "n",
                                   Use.cor = My_use.cor, Method.cor = My_method.cor)
    
    Diff.mat <- round(MC.mv.gf$R2 - MC.ss.gf$R2, digits = 2)
    CP1 <- corrplot(Diff.mat, tl.col="black", tl.srt=45, tl.cex = .7, method = "number", cl.align.text = "l", cl.pos = "n",
                    sig.level = 0.05, insig = "pch", pch.cex = 2)
    
    Diff.mat <- round(MC.mv.gf$R2 - MC.sl.gf$R2, digits = 2)
    CP2 <- corrplot(Diff.mat, tl.col="black", tl.srt=45, tl.cex = .7, method = "number", cl.align.text = "l", cl.pos = "n",
                    sig.level = 0.05, insig = "pch", pch.cex = 2)
    
    #### R2 compar ####
    p1 <- R2.compar(MV = MC.mv.gf, MP = MC.ss.gf, Repel.outliers = F, Show.Plotly = F, 
                    Compare.slopes = F, RMA = T, Avg.r.Fisher.z = T,
                    Leg.pos = c(0.2, 0.8), shade.areas = T, Display.message = F,
                    Xlab = expression(italic(r)~(CWM-pollen-italic(fine))), Ylab = expression(italic(r)~(CWM-vegetation)), 
                    Nb.labs = 4, Panel.lab = "(A)")
    
    p2 <- R2.compar(MV = MC.mv.gf, MP = MC.sl.gf, 
                    Compare.slopes = F, RMA = T, Avg.r.Fisher.z = T,
                    Leg.pos = "none",
                    Repel.outliers = T, Show.Plotly = F, shade.areas = T,
                    Xlab = expression(italic(r)~(CWM-pollen-italic(coarse))), Ylab = expression(italic(r)~(CWM-vegetation)), Nb.labs = 4, Panel.lab = "(B)")
    
    pall <- p1 + p2
    H = 340 ; W = 700 ; Save.matcor.trait = "Figures/Article/Figure_7.pdf"
    ggsave(file = Save.matcor.trait, pall, width = W*0.01041666666667, height = H*0.01041666666667)
  }
  
  #### Figure 8 ####
  if(Fig.8 == T){
    #### Matrice correlation ####
    print("**** Figure 8 plotting. ****")
    CWM.arm.MC <- CWM.arm[!grepl("^SD_", names(CWM.arm))]
    CWM.arm.MC <- subset(CWM.arm.MC, select = -c(Zone, N))
    CWM.arm.MC <- CWM.arm.MC[order(names(CWM.arm.MC), decreasing = T)]
    
    Tra.arm.MC <- Tra.arm[c(1:7)]
    Tra.arm.MC <- Tra.arm.MC[c(1,2,6,4,3,7,5)]
    MC.TraCE.CWM <- Matcor.by.core.litho(
      XRF = Tra.arm.MC,
      GDGT = CWM.arm.MC,
      Plot.x = "Age",
      Gaussian.filtering = F, Display.info = T, Proxy2proxy = F, Show.inner = F, ggplot.version = T, 
      Order.hclust = F, Bar.pos = "n", return.plot = T, No.axis.lab = T, Disp.R = "square",
      Permutation.test = T, Nb.permutations = 9999, Display = "full", Xlab.rot = 0, I.confiance = 0.99)
    
    #### Merge data ####
    Keep.clim <- c("Age", "(C) Precipitation", "(D) Temperature", "Zone")
    
    Mplot <- Tra.arm.1
    Mplot <- suppressMessages(full_join(Mplot, Tra.arm.2))
    Mplot <- suppressMessages(full_join(Mplot, Tra.arm.3))
    Mplot <- suppressMessages(full_join(Mplot, Tra.arm.4))
    Tra.arm.m <- reshape2::melt(Mplot, id = c("Age", "Zone"))
    
    Mplot$Type <- "TraCE-21K"
    Mplot <- suppressMessages(full_join(Mplot, CWM.arm.1))
    Mplot <- suppressMessages(full_join(Mplot, CWM.arm.2))
    Mplot <- suppressMessages(full_join(Mplot, CWM.arm.3))
    Mplot <- suppressMessages(full_join(Mplot, CWM.arm.4))
    Mplot <- suppressMessages(full_join(Mplot, CWM.arm.5))
    Mplot <- suppressMessages(full_join(Mplot, CWM.arm.6))
    Mplot$Type[is.na(Mplot$Type)] <- "CWM traits"
    Mplot <- reshape2::melt(Mplot, id = c("Age", "Zone", "Type"))
    Mplot$variable <- factor(Mplot$variable, levels = Keep.clim, ordered = T)
    
    DF.annot <- data.frame(X = c(-Inf, -Inf), Y = c(Inf, Inf), variable = c("(C) Precipitation", "(D) Temperature"))
    
    #### Melt + SD ####
    Keep.unmelt <- c("Age", "N")
    Res <- subset(CWM.arm, select = - c(Zone))
    CI.perc <- 95
    CI.alpha <- .2
    df_means <- reshape2::melt(Res, id.vars = Keep.unmelt, measure.vars = names(Res)[!names(Res) %in% Keep.unmelt & !grepl("^SD_", names(Res))], variable.name = c("Param.clim"), value.name = "Val.clim")
    df_sds <- reshape2::melt(Res, id.vars = Keep.unmelt, measure.vars = names(Res)[grepl("^SD_", names(Res))], variable.name = "Param.clim", value.name = "Val.SD")
    df_sds$Param.clim <- gsub("SD_", "", df_sds$Param.clim)
    Res <- left_join(df_means, df_sds, by = c(Keep.unmelt, "Param.clim"))
    
    Res$se <- Res$Val.SD / sqrt(Res$N)  # Standard error
    Res$CI_lower <- Res$Val.clim - 1.96*Res$se
    Res$CI_upper <- Res$Val.clim + 1.96*Res$se
    CI <- geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = Param.clim), color = NA, size = 0.15, linetype = "dashed", alpha = CI.alpha)
    Res$Type <- "CWM traits"
    
    DF.annot2 <- data.frame(X = c(-Inf, -Inf), Y = c(Inf, Inf), Param.clim = c("Leaf area"), Text = "(A) CWM traits")
    
    #### Plot P1 ####
    p1 <- ggplot(Mplot, aes(x = Age, y = value, colour = Zone, fill = Zone, shape = Type))+
      geom_vline(xintercept = 8200, color = "grey40", linetype = "dashed")+
      geom_vline(xintercept = 11700, color = "grey40", linetype = "dashed")+
      geom_text(inherit.aes = F, data = DF.annot, aes(x = X, y = Y, label = variable), vjust = 1.5, hjust = -0.1, size = 5)+
      geom_point(size = 1, alpha = .5)+
      ylab("z-scores")+
      xlab("Time (cal. year BP)")+
      scale_x_continuous(breaks = seq(0, Lims.2k[2], 1000))+
      facet_grid(.~variable, switch = "y")+
      scale_colour_manual(values = My_color)+
      scale_shape_manual(values = c("TraCE-21K" = 21, "CWM traits" = 4))+
      scale_fill_manual(values = My_color)+
      geom_text_repel(
        data = subset(Mplot, Age == max(Age)),
        mapping = aes(x = Age, label = Zone),
        force = 50,
        segment.curvature = .05,
        nudge_x  = (Lims.2k[2]- Lims.2k[1])/4, direction = "y", hjust = 1,
        size = 4.5, fontface = "bold",
        parse = F, 
        segment.size = 0.18,
        segment.colour = "grey70")+
      geom_smooth(method = "loess", se = T, alpha = .05, fullrange = T, span = .4, na.rm = T, linewidth = .1, formula = 'y ~ x')+
      geom_smooth(inherit.aes = F, data = Tra.arm.m, aes(x = Age, y = value, colour = Zone), method = "loess", se = T, alpha = .1, fullrange = T, span = .4, na.rm = T, linewidth = 1, formula = 'y ~ x')+
      geom_line(inherit.aes = F, data = Tra.arm.m, aes(x = Age, y = value, colour = Zone), linewidth = .4)+
      theme_bw()+
      theme(legend.position = "none", panel.grid = element_blank(), strip.background = element_blank(), 
            plot.margin = unit(x = c(0, 0, 0, 0),units="mm"), panel.spacing = unit(0.15, "lines"),
            plot.background = element_blank(), strip.text = element_blank(), strip.placement = "outside",
            axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    #### Plot P2 ####
    p2 <- ggplot(Res, aes(x = Age, y = Val.clim, colour = Param.clim, shape = Type))+ CI + 
      geom_vline(xintercept = 8200, color = "grey40", linetype = "dashed")+
      geom_text(inherit.aes = F, data = DF.annot2, aes(x = X, y = Y, label = Text), vjust = 1.5, hjust = -0.1, size = 5)+
      geom_vline(xintercept = 11700, color = "grey40", linetype = "dashed")+
      geom_point(size = 2, alpha = .5)+
      ylab("z-scores")+ xlab("Time (cal. year BP)")+
      scale_x_continuous(breaks = seq(0, Lims.2k[2], 1000))+
      facet_grid(Param.clim~., switch = "y", scales = "free")+
      scale_colour_manual(values = My_color)+
      scale_shape_manual(values = c("TraCE-21K" = 21, "CWM traits" = 1))+
      scale_fill_manual(values = My_color)+
      geom_text_repel(
        data = subset(Res, Age == max(Age)), mapping = aes(x = Age, label = Param.clim),
        force = 50, segment.curvature = .05,
        nudge_x  = (Lims.2k[2]- Lims.2k[1])/4, direction = "y", hjust = 1,
        size = 4.5, fontface = "bold", parse = F, segment.size = 0.18, segment.colour = "grey70")+
      geom_line(linewidth = .4)+
      theme_bw()+
      theme(legend.position = "none", panel.grid = element_blank(), strip.background = element_blank(), strip.text = element_text(face = "bold", size = 14),
            strip.text.y = element_blank(), strip.placement = "outside", axis.title.x = element_blank(), panel.background = element_blank(), plot.margin = unit(x = c(0, 0, 0, 0),units="mm"), 
            plot.background = element_blank(), panel.spacing = unit(0.1, "lines"),
            axis.text.x = element_blank()
      )
    
    #### Export ####
    L = "AABB
         AABB
         CCCC"
    
    p1 <- p2 + MC.TraCE.CWM + p1 + plot_layout(design = L)
    H = 825; W = 1100; Save.plot = "Figures/Article/Figure_8.pdf"
    ggsave(file = Save.plot, p1, width = W*0.01041666666667, height = H*0.01041666666667)
  }
  
  
  #### Table A2 ####
  if(Tab.A2 == T){
    #### Parameters for all RDA ####
    DF.RDA1 <- setNames(data.frame(t(MCWT.clim.PT_ss_gf[,Trait]), check.names = F), MCWT.clim.PT_ss_gf$Site)
    row.names(MCWT.clim.PT_ss_gf) <- MCWT.clim.PT_ss_gf$Site
    DF.RDA2 <- setNames(data.frame(t(MCWT.clim.PT_sl_gf[,Trait]), check.names = F), MCWT.clim.PT_sl_gf$Site)
    row.names(MCWT.clim.PT_sl_gf) <- MCWT.clim.PT_sl_gf$Site
    DF.RDA3 <- setNames(data.frame(t(MCWT.clim.MV_gf[,Trait]), check.names = F), MCWT.clim.MV_gf$Site)
    row.names(MCWT.clim.MV_gf) <- MCWT.clim.MV_gf$Site
    Keep.param.clim <- c("AI", "MPWAQ", "MPCOQ", "MTCOQ", "MTWAQ", "MAP", "MAAT")
    Keep.param.clim.2 <- c("MPCOQ", "MTCOQ", "MAP", "MAAT")
    Cluster.groups <- "Biome"
    
    #### RDA ####
    print("**** Table A2 LaTeX export. ****")
    
    VIF.ss.gf <- RDA.pollen.surf(MP = DF.RDA1, MClim = MCWT.clim.PT_ss_gf, GDGT = F, Remove.NA = F, Complete.NA = T,
                                 Choose.clim = Keep.param.clim, Display.plot = F, Csv.sep =",", transp_OK = F, Helinger.trans = F, VIF = T, Display.VIF = F, return.VIF = T)
    
    VIF.ss.gf.2 <- RDA.pollen.surf(MP = DF.RDA1, MClim = MCWT.clim.PT_ss_gf, GDGT = F, Remove.NA = F, Complete.NA = T,
                                   Choose.clim = Keep.param.clim.2, Display.plot = F, Csv.sep =",", transp_OK = F, Helinger.trans = F, VIF = T, Display.VIF = F, return.VIF = T)
    
    VIF.sl.gf <- RDA.pollen.surf(MP = DF.RDA2, MClim = MCWT.clim.PT_sl_gf, GDGT = F, Remove.NA = F, Complete.NA = T,
                                 Choose.clim = Keep.param.clim, Display.plot = F, Csv.sep =",", transp_OK = F, Helinger.trans = F, VIF = T, Display.VIF = F, return.VIF = T)
    
    VIF.sl.gf.2 <- RDA.pollen.surf(MP = DF.RDA2, MClim = MCWT.clim.PT_sl_gf, GDGT = F, Remove.NA = F, Complete.NA = T,
                                   Choose.clim = Keep.param.clim.2, Display.plot = F, Csv.sep =",", transp_OK = F, Helinger.trans = F, VIF = T, Display.VIF = F, return.VIF = T)
    
    VIF.MV <- RDA.pollen.surf(MP = DF.RDA3, MClim = MCWT.clim.MV_gf, GDGT = F, Remove.NA = T, Complete.NA = F,
                              Choose.clim = Keep.param.clim, Display.plot = F, Csv.sep =",", transp_OK = F, Helinger.trans = F, VIF = T, Display.VIF = F, return.VIF = T)
    
    VIF.MV.2 <- RDA.pollen.surf(MP = DF.RDA3, MClim = MCWT.clim.MV_gf, GDGT = F, Remove.NA = T, Complete.NA = F,
                                Choose.clim = Keep.param.clim.2, Display.plot = F, Csv.sep =",", transp_OK = F, Helinger.trans = F, VIF = T, Display.VIF = T, return.VIF = T)
    
    #### Table VIF ####
    Table.VIF <- data.frame(rbind(VIF.MV, VIF.ss.gf, VIF.sl.gf))
    Table.VIF$Dataset <- c("Vegetation", "Pollen (fine)", "Pollen (coarse)")
    Table.VIF$Model <- "All predictors"
    
    Table.VIF.2 <- data.frame(rbind(VIF.MV.2, VIF.ss.gf.2, VIF.sl.gf.2))
    Table.VIF.2$Dataset <- c("Vegetation", "Pollen (fine)", "Pollen (coarse)")
    Table.VIF.2$Model <- "Selected predictors"
    
    Table.VIF <- full_join(Table.VIF, Table.VIF.2)
    names(Table.VIF) <- gsub("_wc", "", names(Table.VIF))
    Table.VIF <- Table.VIF[c(9,8,7,5,4,1,6,2,3)]
    
    Table.VIF$Model.clean <- Table.VIF$Model
    for(i in 2:nrow(Table.VIF)){if(Table.VIF$Model[i] == Table.VIF$Model[i-1]){Table.VIF$Model.clean[i] <- ""}}
    Table.VIF <- Table.VIF[c(ncol(Table.VIF), 2: (ncol(Table.VIF)-1))]
    names(Table.VIF)[names(Table.VIF) == "Model.clean"] <- "Model"
    
    LateX.caption <- "Variance Inflation Factor (VIF) values for the seven climatic variables included in the principal component analysis (Figure 4). VIF values were calculated to assess the degree of collinearity of climate parameters with their correlation with vegetation and pollen datasets, both at the fine and coarse aggregation schemes."
    Tlatex <- xtable::xtable(Table.VIF, caption = LateX.caption, type = "latex", label = "FT_table")
    Save.path.tex <- "Results/Table_LaTeX/Table_A2.tex"
    print(Tlatex, file = Save.path.tex, booktabs = T, include.rownames = F, comment = F,
          caption.placement = "top", sanitize.text.function = function(x){x},
          hline.after = c(-1,0,3,nrow(Table.VIF)))
  }
  
  #### Table A3 ####
  if(Tab.A3 == T){
    print("**** Table A3 LaTeX export. ****")
    PR.PT.ss <- Partial.Regression.CWM(
      dataframe = MCWT.clim.PT_ss_gf,
      List.of.traits = c("TRY_Height", "TRY_LeafArea", "TRY_SSD"),
      Climate.param.1 = "MAP",
      Climate.param.2 = "MPCOQ",
      Climate.param.3 = "MTCOQ"
    )
    
    PR.MV <- Partial.Regression.CWM(
      dataframe = MCWT.clim.MV_gf,
      List.of.traits = c("TRY_Height", "TRY_LeafArea", "TRY_SSD"),
      Climate.param.1 = "MAP",
      Climate.param.2 = "MPCOQ",
      Climate.param.3 = "MTCOQ",
    )
    
    Table.PR <- cbind(PR.MV[[1]], setNames(PR.PT.ss[[1]][c(3:5)], c("Global $R^2_{adj.}$", "Partial $R^{2}$", "$p$-value")))
    print(Table.PR)
    
    LateX.caption <- "Metrics of the partial regressions models comparing CWMs and climate parameters for both vegetation and pollen fine datasets. The set of relationships tested correspond to Figure 6. Statistical significance is denoted as * (p < 0.05), ** (p < 0.01), and *** (p < 0.001)."
    Tlatex <- xtable::xtable(Table.PR, caption = LateX.caption, type = "latex", label = "Table_PR")
    Save.path.tex <- "Results/Table_LaTeX/Table_A3.tex"
    print(Tlatex, file = Save.path.tex, booktabs = T, include.rownames = F, comment = F,
          caption.placement = "top", sanitize.text.function = function(x){x},
          hline.after = c(-1,0,3,nrow(Table.PR)))
  }
  #### Table A4 ####
  if(Tab.A4 == T){
    print("**** Table A4 LaTeX export. ****")
    dir.create("Results/Bootstrap", recursive = T, showWarnings = F)
    
    #### LR with filtered dataset ####
    RL.CWT.clim.V <- LRelation.CWT.clim(CWT = LR.MCWT.clim.MV_gf,
                                        Select.Pclim = c("MTCOQ", "MPCOQ", "MAP"),
                                        Transform.Pclim = c("MPCOQ", "MAP"), Transformation.method = "sqrt",
                                        Select.trait = c("TRY_SSD", "TRY_Height", "TRY_LeafArea"),
                                        Select.eco = c("Type"), Strip.lab = F, Bit.map = T, Pearson.r = T, Add.n = F, Add.n.facet = T,
                                        Add.bootstrap = T, Nb.boot = 9999, Save.bootstrap = "Results/Bootstrap/Bootstrap_CWM_veget_filtered.Rds", r.size = 3.2,
                                        Leg.pos = "none", Add.linear = T, Alpha = .05, Pearson.r.pos = "bottomright")
    
    names(MCWT.clim.MV_gf.imp) <- gsub("_wc", "", names(MCWT.clim.MV_gf.imp))
    MCWT.clim.MV_gf.imp$Type <- "Vegetation" 
    
    #### LR with unfiltered dataset ####
    RL.CWT.clim.V <- LRelation.CWT.clim(CWT = MCWT.clim.MV_gf.imp,
                                        Select.Pclim = c("MTCOQ", "MPCOQ", "MAP"),
                                        Transform.Pclim = c("MPCOQ", "MAP"), Transformation.method = "sqrt",
                                        Select.trait = c("TRY_SSD", "TRY_Height", "TRY_LeafArea"),
                                        Select.eco = c("Type"), Strip.lab = F, Bit.map = T, Pearson.r = T, Add.n = F, Add.n.facet = T,
                                        Add.bootstrap = T, Nb.boot = 9999, Save.bootstrap = "Results/Bootstrap/Bootstrap_CWM_veget_unfiltered.Rds", r.size = 3.2,
                                        Leg.pos = "none", Add.linear = T, Alpha = .05, Pearson.r.pos = "bottomright")
    
    #### Sensitivity Analysis ####
    Bootstrap.unf <- readRDS("Results/Bootstrap/Bootstrap_CWM_veget_unfiltered.Rds")
    Bootstrap.fil <- readRDS("Results/Bootstrap/Bootstrap_CWM_veget_filtered.Rds")
    
    Bootstrap.unf <- Bootstrap.unf[c(1:7)]
    Bootstrap.fil <- Bootstrap.fil[c(1:7)]
    
    RBS <- Bootstrap.Sensitivity(DF1 = MCWT.clim.MV_gf.imp,
                                 DF2 = LR.MCWT.clim.MV_gf, id_col = "Site",
                                 Relation.2.test = c("TRY_Height vs. MAAT",
                                                     "TRY_LeafArea vs. MPCOQ",
                                                     "TRY_SeedMass vs. MTCOQ",
                                                     "TRY_SLA vs. MAP",
                                                     "TRY_SSD vs. MAAT"), 
                                 Nb.boot = 9999, Digits.r = 2, Digits.slope = 4,
                                 Save.LateX = "Results/Table_LaTeX/Table_A4.tex")
    print(RBS)
    }
  
  
  #### Table A5 ####
  if(Tab.A5 == T){
    print("**** Table A5 LaTeX export. ****")
    Tab.to.exp <- data.frame(Rank = seq(1, nrow(data.frame(Pollen.PCA.sl.contrib))), 
                             Pollen.type = names(Pollen.PCA.sl.contrib),
                             "PCA contribution" = round(Pollen.PCA.sl.contrib, digits = 3))
    
    Tab.to.exp$Identification.level = Table.Taxon[match(Tab.to.exp$Pollen.type, gsub("\\.", " ", Table.Taxon$Nom)),12]
    Tab.to.exp$Identification.level[Tab.to.exp$Identification.level == 1] <- "Family"
    Tab.to.exp$Identification.level[Tab.to.exp$Identification.level == 2] <- "Sub-family"
    Tab.to.exp$Identification.level[Tab.to.exp$Identification.level == 3] <- "Genus"
    Tab.to.exp$Identification.level[Tab.to.exp$Identification.level == 4] <- "Sub-genus"
    Tab.to.exp$Identification.level[Tab.to.exp$Identification.level == 5] <- "Species"
    # Pollen.PCA.sl.contrib1 <- names(Pollen.PCA.sl.contrib1)
    # Pollen.PCA.sl.contrib2 <- names(Pollen.PCA.sl.contrib2)
    Mean.FA <- round(colMeans(MP_sl[names(MP_sl) %in% row.names(Tab.to.exp)]), digits = 2)*100
    Tab.to.exp$'Mean FA (%)' <- Mean.FA[match(row.names(Tab.to.exp),names(Mean.FA))]
    names(Tab.to.exp) <- gsub("\\.", " ", names(Tab.to.exp))
    
    Tab.to.exp <- Tab.to.exp[c(1:22),]
    Save.path.tex <- "Results/Table_LaTeX/Table_A5.tex"
    LateX.caption <- "Presentation of the main ACA pollen types inferred by PCA contribution and average fractional adundances of the whole pollen surface database."
    Tlatex <- xtable::xtable(Tab.to.exp, caption = LateX.caption, type = "latex", label = "FT_table")
    print(Tlatex, file = Save.path.tex, booktabs = T, include.rownames = F, comment = F, caption.placement = "top", sanitize.text.function = function(x){x})
    
    write.table(Tab.to.exp, "Results/Pollen/PCA_22_type_sl_ACA_contrib.csv", row.names = F)
  }
  
}

