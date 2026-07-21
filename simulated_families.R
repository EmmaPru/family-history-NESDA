# 
# 1) Simulates families with different structures and disorders
# 2) Computes different family history based predictors (FH+, FH proportion, LT-FH) from the simulated data
# 3) Evaluates prediction accuracy (AUC, r2l) of the different predictors
# Input: Scripts/setup.R, Scripts/indicator_functions.R
# Output: Results/outcome_check_denseprop.RData, Results/r2_DEP_famsizes.pdf, Results/r2_DEP_disorders.pdf, Results/AUC_DEP_5fam_3ind.pdf
# Status: DEBUG (problem with simulate_FH function)

#### Preparing the environment ####
rm(list=ls())

# Setup - packages and functions
source(file = "Scripts/setup.R")
source(file = "Scripts/indicator_functions.R")

# # Build the family
# num_sibs <- 2 # number of siblings to simulate
# parents <- c("f", "m") # mother and father
# fam_members <- c("index", paste0("sib", sequence(num_sibs)), parents)

# General notes
# Ks <- c(DEP =  0.19, ANX = 0.2, SCZ = 0.005, BIP = 0.013) # prevalence in Netherlands from NEMESIS-2
# h2ls <- c(DEP =  0.4, ANX = 0.4, SCZ = 0.77, BIP = 0.67) # twin heritability estimates from Polderman et al.
rg_SCZ.BIP.DEP.ANX <- matrix(c( # ldsc regression genetic correlation estimates from Grotzinger et al.
1, 0.682, 0.357, 0.367,
0.682, 1, 0.343, 0.297,
0.357, 0.343, 1, 0.867,
0.367, 0.357, 0.867, 1
),4,4)
rownames(rg_SCZ.BIP.DEP.ANX) <- colnames(rg_SCZ.BIP.DEP.ANX) <- c("SCZ", "BIP", "DEP", "ANX")


rg_DEP.ANX <- rg_SCZ.BIP.DEP.ANX[c("DEP", "ANX"), c("DEP", "ANX")]

sim_reps <- 10
sim_size <- 1e4 # number of simulated persons in one simulation round (for rmvn)

sim.input.list <- list(
  # Depression 
  list(name="DEP, 1 fam member", index_disorder = "DEP", Ks = c(DEP = 0.19), h2ls = c(DEP = 0.4), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1"))
  ,list(name="DEP, 3 fam members", index_disorder = "DEP", Ks = c(DEP = 0.19), h2ls = c(DEP = 0.4), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "m", "f"))
  ,list(name="DEP, 8 fam members", index_disorder = "DEP", Ks = c(DEP = 0.19), h2ls = c(DEP = 0.4), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f"))
  ,list(name="DEP/ANX, 8 fam members", index_disorder = "DEP", Ks = c(DEP = 0.19, ANX = 0.2), h2ls = c(DEP = 0.4, ANX = 0.4), rg = rg_DEP.ANX, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f"))
  ,list(name="DEP/ANX/BIP/SCZ, 8 fam members", index_disorder = "DEP", Ks = c(DEP =  0.19, ANX = 0.2, SCZ = 0.005, BIP = 0.013), h2ls = c(DEP =  0.4, ANX = 0.4, SCZ = 0.77, BIP = 0.67), rg = rg_SCZ.BIP.DEP.ANX, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f"))
  # Anxiety
  ,list(name="ANX, 1 fam member", index_disorder = "ANX", Ks = c(ANX = 0.2), h2ls = c(ANX = 0.4), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1"))
  ,list(name="ANX, 3 fam members", index_disorder = "ANX", Ks = c(ANX = 0.2), h2ls = c(ANX = 0.4), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "m", "f"))
  ,list(name="ANX, 8 fam members", index_disorder = "ANX", Ks = c(ANX = 0.2), h2ls = c(ANX = 0.4), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f"))
  ,list(name="DEP/ANX, 8 fam members", index_disorder = "ANX", Ks = c(DEP = 0.19, ANX = 0.2), h2ls = c(DEP = 0.4, ANX = 0.4), rg = rg_DEP.ANX, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f"))
  ,list(name="DEP/ANX/BIP/SCZ, 8 fam members", index_disorder = "ANX", Ks = c(DEP =  0.19, ANX = 0.2, SCZ = 0.005, BIP = 0.013), h2ls = c(DEP =  0.4, ANX = 0.4, SCZ = 0.77, BIP = 0.67), rg = rg_SCZ.BIP.DEP.ANX, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f"))
  # Bipolar
  ,list(name="BIP, 1 fam member", index_disorder = "BIP", Ks = c(BIP = 0.013), h2ls = c(BIP = 0.67), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1"))
  ,list(name="BIP, 3 fam members", index_disorder = "BIP", Ks = c(BIP = 0.013), h2ls = c(BIP = 0.67), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "m", "f"))
  ,list(name="BIP, 8 fam members", index_disorder = "BIP", Ks = c(BIP = 0.013), h2ls = c(BIP = 0.67), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f"))
  # ,list(name="DEP/ANX, 8 fam members", index_disorder = "DEP", Ks = c(DEP = 0.19, ANX = 0.2), h2ls = c(DEP = 0.35, ANX = 0.32), rg = rg_DEP.ANX, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f"))
  ,list(name="DEP/ANX/BIP/SCZ, 8 fam members", index_disorder = "BIP", Ks = c(DEP =  0.19, ANX = 0.2, SCZ = 0.005, BIP = 0.013), h2ls = c(DEP =  0.4, ANX = 0.4, SCZ = 0.77, BIP = 0.67), rg = rg_SCZ.BIP.DEP.ANX, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f"))
  # Schizophrenia
  ,list(name="SCZ, 1 fam member", index_disorder = "SCZ", Ks = c(SCZ = 0.005), h2ls = c(SCZ = 0.77), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1"))
  ,list(name="SCZ, 3 fam members", index_disorder = "SCZ", Ks = c(SCZ = 0.005), h2ls = c(SCZ = 0.77), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "m", "f"))
  ,list(name="SCZ, 8 fam members", index_disorder = "SCZ", Ks = c(SCZ = 0.005), h2ls = c(SCZ = 0.77), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f"))
  # ,list(name="DEP/ANX, 8 fam members", index_disorder = "DEP", Ks = c(DEP = 0.19, ANX = 0.2), h2ls = c(DEP = 0.35, ANX = 0.32), rg = rg_DEP.ANX, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f"))
  ,list(name="DEP/ANX/BIP/SCZ, 8 fam members", index_disorder = "SCZ", Ks = c(DEP =  0.19, ANX = 0.2, SCZ = 0.005, BIP = 0.013), h2ls = c(DEP =  0.4, ANX = 0.4, SCZ = 0.77, BIP = 0.67), rg = rg_SCZ.BIP.DEP.ANX, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f"))
)

# # Temp
# index_disorder = "DEP"; Ks = c(DEP = 0.19); h2ls = c(DEP = 0.4); rg = 1; sim_reps = sim_reps; sim_size = sim_size; fam_members = c("index", "sib1")
# Ks = c(DEP = 0.19, ANX = 0.2); h2ls = c(DEP = 0.4, ANX = 0.4); rg = rg_DEP.ANX; sim_reps = sim_reps; sim_size = sim_size; fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f")
# simulate_FH(index_disorder = index_disorder, Ks = Ks, h2ls = h2ls, rg = rg, sim_reps = sim_reps, sim_size = sim_size, fam_members = fam_members)


sim.input.list <- list(
  # Depression 
  list(name="DEP, 1 fam member", index_disorder = "DEP", Ks = c(DEP = 0.19), h2ls = c(DEP = 0.4), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1"))
  ,list(name="DEP, 2 fam members", index_disorder = "DEP", Ks = c(DEP = 0.19), h2ls = c(DEP = 0.4), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "m", "f"))
  ,list(name="DEP, 3 fam members", index_disorder = "DEP", Ks = c(DEP = 0.19), h2ls = c(DEP = 0.4), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "m", "f"))
  ,list(name="DEP, 4 fam members", index_disorder = "DEP", Ks = c(DEP = 0.19), h2ls = c(DEP = 0.4), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "m", "f"))
  ,list(name="DEP, 8 fam members", index_disorder = "DEP", Ks = c(DEP = 0.19), h2ls = c(DEP = 0.4), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f"))
  ,list(name="DEP/ANX, 3 fam members", index_disorder = "DEP", Ks = c(DEP = 0.19, ANX = 0.2), h2ls = c(DEP = 0.4, ANX = 0.4), rg = rg_DEP.ANX, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "m", "f"))
  ,list(name="DEP/ANX/BIP, 3 fam members", index_disorder = "DEP", Ks = c(DEP =  0.19, ANX = 0.2, SCZ = 0.005, BIP = 0.013), h2ls = c(DEP =  0.4, ANX = 0.4, SCZ = 0.77, BIP = 0.67), rg = rg_SCZ.BIP.DEP.ANX, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "m", "f"))
  ,list(name="DEP/ANX/BIP/SCZ, 3 fam members", index_disorder = "DEP", Ks = c(DEP =  0.19, ANX = 0.2, SCZ = 0.005, BIP = 0.013), h2ls = c(DEP =  0.4, ANX = 0.4, SCZ = 0.77, BIP = 0.67), rg = rg_SCZ.BIP.DEP.ANX, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "m", "f"))
  ,list(name="DEP/ANX/BIP/SCZ, 8 fam members", index_disorder = "DEP", Ks = c(DEP =  0.19, ANX = 0.2, SCZ = 0.005, BIP = 0.013), h2ls = c(DEP =  0.4, ANX = 0.4, SCZ = 0.77, BIP = 0.67), rg = rg_SCZ.BIP.DEP.ANX, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1", "sib2", "sib3", "sib4", "sib5", "sib6", "m", "f"))
)


outcome_list <- list()
for(i_list in 1:length(sim.input.list)){
  
   list2env(sim.input.list[[i_list]], env = environment())
   ## run simulation 
   outcome <- simulate_FH(index_disorder = index_disorder, Ks = Ks, h2ls = h2ls, rg = rg, sim_reps = sim_reps, sim_size = sim_size, fam_members = fam_members)
   
   ## summarise simulation
   means <- colMeans(outcome)
   ses <- sapply(colnames(outcome), function(x) sd(outcome[,x])/sqrt(nrow(outcome)))
   outcome_meanse <- rbind(means, ses)
   rownames(outcome_meanse) <- c("mean","se")
   
   
   ## outcome: all results from 100 runs dim= c(100,~20) 
   ## outcome_meanse: mean over 100 runs, dim= c(2,~20) 
   outcome_list[[length(outcome_list)+1]]<- list(outcome,outcome_meanse,unlist(sim.input.list[[i_list]]))
}

save(outcome_list, file = here("Results", "outcome_check_denseprop"))

# RT3 Meeting Presentation Plots
ltfh_r2l_results <- as.data.frame(rbind(
  outcome_list[[1]][[2]][,"LTFH_DEP_lm.status.muliab_r2l"]
  , outcome_list[[2]][[2]][,"LTFH_DEP_lm.status.muliab_r2l"]
  , outcome_list[[3]][[2]][,"LTFH_DEP_lm.status.muliab_r2l"]
  , outcome_list[[4]][[2]][,"LTFH_DEP_lm.status.muliab_r2l"]
  , outcome_list[[5]][[2]][,"LTFH_DEP_lm.status.muliab_r2l"]
  , outcome_list[[6]][[2]][,"LTFH_DEP_lm.status.muliab_r2l"]
  , outcome_list[[7]][[2]][,"LTFH_DEP_lm.status.muliab_r2l"]
  , outcome_list[[8]][[2]][,"LTFH_DEP_lm.status.muliab_r2l"]
  , outcome_list[[9]][[2]][,"LTFH_DEP_lm.status.muliab_r2l"]
))

plusminus_r2l_results <- as.data.frame(rbind(
  outcome_list[[1]][[2]][,"FH_DEP_lm.status.plusminus_r2l"]
  , outcome_list[[2]][[2]][,"FH_DEP_lm.status.plusminus_r2l"]
  , outcome_list[[3]][[2]][,"FH_DEP_lm.status.plusminus_r2l"]
  , outcome_list[[4]][[2]][,"FH_DEP_lm.status.plusminus_r2l"]
  , outcome_list[[5]][[2]][,"FH_DEP_lm.status.plusminus_r2l"]
  , outcome_list[[6]][[2]][,"FH_DEP_lm.status.plusminus_r2l"]
  , outcome_list[[7]][[2]][,"FH_DEP_lm.status.plusminus_r2l"]
  , outcome_list[[8]][[2]][,"FH_DEP_lm.status.plusminus_r2l"]
  , outcome_list[[9]][[2]][,"FH_DEP_lm.status.plusminus_r2l"]
))

proportion_r2l_results <- as.data.frame(rbind(
  outcome_list[[1]][[2]][,"FH_DEP_lm.status.proportion_r2l"]
  , outcome_list[[2]][[2]][,"FH_DEP_lm.status.proportion_r2l"]
  , outcome_list[[3]][[2]][,"FH_DEP_lm.status.proportion_r2l"]
  , outcome_list[[4]][[2]][,"FH_DEP_lm.status.proportion_r2l"]
  , outcome_list[[5]][[2]][,"FH_DEP_lm.status.proportion_r2l"]
  , outcome_list[[6]][[2]][,"FH_DEP_lm.status.proportion_r2l"]
  , outcome_list[[7]][[2]][,"FH_DEP_lm.status.proportion_r2l"]
  , outcome_list[[8]][[2]][,"FH_DEP_lm.status.proportion_r2l"]
  , outcome_list[[9]][[2]][,"FH_DEP_lm.status.proportion_r2l"]
))

denseprop_r2l_results <- as.data.frame(rbind(
  outcome_list[[1]][[2]][,"FH_DEP_lm.status.denseprop_r2l"]
  , outcome_list[[2]][[2]][,"FH_DEP_lm.status.denseprop_r2l"]
  , outcome_list[[3]][[2]][,"FH_DEP_lm.status.denseprop_r2l"]
  , outcome_list[[4]][[2]][,"FH_DEP_lm.status.denseprop_r2l"]
  #, outcome_list[[5]][[2]][,"FH_DEP_lm.status.denseprop_r2l"]
  #, outcome_list[[6]][[2]][,"FH_DEP_lm.status.denseprop_r2l"]
  #, outcome_list[[7]][[2]][,"FH_DEP_lm.status.denseprop_r2l"]
  #, outcome_list[[8]][[2]][,"FH_DEP_lm.status.denseprop_r2l"]
  #, outcome_list[[9]][[2]][,"FH_DEP_lm.status.denseprop_r2l"]
))

x <- unlist(map(sim.input.list, "name"))[1:5]
# Plot R2 by family size for depression
r2_plot_famsize <- ggplot() +
  # plusminus
  geom_point(data = plusminus_r2l_results[1:5,], aes(x = x, y = mean, color = "FH+-"), position = position_nudge(-.22), size = 3) +
  geom_errorbar(data = plusminus_r2l_results[1:5,], aes(x = x, ymin = mean - se, ymax = mean + se, color = "FH+-"), width = .2, position = position_nudge(-.22)) +
  # proportion
  geom_point(data = proportion_r2l_results[1:5,], aes(x = x, y = mean, color = "FH Proportion"), position = position_nudge(0), size = 3) +
  geom_errorbar(data = proportion_r2l_results[1:5,], aes(x = x, ymin = mean - se, ymax = mean + se, color = "FH Proportion"), width = .2, position = position_nudge(0)) +
  # ltfh
  geom_point(data = ltfh_r2l_results[1:5,], aes(x = x, y = mean, color = "LT-FH"), position = position_nudge(.22), size = 3) +
  geom_errorbar(data = ltfh_r2l_results[1:5,], aes(x = x, ymin = mean - se, ymax = mean + se, color = "LT-FH"), width = .2, position = position_nudge(.22)) +
  
  scale_color_manual(values = c("FH+-" = "#4B0C6BFF", "FH Proportion" = "#CF4446FF", "LT-FH"= "#FB9A06FF"),
                     labels = c( "FH Proportion", "FH +-", "LT-FH"),
                     guide = guide_legend(
                       title = "Predictor" 
                     )) +
  ylim(0.0, 0.15) +
  ylab("r2l") +
  xlab("Family") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, size = 12, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle("Assessment of Depression Prediction", subtitle = "liability scale r² of lm(true.status ~ predictor)")

r2_plot_famsize
ggsave(here("Results", "r2_DEP_famsizes.pdf"), r2_plot_famsize)


# Plot R2 by disorder number for depression
x <- unlist(map(sim.input.list, "name"))[c(3, 6, 7, 8, 9)]
# Plot R2 by family size for depression
r2_plot_disorders <- ggplot() +
  # plusminus
  geom_point(data = plusminus_r2l_results[c(3, 6, 7, 8, 9),], aes(x = x, y = mean, color = "FH+-"), position = position_nudge(-.22), size = 3) +
  geom_errorbar(data = plusminus_r2l_results[c(3, 6, 7, 8, 9),], aes(x = x, ymin = mean - se, ymax = mean + se, color = "FH+-"), width = .2, position = position_nudge(-.22)) +
  # proportion
  geom_point(data = proportion_r2l_results[c(3, 6, 7, 8, 9),], aes(x = x, y = mean, color = "FH Proportion"), position = position_nudge(0), size = 3) +
  geom_errorbar(data = proportion_r2l_results[c(3, 6, 7, 8, 9),], aes(x = x, ymin = mean - se, ymax = mean + se, color = "FH Proportion"), width = .2, position = position_nudge(0)) +
  # ltfh
  geom_point(data = ltfh_r2l_results[c(3, 6, 7, 8, 9),], aes(x = x, y = mean, color = "LT-FH"), position = position_nudge(.22), size = 3) +
  geom_errorbar(data = ltfh_r2l_results[c(3, 6, 7, 8, 9),], aes(x = x, ymin = mean - se, ymax = mean + se, color = "LT-FH"), width = .2, position = position_nudge(.22)) +
  
  scale_color_manual(values = c("FH+-" = "#4B0C6BFF", "FH Proportion" = "#CF4446FF", "LT-FH"= "#FB9A06FF"),
                     labels = c( "FH Proportion", "FH +-", "LT-FH"),
                     guide = guide_legend(
                       title = "Predictor"
                     )) +
  ylim(0.0, 0.15) +
  ylab("r2l") +
  xlab("Family") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, size = 12, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle("Assessment of Depression Prediction", subtitle = "liability scale r² of lm(true.status ~ predictor)")

r2_plot_disorders
ggsave(here("Results", "r2_DEP_disorders.pdf"), r2_plot_disorders)



# Plot AUC
# all AUC names (from outcome_meanse)
# regular expression AUC$

# get mean and se from outcome_list
# results <- map(outcome_list[[outcome_meanse]], "AUC")


# Plot AUC
ltfh_auc_results <- as.data.frame(rbind(
  outcome_list[[1]][[2]][,"LTFH_DEP_lm.status.prob_AUC"]
  , outcome_list[[2]][[2]][,"LTFH_DEP_lm.status.prob_AUC"]
  , outcome_list[[3]][[2]][,"LTFH_DEP_lm.status.prob_AUC"]
  , outcome_list[[4]][[2]][,"LTFH_DEP_lm.status.prob_AUC"]
  , outcome_list[[5]][[2]][,"LTFH_DEP_lm.status.prob_AUC"]
  ))

plusminus_auc_results <- as.data.frame(rbind(
  outcome_list[[1]][[2]][,"FH_DEP_lm.status.plusminus_AUC"]
  , outcome_list[[2]][[2]][,"FH_DEP_lm.status.plusminus_AUC"]
  , outcome_list[[3]][[2]][,"FH_DEP_lm.status.plusminus_AUC"]
  , outcome_list[[4]][[2]][,"FH_DEP_lm.status.plusminus_AUC"]
  , outcome_list[[5]][[2]][,"FH_DEP_lm.status.plusminus_AUC"]
))

proportion_auc_results <- as.data.frame(rbind(
  outcome_list[[1]][[2]][,"FH_DEP_lm.status.proportion_AUC"]
  , outcome_list[[2]][[2]][,"FH_DEP_lm.status.proportion_AUC"]
  , outcome_list[[3]][[2]][,"FH_DEP_lm.status.proportion_AUC"]
  , outcome_list[[4]][[2]][,"FH_DEP_lm.status.proportion_AUC"]
  , outcome_list[[5]][[2]][,"FH_DEP_lm.status.proportion_AUC"]
))


x <- unlist(map(sim.input.list, "name"))

auc_plot <- ggplot() +
  # plusminus
  geom_point(data = plusminus_auc_results, aes(x = x, y = mean, color = "FH+-"), position = position_nudge(-.22), size = 3) +
  geom_errorbar(data = plusminus_auc_results, aes(x = x, ymin = mean - se, ymax = mean + se, color = "FH+-"), width = .2, position = position_nudge(-.22)) +
  # proportion
  geom_point(data = proportion_auc_results, aes(x = x, y = mean, color = "FH Proportion"), position = position_nudge(0), size = 3) +
  geom_errorbar(data = proportion_auc_results, aes(x = x, ymin = mean - se, ymax = mean + se, color = "FH Proportion"), width = .2, position = position_nudge(0)) +
  # ltfh
  geom_point(data = ltfh_auc_results, aes(x = x, y = mean, color = "LT-FH"), position = position_nudge(.22), size = 3) +
  geom_errorbar(data = ltfh_auc_results, aes(x = x, ymin = mean - se, ymax = mean + se, color = "LT-FH"), width = .2, position = position_nudge(.22)) +
  
  scale_color_manual(values = c("FH+-" = "#4B0C6BFF", "FH Proportion" = "#CF4446FF", "LT-FH"= "#FB9A06FF"),
                     labels = c( "FH Proportion", "FH +-", "LT-FH"),
                     guide = guide_legend(
                       title = "Indicator"
                     )) +
  ylim(0.5, 0.7) +
  ylab("AUC") +
  xlab("Family") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Assessment DEP prediction", subtitle = "AUC of lm(true.status ~ predictor)")

auc_plot
ggsave(here("Results", "AUC_DEP_5fam_3ind.pdf"), auc_plot)


# Plot R2 for DEP
ltfh_r2l_results <- as.data.frame(rbind(
  outcome_list[[1]][[2]][,"LTFH_DEP_lm.status.muliab_r2l.DEP"]
  , outcome_list[[2]][[2]][,"LTFH_DEP_lm.status.muliab_r2l.DEP"]
  , outcome_list[[3]][[2]][,"LTFH_DEP_lm.status.muliab_r2l.DEP"]
  , outcome_list[[4]][[2]][,"LTFH_DEP_lm.status.muliab_r2l.DEP"]
  , outcome_list[[5]][[2]][,"LTFH_DEP_lm.status.muliab_r2l.DEP"]
))

plusminus_r2l_results <- as.data.frame(rbind(
  outcome_list[[1]][[2]][,"FH_DEP_lm.status.plusminus_r2l.DEP"]
  , outcome_list[[2]][[2]][,"FH_DEP_lm.status.plusminus_r2l.DEP"]
  , outcome_list[[3]][[2]][,"FH_DEP_lm.status.plusminus_r2l.DEP"]
  , outcome_list[[4]][[2]][,"FH_DEP_lm.status.plusminus_r2l.DEP"]
  , outcome_list[[5]][[2]][,"FH_DEP_lm.status.plusminus_r2l.DEP"]
))

proportion_r2l_results <- as.data.frame(rbind(
  outcome_list[[1]][[2]][,"FH_DEP_lm.status.proportion_r2l.DEP"]
  , outcome_list[[2]][[2]][,"FH_DEP_lm.status.proportion_r2l.DEP"]
  , outcome_list[[3]][[2]][,"FH_DEP_lm.status.proportion_r2l.DEP"]
  , outcome_list[[4]][[2]][,"FH_DEP_lm.status.proportion_r2l.DEP"]
  , outcome_list[[5]][[2]][,"FH_DEP_lm.status.proportion_r2l.DEP"]
))


x <- unlist(map(sim.input.list, "name"))

r2_plot <- ggplot() +
  # plusminus
  geom_point(data = plusminus_r2l_results, aes(x = x, y = mean, color = "FH+-"), position = position_nudge(-.22), size = 3) +
  geom_errorbar(data = plusminus_r2l_results, aes(x = x, ymin = mean - se, ymax = mean + se, color = "FH+-"), width = .2, position = position_nudge(-.22)) +
  # proportion
  geom_point(data = proportion_r2l_results, aes(x = x, y = mean, color = "FH Proportion"), position = position_nudge(0), size = 3) +
  geom_errorbar(data = proportion_r2l_results, aes(x = x, ymin = mean - se, ymax = mean + se, color = "FH Proportion"), width = .2, position = position_nudge(0)) +
  # ltfh
  geom_point(data = ltfh_r2l_results, aes(x = x, y = mean, color = "LT-FH"), position = position_nudge(.22), size = 3) +
  geom_errorbar(data = ltfh_r2l_results, aes(x = x, ymin = mean - se, ymax = mean + se, color = "LT-FH"), width = .2, position = position_nudge(.22)) +
  
  scale_color_manual(values = c("FH+-" = "#4B0C6BFF", "FH Proportion" = "#CF4446FF", "LT-FH"= "#FB9A06FF"),
                     labels = c( "FH Proportion", "FH +-", "LT-FH"),
                     guide = guide_legend(
                       title = "Indicator"
                     )) +
  ylim(0.0, 0.15) +
  ylab("r2l") +
  xlab("Family") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Assessment DEP prediction", subtitle = "r2l of lm(true.status ~ predictor)")

r2_plot
ggsave(here("Results", "r2_DEP_5fam_3ind.pdf"), r2_plot)


# Plot R2 for SCZ
ltfh_r2l_results <- as.data.frame(rbind(
  outcome_list[[15]][[2]][,"LTFH_SCZ_lm.status.muliab_r2l"]
  , outcome_list[[16]][[2]][,"LTFH_SCZ_lm.status.muliab_r2l"]
  , outcome_list[[17]][[2]][,"LTFH_SCZ_lm.status.muliab_r2l"]
  #, outcome_list[[4]][[2]][,"LTFH_SCZ_lm.status.muliab_r2l"]
  , outcome_list[[18]][[2]][,"LTFH_SCZ_lm.status.muliab_r2l"]
))

plusminus_r2l_results <- as.data.frame(rbind(
  outcome_list[[15]][[2]][,"FH_SCZ_lm.status.plusminus_r2l"]
  , outcome_list[[16]][[2]][,"FH_SCZ_lm.status.plusminus_r2l"]
  , outcome_list[[17]][[2]][,"FH_SCZ_lm.status.plusminus_r2l"]
  #, outcome_list[[4]][[2]][,"FH_SCZ_lm.status.plusminus_r2l"]
  , outcome_list[[18]][[2]][,"FH_SCZ_lm.status.plusminus_r2l"]
))

proportion_r2l_results <- as.data.frame(rbind(
  outcome_list[[15]][[2]][,"FH_SCZ_lm.status.proportion_r2l"]
  , outcome_list[[16]][[2]][,"FH_SCZ_lm.status.proportion_r2l"]
  , outcome_list[[17]][[2]][,"FH_SCZ_lm.status.proportion_r2l"]
  #, outcome_list[[4]][[2]][,"FH_SCZ_lm.status.proportion_r2l"]
  , outcome_list[[18]][[2]][,"FH_SCZ_lm.status.proportion_r2l"]
))


x <- unlist(map(sim.input.list, "name"))[15:18]

r2_plot <- ggplot() +
  # plusminus
  geom_point(data = plusminus_r2l_results, aes(x = x, y = mean, color = "FH+-"), position = position_nudge(-.22), size = 3) +
  geom_errorbar(data = plusminus_r2l_results, aes(x = x, ymin = mean - se, ymax = mean + se, color = "FH+-"), width = .2, position = position_nudge(-.22)) +
  # proportion
  geom_point(data = proportion_r2l_results, aes(x = x, y = mean, color = "FH Proportion"), position = position_nudge(0), size = 3) +
  geom_errorbar(data = proportion_r2l_results, aes(x = x, ymin = mean - se, ymax = mean + se, color = "FH Proportion"), width = .2, position = position_nudge(0)) +
  # ltfh
  geom_point(data = ltfh_r2l_results, aes(x = x, y = mean, color = "LT-FH"), position = position_nudge(.22), size = 3) +
  geom_errorbar(data = ltfh_r2l_results, aes(x = x, ymin = mean - se, ymax = mean + se, color = "LT-FH"), width = .2, position = position_nudge(.22)) +
  
  scale_color_manual(values = c("FH+-" = "#4B0C6BFF", "FH Proportion" = "#CF4446FF", "LT-FH" = "#FB9A06FF"),
                     labels = c( "FH Proportion", "FH +-", "LT-FH"),
                     guide = guide_legend(
                       title = "Indicator"
                     )) +
  ylim(0.0, 0.55) +
  ylab("r2l") +
  xlab("Family") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Assessment SCZ prediction", subtitle = "r2l of lm(true.status ~ predictor)")

r2_plot
ggsave(here("Results", "r2_SCZ_4fam_3ind.pdf"), r2_plot)
