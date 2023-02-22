
#####################################

#JAAR PER JAAR

recalc <- FALSE
if (recalc) {
  config <- expand.grid(sd_resi = 0.07, sd_plot = 0.07, sd_tree = 0.09,  
                        sims = 200,
                        plots = c(10, 25,50,75),
                        trees = c(5, 10, 20),
                        effect = 0.0025 * c(1,2,3,4),
                        intercept = 0.25) %>% 
    mutate(row = 1:n())
  
  for (i in 36:nrow(config)) {
    print(paste("scenario:", i,  "/", nrow(config)))
    S  = config$sd_resi[i]
    SP = config$sd_plot[i]
    ST = config$sd_tree[i]
    ITC = config$intercept[i]
    FIX = config$effect[i]
    ROW = config$row[i]
    
    V_RE = list(p = SP**2, g = ST**2)
    PLT = config$plots[i]
    TREE = config$trees[i]
    nsim = config$sims[i]
    filedesc = paste0("JPJ_",sprintf("%04d", ROW), 
                      "_EFF_",FIX, "_NP_",PLT, "_NT_", TREE,
                      "_SP_",SP,
                      "_ST_",ST,"_SE_",S,"_SIMS_",nsim, '.RDS')
    
    X = expand.grid(x = 0:1, 
                    p = 1:PLT, 
                    g = 1:TREE)
    model <- makeLmer(y ~ x + (1 | p / g) , 
                      fixef = c(ITC, FIX),
                      VarCorr = V_RE, 
                      sigma = S, 
                      data = X)
    power <- powerSim(model, nsim = nsim)
    saveRDS(power, file = file.path("output", filedesc))
  }
  
  jpjlist <- list.files(path = "output", pattern = "JPJ*")
  jpjlist <- sort(jpjlist[grep("_200.RDS", jpjlist)])
  k <- 0
  jpj_sims <- NULL
  for (fn in jpjlist) {
    k <- k + 1
    ROW = substring(fn, 5,8)
    jpj_sims[[k]] <- readRDS(file.path("output", fn))  
  }
  
  pwrs <- bind_rows(lapply(jpj_sims, simr_power_intervals))
  pwrs <- bind_cols(config, pwrs)
  saveRDS(pwrs, file.path("output", "simresult_jpj_200.RDS"))
}




############################################

### NO TREE EFFECT (sd(trees)/sqrt(n_trees))
