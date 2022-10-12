
# Model performance analysis

# read model outputs
mdl_coef = read_csv("outputs/models/model_coefs.csv")
mdl_perf = read_csv("outputs/models/model_performance.csv")

# best models performance 
mdl_perf_best <- mdl_perf %>%
  separate(col = id, 
           into = c("group_number", "region_sst", "region_mslp", "region_rhum", "name", "x", "y"), 
           sep = "_", 
           convert = TRUE) %>%
  group_by(x, y) %>%  
  filter(p.value < 0.05, R2_test >= 0) %>%   
  top_n(-1, AIC) %>%
    mutate(which = paste(region_sst, model, sep = ": ") %>% 
           as.factor(),
         pixel_model = as.numeric(which)
  ) 

# check out the best region-models
mdl_perf_best %>% 
  group_by(which) %>%
  summarise(mean = mean(NSE), 
            N = n())


# tables ------------------------------------------------------------------

# forecast point-pixel 
tbl_mdl <- mdl_perf_best %>% 
  dplyr::filter(name != 'other') %>% ungroup() %>% 
  dplyr::select(name, region_sst, model, adj.r.squared, NSE, AIC, BIC) %>% 
  mutate(name = name, region_sst, model, 
         adj.r.squared = sprintf("%.3f", adj.r.squared),
         NSE = sprintf("%.3f", NSE),
         AIC = round(AIC,0), BIC = round(BIC,0)
  ) %>% unique()


# plots  ------------------------------------------------------------------
p <- 
  mdl_perf_best %>% 
  filter(model == "poly") %>% 
  ggplot(aes(x = x, y = y, fill = NSE)) +
  geom_raster() + 
    scale_fill_viridis_c() +
    theme_bw() +
    theme(panel.grid = element_blank())
    ggsave(filename = "plot_perf.png", 
           path = "outputs/plots/", units = "cm", width = 10, height = "10", scale = 1.5, )

# -------------------------------------------------------------------------


# plots -------------------------------------------------------------------


# tables
    
    


