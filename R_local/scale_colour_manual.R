mtcars %>% 
  mutate(class = recode_factor(cyl, 
                               "8" = "alto", 
                               "6" = "medio", 
                               "4" = "bajo")) %>% 
ggplot() +
  geom_point(mapping = aes(mpg, disp, colour = class)) +
  scale_colour_manual(values = 
                        c("alto" =  "red", 
                          "medio" = "yellow", 
                          "bajo" =  "blue"    ))
  

mtcars %>% 
  mutate(class = ifelse(cyl == 8, "alto", 
                        ifelse(cyl == 6, "medio", 
                               ifelse(cyl == 4, "bajo", ""))))

datos::millas

