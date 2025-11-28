# Replication Code for Conjoint Analysis "Agrivoltaics can reduce political polarization and local opposition to solar energy" #####################################################
# Authors: Lukas Fesenfeld, Leon Sistek, Simon Montfort, Dionis Anderegg, Jürg Rohrer, Tobias Schmidt 
# Date: 30.06.2025


# R.version
# platform       aarch64-apple-darwin20      
# arch           aarch64                     
# os             darwin20                    
# system         aarch64, darwin20           
# status                                     
# major          4                           
# minor          3.2                         
# year           2023                        
# month          10                          
# day            31                          
# svn rev        85441                       
# language       R                           
# version.string R version 4.3.2 (2023-10-31)
# nickname       Eye Holes  

setwd("/Users/simon/Documents/repo/agri-pv")

##########################################################
# descriptive statistics
##########################################################

dat <- readRDS("data-survey/swiss_immigration_survey_data.rds")

dat_desc <- dat %>% 
  dplyr::select(choice, age, gender, educ, income, employment, UserLanguage,
                immi_pc_quart, attrib1_lab, attrib2_lab, attrib3_lab, attrib4_lab, attrib5_lab, attrib6_lab,
                mech_lang_bin, mech_cult_bin, mech_proud_bin, mech_devel_bin,
                left_right_bin) %>% 
  mutate_all(as.character) 


p_desc <- dat_desc %>% 
  pivot_longer(., everything(), names_to = "Question", values_to = "Response") %>% 
  mutate(Question = factor(Question, levels = c("choice", "attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab",
                                                "age", "gender", "educ", "income", "employment", "UserLanguage",
                                                "immi_pc_quart",
                                                "mech_lang_bin", "mech_cult_bin", "mech_proud_bin", "mech_devel_bin",
                                                "left_right_bin"
                                                # "urban_rural"
  ))) %>% 
  group_by(Question, Response) %>% 
  count() %>% 
  # mutate(Response = factor(Response, levels = as.character(c(0:14, NA)))) %>% 
  ggplot(aes(x = Response, y = n)) +
  geom_col() + labs(x = "", y = "") +
  coord_flip() +
  facet_wrap(~Question, scales = "free_y", ncol = 3,
             labeller = labeller(Question = c("choice" = "Choice",
                                              "attrib1_lab" = "Recipient developing country", 
                                              "attrib2_lab" = "Number of climate\nmigrants to accept\nfrom this country per year", 
                                              "attrib3_lab" = "Climate aid to give\nto this country\n(CHF) per year", 
                                              "attrib4_lab" = "Value of Swiss\ntrade with this\ncountry", 
                                              "attrib5_lab" = "Extreme weather event", 
                                              "attrib6_lab" = "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council",
                                              "age" = "Age", 
                                              "gender" = "Gender",
                                              "educ" = "Education", 
                                              "income" = "Income", 
                                              "employment" = "Employment",
                                              "UserLanguage" = "Language",
                                              "immi_pc_quart" = "Immigration pc quartiles",
                                              "mech_lang_bin" = "Language Mechanism", 
                                              "mech_cult_bin" = "Culture Mechanism", 
                                              "mech_proud_bin" = "Proudness Mechanism", 
                                              "mech_devel_bin" = "Development Mechanism",
                                              "left_right_bin" = "Left-right" 
             ))
  ) +
  theme_light()
p_desc
ggsave(p_desc, filename = "plots/p_desc.pdf", height = 14, width = 10)