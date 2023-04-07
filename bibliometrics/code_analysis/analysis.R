

analysis_refs<-complete_data %>%  
  filter(SO!="rbt") %>% 
  filter(SO!="trop_ecol") %>% 
  filter(PY>1968) %>% 
  filter(PY<2021) 
unique(merged_refs$SO)


