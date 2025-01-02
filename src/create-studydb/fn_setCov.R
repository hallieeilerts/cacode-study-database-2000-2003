fn_setCov <- function(AGESEXSUFFIX){
  
  if(AGESEXSUFFIX == "05to09y") {
    vxf <- c("corruption",
             "edu_mean_mf",
             "gini",
             "height_mf",
             "lbw",
             "pfpr",
             "pollution_air",
             "thinness_mf",
             "unemployment_neet_mf",
             "urban",
             "vac_dtp3",
             "mr05to19",
             "wealth_lowest")
    
  }
  
  if (AGESEXSUFFIX == "10to14y") {
    vxf <- c("alcohol_mf",
             "corruption",
             "depression_mf",
             "gini",
             "ors_mf",
             "pfpr",
             "pollution_air",
             "mr05to19",
             "sex_age15_mf",
             "thinness_mf",
             "urban",
             "vac_dtp3",
             "wealth_lowest")
    
  }
  
  if (AGESEXSUFFIX == "15to19yF") {
    vxf <- c("alcohol_mf",
             "birth_healthfacility3",
             "childbearing",
             "contraception_met",
             "depression_f",
             "gini",
             "literacy_f",
             "mr05to19", 
             "sex_age15_f",
             "thinness_f",
             "urban",
             "wealth_lowest")
    
  }  
  
  if (AGESEXSUFFIX == "15to19yM") {
    vxf <- c("alcohol_mf",
             "corruption",
             "depression_m",
             "gini",
             "labor_participation_m",
             "literacy_m",
             "marriage_m",
             "mr05to19",
             "unemployment_neet_m",
             "urban",
             "wealth_lowest" )
    
  }
  return(vxf)
}
