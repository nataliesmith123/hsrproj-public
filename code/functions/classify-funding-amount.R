
## @knitr fundingamts


FUNDINGAMT = function(INPUT_DS){
  

  OUTPUT_DS <- INPUT_DS %>%
    #select(ProjectID, TotalFundingAmount, Fundings, primaryFunder, lengthProj) %>%
    
    mutate(Fundings = if_else(!is.na(TotalFundingAmount), NA_character_, Fundings)) %>%
    
    # mean if the total funding is given directly
    mutate(yearlyFunding1 = as.numeric(TotalFundingAmount) / lengthProj,
           
           
           # for those who don't have a total funding amount listed, we need to extract the yearly amounts
           # remove numbers we don't care about - the years
           # and commas, which will make numbers>999 look like multiple numbers to R
           # then take the mean to get average yearly funding 
           yearlyFunding2 = str_extract_all(str_remove_all(Fundings, 
                                                           paste(c("\\,", "2012", "2013", 
                                                                   "2014", "2015", "2016", 
                                                                   "2017", "2018", "2019"), 
                                                                 collapse = "|")), 
                                            "\\d+"), 
           avgFunding = map_dbl(yearlyFunding2, function(x) mean(as.numeric(x))), 
           
           avgYearlyFunding = case_when(
             !is.na(yearlyFunding1) ~ yearlyFunding1, 
             !is.na(avgFunding) ~ avgFunding,
             ProjectID==20182042 ~ -1, 
             ProjectID==20141381 ~ 749473, 
             TRUE ~ as.numeric(NA)
           )
           ) %>%
    select(-yearlyFunding1, -yearlyFunding2, -avgFunding)

    return(OUTPUT_DS)

}
