
## @knitr researchfx

TYPES = function(INPUT_DS){
  
  OUTPUT <- 
    INPUT_DS %>%
    mutate(grantFirst = str_sub(GrantID, 1, 1), 
           grantType = str_sub(GrantID, 1, 3), 
           isResearch = if_else(# These are the combinations of funders and grant numbers that qualify as research
                              # R and K level grants from federal agencies
                               primaryFunder %in% c("NIH", "AHRQ", "HRSA") & grantFirst %in% c("R", "K") | 
                              
                              # U level grants from the CDC
                               primaryFunder=="CDC" & grantFirst %in% c("U") |
                                
                              # U level grants from the NIH
                                # I DON'T UNDERSTAND U LEVEL GRANTS? 
                                primaryFunder=="NIH" & grantType %in% c("U01", "UH3", "U54", "UC4", 
                                                                        "F30", "F31", "F32") | 
                                
                              # Comparative-effectiveness grants from PCORI
                                primaryFunder=="PCORI" & grantType %in% c("CE-", "CER", "IH-", 
                                                                          "IHS", "ME-", "AD-") | 
                              
                              # VA intramural research grants
                                primaryFunder=="VA" & grantType %in% c("I01", "IIR", "IK2", 
                                                                       "PPO", "RRP", "SDP", "SDR", "VCA") | 
                                
                              # Some other AHRQ grants
                                primaryFunder=="AHRQ" & grantType %in% c("IM1", "CRD") | 
                                
                              # Adding from manual review
                                # all of the titles/abstracts look like some sort of research to me 
                                primaryFunder %in% c("AETNA", "GBMOORE", "NIDILRR", "PFIZER", "RRF")  
                                
                              # Things that are not research: 
                                # commonwealth fund Israeli grant thing
                                # the HRSA center grants
                                # PCORI center grants - CDRN - clinical data research networks. Seems to fund infrastructure
                                # NIH UL1
                                # PHARM grant -- alliance of pharmacy schools
                                # RWJF grants all look like training
                              , 
                              
                              
                              # if a grant meets any of those conditions is a research grant
                               true = 1, 
                              
                              # and if not, then it is not research 
                               false = 0),
           
           
           ## fixing mis-classifications as a result of programmatically assigning research status.
           # these were found during the manual review of the abstracts for SEM classification
           # 20154193 is an R13 project funded by AHRQ for a conference
           # 20162180 is an R13 project funded by NIH/NHLBI for SBM annual meeting
           # 20174271 is a KL2 center project 
           isResearch = if_else(ProjectID %in% c("20154193", "20162180", "20174271"), 
                                true=0, 
                                false=isResearch))
  
  

  return(OUTPUT)
  
}


#write.csv(tmp1, file="output/isGrantResearch.csv", row.names = FALSE)

