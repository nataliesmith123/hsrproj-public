
## @knitr funderfx


FUNDERS = function(INPUT_DS){
  
  OUTPUT <- 
  INPUT_DS %>%
  
  mutate(funder = str_extract_all(SAList,  "(?<=\\().+?(?=\\))"), 
         numFunders = lengths(funder), 
         NIH = map_int(map(funder, str_detect, pattern = "NIH"), any), 
         AHRQ = map_int(map(funder, str_detect, pattern = "AHRQ"), any), 
         RWJF = map_int(map(funder, str_detect, pattern = "RWJF"), any), 
         CDC = map_int(map(funder, str_detect, pattern = "CDC"), any), 
         PCORI = map_int(map(funder, str_detect, pattern = "PCORI"), any), 
         VA = map_int(map(funder, str_detect, pattern = "VA"), any),
         HRSA = map_int(map(funder, str_detect, pattern = "HRSA"), any),
         NIDILRR = map_int(map(funder, str_detect, pattern = "NIDILRR"), any),
         RRF = map_int(map(funder, str_detect, pattern = "RRF"), any),
         Commonwealth = map_int(str_detect(SAList, pattern="Commonwealth Fund"), any),
         PharmAlliance = map_int(str_detect(SAList, pattern="PharmAlliance"), any),
         Pfizer = map_int(str_detect(SAList, pattern="Pfizer"), any),
         Gordon = map_int(str_detect(SAList, pattern="Gordon and Betty Moore Foundation"), any),
         Aetna = map_int(str_detect(SAList, pattern="Aetna"), any),
         ADA = map_int(str_detect(SAList, pattern="American Diabetes Association"), any),
         CMS = map_int(str_detect(SAList, pattern="Centers for Medicare and Medicaid Services"), any),
         USDA = map_int(map(funder, str_detect, pattern="USDA"), any),
         DHHS = map_int(map(funder, str_detect, pattern="DHHS"), any),
         NSF = map_int(map(funder, str_detect, pattern="NSF"), any),
         
         
         # just leaving these as others
         # NCDHHS = map_int(str_detect(SAList, pattern="North Carolina Department"), any),
         # HSPHARM = map_int(str_detect(SAList, pattern="American Society of Health-System Pharmacists"), any),
         # SAList = InHealth
         # SAList=Florida Agency for Health Care
         # SAList=Massachusetts Division of Health Care Finance
         # Cincinnati Children's Hospital
         # Academic Medicine and Managed Care Forum
         # Parke Davis
         # WTGrant
         # PA Department of Health
         # BCBS
         # Office of Minority Health
         # New Mexico Dept of Health
         # United Hospital Fund
         # AARP
         # US Army
         # California Health Care Foundation
         # Center for Health Care Strategies
         # GlaxoSmithKline
         # Indian Health Service
         # National Multiple Sclerosis Society
         # Pew Charitable Trusts
         # Kellogg Foundation 
         
         primaryFunder = case_when(
           NIH==1 ~ "NIH",
           AHRQ==1 ~ "AHRQ",
           RWJF==1 ~ "RWJF",
           CDC==1 ~ "CDC",
           PCORI==1 ~ "PCORI",
           VA==1 ~ "VA", 
           HRSA==1 ~ "HRSA", 
           NIDILRR==1 ~ "NIDILRR", 
           RRF==1 ~ "RRF", 
           Commonwealth==1 ~ "CFUND",
           PharmAlliance==1 ~ "PHARM",
           Pfizer==1 ~ "PFIZER", 
           Gordon==1 ~ "GBMOORE", 
           Aetna==1 ~ "AETNA",
           ADA==1 ~ "ADA", 
           CMS==1 ~ "CMS",
           USDA==1 ~ "USDA",
           DHHS==1 ~ "DHHS",
           NSF==1 ~ "NSF",
           TRUE ~ "OTHER"
         )
         ) %>%
    select(-funder)
  
  return(OUTPUT)
  
  }




    
