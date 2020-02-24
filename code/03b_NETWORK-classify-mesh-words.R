
## @knitr meshclassification


# and now finally we can aggregate these simple mesh terms into larger CONCEPTS 
# see the classification code for the vectors that define what words go in to specific concepts
meshClassified <- meshSimpleReduced %>% 
  
  mutate(Classification = case_when(
    
    # Diabetes Types
    # Almost too general, will likely be dropped later because MANY articles are tagged with it
      MeSHSimple %in% "Diabetes Mellitus" ~ "Diabetes \n General",  
    
    # Sub-types
      MeSHSimple %in% "Diabetes Mellitus, Type 1" ~ "Type 1 \n Diabetes", 
      MeSHSimple %in% "Diabetes Mellitus, Type 2" ~ "Type 2 \n Diabetes", 
      MeSHSimple %in% "Diabetes, Gestational" ~ "Gestational \n Diabetes", 
      
    # Related
      MeSHSimple %in% "Prediabetic State" ~ "Prediabetes", 
      MeSHSimple %in% "Diabetes Complications" ~ "Diabetes \n Complications", 
      MeSHSimple %in% RESPIRATORY ~ "Respiratory \n Related",
      MeSHSimple %in% KIDNEY ~ "Kidney \n Related",

    
    # Comorbidities
      MeSHSimple %in% CHRONCOMORBID ~ "Chronic Disease \n Comorbidity", 
      MeSHSimple %in% CARDIOV ~ "Cardiovascular", 
      MeSHSimple %in% BP ~ "Blood \n Pressure",
      MeSHSimple %in% BLOODGLUCOSE ~ "BloodGlucose/Insulin",
      MeSHSimple %in% BLOODLIPIDS ~ "Blood \n Lipids", 

    # Other potentially related diseases/issues
      MeSHSimple %in% HIV ~ "HIV", 
      MeSHSimple %in% SLEEP ~ "Sleep", 
      MeSHSimple %in% OSTEO ~ "Osteoarthritis", 
      
    # Risk factors
      MeSHSimple %in% WEIGHT ~ "Weight \n Related",
      MeSHSimple %in% WEIGHTLOSS ~ "Weight \n Loss",
      MeSHSimple %in% DIET ~ "Diet", 
      MeSHSimple %in% MENTALHEALTH ~ "Mental \n Health", 
      MeSHSimple %in% EXERCISE ~ "PA/Exercise",

    
    
    # Populations
      MeSHSimple %in% VETERANS ~ "Veterans", 
      MeSHSimple %in% FEMALE ~ "Women", 
      MeSHSimple %in% MALE ~ "Men",       
      MeSHSimple %in% YOUNG ~ "Children \n AdolescentYA", 
      MeSHSimple %in% AGED ~ "Older \n Adults", 

      MeSHSimple %in% CARE ~ "Caregivers", 
      MeSHSimple %in% DISABLE ~ "Disabled \n Persons",
    
      MeSHSimple %in% AA ~ "African \n Americans", 
      MeSHSimple %in% HISPANIC ~ "Hispanic \n Americans", 
      MeSHSimple %in% AIAN ~ "American \n Natives", 
      MeSHSimple %in% ASIAN ~ "Asian \n Americans", 
      MeSHSimple %in% MINORITY ~ "Minority \n Population", 
    
      MeSHSimple %in% URBAN ~ "Urban",
      MeSHSimple %in% RURAL ~ "Rural \n Health", 
    
    
    MeSHSimple %in% HSR ~ "HSR", 
    MeSHSimple %in% ACCESS ~ "Access", 
    MeSHSimple %in% DELIVERY ~ "Delivery",
    MeSHSimple %in% QUALITY ~ "Quality",
    MeSHSimple %in% COST ~ "Cost",
    MeSHSimple %in% PATIENT ~ "Patient \n Centered", 
    MeSHSimple %in% DISPARITIES ~ "Disparities", 
    MeSHSimple %in% GEOGRAPHY ~ "Geography",
    MeSHSimple %in% DECISION ~ "Decision \n Support",
    MeSHSimple %in% "Telemedicine" ~ "Telemedicine",
    MeSHSimple %in% CONTINUITY ~ "Continuity \n of Care", 
    MeSHSimple %in% MEDADHERE ~ "Medication \n Adherence",
    
    
    MeSHSimple %in% INDIVIDUAL ~ "Individual \n Focus",
    MeSHSimple %in% INTERPERSONAL ~ "Interpersonal \n Focus",
    MeSHSimple %in% COMMUNITY ~ "Community \n Focus", 
    MeSHSimple %in% POLICY ~ "PolicyFocus",
    
    
    
    MeSHSimple %in% HEALTHCOMMPROM ~ "HealthComm \n Promotion", 
    MeSHSimple %in% PCAREPREVENTION ~ "PrimaryCare \n Prevention", 
    MeSHSimple %in% INSURANCE ~ "Insurance", 
    MeSHSimple %in% EHR ~ "Health Records \n Including Electronic", 
    MeSHSimple %in% ELECTRONIC ~ "eHealth \n mHealth", 
    MeSHSimple %in% OUTCOMES ~ "Outcomes \n Related", 
    MeSHSimple %in% HOSPITALS ~ "Hospital \n Inpatient", 
    MeSHSimple %in% SES ~ "SES related", 
    MeSHSimple %in% GIS ~ "Geography/GIS", 
    MeSHSimple %in% PRACTICIONERS ~ "Practicioners", 
    MeSHSimple %in% PREGNANCY ~ "Pregnancy \n Related", 
    MeSHSimple %in% AMBULATORY ~ "Ambulatory \n Outpatient", 
    MeSHSimple %in% MUA ~ "Medically \n Underserved Area", 
    MeSHSimple %in% EBM ~ "EBM or \n Guidelines", 
    MeSHSimple %in% PROGRAMS ~ "Program \n Development", 
    MeSHSimple %in% QI ~ "Quality \n Improvement",
    
    # Methods related
      MeSHSimple %in% RCT ~ "RCT", 
      MeSHSimple %in% OBSERVATIONAL ~ "Observational", 
      MeSHSimple %in% ECONOMICSMETRICS ~ "Economics \n Econometrics",
      MeSHSimple %in% MACHINELEARNINGRELATED ~ "MachineLearning \n Related",
      MeSHSimple %in% QUALITATIVE ~ "Qualitative", 
      MeSHSimple %in% SURVEYPSYCHO ~ "Survey Design \n Psychometrics", 
      MeSHSimple %in% ANALYSIS ~ "Analysis Methods",

    # Funders
      MeSHSimple %in% NIDDK ~ "NIDDK", 
      MeSHSimple %in% AHRQ ~ "AHRQ", 
      MeSHSimple %in% CDC ~ "CDC", 
      MeSHSimple %in% CMS ~ "CMS", 
      MeSHSimple %in% WHO ~ "WHO", 
    
    
    TRUE ~ "zUnclassified")) %>%
  
  select(MeSHSimple, Classification) %>%
  
  arrange(Classification)

# not necessary anymore, just output this to show the team what words were unclassified
#write.csv(filter(meshClassified, Classification=="zUnclassified"), file="output/meshUnclassified.csv", row.names = FALSE)  

# get rid of all the mesh classifications
gdata::keep(hsrRecent, meshCleaned, meshSimpleReduced, meshClassified, sure = TRUE)
