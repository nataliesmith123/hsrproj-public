
## @knitr dataimport
hsrProj <- readxl::read_excel("data/DiabetesMellitusMeSHOnly-v1-01-27-20.xlsx")

hsrCleaned <- hsrProj %>%
  
  # data is imported with an extra row of space, get rid of that
  filter(!is.na(ProjectID)) %>%
  
  # these would all be great to have (# subjects, data source), but are all missing in the data download file
  # others aren't missing but just aren't needed. 
  select(-DataSource, -NumberOfSubjects, -PopulationBase, -StudyDesign, 
         -StudyPopulation, -DataBank, -ArchivedYesNo, -GeneralNotes, 
         -DateRevised, -DatePublished, -POList) %>%
  
  # make a project length variable
  # second recode is because some grants begin and end in the same year, so they show up as 0, 
  # but considering these two be one year grants
  mutate(lengthProj = as.numeric(FinalYear) - as.numeric(InitialYear), 
         lengthProj = if_else(lengthProj==0, 1, lengthProj))


## @knitr funders
hsrCleaned <- FUNDERS(hsrCleaned)


## @knitr research

# this function is dependent on who the primary funder is, so must be run after defining primary funders 
hsrCleaned <- TYPES(hsrCleaned)


## @knitr avgfunding
hsrCleaned <- FUNDINGAMT(hsrCleaned)



## @knitr hsrrecent

hsrCleaned <- hsrCleaned %>% 
  
  # only within those grants that are research
  filter(isResearch==1) %>%
  
  # sort so that most recent is first
  # desc means descending = largest to smallest 
  arrange(desc(InitialYear), desc(InitialMonth)) %>%
  
  # create running variable (rownames are reset after using arrange)
  rownames_to_column(var="number") %>%
  
  # convert variables to numeric so they are easier/useful to work with
  mutate(number = as.numeric(number), 
         ProjectID = as.numeric(ProjectID)) 


# and finally, keep numbers 1-250
hsrRecent <- filter(hsrCleaned, number<=250)  


# this piece is needed because some funding numbers weren't reported, but could have been
# when making Figure 1 - the SEM/funding plot, NS output project ID's that had an SEM classification
# but were missing funding information. then searched NIH reporter for those ID's to try and find numbers
# this resulted in some additional data points 
# NOTE: VA funding does not appear to be publicly available so Figure 1 does not include those numbers. 
suppFunding <- readxl::read_excel("data/manual-funding-numbers.xlsx") %>%
  
  # just project ID and the averages NS calculated
  select(ProjectID, avgYearlyFunding) %>%
  
  # rename for some clearer merging/organization
  rename(fundingExtra = avgYearlyFunding)

# merge this supplementary funding information on the hsrRecent dataset
hsrRecent <- hsrRecent %>% left_join(suppFunding, by="ProjectID") 

# combine those two funding pieces
hsrRecent <- hsrRecent %>%
  
  # if the avgYearlyFunding is missing (returned from FUNDINGAMT() function above)
  # then supplement with the fundingExtra variable, otherwise keep it as the original average
  mutate(avgYearlyFunding = if_else(is.na(avgYearlyFunding), 
                                    fundingExtra, 
                                    avgYearlyFunding))




# not needed anymore, can uncomment if wanted. 
  # write.csv(hsrRecent, file="output/hsrRecent.csv", row.names = FALSE)
  # write.csv(hsrRecent %>% select(ProjectID, ProjectTitle, Abstract), file="output/hsrRecentForSEM.csv", row.names = FALSE)
  # write.csv(hsrRecent %>% select(ProjectID), file="output/hsrRecentIDs.csv", row.names = FALSE)

# remove unneeded datasets for a cleaner workspace
rm(hsrProj, # original data
   hsrCleaned, # full data
   suppFunding # supplementary funding info
   )
