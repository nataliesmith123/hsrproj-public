
## @knitr SEMcleaning

SEMOrig <- read.csv("data/hsrRecentIDs-SEMclassified-02-10-20.csv")

SEM <- SEMOrig %>%
  
  # get rid of the three that should not have been included
  # want to do programatically rather than deleting from excel file 
  filter(ProjectID %in% hsrRecent$ProjectID) %>%
  
  
  # keep records that are NOT not applicable
  filter(is.na(Not.Applicable)) %>%
  
  
  # combine more granular levels of the SEM classification
  # during coding, it was very difficult to distinguish and for both display purposes and data confidence we are combining them
  mutate(people = case_when(Individual==1 | Interpersonal==1 ~ "1people", TRUE ~ NA_character_),  
         places = case_when(Organizational==1 | Community==1 ~ "2places", TRUE ~ NA_character_), 
         policy = case_when(Policy==1 ~ "3policy", TRUE ~ NA_character_)) %>%
  
  # remove variables that are not needed for analysis
  select(-Description, -Not.Applicable, -Flag)


# convert from 'wide' dataset to 'long' dataset because some projects targeted more than 1 of the 3 levels
SEMlong <- SEM %>%
  
  # converting from wide to long
  gather(From, Level, people, places, policy) %>%
  
  # only want the projectID and the 'level', which contains either people/places/policy
  select(ProjectID, Level) %>%
  
  # keep only those rows where the people/places/policy variable exists
  # some are NA because of the wide-long conversion
  filter(!is.na(Level)) %>%
  
  # create a factor variable that will be the facet for future dotplot
  # add information in the label that will show up in the facet description in the plot - see
  # SEMavgs dataset printed later for these numbers
  mutate(Level = factor(Level, levels=c("1people", 
                                        "2places", 
                                        "3policy"),
                        
                        labels = c("1people" = "Individual & Interpersonal \n avg= $353,231   n=147", 
                                   "2places" = "Organizational & Community \n avg= $315,272   n=55",
                                   "3policy" = "Policy \n avg= $286,938   n=13"))) %>%
  
  # make the level variable name more informative
  rename(SEM = Level) 

# identify how many of the 3 levels a project targeted
# this will be used to split the funding up later, so that we are not double counting any dollars
tmp <- SEMlong %>% 
  
  # for each projectID
  group_by(ProjectID) %>% 
  
  # identify how many rows there are
  summarise(num = n()) 

# add that number variable to the SEMlong dataset
SEMlong <- left_join(SEMlong, tmp, by="ProjectID")


# now merge on funding from the hsrRecent dataset
SEMlong <- left_join(SEMlong, 
                     hsrRecent %>% select(ProjectID, avgYearlyFunding), 
                     by="ProjectID")
  


# now create the final dataset for graphs
SEMgraph <- SEMlong %>%

  # this project is an very big outlier - the GRADE study
  # budget for 2012 is listed as 23,000,000 (23 million)
  filter(ProjectID != 20153570)  %>%
  
  # in NS manual classification -1 denotes missing funding after searching NIH reporter, 
  # so make sure those are not included
  filter(avgYearlyFunding > 0) %>%
  
  # divide avgYearlyFunding equally among levels targeted
  # this probably underestimates the difference between each of the 3 levels, because
  # individual/interpersonal aims (probably) require more funding that comm/org or policy
  mutate(avgYearlyFunding = avgYearlyFunding/num)



SEMavgs = SEMgraph %>%
  
  # for each level
  group_by(SEM) %>%
  
  # calculate the mean (primary), median (sensitivity), and total n
  summarise(mean = mean(avgYearlyFunding), 
            total = n(), 
            median=median(avgYearlyFunding))

print(SEMavgs)

SEMmissing = SEMlong %>%
  
  # now, we just want to examine how many we are missing funding info for - for caption
  filter(avgYearlyFunding==-1) %>%
  
  # for each level
  group_by(SEM) %>%
  
  # count number of missing
  summarise(numMissing = n())

print(SEMmissing)


rm(SEMOrig, SEM, tmp, SEMlong, SEMavgs, SEMmissing)
