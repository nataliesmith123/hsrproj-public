
## @knitr meshtidy

mesh <- hsrRecent %>% 
  
  # focusing on the mesh headings for each project
  select(ProjectID, MeshHeading) %>%
  
  # remove trailing characters like "</li> <li>"
  mutate(meshTrimmed = str_sub(MeshHeading, start=10, end=-11), 
         listV2 = str_split(MeshHeading, pattern="</li> <li>"), 
         numMeSH = map_int(listV2, length))

# distribution of mesh terms in each research project
table(mesh$numMeSH)

mesh <- mesh %>%
  
  # separate this trimmed mesh term into new columns named A-Z
  # 26 is the max number of mesh terms, so alphabet works nicely 
  separate(meshTrimmed, into=LETTERS[seq(from=1, to=26)], 
           
           # this separator was determined by looking at the data
           sep="</li> <li>") %>%
  
  # remove the mesh heading variable where all mesh terms are stored now that they are split
  select(-MeshHeading)

meshLong <- mesh %>%
  
  # convert from wide dataset to long dataset
  gather(Letter, MeSH, A:Z) %>%
  
  # remove this, we don't actually care about the numbers
  select(-Letter) %>%
  
  # drop the rows that are NA (induced by the separate function used above)
  filter(!is.na(MeSH)) %>%
  
  # these next couple steps work to clean up the mesh terms with qualifiers
  # takes words like "Diabetes Mellitus /* prevention & control </li> /*management"
  # and makes two words - Diabetes Mellitus: prevention and control
  # and Diabetes Mellitus: management
  mutate(flag1 = str_detect(MeSH, pattern="\\</li>"), 
         flag2 = str_detect(MeSH, pattern="\\/"),
         count = str_count(MeSH, pattern="\\</li>")) %>% 
  
  # separate those complex individual mesh words
  # I know to use four because there are max three qualifiers, plus the first word
  separate(MeSH, into = c("first", "second", "third", "fourth"), sep="\\</li>") %>%
  separate(first, into=c("before", "after"), sep="/") %>%
  
  # trim the asterisks and slashes
  # paste function here just used to look for a couple different patterns at a time
  mutate(before=str_trim(str_remove(before, pattern="\\*")), 
         after=str_trim(str_remove(after, pattern="\\*")), 
         second=str_trim(str_remove(second, pattern=paste(c("\\/\\*", "\\/"), collapse = "|"))), 
         third=str_trim(str_remove(third, pattern=paste(c("\\/\\*", "\\/"), collapse = "|"))), 
         fourth=str_trim(str_remove(fourth, pattern=paste(c("\\/\\*", "\\/"), collapse = "|"))))



# now if a given mesh word (remember, this dataset is in long form) does not have qualifiers, it's good as is
meshNoQualifiers <- meshLong %>% 
  
  # BOTH flags must be false 
  filter(flag1==FALSE & flag2==FALSE) %>%
  
  # rename the word to be called MeSH
  rename(MeSH=before) %>%
  
  # keep just needed variables
  select(ProjectID, MeSH)



# now for the mesh terms that do have qualifiers
meshQualifiers <- meshLong %>%
  
  # either flag needs to be true
  filter(flag1==TRUE | flag2==TRUE) %>%
  
  # re-append the mesh qualifiers to the original word (called 'before')
  # this creates the multiple terms we need based on qualifiers
  # (This is unnecessary since we work with the simplified mesh terms anyways...)
  mutate(mesh1 = str_c(before, after, sep=": "), 
         mesh2 = str_c(before, second, sep=": "), 
         mesh3 = str_c(before, third, sep=": "), 
         mesh4 = str_c(before, fourth, sep=": ")) %>%
  
  # keep just the new mesh terms
  select(ProjectID, mesh1, mesh2, mesh3, mesh4) %>%
  
  # convert to long data format again (long x2!)
  gather(v1, v2, mesh1:mesh4) %>%
  
  # remove unnecessary vars and rename to be MeSH
  select(-v1) %>%
  rename(MeSH=v2)


# append the qualifier and no qualifier datasets into one large dataset
# this dataset has just projectID and cleaned up mesh terms
meshCleaned <- rbind(meshNoQualifiers, meshQualifiers)

# now simplify those mesh terms
meshCleaned <- meshCleaned %>%
  
  # remove everything about the ":"
  mutate(MeSHSimple = gsub(": .*","", MeSH)) %>%
  
  # get rid of NA's induced by the transposing
  filter(!is.na(MeSH))


# This is a key dataset - subsets to only MeSH terms that occur more than 2 times 
# this gets entered into the classification code 
meshSimpleReduced <- meshCleaned %>%
  
  # for each simplified mesh term from above
  group_by(MeSHSimple) %>% 
  
  # count how many times it occurred
  summarise(meSHoccurrences=n()) %>%
  
  # keep only those that are not missing (redundant)
  filter(!is.na(MeSHSimple)) %>%
  
  # get rid of the Humans mesh term - all are tagged with it
  filter(MeSHSimple!="Humans") %>%
  
  # keep only simplified mesh terms that occur 3 or more times
  filter(meSHoccurrences>2)


rm(mesh, meshLong, meshNoQualifiers, meshQualifiers)

