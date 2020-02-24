

## @knitr meshnetwork

# merge the classifications onto the full dataset of cleaned mesh terms
# for each individual mesh term, merge based on the simple version
# that is linked to the concept classification
meshTotal <- left_join(meshCleaned, 
                       meshClassified, 
                       by="MeSHSimple")


meshClassOnly <-  meshTotal %>%
  
  # keep only rows that had a classification (which means they had >2 occurrences)
  filter(!is.na(Classification)) %>%
  
  # remove unnecessary columns
  select(-MeSH, -MeSHSimple) %>%
  
  # keep only distinct rows - so deletes duplicate classifications within the same projectID
  distinct()


# make the 'nodelist' for the network - list of all the vertices/dots
meshNodes <- meshClassOnly %>%
  
  # for each unique classification
  group_by(Classification) %>%
  
  # count how many times it occurs 
  summarize(vsize = n()) 
# we can do this now, it's alright
# the edge list manipulations below don't induce any isolates, I checked 




# now we can start making the edgelist for the network
meshWide <- meshClassOnly %>%
  
  # within each project ID
  group_by(ProjectID) %>%
  
  # create a running variable for the classifications to use when transposing
  mutate(id = 1:n()) %>%
  
  # go back to wide form (needed for linkages)
  spread(key=id, value=Classification)


# now join that wide dataset back to the dataset with unique mesh terms only
# for one project with 3 mesh terms, now looks like this: 
  # A mesh1 mesh1 mesh2 mesh3
  # A mesh2 mesh1 mesh2 mesh3
  # A mesh3 mesh1 mesh2 mesh3
tmp1 <- left_join(meshClassOnly, 
                  meshWide, 
                  by="ProjectID")

# this can now be used to make an 'edgelist'
meshEdges <- tmp1 %>%
  
  # transpose so that EACH linkage is its own row
  # call the merged classification 'class2'
  gather(colnum, class2, `1`:`19`) %>%
  
  # and the original classification should be named class1
  rename(class1 = Classification) %>%
  
  # remove colnum
  select(-colnum) %>%
  
  # remove induced NAs from gather
  filter(!is.na(class1) & !is.na(class2)) %>%
  
  # remove self loops
  filter(class1 != class2) %>%
  
  # for each unique linkage
  group_by(class1, class2) %>%
  
  # count how many times that linkage occurred
  summarise(eweight=n()) %>%
  
  # arrange for niceness
  arrange(class1, class2) %>%
  
  # keep only linkages/connections that occured in more than 3 projects
  filter(eweight>3)
# one issue that isn't fixed
# a-b and b-a linkages are still in here -- that's ok, we will fix this in the next step 
# wouldn't even be a big deal if we didn't fix it, since for visualization purposes our edges would lay on top of each other
# only thing it would affect is the community detection algorithm, and it would be consistent across all edges


# just need the mesh edges and mesh nodes datasets now 
rm(meshClassOnly, meshWide, tmp1, meshClassified, meshSimpleReduced, meshCleaned)




