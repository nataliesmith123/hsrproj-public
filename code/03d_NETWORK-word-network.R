
## @knitr meshnetwork2

library(igraph)
library(ggnetwork)
library(ggraph)
library(graphlayouts)

# create our igraph object!
meshGraph <- graph_from_data_frame(meshEdges,
                                   directed=FALSE, 
                                   vertices = meshNodes)

rm(meshNodes, meshEdges)

# simplify to remove the duplicate a-b and b-a linkages
# specifying to take the average edgeweight of each simplified link pair, which should be the average of two identical numbers
meshGraphSimple <- igraph::simplify(meshGraph,
                                    remove.multiple = TRUE,
                                    edge.attr.comb = "mean")
rm(meshGraph)

# remove nodes/concepts that are too broad and/or connected to be useful, or are unclassified 
meshGraphSimpleSubset <- induced_subgraph(meshGraphSimple, 
                                          vids=!(V(meshGraphSimple)$name %in% c("zUnclassified", 
                                                                                "Diabetes \n General", 
                                                                                "Geography", 
                                                                                "Outcomes \n Related", 
                                                                                "Individual \n Focus", 
                                                                                "Program \n Development", 
                                                                                "HealthComm \n Promotion", 
                                                                                "AHRQ", "NIDDK", "CMS", "VA")))
rm(meshGraphSimple)

# and now remove the few concepts that become isolated concepts after this
# e.g., osteoarthritis
meshGraphSimpleSubset <- induced_subgraph(meshGraphSimpleSubset, 
                                          vids=degree(meshGraphSimpleSubset)>0)




# and now we can detect communities within this network!
# only want to run the algorithm on NS desktop for reproducibility
# this code is kept in here for transparency, but have commented out since we have final results now
# if (Sys.info()[["nodename"]]=="DESKTOP-B5DVP0R" & file.exists("output/SPINGLASS_RESULTS.Rdata")==FALSE){
    # set.seed(28215)
    # spgl <- cluster_spinglass(meshGraphSimpleSubset, E(meshGraphSimpleSubset)$eweight, gamma=1.2)
    # save(spgl, file="output/SPINGLASS_RESULTS.Rdata")
# } else {
#   load("output/SPINGLASS_RESULTS.Rdata")
# }


# load the community detection results
load("output/SPINGLASS_RESULTS.Rdata")

# there were originally 7 communities/themes
sizes(spgl)

# add these classifications to the network
meshGraphSimpleSubset <- igraph::set_vertex_attr(meshGraphSimpleSubset, 
                                                 name = "community", 
                                                 value=membership(spgl))
rm(spgl)


# now to do a little data manipulation for final themes and visualization
# get nodes and edges
tmp1edges <- igraph::as_data_frame(meshGraphSimpleSubset, what="edges")

# within nodes...
tmp1nodes <- igraph::as_data_frame(meshGraphSimpleSubset, what="vertices") %>% 
  
  # just keep the community
  select(name, community) %>%
  
  # RECLASSIFY COMMUNITIES
  # this combination was decided upon by NS, KM, and BB in a team meeting
  # some communities seem to focus on pretty different subsets 
  # so are combined into the same theme for interpretation and visualization
  mutate(commF = factor(community, levels=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                        labels=c("1" = "Diabetes management in veterans", 
                                 "2" = "Complications and comorbidity management", 
                                 "3" = "Decision Support", 
                                 "4" = "T1D management", 
                                 "5" = "T2D management 1", 
                                 "6" = "Lipids & pressure", 
                                 "7" = "T2D prevention", 
                                 "8" = "T2D management 2")), 
         
         hsrCommunity = case_when(# Diabetes and associated complication management
                                  community==1 ~ 1, 
                                  community==2 ~ 1, 
                                  
                                  # Decision Support
                                  community==3 ~ 2, 
                                  
                                  #t1d management
                                  community==4 ~ 3, 
                                  
                                  #t2d management
                                  community==5 ~ 4, 
                                  community==6 ~ 4, 
                                  community==8 ~ 4,
                                  
                                  # prevention
                                  community==7 ~ 5 
                                  ),

         # now create a factor variable with the name of the community
         hsrCommF = factor(hsrCommunity, levels=c("1", "2", "3", "4", "5"),
                           labels = c("1" = "Diabetes and complication management",
                                      "2" = "Decision support",
                                      "3" = "T1D management",
                                      "4" = "T2D management",
                                      "5" = "T2D prevention")))
  

# upweight edges if they are in the same community
# join the nodelist to the edgelist - get community designation for the FROM concept
tmp2edges <- left_join(tmp1edges, 
                       (tmp1nodes %>% select(name, community)), 
                       by=c("from"="name")) %>%
  
  # and now repeat, to get community designation for the TO concept
  left_join((tmp1nodes %>% select(name, community)), 
            by=c("to" = "name")) %>%
  
  # and finally, upweight if the communities are the same
  mutate(UPweight = if_else(community.x==community.y, 60, 1))




meshGraphNew <- graph_from_data_frame(d=tmp2edges, 
                                      directed=FALSE, 
                                      vertices=tmp1nodes)

rm(meshGraphSimpleSubset, tmp1edges, tmp2edges)  


## @knitr meshnetworkviz
 #set.seed(142378)
 #layoutGG = layout_with_fr(meshGraphNew, weights=E(meshGraphNew)$UPweight)
 #save(layoutGG, file="output/GGNETWORK_LAYOUT.Rdata")

# load the pre-specified layout
 load("output/GGNETWORK_LAYOUT.Rdata")
 
 rownames(layoutGG) <- tmp1nodes$name
 layoutGG["Disabled \n Persons", 1] <- 0.3
 layoutGG["Disabled \n Persons", 2] <- -0
 
 layoutGG["Urban", 1] <- 0
 
 layoutGG["Type 1 \n Diabetes", 1] <- -1.7
 
 layoutGG["Blood \n Lipids", 2] <- -0.2
 
 layoutGG["SES related", 2] <- -0
 

# and now we can visualize this network, using the layout we just pulled in
set.seed(1234)
base <- ggraph(graph=meshGraphNew, 
       layout=layoutGG) +
  
  # remove all axes and the legend
  theme_blank() + theme(legend.position = "none")+ 
  
  # modify theme elemnts, font sizes etc.
  theme(plot.title = element_text(size=17, face="bold", hjust=0), 
        plot.subtitle = element_text(size=11, hjust=0), 
        plot.caption = element_text(hjust = 0), 
        plot.margin = margin(t = 0.25, r = 0.4, b = 0.25, l = 0.3, unit = "in")) + 
  

  
  # add lots of informative labels and titles
  labs(title = "Figure 2: Diabetes research themes", 
       subtitle = "Themes were created using a network of linked MeSH terms and parsing highly-connected groups of terms using network analysis. 
       ", 
       caption = "Notes: T1D = type 1 diabetes. T2D = type 2 diabetes. Concepts displayed are aggregated MeSH terms. Each MeSH term must have occurred in at least two projects. To be 
       linked, those concepts must have appeared in the same project at least four times. Research themes were derived using the spinglass community detection algorithm 
       in R. Related communities were combined to form five cohesive research themes. Linkages between MeSH terms not shown for interpretability. Dots represent MeSH 
       concepts. Text labels for MeSH concepts are arranged to prevent overlap and are connected to their specific dot/MeSH concept with lines when necessary.")





# 5 communities -----------------------------------------------------------

nejm_not_LAZY = c("Diabetes and complication management" = "#BC3C29FF",
                  "Decision support" = "#7876B1FF",
                  "T2D management" = "#0072B5FF",
                  "T1D management" = "#E18727FF",
                  "T2D prevention" = "#20854EFF")

base + 
  
  # and now add the network points
  geom_node_point(aes(colour=hsrCommF), size=3) + 
  
  # label those points with the concept name
  geom_node_label(aes(label=name, 
                      colour=hsrCommF), 
                  size=3, 
                  repel=TRUE, 
                  fontface="bold") + 

  # specify that we want to use the reordered colors specified above
   scale_color_manual(values=nejm_not_LAZY) +
   scale_fill_manual(values=nejm_not_LAZY) + 


  annotate("text", x=0.6, y=2.4, label="Diabetes and \n Complication Management",
           color = "#BC3C29FF", size=7, fontface=2) +

  annotate("text", x=-0.5, y=-0.5, label="Diabetes Management",
           color = "#0072B5FF", size=7, fontface=2) +

  annotate("text", x=-1.8, y=0.6, label="T1D \n in Youth",
           color = "#E18727FF", size=7, fontface=2) +

  annotate("text", x=-1.6, y=2.2, label="Decision \n Support",
           color = "#7876B1FF", size=7, fontface=2) +

  annotate("text", x=1, y=0.1, label="T2D \n Prevention",
           color = "#20854EFF", size=7, fontface=2)

# note we are not graphing the edges because they look awful
  
ggsave(filename = "output/Fig-2-5comms.png", plot=last_plot(), device="png", 
       units="in", width=11, height=8.5, dpi=500) 



# 8 communities for some nuance -----------------------------------------------------------

nejm_not_LAZY2 = c("Diabetes management in veterans" = "#da6958",
                  "Complications and comorbidity management" = "#922f20",
                  
    
                  "T2D management 1" = "#0072B5FF",
                  "T2D management 2" = "#00a2ff",
                  "Lipids & pressure" = "#005180",
                  
                  "Decision Support" = "#7876B1FF",
                  "T2D prevention" = "#20854EFF",
                  "T1D management" = "#E18727FF"
                  
                  )

base + 
  
  # and now add the network points
  geom_node_point(aes(colour=commF), size=3) + 
  
  # label those points with the concept name
  geom_node_label(aes(label=name, 
                      colour=commF), 
                  size=3, 
                  repel=TRUE, 
                  fontface="bold") + 
  
  # specify that we want to use the reordered colors specified above
  scale_color_manual(values=nejm_not_LAZY2) +
  scale_fill_manual(values=nejm_not_LAZY2) + 
  
  
  annotate("text", x=0.6, y=2.4, label="Diabetes and \n Complication Management",
           color = "#BC3C29FF", size=7, fontface=2) +
  
  annotate("text", x=-0.5, y=-0.5, label="Diabetes Management",
           color = "#0072B5FF", size=7, fontface=2) +
  
  annotate("text", x=-1.8, y=0.6, label="T1D \n in Youth",
           color = "#E18727FF", size=7, fontface=2) +
  
  annotate("text", x=-1.6, y=2.2, label="Decision \n Support",
           color = "#7876B1FF", size=7, fontface=2) +
  
  annotate("text", x=1, y=0.1, label="T2D \n Prevention",
           color = "#20854EFF", size=7, fontface=2)

# note we are not graphing the edges because they look awful

ggsave(filename = "output/Fig-2-nuance.png", plot=last_plot(), device="png", 
       units="in", width=11, height=8.5, dpi=500) 



# remove everything from memory 
rm(list=objects())
