
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

# remove nodes/concepts that are too broad and/or connected to be useful
meshGraphSimpleSubset <- induced_subgraph(meshGraphSimple, 
                                          vids=!(V(meshGraphSimple)$name %in% c("zUnclassified", 
                                                                                "Diabetes \n General", 
                                                                                "Geography", 
                                                                                "Outcomes \n Related", 
                                                                                "Individual \n Focus", 
                                                                                "Program \n Development", 
                                                                                "HealthComm \n Promotion", 
                                                                                "AHRQ", "NIDDK")))
rm(meshGraphSimple)

# and now remove the few concepts that become isolated concepts after this
# e.g., osteoarthritis
meshGraphSimpleSubset <- induced_subgraph(meshGraphSimpleSubset, 
                                          vids=degree(meshGraphSimpleSubset)>0)




# and now we can detect communities within this network!
# only want to run the algorithm on NS laptop for reproducibility
# this code is kept in here for transparency, but have commented out since we have final results now
# if (Sys.info()[["nodename"]]=="NATSMITH-PC" & file.exists("output/SPINGLASS_RESULTS.Rdata")==FALSE){
#   set.seed(28215)
#   spgl <- cluster_spinglass(meshGraphSimpleSubset, E(meshGraphSimpleSubset)$eweight, gamma=1.18)
#   save(spgl, file="output/SPINGLASS_RESULTS.Rdata")
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
  # original communities 2 and 5, and 4 and 7 examine very related concepts
  # so are combined into the same theme for interpretation and visualization
  mutate(hsrCommunity = case_when(community==1 ~ 1, 
                                  community==2 | community==5 ~ 2, 
                                  community==3 ~ 3, 
                                  community==4 | community==7 ~ 4, 
                                  community==6 ~ 5), 
         
         # now create a factor variable with the name of the community
         hsrCommF = factor(hsrCommunity, levels=c("1", "2", "3", "4", "5"), 
                           labels = c("1" = "Decision Support", 
                                      "2" = "Diabetes Management", 
                                      "3" = "T1D Management in Children/YA", 
                                      "4" = "T2D prevention", 
                                      "5" = "Comorbidities and Complications")))
  

# upweight edges if they are in the same community
# join the nodelist to the edgelist - get community designation for the FROM concept
tmp2edges <- left_join(tmp1edges, 
                       (tmp1nodes %>% select(name, hsrCommunity)), 
                       by=c("from"="name")) %>%
  
  # and now repeat, to get community designation for the TO concept
  left_join((tmp1nodes %>% select(name, hsrCommunity)), 
            by=c("to" = "name")) %>%
  
  # and finally, upweight if the communities are the same
  mutate(UPweight = if_else(hsrCommunity.x==hsrCommunity.y, 60, 1))




meshGraphNew <- graph_from_data_frame(d=tmp2edges, 
                                      directed=FALSE, 
                                      vertices=tmp1nodes)

#rm(meshGraphSimpleSubset, tmp1nodes, tmp1edges, tmp2edges)  


## @knitr meshnetworkviz
# ONLY RUN ONCE ON LAPTOP FOR REPRODUCIBILITY
#layoutGG = layout_with_fr(meshGraphNew, weights=E(meshGraphNew)$UPweight)
#save(layoutGG, file="output/GGNETWORK_LAYOUT.Rdata")

# load the pre-specified layout
load("output/GGNETWORK_LAYOUT.Rdata")

rownames(layoutGG) <- tmp1nodes$name
layoutGG["Urban", 1] <- 1.05
layoutGG["Urban", 2] <- -0.8

layoutGG["Quality \n Improvement", 1] <- 0
layoutGG["Quality \n Improvement", 2] <- -0.4

layoutGG["Ambulatory \n Outpatient", 1] <- -0.3
layoutGG["Ambulatory \n Outpatient", 2] <- 0.1

layoutGG["Geography/GIS", 1] <- -0.7
layoutGG["Geography/GIS", 2] <- 0.1

layoutGG["PA/Exercise", 1] <- -0.7

layoutGG["Men", 2] <- -2.3

layoutGG["Women", 1] <- -0.76
layoutGG["Women", 2] <- -2.6

layoutGG["African \n Americans", 1] <- 0
layoutGG["African \n Americans", 2] <- -2.3

layoutGG["EBM or \n Guidelines", 2] <- -1.3

# not being lazy, and reordering the NEJM theme colors, also from the ggsci package
# need to reorder so that blue and green aren't right next to each other
nejm_not_LAZY = c("Comorbidities and Complications" = "#BC3C29FF", 
                  "Decision Support" = "#7876B1FF", 
                  "Diabetes Management" = "#0072B5FF", 
                  "T1D Management in Children/YA" = "#E18727FF", 
                  "T2D prevention" = "#20854EFF")

set.seed(45239)
V(meshGraphNew)$name[[4]] <- "Native \n Americans"
# and now we can visualize this network, using the layout we just pulled in
ggraph(graph=meshGraphNew, 
       layout=layoutGG) +
  
  # remove all axes and the legend
  theme_blank() + theme(legend.position = "none")+ 
  
  # modify theme elemnts, font sizes etc.
  theme(plot.title = element_text(size=17, face="bold", hjust=0), 
        plot.subtitle = element_text(size=11, hjust=0), 
        plot.caption = element_text(hjust = 0), 
        plot.margin = margin(t = 0.25, r = 0.4, b = 0.25, l = 0.3, unit = "in")) + 
  
  # specify that we want to use the reordered colors specified above
  scale_color_manual(values=nejm_not_LAZY) +
  scale_fill_manual(values=nejm_not_LAZY) + 
  
  # add lots of informative labels and titles
  labs(title = "Figure 2: Diabetes research themes", 
       subtitle = "Themes were created using a network of linked MeSH terms and parsing highly-connected groups of terms using network analysis. 
       ", 
       caption = "Notes: T1D = type 1 diabetes. T2D = type 2 diabetes. Concepts displayed are aggregated MeSH terms. Each MeSH term must have occurred in at least two projects. To be 
       linked, those concepts must have appeared in the same project at least four times. Research themes were derived using the spinglass community detection algorithm in R. 
       Related communities were combined to form five cohesive research themes. Linkages between MeSH terms not shown for interpretability. Dots represent MeSH concepts. 
       Text labels for MeSH concepts are arranged to prevent overlap and are connected to their specific dot/MeSH concept with lines when necessary.") + 
  
  # and now add the network points
  geom_node_point(aes(colour=hsrCommF), size=3) + 
  
  # label those points with the concept name
  geom_node_label(aes(label=name, 
                      colour=hsrCommF), 
                  size=3, 
                  repel=TRUE, 
                  fontface="bold") +
  
  #annotate("rect", xmin = -1.55, xmax = -1, ymin = -0.26, ymax = 0.06,
  #         alpha = .2) + 
  annotate("text", x=-1.25, y=-0.1, label="Comorbidities \n and Complications", 
           color = "#BC3C29FF", size=7, fontface=2) + 
  
  annotate("text", x=0.3, y=0.45, label="Diabetes \n Management", 
           color = "#0072B5FF", size=7, fontface=2) + 
  
  annotate("text", x=0.63, y=-0.35, label="T1D Management \n in Youth", 
           color = "#E18727FF", size=7, fontface=2) + 
  
  annotate("text", x=0.8, y=-2.3, label="Decision Support", 
           color = "#7876B1FF", size=7, fontface=2) + 
  
  annotate("text", x=-1.26, y=-1.6, label="Community-based \n T2D prevention", 
           color = "#20854EFF", size=7, fontface=2)

# note we are not graphing the edges because they look awful
  

# save as vector and raster
ggsave(filename = "output/Fig-2-concept-network.pdf", plot=last_plot(), device="pdf", 
       units="in", width=11, height=8.5) 
ggsave(filename = "output/Fig-2-concept-network.png", plot=last_plot(), device="png", 
       units="in", width=11, height=8.5) 


# remove everything from memory 
rm(list=objects())
