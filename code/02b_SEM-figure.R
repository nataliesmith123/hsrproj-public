
## @knitr SEMviz


ggplot(data=SEMgraph, aes(x=SEMgraph$avgYearlyFunding, # avg funding on the x-axis
                          colour=fct_rev(SEMgraph$SEM), # color (OUTLINE) dots by the three SEM levels (reversing, so policy is on top)
                          fill=fct_rev(SEMgraph$SEM))) + # fill (INSIDE) dots by the three SEM levels
  
  # use JAMA color scheme for the outline and fill colors
  ggsci::scale_color_jama() + 
  ggsci::scale_fill_jama() + 
  
  # display separate graph for each of the three SEM levels
  lemon::facet_rep_wrap(fct_rev(SEMgraph$SEM), # again, reversing so policy is on top
                        nrow=3, ncol=1, # stack on top of each other
                        scales = "fixed", 
                        repeat.tick.labels=TRUE) + # I want an x-axis shown for each facet
  
  # informative labels and captions, etc.
  labs(y="Count", 
       
       title="Figure 1: Distribution of average yearly funding of included projects", 
       
       subtitle="The majority of projects focused on on individual and interpersonal factors, and on average, those projects received more funding.",
       
       caption="Notes: Numeric results should be interpreted with caution - 77% (192/250) projects were included. Figure excludes 58 projects where the social ecological model was not applicable (n=23) 
       and/or funding information was not available (n=39; primarily Veteran's Administration projects). If a study targeted more than one level (e.g., a multilevel diabetes self management 
       intervention), average yearly funding is divided equally among levels. Projects were classified into levels of the social-ecological model by study team members. One researcher classified 
       all 250 studies, two researchers each reviewed a random 10%.
       
       Median funding amounts: $341,111 (indiv/inter), $284,134 (org/comm), $448,104 (policy).") + 
  
  # simple black and white theme
  theme_bw() + 
  
  # alter specific theme elements, like font size and alignment
  theme(legend.position = "none", 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        strip.text.x = element_text(size=12), 
        plot.title = element_text(size=14, hjust = 0, face="bold"), 
        plot.subtitle = element_text(size=11, hjust=0), 
        plot.caption = element_text(hjust=0), 
        axis.title.x = element_blank(), 
        plot.margin = margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "in")) +
  
  # format the x-axis to look like dollars
  scale_x_continuous(labels = scales::dollar_format()) +
  
  # and FINALLY, add the data via a dotplot. I have adjusted the sizes and binwidth iteratively until I like the sizing in the exported graphs
  geom_dotplot(binwidth = 24000, dotsize=0.8) 


ggsave(filename = "output/Fig-1-SEM-funding.png", plot=last_plot(), device="png", 
       units="in", width=11, height=8.5, dpi=500)



rm(SEMgraph)





