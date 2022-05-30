library(ggplot2)
library(gridExtra)
library(ggpattern)

# boxplot graph -----------------------------------------------------------

#Vetores com cores dos plots
col_be <- c("grey","orange","purple","purple","purple","grey","green","blue","blue","red",
            "purple","purple","orange", "orange", "orange", "orange","orange","orange", "black","black","black","brown","green","green","green","blue","blue","blue","red") #organização 1 (wd, pt, rr)

td <- ggplot(db_be) +
  geom_boxplot_pattern(
    aes(x=BEs, y=elevation, pattern=alt_class), col=col_be,
    pattern_density=0.2, pattern_colour= 'black', pattern_spacing=0.01, lwd=.3)+
  labs(x= "Biotic Elements", y= "Elevation (m)")+
  scale_y_continuous(limits = c(0, 2000))+
  scale_pattern_manual(values=c("none", "none", "circle"))+
  scale_x_discrete(limits=c("2","23","15","28","26","29","22","14","4","13","20","17","11","27","19", "10", "6","25","18","16","7","12","5","9","3","24","8","21","1"))+
  geom_hline(yintercept = 592)+
  geom_vline(xintercept = 1.5, linetype=2)+
  geom_vline(xintercept = 10.5, linetype=2)+
  theme_classic()+
  theme(legend.position='none', 
        axis.title = element_text(size=10, margin = margin(t=0, r=0, b=0, l=0, unit="mm")), 
        axis.text = element_text(size=8))
td

alt_graph <- td + geom_label(x=5, y=2000, label="Partial") + geom_label(x=19, y=2000, label="Restricted")
alt_graph

ggsave("Fig 3_new.png",
       device = png,
       plot = alt_graph,
       path = here("outputs", "figures"),
       width = 168,
       height = 84,
       units = "mm",
       dpi = 300,
       )
