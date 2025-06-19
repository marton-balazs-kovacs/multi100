source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/helpers/Packages.R"))
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/helpers/functions.R"))
load(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/ModelData_and_Draws/Models.RData"))
load(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/ModelData_and_Draws/Draws.RData"))


################################################################################
standardized_gMEs<- rbind(
standardized_gME(Fuhrmann_Draws,export_Fuhrmann,"Fuhrmann_JournConflictRes_2010_8Wy0"),
standardized_gME(Brancati_Draws,export_Brancati,"Brancati_JournConflictRes_2013_V0PA"),
standardized_gME(Luttrell_Draws,export_Luttrell,"Luttrell_JournExpSocPsych_2016_rjb"),
standardized_gME(PIETRYKA_Draws,export_PIETRYKA,"PIETRYKA_AmPoliSciRev_2017_yjkQ")
)

#save(standardized_gMEs,file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/standardized_gMEs.RData"))

################################################################################


colors<- scale_fill_manual(values=c("Family: binomial(Link: logistf)" = "#F8766D","Family: binomial(Link: logit)" = "#00BFC4", "Family: gaussian(Link: identity)"="#C77CFF","Family: binomial(Link: probit)"="forestgreen"),drop=FALSE)

plot_Fuhrmann<-forest_density_plot(Fuhrmann_Draws,"Fuhrmann paper", "Exemplary plot", "Average change in probability", seq(-0.015,0.06,by=0.01),collevs = "Family: binomial(Link: probit)") +
  colors
plot_Brancati<-forest_density_plot(Brancati_Draws,"Brancati paper", "Exemplary plot", "Average change in probability", seq(-0.015,0.04,by=0.01)) +
  colors
plot_Luttrell<-forest_density_plot(Luttrell_Draws,"Luttrell paper", "Exemplary plot", "Average change",seq(-0.1,0.15,by=0.05)) +
  colors
plot_PIETRYKA<-forest_density_plot(PIETRYKA_Draws,"PIETRYKA paper", "Exemplary plot", "Average change",seq(-0.2,0.6,by=0.1)) +
  colors


ggarrange(plot_Fuhrmann+facet_grid(group~.)+ggtitle("Fuhrmann, Brancati, Luttrell, & Pietryka  papers")+labs(subtitle = "Exemplary plot",x="")+theme(plot.margin = unit(c(0.2,0.2,-1,0.2), 'lines')),
          plot_Brancati+facet_grid(group~.)+ggtitle("")+labs(subtitle = "")+theme(plot.margin = unit(c(-1,0.2,0.2,0.2), 'lines')),
          plot_PIETRYKA+facet_grid(group~.)+ggtitle("")+labs(subtitle = "",x="")+theme(plot.margin = unit(c(0.2,0.2,-1,0.2), 'lines')),
          plot_Luttrell+facet_grid(group~.)+ggtitle("")+labs(subtitle = "")+theme(plot.margin = unit(c(-1,0.2,0.2,0.2), 'lines')),
          nrow=4,common.legend = TRUE,legend="bottom",heights=c(8,6.5,7,6.5))

#ggsave(filename = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/First4.png"),width=12,height=18)

################################################################################
# Adjusting plot for publication

manipulate_data <- function(df,number){
  df["family"] <- ifelse(sapply(df["model"],function(x){ grepl("Original", x) | grepl("original", x)}),"Original analysis","Re-analysis")
  df["model"] <- sapply(df["model"],function(x){
    ifelse(grepl("Original", x) | grepl("original", x),paste("zzz",x),x)
  })
  df["group"] <- paste0("paper ",number)
  return(df)
}

Fuhrmann_Draws_updated <- manipulate_data(Fuhrmann_Draws,40)
Brancati_Draws_updated <- manipulate_data(Brancati_Draws,22)
Luttrell_Draws_updated <- manipulate_data(Luttrell_Draws,63)
PIETRYKA_Draws_updated <- manipulate_data(PIETRYKA_Draws,75)

colors_publication <- scale_fill_manual(values=c("Original analysis"="forestgreen", "Re-analysis"="gray"),drop=FALSE)

plot_Fuhrmann_publication <-
  forest_density_plot_publication(
    Fuhrmann_Draws_updated,
    "Fuhrmann paper",
    "Exemplary plot",
    "Average change in probability",
    seq(-0.015, 0.06, by = 0.01),
    collevs = "Family: binomial(Link: probit)"
  ) +
  colors_publication +  theme(strip.text = element_text(size = 22))
plot_Brancati_publication <-
  forest_density_plot_publication(
    Brancati_Draws_updated,
    "Brancati paper",
    "Exemplary plot",
    "Average change in probability",
    seq(-0.015, 0.015, by = 0.005)
  ) +
  colors_publication +  theme(strip.text = element_text(size = 22))
plot_Luttrell_publication <-
  forest_density_plot_publication(
    Luttrell_Draws_updated,
    "Luttrell paper",
    "Exemplary plot",
    "Average change",
    seq(-0.1, 0.2, by = 0.05)
  ) +
  colors_publication + theme(strip.text = element_text(size = 22))
plot_PIETRYKA_publication <-
  forest_density_plot_publication(
    PIETRYKA_Draws_updated,
    "PIETRYKA paper",
    "Exemplary plot",
    "Average change",
    seq(-0.2, 0.6, by = 0.1)
  ) +
  colors_publication + theme(strip.text = element_text(size = 22))


combined_plot <-ggarrange(
  plot_Fuhrmann_publication +
    facet_grid(group~.) +
    ggtitle("") +
    labs(subtitle = "", x = "") +
    theme(
      plot.margin = unit(c(0.2,0.2,-1,0.2), 'lines')    ),
  
  plot_Brancati_publication +
    facet_grid(group~.) +
    ggtitle("") +
    labs(subtitle = "", x = "") +
    theme(
      plot.margin = unit(c(0.2,0.2,-1,0.2), 'lines')
    ),
  
  plot_PIETRYKA_publication +
    facet_grid(group~.) +
    ggtitle("") +
    labs(subtitle = "", x = "") +
    theme(
      plot.margin = unit(c(0.2,0.2,-1,0.2), 'lines')
    ),
  
  plot_Luttrell_publication +
    facet_grid(group~.) +
    ggtitle("") +
    labs(subtitle = "", x = "Average change in target expectation") +
    theme(
      plot.margin = unit(c(-1,0.2,0.2,0.2), 'lines')
    ),
  nrow = 4,
  common.legend = TRUE,
  legend = "top",
  heights = c(8, 6.5, 7, 6.5)
)


ggsave(filename = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/gMEs.png"),plot = combined_plot,width=12,height=18,bg = "white")






