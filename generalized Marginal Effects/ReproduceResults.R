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
