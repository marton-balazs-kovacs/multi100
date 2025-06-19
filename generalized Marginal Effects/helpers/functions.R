targetSD<-function(list,modName){y<-str_replace_all(as.character(list[[which(names(list)==modName)]]$formula[2]),"[^[:alnum:]_]","")
  if(is.na(y)){y<-str_replace_all(as.character(list[[which(names(list)==modName)]]$formula_old[2]),"[^[:alnum:]_]","")}
  dat<-list[[which(names(list)==modName)]]$data
  return(sd(as.numeric(as.data.frame(as.data.frame(dat)[,which(names(dat)==y)])[,1]),na.rm=TRUE))}

standardized_gME<-function(Draws,Models,paperID){
  model<-unique(Draws$model)
  return<-data.frame()
  add<-data.frame(Paper_ID=paperID)
  for(i in seq_along(model)){
    add$standardized_gME<-mean(Draws$samples[which(Draws$model==model[i])],na.rm=TRUE)/targetSD(Models,model[i])
    add$Analyst_ID<-unique(str_split(Draws$model[which(Draws$model==model[i])],"_\\._",simplify = TRUE)[,2])
    return<-rbind(return,add)
  }
  return(return)
}



modfun<-function(df,col1,col2){
  values_by_level <- split(df[,which(names(df)==col2)], df[,which(names(df)==col1)])
  output0<-unlist(lapply(values_by_level,function(x)as.character(unique(x))))
  return(list(paste0("<span style = 'color:black;font-size: 15px;'>**_",str_split(names(output0),"_\\._",simplify = TRUE)[,2],": ",str_split(names(output0),"_\\._",simplify = TRUE)[,1],"_**</span><br>"),output0))
}

rename_regs<-function(names,formula=NULL){
  new_names<-gsub("^[^a-zA-Z0-9]+", "", names)
  if(is.null(formula)){return(list(new_names=new_names))
  }else{
    substitution<-new_names
    names(substitution)<-paste0("`",names,"`")
    new_formula<-str_replace_all(as.character(formula), substitution)
    return(list(new_names=new_names,new_formula=new_formula))
  }
}


forest_density_plot <- function(df, title, subtitle, xLabel, xBreaks,point_interval="median_qi",collevs=NULL) {
  if(any(isFALSE(c("samples","model","family","formula","group")%in%names(df)))){stop("The data frame is unsuitable (necessary columns are missing).")}
  lim_min<-min(xBreaks)
  lim_max<-max(xBreaks)
  labels<-modfun(df, "model", "formula")
  output <- ggplot(df, aes(y = as.factor(model), x = samples, fill = factor(family,levels=c(levels(factor(family)),collevs)))) +
    geom_density_ridges(alpha = 0.4, scale = 0.9, color = "black") +
    labs(x = xLabel, y = "", title = title, subtitle = subtitle, fill="Family") +
    theme_minimal() + stat_pointinterval(point_interval = point_interval)+
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    scale_y_discrete(labels = gsub("`", "",paste0(labels[[1]],unlist(lapply(strwrap(labels[[2]], width = 55, simplify = FALSE), paste, collapse = "<br> "))))) +
    scale_x_continuous(breaks = xBreaks,limits = c(lim_min,lim_max),
                       expand = expansion(mult = c(0.0001,0),add = c(0, 0.002))) +
    #facet_grid(group ~ ., scales = "free", space = "free") +
    theme(#axis.text.y = element_text(size = 8),
      axis.text.y.left = element_markdown(),
      axis.text.x = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      legend.position = "bottom",
      legend.justification = "left",
      panel.margin = unit(1, "lines"),
      strip.text.y = element_text(angle = -90),
      strip.background = element_rect(color = "black", size = 1.5, linetype = "solid"))
  return(output)
}

forest_density_plot_publication <- function(df, title, subtitle, xLabel, xBreaks,point_interval="median_qi",collevs=NULL) {
  if(any(isFALSE(c("samples","model","family","formula","group")%in%names(df)))){stop("The data frame is unsuitable (necessary columns are missing).")}
  lim_min<-min(xBreaks)
  lim_max<-max(xBreaks)
  labels<-modfun(df, "model", "formula")
  output <- ggplot(df, aes(y = as.factor(model), x = samples, fill = factor(family,levels=c(levels(factor(family)),collevs)))) +
    geom_density_ridges(alpha = 0.4, scale = 0.9, color = "black") +
    labs(x = xLabel, y = "", title = title, subtitle = subtitle, fill="") +
    theme_minimal() + stat_pointinterval(point_interval = point_interval)+
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    #scale_y_discrete(labels = gsub("`", "",paste0(labels[[1]],unlist(lapply(strwrap(labels[[2]], width = 55, simplify = FALSE), paste, collapse = "<br> "))))) +
    scale_x_continuous(breaks = xBreaks,limits = c(lim_min,lim_max),
                       expand = expansion(mult = c(0.0001,0),add = c(0, 0.002))) +
     guides(fill = guide_legend(nrow = 2)) +
    #facet_grid(group ~ ., scales = "free", space = "free") +
    theme(axis.text.y = element_blank(),
      axis.text.x = element_text(size = 22),
      axis.title = element_text(size = 26),
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 18, face = "italic"),
      legend.position = "bottom",
      panel.margin = unit(1, "lines"),
      strip.background = element_rect(color = "black", size = 1.5, linetype = "solid"),
      legend.text = element_text(size = 20),
      legend.key.size = unit(1.5, "lines"),
      legend.key.width = unit(1.5, "lines")
      )
  return(output)
}
