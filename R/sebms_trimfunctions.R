### trimfunctions_Swedishbutterflies.R
### 
### Author: LP
### Date: 25 October 2017
### Modified 2018-11-21
### 
###


#' Create an Object with Species Number per Year and Site with Visit Frequency
#' 
#' Create a tibble with species ID and name together with the total number of observations of each species and the frequency (1 / #visits)
#'
#' @param year the year of interest
#' @param Art the species of interest
#' @param filterPattern a regex pattern to filter SQL query
#' @param topList logical; whether the top list of species should be used
#' @param topNumber the number of top most observed species
#' @param source the data sources
#' 
#' @import dplyr
#' @importFrom tidyr complete fill nest unnest
#' @importFrom purrr map2 possibly
#' @importFrom glue glue
#'
#' @return a tibble with site, year, as well as the number of individuals
#'   observed and the observation frequency for that year, species, and site.
#' @export
get_trimInfile <- function(year=2010:2023, Art = 1:200, filterPattern=NULL, topList=FALSE, topNumber=200, source = c(54,55,56,63,64,66,67)){
  
  trimSpecies <- sebms_trimSpecies(year = year, Art = Art, topList = topList, source = source) %>% 
    slice_head(n=topNumber)
  
  spein <- function(df = data, speuid) {
    
    #print(paste("Working on species with ID",speuid))
    minw <- df %>% pull(min) # first posible week of observation
    maxw <- df %>% pull(max) # öast possible week of observation
    
    obses <- sebms_trimobs(year = year, Art = speuid, filterPattern = filterPattern, minmax = minw:maxw, source = source) %>% 
      mutate(total_number = as.numeric(total_number))
    
    visits <- sebms_trimvisits(year = year, minmax = minw:maxw, source = source) %>% 
      mutate(visit = as.numeric(visit))
    
    if(nrow(obses) > 0) { #Precondition to skip species with zero observations
      
      ## TRIM infile generation (If species have been seen any year in 'year' all site with a visit get 'total_number' of 0. Non-visited sites any year gets a NA)
      obsTidy <- obses %>%
        complete(siteuid, year = seq(min(year), max(year), by = 1), fill = list(total_number = 0)) %>%
        left_join(visits, by = c("siteuid", "year")) %>% 
        mutate(total_number = if_else(is.na(visit), NA, total_number),
               visit = if_else(is.na(visit), 1, visit),
        ) %>%
        mutate(freq = 1/visit) %>% 
        select(-visit)
      
      ## Lars code that works now
      # obsTidy <- obses %>%
      #   complete(siteuid,
      #            year = seq(min(year), max(year), by = 1),
      #            fill = list(total_number = 99999)) %>%
      #   left_join(visits, by = c("siteuid", "year")) %>%
      #   mutate(total_number = ifelse(is.na(visit), NA, total_number)) %>%
      #   mutate(visit = ifelse(is.na(visit), 1, visit)) %>%
      #   mutate(total_number = ifelse(total_number == 99999, 0, total_number)) %>%
      #   mutate (freq = 1 / besok) %>%
      #   select(-besok) %>%
      #   mutate(total_number = as.numeric(total_number)) %>%
      #   filter(year %in% year) #This is in order to avoid a wrong number of sites by counting sites monitored after the year of the report
      
    }else{
      print(paste("Species with ID ",speuid," skipped, no observations!"))
    }
    
    return(obsTidy)
  }
  
  trimSpecies %>% 
    group_by(speuid, art) %>% 
    nest() %>% 
    ungroup() %>% 
    mutate(obslist = map2(data, speuid, possibly(~spein(df = .x, speuid = .y)), .progress = "Making trimspecies infile...")) %>% 
    select(-data) %>% 
    unnest(obslist)
  
}


#' Calculate TRIM Index
#'
#' @inheritParams get_trimInfile
#' @param infile file with site, year, total observations and reverse frequency weight (1/#visits), or an object from [get_trimInfile()]
#' @param ... further arguments to pass to [get_trimInfile()]
#' 
#' @importFrom rtrim trim
#' @import dplyr
#' @importFrom stringr str_detect
#' @return a trim file with yearly changes of each species. 
#' @export
get_trimIndex <- function(infile=NULL, year = 2010:2023, Art = 1:200, ...) {
  
  if(is.null(infile)) {
    arglist <- list(...)
    
    if(!is.null(arglist$filterPattern)) {
      fp <- arglist$filterPattern
      infile <- get_trimInfile(filterPattern = fp, year = year, Art = Art) %>% 
        select(siteuid, year, total_number, freq) %>% 
        group_by(speuid) %>% 
        nest() %>% 
        ungroup()
    }else{
      infile <- get_trimInfile(year = year, Art = Art)
      if (nrow(infile) > 0) {
        infile <- infile %>% 
          select(site = siteuid, speuid, art, year, total_number, freq) %>% 
          group_by(speuid, art) %>% 
          nest() %>% 
          ungroup()
      }
    }
  }else {
    infile <- infile %>% 
      select(site = siteuid, speuid, art, year, total_number, freq) %>% 
      group_by(speuid, art) %>% 
      nest() %>% 
      ungroup()
  }
  
  if(length(infile) != 0){
    
    #    dl.li <- vector(mode = 'list', length = length(infile))
    #names(dl.li) <- gsub("\ Spec.*","",names(infile)) #names(infile)
    trimfun <- function(df){
      rtrim::trim(total_number ~ site + year, data = df, weights = "freq",  model = 2, serialcor = TRUE,overdisp = TRUE,changepoints = "all",autodelete = TRUE, max_iter = 1000)
    }
    
    trimList <- map(infile$data, trimfun, .progress = "Run trimfunction...") %>% 
      suppressWarnings() %>% 
      set_names(infile$art) #%>% 
    
    return(trimList)}
  else {return(infile)}
}


#' Palette Used in ggplots for Trim Index
#' @return vector of color hex codes
#' @export
sebms_trimpal <- c("#FFB000", "#648FFF", "#DC267F")



#' Modify y axis Max Value
#' 
#' Sets the max value and steps on y-axis
#'
#' @param x the max of the trim values 
#'
#' @return a max value, a step value and ??
#' @noRd
yAxisModifier <- function(x) {
  case_when(x < 5 ~ c(4,.5, 8),
            x <10 ~ c(10,2, 5),
            x <15 ~ c(15,3, 5),
            x <20 ~ c(20, 5, 4),
            x <25 ~ c(25, 5, 5),
            x <30 ~ c(30, 5, 6),
            x <40 ~ c(40,10, 4),
            x <50 ~ c(50,10, 5),
            x <100 ~ c(100,20, 5),
            x <250 ~ c(250,50, 5),
            TRUE ~c(500,100, 5))
}

####
### 

#' Create and Save TRIM Plots
#'
#' @param trimIndex optional; a trimIndex object from the [get_trimIndex()]
#' @param year the years to calculate trimindex on, ignored if trimIndex is not
#'   NULL
#' @param Art the species of interest, ignored if trimIndex is not NULL
#' @param ... optional; other arguments passed on to [get_trimInfile()]
#'
#' @import ggplot2
#' @importFrom stringr str_replace
#' @importFrom glue glue
#' @importFrom rtrim index overall
#' 
#' @return figures in png format of the species trends with confidence interval
#' @export
get_trimPlots <- function(trimIndex = NULL, year = 2010:2023, Art = 1:200, ...) {
  
  # This creates a trimIndex file if none is provided
  if(is.null(trimIndex)) {
    
    arglist <- list(...)
    
    if(!is.null(arglist$filterPattern)){ # If you need a filter for the trimInfile creation
      
      fp <- arglist$filterPattern
      
      trimIndex <- get_trimInfile(year = year, Art = Art, filterPattern = fp) %>% 
        get_trimIndex()
      
    }else{ # If you want to use the defaults
      trimIndex <- get_trimIndex(year = year, Art = Art)
    }
  }  
  
  
  
  trimplots <- function(df, art= "Luktgräsfjäril") {
    #TODO make this plotting a function and run it in map per species instead. 
    if(inherits(df, 'trim')) {
      
      m2 <- df
      if(typeof(m2) == "list"){
        fname <- as.character({{ art }})
        fname <- str_replace(fname, "/", "_") #replacing escape characters in species name
        print(m2)
        
        yAxisAdjusted <- yAxisModifier(max(index(m2,base = min(m2$time.id))$imputed + 1.96*index(m2,base = min(m2$time.id))$se_imp))
        
        gcomma <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE) #Called later, enables commas instead of points for decimal indication
        
        
        indco <- overall(m2)
        indco <- as.vector(indco[[2]])
        indco <- indco[8]
        
        #TODO This if else should be possible to do inside ggplot with lty and col by category
        if (indco == "Uncertain") {
          col <- sebms_trimpal[3]
          lt <- "longdash"
        } else if (indco == "Strong decrease (p<0.05)" | indco == "Strong decrease (p<0.01)" | indco == "Moderate decrease (p<0.05)" | indco == "Moderate decrease (p<0.01)") {
          col <- sebms_trimpal[2]
          lt <- "solid"
        } else if (indco == "Stable") {
          col <- sebms_trimpal[3]
          lt <- "solid"
        }  else {
          col <- sebms_trimpal[1]
          lt <- "solid"
        }
        
        cd1 <- "as.theme(axis.text.y=element_text(size=42, #y-axis labels colour&size
  angle=0),axis.text.x=element_text(size=42,  #x-axis labels colour&size
  angle=0),plot.title = element_text(size=48, #Chart title size
  margin=margin(0,0,30,0)),panel.grid.major = element_blank(), #Distance between title and chart
  panel.grid.minor = element_blank(),panel.background = element_blank(), #Grid and background
  axis.line = element_line)"
        cd2 <- "as.theme(axis.text.y=element_text(size=42, #y-axis labels colour&size
  angle=0),axis.text.x=element_text(size=42,  #x-axis labels colour&size
  angle=0),plot.title = element_text(size=48, #Chart title size
  margin=margin(0,0,30,0)),panel.grid.major = element_blank(), #Distance between title and chart
  panel.grid.minor = element_blank(),panel.background = element_blank(), #Grid and background
  axis.line = element_line)"
        
        mrgn1 <- margin(0,0,80,0)
        mrgn2 <- margin(0,0,30,0)
        
        titles <- if((nchar(fname) < 18)) {
          paste0(fname ,' ',"(", m2$nsite, " lokaler)")
        }else{
          paste0(fname ,' ',"\n(", m2$nsite, " lokaler)")}
        
        if(nchar(fname) < 18) {
          mrgn <- mrgn1
        }else{
          mrgn <- mrgn2}
        
        fname <- str_replace(fname, "/", "_")#Restoring species name to original string that will appear in graph title
        Encoding(fname) <- 'UTF-8'
        
        
        ggplot(data = index(m2,base = min(m2$time.id)), aes(x = time,y = imputed)) +
          geom_line(linetype = paste(lt),
                    colour = paste(col),
                    linewidth = 2.8) +#central line #colour=rgb(155,187,89,max=255)
          geom_line(aes(x = time,
                        y = index(m2,base = min(m2$time.id))$imputed-1.96*index(m2,base = min(m2$time.id))$se_imp),
                    linetype = "longdash",
                    linewidth = 1.6,
                    data = index(m2,base = min(m2$time.id))) +#interval line 1
          geom_line(aes(x = time,
                        y = index(m2,base = min(m2$time.id))$imputed+1.96*index(m2, base = min(m2$time.id))$se_imp),
                    linetype = "longdash",
                    linewidth = 1.6,
                    data = index(m2,base = min(m2$time.id))) +#interval line 2
          # + xlim(startyear, endyear) #x-axis sectioning
          expand_limits(x = 2010)+
          geom_hline(yintercept = seq(from = 0, to = yAxisAdjusted[1], by=yAxisAdjusted[2])) +#Horizontal background lines #from=yAxisAdjusted[2]
          scale_y_continuous(labels = gcomma,
                             breaks = seq(from = 0, to = yAxisAdjusted[1], by = yAxisAdjusted[2]),
                             expand = c(0,0)) +#y-axis sectioning & comma labelling #from=yAxisAdjusted[2]
          scale_x_continuous(breaks = seq(2010,2020, by = 5))+
          labs(title = titles) +#Chart title text
          theme(plot.title = element_text(hjust = 0.5,
                                          size = 48, #Chart title size
                                          margin = mrgn),
                panel.grid.major = element_blank(), #Distance between title and chart
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), #Grid and background
                axis.text.x = element_text(colour = "black", size = 42,  #x-axis labels colour&size 
                                           angle = 0),
                axis.text.y = element_text(colour = "black",
                                           size = 42, #y-axis labels colour&size
                                           angle = 0),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.margin = margin(8, 20, 0, 0),
                axis.line = element_line(colour = "black")) #Axis colours
        
        
        
      }
    }
  }
  
  ggs <- vector("list", length = length(trimIndex))
  
  ggs <- map2(trimIndex, names(trimIndex), ~trimplots(.x, .y), .progress = "Making trimplots...")
  
  walk2(ggs, names(trimIndex), ~ggsave(plot = .x, filename = glue("{.y}.png"), width = 748, height = 868, units = "px", dpi = 72), .progress = "Saving trimplots...")
  
  
}

################################################################################
### Generate list of imputed values per species

get_imputedList<-function(trimIndex=NULL,origin='sverige', indicator_layout=FALSE, startyear=2010,endyear=2023,path=getwd(), ...){
  if(is.null(trimIndex)){
    arglist <- list(...)
    if(!is.null(arglist$filterPattern)){
      fp <- arglist$filterPattern
      infile=get_trimInfile(startyear=startyear, endyear=endyear, filterPattern=fp)
      trimIndex=get_trimIndex(infile=infile)
    }
    else{
      trimIndex=get_trimIndex(startyear=startyear, endyear=endyear)}
  }
  singleList =list()
  for(i in 1:length(trimIndex)){
    if(inherits(trimIndex[[i]]$value, 'trim')){
      singleList[[i]] <- cbind(spe_uid=trimIndex[[i]]$speuid,species=names(trimIndex[i]),samplingorigin=origin,index(trimIndex[[i]]$value),converged=trimIndex[[i]]$value$converged)
    }
  }    
  imputedList <- do.call(rbind, singleList) 
  if(indicator_layout==TRUE){
    imputedList <- imputedList[, -c(2,3,7)]
    names(imputedList) <- c('species','year','index','se')
  } 
  else {imputedList <- imputedList[, -c(1)]}
  return(imputedList)
}

################################################################################
### Trim Swedish set to match local set

get_trimmedImputedSwedishList<-function(imputedLocalList=NULL, imputedSwedishList=NULL, startyear=2010,endyear=2023,path=getwd()){
  trimmedSwedishList <- imputedSwedishList[imputedSwedishList$species %in% levels(as.factor(imputedLocalList$species)),]
}



################################################################################
### Create and save local TRIM plots with national TRIM reference

get_trimComparedPlots<-function(imputedLocalList=NULL, trimmedImputedSwedishList=NULL, startyear=2010,endyear=2023,path=getwd()){
  
  require(ggplot2)
  # sebms_palette <- c("#9BBB59", "#C0504D")
  sebms_palette <- c("#FFB000", "#648FFF", "#DC267F")
  imputedLocalList <- droplevels(imputedLocalList)
  trimmedImputedSwedishList <- droplevels(trimmedImputedSwedishList)
  for(i in 1:length(levels(as.factor(imputedLocalList$species)))){
    
    plotSpecies <-  levels(as.factor(imputedLocalList$species))[i]   
    
    swedish <- trimmedImputedSwedishList[trimmedImputedSwedishList$species == plotSpecies,]
    local <- imputedLocalList[imputedLocalList$species == plotSpecies,]
    
    fname<-as.character(plotSpecies)
    fname<-gsub("/", "_", fname) #replacing escape characters in species name
    
    
    yAxisAdjusted <- yAxisModifier(max(c(swedish$imputed+1.96*swedish$se_imp,local$imputed+1.96*local$se_imp)))
    
    
    gcomma <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE) #Called later, enables commas instead of points for decimal indication
    
    imputedCombined <- merge(swedish,local, by="time",all=TRUE)
    imputedCombined <- imputedCombined[, -c(3,5,6,7,8,10,11)]
    names(imputedCombined) <- c("time","species","imputed_sweden","imputed_local")
    
    
    
    cd1<-"as.theme(axis.text.y=element_text(size=42, #y-axis labels colour&size
          angle=0),axis.text.x=element_text(size=42,  #x-axis labels colour&size
          angle=0),plot.title = element_text(size=48, #Chart title size
          margin=margin(0,0,30,0)),panel.grid.major = element_blank(), #Distance between title and chart
          panel.grid.minor = element_blank(),panel.background = element_blank(), #Grid and background
          axis.line = element_line)"
    cd2<-"as.theme(axis.text.y=element_text(size=42, #y-axis labels colour&size
          angle=0),axis.text.x=element_text(size=42,  #x-axis labels colour&size
          angle=0),plot.title = element_text(size=48, #Chart title size
          margin=margin(0,0,30,0)),panel.grid.major = element_blank(), #Distance between title and chart
          panel.grid.minor = element_blank(),panel.background = element_blank(), #Grid and background
          axis.line = element_line)"
    
    mrgn1<-margin(0,0,80,0)
    mrgn2<-margin(0,0,30,0)
    if(nchar(fname)<18){mrgn<-mrgn1}else{mrgn<-mrgn2}
    
    names(imputedCombined) <- c("time","species","imputed_sweden","imputed_local")
    
    
    path<-paste(path,"/",sep="")
    png(filename=paste(path,noquote(fname),"_comparison.png",sep=""), width=748, height=868)
    fname<-gsub("/", "_", fname)#Restoring species name to original string that will appear in graph title
    Encoding(fname)<-'UTF-8'
    print((ggplot(data=imputedCombined, aes(x=time,y=imputed_sweden)) + geom_line(linetype="longdash",size=1.6) #central line #colour=rgb(155,187,89,max=255)
           + geom_line(linetype="solid",colour=sebms_palette[3],size=2.8, data=imputedCombined, aes(x=time,y=imputed_local)) #interval line 1
           # + xlim(startyear, endyear) #x-axis sectioning
           + expand_limits(x=2010)
           + geom_hline(yintercept = seq(from=0, to=yAxisAdjusted[1], by=yAxisAdjusted[2])) #Horizontal background lines #from=yAxisAdjusted[2]
           + scale_y_continuous(labels=gcomma,breaks=seq(from=0, to=yAxisAdjusted[1], by=yAxisAdjusted[2]),expand=c(0,0)) #y-axis sectioning & comma labelling #from=yAxisAdjusted[2]
           + scale_x_continuous(breaks= seq(2010,2020, by=5))
           + theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.margin = margin(8, 20, 0, 0)) #enable axis titles #axis.title.x=element_blank()
           + ggtitle(fname) #Chart title text
           + theme(axis.text.y=element_text(colour="black",size=42, #y-axis labels colour&size
                                            angle=0),axis.text.x=element_text(colour="black",size=42,  #x-axis labels colour&size
                                                                              angle=0),plot.title = element_text(size=48, #Chart title size
                                                                                                                 margin=mrgn),panel.grid.major = element_blank(), #Distance between title and chart
                   panel.grid.minor = element_blank(),panel.background = element_blank(), #Grid and background
                   axis.line = element_line(colour = "black"))) + theme(axis.ticks.length=unit(0.5, "cm"))+ theme(plot.title = element_text(hjust = 0.5))) #Axis colours
    
    dev.off() 
  } 
  
}


################################################################################
### Generate Index file for TRIM runs
#require(xlsx)

get_trimIndexOutputXLS<-function(horisontalImputedList=NULL, imputedList=NULL, slopeList=NULL, filename='Namnlös',startyear=2010,endyear=2023,path=getwd()){
  polishedImputedList <- imputedList
  polishedImputedList[polishedImputedList$converged==TRUE,]$converged <- 'JA'
  polishedImputedList[polishedImputedList$converged==FALSE,]$converged <- 'NEJ'
  names(polishedImputedList) <- c('Art','Ursprung','År','Index (imputerat)','Standard Error (imputerat)','Konvergerat')
  polishedImputedList$Art <- as.character(polishedImputedList$Art)
  
  polishedImputedList <- polishedImputedList[order(polishedImputedList$Art,polishedImputedList$'År'),]
  write.xlsx(horisontalImputedList, file = paste0(filename,'.xls'), sheetName=paste0('Tabell ',endyear), row.names=FALSE, showNA=FALSE)
  write.xlsx(polishedImputedList, file = paste0(filename,'.xls'), sheetName="Index", row.names=FALSE, append=TRUE, showNA=FALSE)
  write.xlsx(slopeList, file = paste0(filename,'.xls'), sheetName="Trender", row.names=FALSE, append=TRUE, showNA=FALSE)
}


################################################################################
### Generate list of slope statistics per species

get_slopeList<-function(trimIndex=get_trimIndex(), startyear=2010,endyear=2023,path=getwd()){
  singleList =list()
  for(i in 1:length(trimIndex)){
    if(inherits(trimIndex[[i]]$value, 'trim')){
      oa <- overall(trimIndex[[i]]$value)
      d <- data.frame(
        species=names(trimIndex[i]),
        avcount = round(mean(trimIndex[[i]]$value$time.totals$observed),1),
        nroute = trimIndex[[i]]$value$nsite,
        slope = round(oa$slope$mul, 4),
        se_slope = round(oa$slope$se_mul, 4),
        pctslope = round((oa$slope$mul - 1)*100, 1),
        p_slope = oa$slope$p,
        sig = ifelse(oa$slope$p<=0.05,
                     ifelse(oa$slope$p>0.01, '*',
                            ifelse(oa$slope$p>0.001, '**', '***')), 'NS'),
        interpret = oa$slope$meaning,
        stringsAsFactors = F)
      singleList[[i]] <- d
    }
  }    
  slopeList <- do.call(rbind, singleList)   
  
  names(slopeList) <- c('Art','Medelantal individer','Antal lokaler','Linjens lutning','Standard error för linjens lutning','Procentuell förändring per år', 'p-värde','Signifikans', 'Tolkning')
  slopeList$Art <- as.character(slopeList$Art)
  slopeList <- slopeList[order(slopeList$Art),]
}



################################################################################
### Generate horisontal list of TRIM statistics per species

get_horisontalImputedList<-function(trimIndex=get_trimIndex(),startyear=2010,endyear=2023,path=getwd()){
  singleList =list()
  for(i in 1:length(trimIndex)){
    if(inherits(trimIndex[[i]]$value, 'trim')){
      d1 <- data.frame(species=names(trimIndex[i]),Ind = round(mean(trimIndex[[i]]$value$time.totals$observed),1), Nrutt = trimIndex[[i]]$value$nsite, time=startyear:endyear)
      d2 <- index(trimIndex[[i]]$value)[, c('time','imputed')]
      d3 <- merge(d1, d2, all.x = T)
      d4 <- reshape(d3, timevar='time', idvar='species', v.names='imputed',times='', direction='wide')
      oa <- overall(trimIndex[[i]]$value)
      sig <- ifelse(oa$slope$p<=0.05,
                    ifelse(oa$slope$p>0.01, '*',
                           ifelse(oa$slope$p>0.001, '**', '***')), 'NS')
      singleList[[i]] <- cbind(d4, percslope=round((oa$slope$mul - 1)*100, 1), sign=sig)
      
      
    } else if(is.null(trimIndex[[i]])==FALSE){
      
      # d1 <- data.frame(species=names(trimIndex[i]),Ind = round(sum(trimIndex[[i]]$data$total_number, na.rm=TRUE)/(endyear-startyear+1),1), Nrutt =length(unique(trimIndex[[i]]$data$site)), time=startyear:endyear)
      d1 <- data.frame(species=names(trimIndex[i]),
                       Ind = round(sum(trimIndex[[i]]$data$total_number, na.rm=TRUE)/(length(unique((trimIndex[[i]]$data)[!is.na(trimIndex[[i]]$data$total_number),]$year))),1),
                       Nrutt =length(unique(trimIndex[[i]]$data$site)),
                       time=startyear:endyear)
      d2 <- data.frame(time=NA, imputed=NA)
      d3 <- merge(d1, d2, all.x = T)
      d4 <- reshape(d3, timevar='time', idvar='species', v.names='imputed',times='', direction='wide')
      singleList[[i]] <- cbind(d4, percslope=NA, sign=NA)
    }
    
  }    
  horisontalImputedList <- do.call(rbind, singleList)  
  
  names(horisontalImputedList) <- c('Art','Medelantal individer','Antal lokaler',startyear:endyear,'Procentuell förändring per år', 'Signifikans')
  horisontalImputedList$Art <- as.character(horisontalImputedList$Art)
  horisontalImputedList <- horisontalImputedList[order(horisontalImputedList$Art),]
  
}

################################################################################
### Run indicator analysis

get_indicatorAnalyses<-function(infile=get_imputedList(indicator_layout=TRUE), origin='Sverige', indicators = 'ALL',path=getwd()){
  require(BRCindicators)
  library(tidyverse)
  
  indicators<-list(
    grassland = c(67,19,26,117,40,50,70,8,119,55,110,101),
    agricultural = c(110,17,92,29,30,28,19,70,91,40,119,26,118,93),
    forest = c(19,71,120,46,105,109,118,38,95,115),
    common = c(118,119,38,30,17,92,91,71,28,117,29,105,77,70,19,46,73,90,115,72)) #c(118,38,30,115,17,92,91,19,105,112)) #c(118,119,38,30,17,92,91,71,28,117,29,105,77,70,19,46,73,90,115,72)) #c(118,38,30,115,17,92,91,19,105,112)
  
  indata <- infile %>% 
    mutate(species=as.factor(species), index=100*index,se=100*se)
  
  for (k in names(indicators)){
    print(paste0("Working on the ",k," indicator"))
    dat <-  filter(indata, species %in% indicators[[k]] )
    msi_out <- msi(dat,plotbaseyear=2010,SEbaseyear=2010, index_smooth='INDEX',lastyears=6,jobname=paste0(k,'.',origin))
    write.table(file=paste0(k,".csv"), sep=",", x=msi_out$results[2:8], row.names=FALSE)
  }
  
}

