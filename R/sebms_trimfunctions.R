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
  
  
  
  trimplots <- function(df, art) {
    #TODO make this plotting a function and run it in map per species instead. 
    if(inherits(df, 'trim')) {
      
      m2 <- df
      Index <- index(m2,base = min(m2$time.id)) # Calculates the Index value
      
      if(typeof(m2) == "list"){
        
        fname <- as.character({{ art }}) %>% 
          str_replace_all("/", "_") #replacing escape characters in species name
        #print(m2)
        
        yAxisAdjusted <- yAxisModifier(max(Index$imputed + 1.96*Index$se_imp))
        
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
        
        # mrgn1 <- margin(0,0,80,0)
        # mrgn2 <- margin(0,0,30,0)
        # 
        titles <- case_when(nchar(fname) < 18  ~ paste0(fname ,' ',"(", m2$nsite, " lokaler)"),
                            str_detect(fname, "_") ~ glue("{str_replace_all(fname, '_', '\n')} \n({m2$nsite} lokaler)"),
                            TRUE ~ paste0(fname ,' ',"\n(", m2$nsite, " lokaler)"))
        
        if(nchar(fname) < 18) {
          mrgn  <- margin(0,0,80,0)
        }else {
          mrgn <- margin(0,0,30,0)
        }
        
        Encoding(fname) <- 'UTF-8'
        
        Index %>% 
          ggplot(aes(x = time,y = imputed)) +
          geom_line(linetype = paste(lt),
                    colour = paste(col),
                    linewidth = 2.8) +#central line #colour=rgb(155,187,89,max=255)
          geom_line(aes(x = time,
                        y = imputed-1.96*se_imp),
                    linetype = "longdash",
                    linewidth = 1.6) +#interval line 1
          geom_line(aes(x = time,
                        y = imputed+1.96*se_imp),
                    linetype = "longdash",
                    linewidth = 1.6) +#interval line 2
          # + xlim(startyear, endyear) #x-axis sectioning
          expand_limits(x = 2010)+
          geom_hline(yintercept = seq(from = 0, to = yAxisAdjusted[1], by=yAxisAdjusted[2])) +#Horizontal background lines #from=yAxisAdjusted[2]
          scale_y_continuous(labels = gcomma,
                             breaks = seq(from = 0, to = yAxisAdjusted[1], by = yAxisAdjusted[2]),
                             expand = c(0,0)) +#y-axis sectioning & comma labelling #from=yAxisAdjusted[2]
          scale_x_continuous(breaks = seq(min(year),max(year), by = 5))+
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
  spname <- names(trimIndex) %>% 
    str_replace_all("/", "_") %>% 
    list()
  
  ggs <- map2(trimIndex, spname, ~trimplots(.x, .y), .progress = "Making trimplots...")
  
  walk2(ggs, spname, ~ggsave(plot = .x, filename = glue("{.y}.png"), width = 748, height = 868, units = "px", dpi = 72), .progress = "Saving trimplots...")
  
  
}


################################################################################
### Create and save local TRIM plots with national TRIM reference

get_trimComparedPlots<-function(imputedLocalList=NULL, trimmedImputedSwedishList=NULL, startyear=2010,endyear=2023,path=getwd()){
  
  if (is.null(imputedLocallist)) {
    imputetLocallist <- get_imputetlist(origin = 'sverige', indicator_layout = FALSE, year= 2010:2023) 
  }
  
  imputedLocalList <- fct_drop(imputedLocalList)
  trimmedImputedSwedishList <- fct_drop(trimmedImputedSwedishList)
  
  for(i in 1:length(levels(as.factor(imputedLocalList$species)))){
    
    plotSpecies <-  levels(as.factor(imputedLocalList$species))[i]   
    
    swedish <- trimmedImputedSwedishList[trimmedImputedSwedishList$species == plotSpecies,]
    local <- imputedLocalList[imputedLocalList$species == plotSpecies,]
    
    fname <- plotSpecies %>%
      as.character() %>%
      str_replace("/", "_") #replacing escape characters in species name
    
    
    yAxisAdjusted <- yAxisModifier(max(c(swedish$imputed+1.96*swedish$se_imp,local$imputed+1.96*local$se_imp)))
    
    
    gcomma <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE) #Called later, enables commas instead of points for decimal indication
    
    imputedCombined <- merge(swedish,local, by="time",all=TRUE)
    imputedCombined <- imputedCombined[, -c(3,5,6,7,8,10,11)]
    names(imputedCombined) <- c("time","species","imputed_sweden","imputed_local")
    
    
    mrgn1 <- margin(0,0,80,0)
    mrgn2 <- margin(0,0,30,0)
    if_else(nchar(fname) < 18, mrgn = mrgn1, mrgn = mrgn2)
    
    names(imputedCombined) <- c("time","species","imputed_sweden","imputed_local")
    
    fname <- str_replace_all(fname, "/", "_")#Restoring species name to original string that will appear in graph title
    
    Encoding(fname) <- 'UTF-8'
    
    imputedCombined %>% 
      ggplot(aes(x = time, y = imputed_sweden)) + 
      geom_line(linetype = "longdash", linewidth = 1.6) + #central line #colour=rgb(155,187,89,max=255)
      geom_line(aes(x = time, y = imputed_local), linetype = "solid", colour = sebms_palette[3], linewidth = 2.8, ) + #interval line 1
      # + xlim(startyear, endyear) #x-axis sectioning
      expand_limits(x = 2010) +
      geom_hline(yintercept = seq(from = 0, to = yAxisAdjusted[1], by = yAxisAdjusted[2])) + #Horizontal background lines #from=yAxisAdjusted[2]
      scale_y_continuous(labels = gcomma,
                         breaks = seq(from = 0, to = yAxisAdjusted[1], by = yAxisAdjusted[2]),
                         expand = c(0,0)) + #y-axis sectioning & comma labelling #from=yAxisAdjusted[2]
      scale_x_continuous(breaks = seq(min(year), max(year), by = 5)) +
      #enable axis titles #axis.title.x=element_blank()
      labs(title = fname, x = NULL, y = NULL) + #Chart title text
      theme(plot.margin = margin(8, 20, 0, 0),
            plot.title = element_text(size = 48, #Chart title size
                                      margin = mrgn,
                                      hjust = 0.5),
            axis.ticks.length = unit(0.5, "cm"),
            axis.text.y = element_text(colour = "black",
                                       size = 42, #y-axis labels colour&size
                                       angle = 0),
            axis.text.x = element_text(colour = "black",
                                       size = 42,  #x-axis labels colour&size
                                       angle = 0),
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(), #Distance between title and chart
            panel.grid.minor = element_blank(),
            panel.background = element_blank() #Grid and background
      ) #Axis colours
    
    
  } 
  
  ggsave(filename=glue("{fname} _comparison.png", sep=""), width=748, height=868)
}

