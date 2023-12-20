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
#' @inheritParams sebms_abundance_per_species_plot
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
get_trimInfile <- function(year=2010:2023, Art = 1:200, Län = ".", Landskap = ".", Kommun = ".", filterPattern=NULL, topList=FALSE, topNumber=200, source = c(54,55,56,63,64,66,67)){
  
  trimSpecies <- sebms_trimSpecies(year = year, Art = Art, topList = topList, source = source) %>% 
    slice_head(n=topNumber)
  
  spein <- function(df = data, speuid) {
    
    #print(paste("Working on species with ID",speuid))
    minw <- df %>% pull(min) # first posible week of observation
    maxw <- df %>% pull(max) # öast possible week of observation
    
    obses <- sebms_trimobs(year = year, Art = speuid, Län = Län, Landskap = Landskap, Kommun = Kommun, filterPattern = filterPattern, minmax = minw:maxw, source = source) %>% 
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
#' @importFrom lubridate year today
#' @importFrom stringr str_detect
#' 
#' @return a trim file with yearly changes of each species. 
#' @export
get_trimIndex <- function(infile=NULL, year = 2010:lubridate::year(lubridate::today()), Art = 1:200, ...) {
  
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
      # set_names(infile$speuid) %>% 
      set_names(infile$art)
    
    return(trimList)}
  else {return(infile)}
}



#' Palette Used in ggplots for Trim Index
#' 
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
#' @importFrom purrr map2 walk2
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
          mrgn <- margin(0,0,80,0)
        }else {
          mrgn <- margin(0,0,30,0)
        }
        
        Encoding(fname) <- 'UTF-8'
        
        Index %>% 
          ggplot(aes(x = time,y = imputed)) +
          geom_line(linetype = paste(lt),
                    colour = paste(col),
                    linewidth = 2.8) + #central line #colour=rgb(155,187,89,max=255)
          geom_line(aes(x = time,
                        y = imputed - 1.96*se_imp),
                    linetype = "longdash",
                    linewidth = 1.6) + #interval line 1
          geom_line(aes(x = time,
                        y = imputed + 1.96*se_imp),
                    linetype = "longdash",
                    linewidth = 1.6) + #interval line 2
          # + xlim(startyear, endyear) #x-axis sectioning
          expand_limits(x = min(year), y = c(0,yAxisAdjusted[1])) +
          # geom_hline(yintercept = seq(from = 0, to = yAxisAdjusted[1], by = yAxisAdjusted[2])) +#Horizontal background lines #from=yAxisAdjusted[2]
          scale_y_continuous(labels = gcomma,
                             breaks = seq(from = 0, to = yAxisAdjusted[1], by = yAxisAdjusted[2]),
                             expand = c(0,0)) +#y-axis sectioning & comma labelling #from=yAxisAdjusted[2]
          scale_x_continuous(breaks = seq(min(year),max(year), by = 5))+
          labs(title = titles) +#Chart title text
          theme(text = element_text(family = "Arial"),
                plot.title = element_text(hjust = 0.5, # Centered
                                          size = 48, #Chart title size
                                          margin = mrgn), #Distance between title and chart
                panel.grid.major.y = element_line(linewidth = 1, colour = "grey40"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), #Grid and background
                axis.text.x = element_text(colour = "black", size = 42,  #x-axis labels colour&size 
                                           angle = 0),
                axis.text.y = element_text(colour = "black",
                                           size = 42, #y-axis labels colour&size
                                           angle = 0),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_line(linewidth = 1, colour = "grey40"),
                axis.ticks.length = unit(4, "mm"),
                plot.margin = margin(8, 20, 0, 0),
                axis.line = element_line(colour = "black") #Axis colours
          ) 
      }
    }
  }
  
  ggs <- vector("list", length = length(trimIndex))
  spname <- names(trimIndex) %>% 
    str_replace_all("/", "_")
  
  ggs <- map2(trimIndex, spname, ~trimplots(.x, .y), .progress = "Making trimplots...")
  
  walk2(ggs, spname, ~ggsave(plot = .x, filename = glue("{.y}.png"), width = 748, height = 868, units = "px", dpi = 72), .progress = "Saving trimplots...")
  
}



### 

#' Generate List of Imputed Values per Species
#'
#'
#' @inheritParams get_trimInfile
#' @param origin the origin of list species
#' @param indicator_layout logical; whether the list should contain the 
#' @param ... extra filter parameters passed to the [trimInfile()] function
#' 
#' @importFrom lubridate year today
#' @importFrom dplyr bind_rows bind_cols select
#' @importFrom stringr str_replace_all
#' @importFrom rtrim index
#' @importFrom purrr map2 list_rbind
#'
#' @return a data frame with trim indices per species
#' @export
get_imputedList <- function(trimIndex = NULL, Art = 1:200, Län = ".", Landskap = ".", Kommun = ".", indicator_layout = FALSE, year = 2010:lubridate::year(lubridate::today()), ...) {
  
  if(is.null(trimIndex)) { # If there is no trimIndex
    
    arglist <- list(...)
    if(!is.null(arglist$filterPattern)) { # If you have used filterPattern
      
      fp <- arglist$filterPattern
      infiletrimIndex = get_trimInfile(year = year, filterPattern = fp) %>% 
        get_trimIndex()
      
    }else { # If no filterPattern is used in ...
      
      trimIndex <- get_trimInfile(year = year, Art = Art, Län = Län, Landskap = Landskap, Kommun = Kommun) %>% 
        get_trimIndex(year = year)
    }
    
  } # If there were a trimIndex file supplied, use that
  
  trimspelist <- function(df, art) {
    
    if(inherits(df, 'trim')) {
      
      if (all(Län == ".",Landskap == ".",Kommun == ".")) {
        origin = "Sweden"
      }else {
      origin <- glue("{Län}{Landskap}{Kommun}") %>% str_remove_all("\\.") %>% str_replace_all(" ", "-")
      }
      bind_cols(#spe_uid = speuid,
        species = as.character({{ art }}) %>% str_replace_all("/", "_"),
        origin = as.character(origin),
        index(df),
        converged = df$converged)
    }
  }
  
  imputedList = vector("list", length = length(trimIndex))
  
  spname <- names(trimIndex) %>% 
    str_replace_all("/", "_")
  
  imputedList <- map2(trimIndex, spname, ~trimspelist(.x, .y)) %>% 
    list_rbind() 
  
  if(indicator_layout == TRUE) { # If you want indicator layout
    
    imputedList <- imputedList %>% 
      select(origin, species, year = time, index = imputed, se = se_imp)
    #names(imputedList) <- c('origin', 'year', 'index', 'se_imp', 'converged')
  }else { # If only list is wanted
    imputedList <- imputedList# %>% 
    #select(-spe_uid)
  }
  
  # Add speuid to list
  imputedList <- sebms_trimSpecies(Art = Art) %>% 
    select(speuid, art) %>% 
    right_join(imputedList, by = c("art" = "species"))
  
  return(imputedList)
}



#' Create and Save Local TRIM Plots with National TRIM Reference
#'
#' @inheritParams get_trimInfile
#' @param trimmedImputedSwedishList data frame of national wide data of species
#'   trim values
#'
#' @importFrom lubridate year today
#' @importFrom stringr str_replace_all
#' @importFrom tidyr nest
#' @importFrom purrr map2 walk2
#' @import dplyr
#' @import ggplot2
#'
#' @return figures saved as png comparing national and local trim indices
#' @export
get_trimComparedPlots <- function(Län = ".", Landskap = ".", Kommun = ".", Art = 1:200, trimmedImputedSwedishList=NULL, year = 2010:lubridate::year(lubridate::today())) {
  
  #1 Run trim index on species with local data
  #2 Of the local species not all may be possible to run
  #3 For the remaining species that did run through thte local trim calc run those species on Swedish data for Sweden.
  
  imputedLocalList <- get_imputedList(origin = 'sverige', Art = Art, Län = Län, Landskap = Landskap, Kommun = Kommun, indicator_layout = FALSE, year = year) 
  
  if (is.null(trimmedImputedSwedishList)) {
    
    speuid <- imputedLocalList %>% pull(speuid)
    
    trimmedImputedSwedishList <- get_imputedList(origin = 'sverige', Art = c(speuid), indicator_layout = FALSE, year = year) 
  }
  
  plotcomp <- function(df, art) {
    
    swedish <- trimmedImputedSwedishList %>% 
      filter(species == {{ art }})
    
    local <- imputedLocalList %>% 
      filter(species == {{ art }})
    
    fname <- as.character({{ art }}) %>%
      str_replace_all("/", "_") #replacing escape characters in species name
    
    
    yAxisAdjusted <- yAxisModifier(max(c(swedish$imputed+1.96*swedish$se_imp,local$imputed+1.96*local$se_imp)))
    
    gcomma <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE) #Called later, enables commas instead of points for decimal indication
    
    imputedCombined <- swedish %>% 
      full_join(local, by = c("species", "time")) %>%
      transmute(time = time, species = species, imputed_sweden = imputed.x, imputed_local = imputed.y)
    
    if(nchar(fname) < 18) {
      mrgn <- margin(0,0,80,0, unit = "pt")
    }else{
      mrgn <- margin(0,0,30,0, unit = "pt")
    }
    
    Encoding(fname) <- 'UTF-8'
    
    imputedCombined %>% 
      ggplot(aes(x = time)) + 
      geom_line(aes(y = imputed_local), linetype = "solid", colour = sebms_palette[2], linewidth = 2.8) + #interval line 1
      geom_line(aes(y = imputed_sweden), linetype = "longdash", linewidth = 1.6) + #central line #colour=rgb(155,187,89,max=255)
      # + xlim(startyear, endyear) #x-axis sectioning
      expand_limits(x = min(year), y = c(0, yAxisAdjusted[1])) +
      #geom_hline(yintercept = seq(from = 0, to = yAxisAdjusted[1], by = yAxisAdjusted[2])) + #Horizontal background lines #from=yAxisAdjusted[2]
      scale_y_continuous(labels = gcomma,
                         breaks = seq(from = 0, to = yAxisAdjusted[1], by = yAxisAdjusted[2]),
                         expand = c(0,0)) + #y-axis sectioning & comma labelling #from=yAxisAdjusted[2]
      scale_x_continuous(breaks = seq(min(year), max(year), by = 5)) +
      #enable axis titles #axis.title.x=element_blank()
      labs(title = fname, x = NULL, y = NULL) + #Chart title text
      theme(text = element_text(family = "Arial"),
            plot.margin = margin(8, 20, 0, 0),
            plot.title = element_text(size = 48, #Chart title size
                                      margin = mrgn,#Distance between title and chart
                                      hjust = 0.5),
            panel.background = element_blank(), #Grid and background
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(linewidth = 1, colour = "grey40"),
            panel.grid.minor = element_blank(),
            axis.ticks = element_line(linewidth = 1, colour = "grey40"),
            axis.ticks.length = unit(0.5, "cm"),
            axis.text.y = element_text(colour = "black", #y-axis labels colour&size
                                       size = 42,
                                       angle = 0),
            axis.text.x = element_text(colour = "black", #x-axis labels colour & size
                                       size = 42,
                                       angle = 0),
            axis.line = element_line(colour = "black") #Axis colours
      )
    
    
  } 
  
  ggs <- imputedLocalList %>% 
    group_by(species) %>% 
    nest() %>% 
    ungroup() %>% 
    mutate(plots = map2(data, species, ~plotcomp(.x, .y)))
  walk2(ggs$plots, ggs$species, ~ggsave(plot = .x, filename = glue("{.y}_comparison.png"), width=748, height=868, dpi = 72, units = "px"))
  
}


#' Indicator Species for Trim Index
#'
#' @return list of species uids for 'grassland', 'agricultural', 'forest', and
#'   'common20' species
#' @export
indicatorlist <- list(grassland = c(67,19,26,117,40,50,70,8,119,55,110,101),
                      agricultural = c(110,17,92,29,30,28,19,70,91,40,119,26,118,93),
                      forest = c(19,71,120,46,105,109,118,38,95,115),
                      common20 = c(118,119,38,30,92,17,91,71,117,28,29,70,105,19,90,77,46,73,89,115))



#' Run Indicator Analysis
#'
#' @inheritParams get_imputedList
#' @param infile list of imputed index from [get_imputedList(indicator_layout =
#'   TRUE)]
#' @param indicators optional; you can add a concatenated list of indicator
#'   species uids for a new indicator
#' @param indicatorname the name of the new indicator. If indicators is given
#'   but without setting name the indicator will be named 'NewInd'
#'
#' @importFrom BRCindicators msi
#' @import dplyr
#' @importFrom glue glue
#' @importFrom readr write_csv2
#' @importFrom purrr set_names
#'
#' @return
#' @export
get_indicatorAnalyses <- function(infile = NULL, baseyear = 2010, Län = ".", Landskap = ".", Kommun = ".", indicators = NULL, indicatorname = NULL) {
  
  if(!is.null(indicators)) { # If a new indicator is added
    # If no new name is added
    if(is.null(indicatorname)) {
      indicatorname <- "NewInd"
    }
    # Add the new indicator as a list item to 'indicatorlist'
    indicatorlist <- list(indicators) %>%
      set_names(indicatorname) %>% 
      append(indicatorlist)
    
    # indicatorlist <- map(list(indicators), ~list(.x)) %>% 
    #   set_names(indicatorname) %>% 
    #   list_flatten() %>% 
    #   c(indicatorlist) %>% 
    #   list_flatten()
  }

  speid <- unlist(indicatorlist, use.names = F) %>% 
    unique()
  
  if(is.null(infile)) {
    indata <- get_imputedList(Art = c(speid), indicator_layout = TRUE, Län = Län, Landskap = Landskap, Kommun = Kommun) %>%
      transmute(origin,
                speuid,
                species = as.factor(art),
                year = as.double(year),
                index = 100 * index,
                se = 100 * se)
  }else {
    indata <- infile %>%
      mutate(species = as.factor(species),
             index = 100 * index,
             se = 100 * se)
  }
  
  
  for (k in names(indicatorlist)){
    print(paste0("Working on the ",k," indicator"))
    
    dat <- indata %>% 
      filter(speuid %in% indicatorlist[[k]]) %>% 
      select(-origin, -speuid)
    
    msi_out <- msi(dat, plotbaseyear = baseyear, SEbaseyear = baseyear, index_smooth = 'INDEX', lastyears = 10, jobname = paste0(k,'.',origin))
    
    write_csv2(file = glue("{k}_indicator_in_{indata %>% pull(origin) %>% unique()}.csv"), x = msi_out$results)
  }
  
}
