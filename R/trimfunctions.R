### trimfunctions_Swedishbutterflies.R
###
### Author: LP
### Date: 25 October 2017
### Modified: 2018-11-21
###
### Rewritten: Georg A
### Date: January 2024
###


#' Create an Object with Species Number per Year and Site with Visit Frequency
#'
#' Create a tibble with species ID and name together with the total number of observations of each species and the frequency (1 / #visits)
#'
#' @inheritParams sebms_abundance_per_species_plot
#' @param years the year span of interest, set as 'firstyear:lastyear'.
#' @param Art the species of interest
#' @param Region character or reg ex; which region do you want. Possible values are:
#'   'SGot', 'OGot', 'VGotSve', 'OSve', 'NSveNor', 'NNor'.

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
get_trimInfile <- function(years=2010:lubridate::year(lubridate::today())-1, Art = 1:200, Län = ".", Region = ".", Landskap = ".", Kommun = ".", filterPattern=NULL, topList=FALSE, topNumber=200, verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84)){

  trimSpecies <- sebms_trimSpecies(year = years, Art = Art, topList = topList, source = source) %>%
    distinct(speuid, art, .keep_all = T) %>%
    slice_head(n=topNumber)

  spein <- function(df = data, speuid) {

    #print(paste("Working on species with ID",speuid))
    minW <- df %>% pull(min) # first posible week of observation
    maxW <- df %>% pull(max) # last possible week of observation

    obses <- sebms_trimobs(year = years, Art = speuid, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, filterPattern = filterPattern, minmax = minW:maxW, verification = verification, source = source) %>%
      mutate(total_number = as.numeric(total_number)) %>%
      summarise(total_number = sum(total_number, na.rm = T), .by = c(siteuid, year, län, landskap, kommun))

    visits <- sebms_trimvisits(year = years, minmax = minW:maxW, source = source) %>%
      mutate(visit = as.numeric(visit))

    if(nrow(obses) > 0) { #Precondition to skip species with zero observations

      ## TRIM infile generation (If species have been seen any year in 'year' all site with a visit get 'total_number' of 0. Non-visited sites any year gets a NA)
      obsTidy <- obses %>%
        complete(siteuid, year = seq(min(years), max(years), by = 1), fill = list(total_number = 0)) %>%
        left_join(visits, by = c("siteuid", "year")) %>%
        mutate(total_number = if_else(is.na(visit), NA, total_number),
               visit = if_else(is.na(visit), 1, visit),
        ) %>%
        mutate(freq = 1/visit) %>%
        select(-visit)

      ## Lars code that works now
      # obsTidyLP <- obses %>%
      #   complete(siteuid,
      #            year = seq(min(years), max(years), by = 1),
      #            fill = list(total_number = 99999)) %>%
      #   left_join(visits, by = c("siteuid", "year")) %>%
      #   mutate(total_number = ifelse(is.na(visit), NA, total_number)) %>%
      #   mutate(visit = ifelse(is.na(visit), 1, visit)) %>%
      #   mutate(total_number = ifelse(total_number == 99999, 0, total_number)) %>%
      #   mutate(freq = 1 / visit) %>%
      #   select(-visit) %>%
      #   mutate(total_number = as.numeric(total_number)) %>%
      #   filter(year %in% years) #This is in order to avoid a wrong number of sites by counting sites monitored after the year of the report

    }else{
      message(paste("Species with ID ",speuid," skipped, no observations!"))
    }

    return(obsTidy)
  }

  Infile <- trimSpecies %>%
    group_by(speuid, art) %>%
    nest() %>%
    ungroup() %>%
    mutate(obslist = map2(data, speuid, possibly(~spein(df = .x, speuid = .y)), .progress = "Making trimspecies infile...")) %>%
    select(-data) %>%
    unnest(obslist)

  return(Infile)
}


#' Calculate TRIM Index
#'
#' @inheritParams get_trimInfile
#' @param infile file with site, year, total observations and reverse frequency weight (1/#visits), or an object from [get_trimInfile()]
#' @param ... further arguments to pass to [get_trimInfile()]
#'
#' @importFrom rtrim trim
#' @import dplyr
#' @importFrom tidyr nest
#' @importFrom purrr map possibly set_names
#' @importFrom lubridate year today
#' @importFrom stringr str_detect
#'
#' @return a trim file with yearly changes of each species.
#' @export
get_trimIndex <- function(infile=NULL, years = 2010:lubridate::year(lubridate::today())-1, Art = 1:200, Län = ".", Region = ".", Landskap = ".", Kommun = ".", verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84), ...) {

  if(is.null(infile)) {
    arglist <- list(...)

    if(!is.null(arglist$filterPattern)) { #If a filterpattern have been given
      fp <- arglist$filterPattern
      infile <- get_trimInfile(filterPattern = fp, years = years, Art = Art, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source) %>%
        select(siteuid, year, total_number, freq) %>%
        group_by(speuid) %>%
        nest() %>%
        ungroup()
    }else{ # If there is no filterpattern
      infile <- get_trimInfile(years = years, Art = Art, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source)
      if (nrow(infile) > 0) {
        infile <- infile %>%
          select(site = siteuid, speuid, art, year, total_number, freq) %>%
          group_by(speuid, art) %>%
          nest() %>%
          ungroup()
      }
    }
  }else { # If there is a 'infile' given
    infile <- infile %>%
      select(site = siteuid, speuid, art, year, total_number, freq) %>%
      group_by(speuid, art) %>%
      nest() %>%
      ungroup()
  }

  # Make trim with set arguments into function to make it cleaner by the map() function below
  trimfun <- function(df){
    rtrim::trim(total_number ~ site + year, data = df, weights = "freq",  model = 2, serialcor = TRUE,overdisp = TRUE, changepoints = "all",autodelete = TRUE, max_iter = 1000)
  }

  if(length(infile) != 0) {

    trimList <- map(infile$data, possibly(trimfun), .progress = "Run trimfunction...") %>%
      suppressWarnings() %>%
      # set_names(infile$speuid) %>%
      set_names(infile$art)

    return(trimList)
  }else { # If infile is of zero length
    return(infile)
  }
}



####
###

#' Create and Save TRIM Plots
#'
#' @inheritParams get_trimInfile
#' @param trimIndex optional; a trimIndex object from the [get_trimIndex()]
#' @param years the years to calculate trim index on, ignored if a trimIndex file is given
#' @param Art the species of interest, ignored if trimIndex is not NULL
#' @param write logical; if you want to write result to csv files, default TRUE
#' @param print logical; if you want to print result to output, default FALSE

#' @param ... optional; other arguments passed on to [get_trimInfile()]
#'
#' @import ggplot2
#' @importFrom stringr str_replace
#' @importFrom glue glue
#' @importFrom rtrim index overall
#' @importFrom purrr map2 walk2 discard
#'
#' @return figures in png format of the species trends with confidence interval
#' @export
get_trimPlots <- function(trimIndex = NULL, years = 2010:lubridate::year(lubridate::today())-1, Art = 1:200, Län = ".", Landskap = ".", Kommun = ".", filepath = getwd(), tag = NULL, xaxis_sep = 5, verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84), write = TRUE, print = TRUE, ...) {

  # This creates a trimIndex file if none is provided
  if(is.null(trimIndex)) {

    arglist <- list(...)

    if(!is.null(arglist$filterPattern)){ # If you need a filter for the trimInfile creation

      fp <- arglist$filterPattern

      trimIndex <- get_trimInfile(years = years, Art = Art, filterPattern = fp, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source) %>%
        get_trimIndex()

    }else{ # If you want to use the defaults
      trimIndex <- get_trimIndex(years = years, Art = Art, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source)
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
          expand_limits(x = min(years), y = c(0,yAxisAdjusted[1])) +
          # geom_hline(yintercept = seq(from = 0, to = yAxisAdjusted[1], by = yAxisAdjusted[2])) +#Horizontal background lines #from=yAxisAdjusted[2]
          scale_y_continuous(labels = gcomma,
                             breaks = seq(from = 0, to = yAxisAdjusted[1], by = yAxisAdjusted[2]),
                             expand = c(0,0)) +#y-axis sectioning & comma labelling #from=yAxisAdjusted[2]
          scale_x_continuous(breaks = seq(min(years),max(years), by = xaxis_sep)) +
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

  trimIndex <- trimIndex %>% discard(is.null)

  ggs <- vector("list", length = length(trimIndex))
  spname <- names(trimIndex) %>%
    str_replace_all("/", "_")

  ggs <- map2(trimIndex, spname, ~trimplots(.x, .y), .progress = "Making trimplots...")

  #set tag
  if (is.null(tag)) {
    tag = ""
  }else {
    tag = glue("_{tag}")
  }
  #set filepath
  filepath <- normalizePath(filepath)

  if (write) {
    walk2(ggs, spname, ~ggsave(plot = .x, filename = glue("{filepath}/{.y}{tag}.png"), width = 748, height = 868, units = "px", dpi = 72), .progress = "Saving trimplots...")
  }

  if (print) {
    print(ggs)
  }
}


#' Generate List of Imputed Values per Species
#'
#'
#' @inheritParams get_trimInfile
#' @param trimIndex a trim index object from [get_trimindex()]
#' @param indicator_layout logical; whether to use the indicator species and add
#'   nr of sites statistics. If TRUE this overrides `Art`
#' @param write logical; if index should be written to csv
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
get_imputedList <- function(trimIndex = NULL, years = 2010:lubridate::year(lubridate::today())-1, Art = 1:200, Län = ".", Region = ".", Landskap = ".", Kommun = ".", filepath = getwd(), tag = NULL, indicator_layout = FALSE, verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84), write = FALSE, ...) {

  if (indicator_layout) {
    speid <- unlist(indicatorlist, use.names = F) %>%  # 'indicatorlist' is loaded by package
      unique()
  }else {
    speid <- Art
  }

  if(is.null(trimIndex)) { # If there is no trimIndex

    arglist <- list(...)
    if(!is.null(arglist$filterPattern)) { # If you have used filterPattern

      fp <- arglist$filterPattern
      infiletrimIndex = get_trimInfile(years = years, filterPattern = fp, verification = verification, source = source) %>%
        get_trimIndex()

    }else { # If no filterPattern is used in ...

      trimIndex <- get_trimInfile(years = years, Art = speid, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source) %>%
        get_trimIndex(years = years)
    }

  } # If there were a trimIndex file supplied, use that

  trimspelist <- function(df, art) {

    if(inherits(df, 'trim')) {

      if (all(Län == ".",Landskap == ".",Kommun == ".")) {
        origin = "Sweden" # If no region was selected use Sweden
      }else {
        origin <- glue("{Län}{Landskap}{Kommun}") %>% str_remove_all("\\.") %>% str_replace_all(" ", "-") # If any region was chosen, add that to origin
      }

      bind_cols(#spe_uid = speuid,
        art = as.character({{ art }}) %>% str_replace_all("/", "_"),
        origin = as.character(origin),
        index(df),
        nsite = df$nsite,
        converged = df$converged
      )
    }
  }


  spname <- names(trimIndex) %>%
    str_replace_all("/", "_")

  imputedList <- map2(trimIndex, spname, ~trimspelist(.x, .y)) %>%
    list_rbind()

  # Add speuid to list
  trendList <- sebms_trimSpecies(Art = speid) %>%
    select(speuid, art) %>%
    mutate(art = str_replace_all(art, "/", "_")) %>%
    right_join(imputedList, by = c("art"))

  if(indicator_layout) { # If you want indicator layout

    sitecalc <- function(indic){
      trendList %>%
        filter(speuid %in% indic) %>%
        mutate(maxsite = round(max(nsite)),
               minsite = round(min(nsite)),
               meansite = round(mean(nsite)),
               mediansite = round(median(nsite)),
               sdsite = round(sd(nsite))
        )
    }

    imputedList <- map(indicatorlist, sitecalc) %>%
      bind_rows(.id = "indicator") %>%
      select(indicator, origin, speuid, art, year = time, index = imputed, se = se_imp, nsite, maxsite, minsite, meansite, mediansite, sdsite)

  }else { # If NO indicator layout is wanted
    imputedList <- trendList %>%
      select(origin, speuid, art, year = time, index = imputed, se = se_imp, converged)
  }


  if (write) {
    #set tag
    if (is.null(tag)) {
      tag = ""
    }else {
      tag = glue("_{tag}")
    }
    #set filepath
    filepath <- normalizePath(filepath)

    Year <- glue("{min(years)}-{max(years)}")
    write_csv2(imputedList, glue("{filepath}/Index_{Year}{tag}.csv"))
  }
  return(imputedList) # Called Index.csv in LP code
}



#' Generate Trend Index for Each Species
#'
#' Generate a file with the trend for each species and the significans of model.
#' Also show the number of sites where species existed.
#'
#' @inheritParams get_imputedList
#' @param indicators logical; if TRUE use the indicators as species selection,
#'   this override `Art`
#' @import dplyr
#' @importFrom lubridate year today
#' @importFrom glue glue
#' @importFrom stringr str_remove_all str_replace_all
#' @importFrom readr write_csv2
#' @importFrom rtrim overall
#' @importFrom purrr map list_rbind
#'
#' @return trendindex per species with the number of sites used
#' @export
get_trendIndex <- function(trimIndex = NULL, years = 2010:lubridate::year(lubridate::today())-1, Art = 1:200, Län = ".", Region = ".", Landskap = ".", Kommun = ".", filepath = getwd(), tag = NULL, indicators = TRUE, verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84), write = FALSE, ...) {

  if(is.null(trimIndex)) { # If there is no trimIndex

    if (indicators) { # if indicator species should be used
      warning("indicators is set to TRUE and the Indicator species will be used!", immediate. = T)
      speid <- unlist(indicatorlist, use.names = F) %>%  # 'indicatorlist' is loaded by package
        unique()

      trimIndex <- get_trimInfile(years = years, Art = speid, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source) %>%
        get_trimIndex(years = years)

    }else { # if your own selection of species should be used
      trimIndex <- get_trimInfile(years = years, Art = Art, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source) %>%
        get_trimIndex(years = years)

    }
  } # If there were a trimIndex file supplied, use that

  trimspelist <- function(df, art) {

    if(inherits(df, 'trim')) {

      if (all(Län == ".",Landskap == ".",Kommun == ".")) {
        origin = "Sweden" # If no region was selected use Sweden
      }else {
        origin <- glue("{Län}{Landskap}{Kommun}") %>% str_remove_all("\\.") %>% str_replace_all(" ", "-") # If any region was chosen, add that to origin
      }

      bind_cols(#spe_uid = speuid,
        art = as.character({{ art }}) %>% str_replace_all("/", "_"),
        origin = as.character(origin),
        overall(df)$slope,
        nsite = df$nsite
      )
    }
  }

  spname <- names(trimIndex) %>%
    str_replace_all("/", "_")

  trendList <- map2(trimIndex, spname, ~trimspelist(.x, .y)) %>%
    list_rbind()



  # Add speuid to list and select variables
  trendList <- sebms_trimSpecies(Art = Art) %>%
    select(speuid, art) %>%
    right_join(trendList, by = c("art")) %>%
    select(origin, speuid, art, nsite, add, mul, p, meaning)


  if (write) {
    #set tag
    if (is.null(tag)) {
      tag = ""
    }else {
      tag = glue("_{tag}")
    }
    #set filepath
    filepath <- normalizePath(filepath)

    Year <- glue("{min(years)}-{max(years)}")
    #write_csv2(imputedList, glue("Index_{Year}.csv"))
    write_csv2(trendList, glue("{filepath}/Trendindex_{Year}{tag}.csv"))
  }
  return(trendList)
}


#' Create and Save Local TRIM Plots with National TRIM Reference
#'
#' @inheritParams get_trimPlots
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
get_trimComparedPlots <- function(years = 2010:lubridate::year(lubridate::today())-1, Art = 1:200, Län = ".", Region = ".", Landskap = ".", Kommun = ".", filepath= getwd(), tag = NULL, trimmedImputedSwedishList=NULL, verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84), write = TRUE, print = TRUE) {

  #1 Run trim index on species with local data
  #2 Of the local species not all may be possible to run
  #3 For the remaining species that did run through thte local trim calc run those species on Swedish data for Sweden.

  imputedLocalList <- get_imputedList(years = years, Art = Art, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source)

  if (is.null(trimmedImputedSwedishList)) {

    speuid <- imputedLocalList %>% distinct(speuid) %>% pull(speuid)

    trimmedImputedSwedishList <- get_imputedList(years = years, Art = c(speuid), indicator_layout = FALSE, verification = verification, source = source)
  }

  plotcomp <- function(df, species) {

    swedish <- trimmedImputedSwedishList %>%
      filter(art == {{ species }}) %>%
      fill(index, se, .direction = "downup")

    local <- imputedLocalList %>%
      filter(art == {{ species }}) %>%
      fill(index, se, .direction = "downup")


    fname <- as.character({{ species }}) %>%
      str_replace_all("/", "_") #replacing escape characters in species name


    yAxisAdjusted <- yAxisModifier(max(c(swedish$index+1.96*swedish$se,local$index+1.96*local$se)))

    gcomma <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE) #Called later, enables commas instead of points for decimal indication

    imputedCombined <- swedish %>%
      full_join(local, by = c("art", "year")) %>%
      transmute(year = year, species = art, imputed_sweden = index.x, imputed_local = index.y)

    if(nchar(fname) < 18) {
      mrgn <- margin(0,0,80,0, unit = "pt")
    }else{
      mrgn <- margin(0,0,30,0, unit = "pt")
    }

    Encoding(fname) <- 'UTF-8'

    imputedCombined %>%
      ggplot(aes(x = year)) +
      geom_line(aes(y = imputed_local), linetype = "solid", colour = sebms_palette[2], linewidth = 2.8) + #interval line 1
      geom_line(aes(y = imputed_sweden), linetype = "longdash", linewidth = 1.6) + #central line #colour=rgb(155,187,89,max=255)
      # + xlim(startyear, endyear) #x-axis sectioning
      expand_limits(x = min(years), y = c(0, yAxisAdjusted[1])) +
      #geom_hline(yintercept = seq(from = 0, to = yAxisAdjusted[1], by = yAxisAdjusted[2])) + #Horizontal background lines #from=yAxisAdjusted[2]
      scale_y_continuous(labels = gcomma,
                         breaks = seq(from = 0, to = yAxisAdjusted[1], by = yAxisAdjusted[2]),
                         expand = c(0,0)) + #y-axis sectioning & comma labelling #from=yAxisAdjusted[2]
      scale_x_continuous(breaks = seq(min(years), max(years), by = 5)) +
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
    group_by(art) %>%
    nest() %>%
    ungroup() %>%
    mutate(plots = map2(data, art, ~plotcomp(.x, .y)))

  if (write) {
    #set tag
    if (is.null(tag)) {
      tag = ""
    }else {
      tag = glue("_{tag}")
    }
    #set filepath
    filepath <- normalizePath(filepath)

    walk2(ggs$plots, ggs$art, ~ggsave(plot = .x, filename = glue("{filepath}/{.y}_comparison{tag}.png"), width=748, height=868, dpi = 72, units = "px"))
  }

  if (print) {
    print(ggs$plots)
  }
}


#' Run Indicator Analysis
#'
#' @inheritParams get_imputedList
#' @param infile list of imputed index from [get_imputedList(indicator_layout =
#'   TRUE)]
#' @param write logical; if you want to write result to csv files, default TRUE.
#' @param print logical; if you want to print result to output, default FALSE
#' @param indicators optional; you can add a concatenated list of indicator
#'   species uids for a new indicator
#' @param indicatorname the name of the new indicator. If indicators is given
#'   but without setting name the indicator will be named 'NewInd'
#' @param lastyear number of years to do a trend comparison with
#'
#' @importFrom BRCindicators msi
#' @import dplyr
#' @importFrom glue glue
#' @importFrom readr write_csv2
#' @importFrom purrr set_names walk2 map2 possibly
#'
#' @return two csv files for each indicator groups. One with indicator index and
#'   changes and one with trend data.
#' @export
get_indicatorAnalyses <- function(infile = NULL, years = 2010:lubridate::year(lubridate::today())-1, lastyear = 7, Län = ".", Region = ".", Landskap = ".", Kommun = ".", filepath = getwd(), tag = NULL, verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84), write = TRUE, print = FALSE, indicators = NULL, indicatorname = NULL) {

  if(!is.null(indicators)) { # If a new indicator is added
    # If no new name is added
    cat("Making new indicator from your species...")
    if(is.null(indicatorname)) {
      cat("Setting name to new indicator to 'NewInd'")
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

  speid <- unlist(indicatorlist, use.names = F) %>%  # 'indicatorlist' is loaded by package
    unique()

  if(is.null(infile)) { # If no infile is given
    infile <- get_imputedList(Art = c(speid), years = years, indicator_layout = TRUE, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source)
  }

  indata <- infile %>%
    transmute(indicator,
              origin,
              speuid,
              species = as.factor(art),
              year = as.double(year),
              index = 100 * index,
              se = 100 * se,
              nsite,
              maxsite,
              minsite,
              meansite,
              mediansite,
              sdsite)



  indicalc <- function(spi, indn) {

    dat <- indata %>%
      filter(speuid %in% spi) %>%
      distinct(species, year, index, se)

    origin <- indata %>% distinct(origin) %>% pull()

    msi_out <- msi(dat, plotbaseyear = min(years), SEbaseyear = min(years), index_smooth = 'INDEX', lastyears = lastyear, jobname = glue("{indn}:{origin}"))

    result <- msi_out$results[1:8] %>%
      left_join(indata %>% filter(indicator %in% indn) %>% select(year,
                                                                  maxsite,
                                                                  minsite,
                                                                  meansite,
                                                                  mediansite,
                                                                  sdsite) %>% distinct(), by = c("year"))

    if (write) {
      #set tag
      if (is.null(tag)) {
        tag = ""
      }else {
        tag = glue("_{tag}")
      }
      #set filepath
      filepath <- normalizePath(filepath)

      write_csv2(file = glue("{filepath}/{indn}_indicator_in_{origin}{tag}.csv"), x = result)
      write_csv2(file = glue("{filepath}/{indn}_trends_in_{origin}{tag}.csv"), x = msi_out$trends)
    }
    return(msi_out)
  }

  grindicators <- map2(indicatorlist, names(indicatorlist), possibly(~indicalc(.x, .y)), .progress = "Calculating Indicator Index...") %>%
    set_names(names(indicatorlist))

  if (print) {
    return(grindicators)
  }
  #walk2(indata, indicatorlist, ~indicalc(.x, .y))
}




#' Create and Save Indicator Trend Plots
#'
#'
#' @inheritParams get_indicatorAnalyses
#' @param msi_out output from [get_indicatorAnalyses()]
#'
#' @import ggplot2
#' @importFrom stringr str_replace
#' @importFrom glue glue
#' @importFrom rtrim index overall
#' @importFrom purrr map2 walk2
#'
#' @usage get_indicatorPlots(
#'    msi_out = NULL,
#'    years = 2010:2023,
#'    Län = ".",
#'    Landskap = ".",
#'    Kommun = ".",
#'    write = TRUE,
#'    print = FALSE
#' )
#' @return trend plots with confidence interval for the indicator groups, saved
#'   as png files.
#' @export
get_indicatorPlots <- function(msi_out = NULL, years = 2010:lubridate::year(lubridate::today())-1, Län = ".", Region = ".", Landskap = ".", Kommun = ".", filepath = getwd(), tag = NULL, verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84), write = TRUE, print = FALSE) {

  if (is.null(msi_out)) {
    msi_out <- get_indicatorAnalyses(years = years, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source, write = FALSE, print = TRUE)
  }

  trimplots <- function(df, indicator) {

    fname <- as.character({{ indicator }}) %>%
      str_replace_all("/", "_") #replacing escape characters in species name

    yAxisAdjusted <- yIndicatorAxisMod(max(df$results$MSI + 1.96*df$results$sd_MSI))

    gcomma <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE) #Called later, enables commas instead of points for decimal indication

    indco <- df$trends$significance[1] %>% as.character()

    if (indco == "uncertain") {
      col <- sebms_trimpal[3]
      lt <- "longdash"
    } else if (indco == "strong decline" | indco == "strong decline" | indco == "moderate decline" | indco == "moderate decline") {
      col <- sebms_trimpal[2]
      lt <- "solid"
    } else if (indco == "stable") {
      col <- sebms_trimpal[3]
      lt <- "solid"
    }  else {
      col <- sebms_trimpal[1]
      lt <- "solid"
    }

    col <- "#9BBB59"

    # titles <- case_when(nchar(fname) < 18  ~ glue("fname ({df$nsite} lokaler)"),
    #                     str_detect(fname, "_") ~ glue("{str_replace_all(fname, '_', '\n')} \n({df$nsite} lokaler)"),
    #                     TRUE ~ glue("fname \n ({df$nsite} lokaler)"))

    titles <- str_to_sentence(fname)

    if(nchar(fname) < 18) {
      mrgn <- margin(0,0,80,0)
    }else {
      mrgn <- margin(0,0,30,0)
    }

    Encoding(fname) <- 'UTF-8'
    maxlim <- max(df$results$year)
    #Index %>%
    df$results %>%
      ggplot(aes(x = year)) +
      #geom_vline(xintercept = min(years), colour = "grey50") +
      geom_line(aes(y = Trend), linetype = paste(lt),
                colour = paste(col),
                linewidth = 2.8) + #central line #colour=rgb(155,187,89,max=255)
      geom_line(aes(y = lower_CL_trend),
                linetype = "dashed",
                linewidth = 1.6) + #interval line 1
      geom_line(aes(y = upper_CL_trend),
                linetype = "dashed",
                linewidth = 1.6) + #interval line 2
      geom_pointrange(aes(y = MSI, ymin = lower_CL_MSI, ymax = upper_CL_MSI), colour = "grey50", size = 2.5, linewidth = 1.1) + # Trend points and 95% conf. interval
      geom_segment(aes(x = year,
                       xend = year,
                       y = -2, yend = 0), colour = "grey50", linewidth = 1) +
      expand_limits(x = min(years), y = c(0,yAxisAdjusted[1])) +
      coord_cartesian(clip = "off") + # Make sure the points can be drawn beyond the smallest and largest x value on panel
      scale_y_continuous(labels = gcomma,
                         breaks = seq(from = 0, to = yAxisAdjusted[1], by = yAxisAdjusted[2]),
                         expand = c(0,0)) +#y-axis sectioning & comma labelling #from=yAxisAdjusted[2]
      scale_x_continuous(breaks = seq(min(years),max(years), by = 2), expand = c(0,0), limits = c(NA, maxlim+0.13)) +
      labs(title = titles) + #Chart title text
      theme(text = element_text(family = "Arial"),
            plot.title = element_text(hjust = 0.5, # Centered
                                      size = 44, #Chart title size
                                      margin = mrgn), #Distance between title and chart
            panel.grid.major.y = element_line(linewidth = 1, colour = "grey50"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), #Grid and background
            axis.text.x = element_text(colour = "black",
                                       size = 28,  #x-axis labels colour&size
                                       angle = 0,
                                       margin = margin(t = 20)),
            axis.text.y = element_text(colour = "black",
                                       size = 28, #y-axis labels colour&size
                                       angle = 0,
                                       margin = margin(r = 20)),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y =  element_line(linewidth = 1, colour = "grey40"),
            axis.ticks.length.y = unit(4, "mm"),
            axis.ticks.x = element_blank(),
            plot.margin = margin(8, 20, 0, 0),
            #axis.line = element_blank() #Axis colours
            axis.line.y = element_line(colour = "grey50"),
            axis.line.x = element_blank()#Axis colours
      )
  }
  #}
  # }
  ggs <- vector("list", length = length(msi_out))
  spname <- names(msi_out)
  ggs <- map2(msi_out, spname, ~trimplots(.x, .y))

  #ggs <- imap(msi_out, trimplots) # The imap(df) is the same as map2(df, names(df)) above. It take the msi_out as first argument and the names(msi_out) as second and feed that into the function

  if(write) {
    #set tag
    if (is.null(tag)) {
      tag = ""
    }else {
      tag = glue("_{tag}")
    }
    #set filepath
    filepath <- normalizePath(filepath)

    walk2(ggs, spname, ~ggsave(plot = .x, filename = glue("{filepath}/{.y}-Indicatorplot{tag}.png"), width = 748, height = 868, units = "px", dpi = 72), .progress = "Saving trimplots...")
  }
  if (print) {
    ggs
  }
}
