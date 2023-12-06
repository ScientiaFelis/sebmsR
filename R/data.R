#TODO: Streamline the functions to the ones necessary and so the output variables of the same kind is named consequently.

#' Open the .Renviron File in Editor
#' 
#' This makes it possible to set the credential variables in .Renviron file.
#' It can be in the user home directory or possibly in the R project working directory.
#'
#' @param homepath the path to the home directory
#'
#' @return open the .Renviron file to add PostgreSQL credentials
#' @export
#'
editcred <- function(homepath = "~/") {
  cat("Add SQL database credentials to .Renviron file\n\n")
  cat("DBUSER = 'username'\n")
  cat("DBPASS = 'passw'\n")
  cat("DBNAME = 'database name'")
  cat("DBPORT = 'database port'")
  
  Renv.file <- paste0(homepath, ".Renviron")
  
  if (file.exists(Renv.file)) {
    
    write("\n\nDBUSER = 'username'\nDBPASS = 'passw'\nDBNAME = 'database name'\nDBPORT = 'database port'", file = Renv.file, append = TRUE)
    file.edit(Renv.file)
    
  }else {
    cat("THERE IS NO .Renviron FILE IN THE GIVEN DIRECTORY\n")
    
    answ <- readline(prompt = "Do you want to create a .Renviron file? ")
    
    if (answ %in% c("Y", "y", "Yes", "yes")) {
      
      write("\n\nDBUSER = 'username'\nDBPASS = 'passw'\nDBNAME = 'database name'\nDBPORT = 'database port'", file = Renv.file, append = TRUE)
      
      file.edit(Renv.file)
      
    }else {
      return()
    }
  }
  
  
  
}


#' Retrieve Species Abundance List per Site, Site type, and Date
#'
#' Returns a data frame of counts per species, site, site type, and date. This
#' is used for the [sebms_species_per_sitetype_plot()]
#'
#' @inheritParams sebms_abundance_per_species_plot
#'  
#' @import tibble
#' @importFrom glue glue
#' @import dplyr
#' @importFrom stringr str_detect str_to_lower
#' @importFrom DBI dbGetQuery
#'
#' @returns a tibble with species ids and names, filtered for trimed species
#'   names, the site ids and names, site type, number of individuals for eahc
#'   species, the date, county, region, and municipality
#' @export
sebms_species_site_count_filtered <- function(year = 2021, Län = ".", Landskap = ".", Kommun = ".", source = c(54,55,56,63,64,66,67,84)){
  
  year <- glue("({paste0({year}, collapse = ',')})") # Make year span to a vector of years for the SQL
  source <- glue("({paste0({source}, collapse = ',')})")
  
  Län <- paste0(str_to_lower(Län),collapse = "|") # Make the list of Län to s regex statement
  Landskap <- paste0(str_to_lower(Landskap),collapse = "|") # Make the list of Landskap to s regex statement
  Kommun <- paste0(str_to_lower(Kommun),collapse = "|") # Make the list of Kommun to s regex statement
  
  county <- regID %>% 
    filter(str_detect(reg_name, Län)) %>% # Filter out matching Län from a look up table
    pull(reg_uid) %>% # pull out the id-numbers
    paste0(collapse = ',') # Make the id-numbers a vector palatable to the SQL
  
  region <- regID %>% 
    filter(str_detect(reg_name, Landskap)) %>% 
    pull(reg_uid) %>% 
    paste0(collapse = ',') # Make the id-numbers a vector palatable to the SQL
  
  municipality <- regID %>% 
    filter(str_detect(reg_name, Kommun)) %>% 
    pull(reg_uid) %>% 
    paste0(collapse = ',') # Make the id-numbers a vector palatable to the SQL
  
  
  q <- glue("
          WITH reg AS
           (SELECT reg_uid AS reg_id, reg_name AS län
             FROM reg_region
             WHERE reg_uid IN ({county}) AND reg_group = 'C'),
           lsk AS
           (SELECT reg_uid AS landskaps_id, reg_name AS landskap
             FROM reg_region
             WHERE reg_uid IN ({region}) AND reg_group = 'P'),
           mun AS
           (SELECT reg_uid AS kommun_id, reg_name AS kommun
             FROM reg_region
             WHERE reg_uid IN ({municipality}) AND reg_group = 'M')

        SELECT
          spe.spe_uid AS speUId,
          spe.spe_semainname As Art,
          sit.sit_uid AS sitUId,
          sit.sit_name AS Lokalnamn,
          sit.sit_type AS sitetype,
          SUM(obs.obs_count) AS Antal,
          vis_begintime::date as Datum,
          --EXTRACT (week FROM vis_begintime::date) AS vecka,
          reg.län,
          lsk.landskap,
          mun.kommun
        
        FROM obs_observation AS obs
        INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
        INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
        INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
        INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
        INNER JOIN spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid      -- så här bör det väl vara?
        INNER JOIN reg ON sit.sit_reg_countyid = reg.reg_id
        INNER JOIN lsk ON sit.sit_reg_provinceid = lsk.landskaps_id
        INNER JOIN mun ON sit.sit_reg_municipalityid = mun.kommun_id
        
        WHERE
          extract('YEAR' from vis_begintime) IN {year}
          AND
          vis_typ_datasourceid IN {source}
          AND ( spv.spv_istrim=TRUE OR spe_uid IN (135,131,132,133,139) ) -- Include nullobs and 4 aggregated species groups
        
       GROUP BY
          Art, Lokalnamn,Datum, sitetype, speUId, sitUId, reg.reg_id, reg.län, lsk.landskaps_id, lsk.landskap, mun.kommun_id, mun.kommun --, date --, vecka
       ORDER BY
          Antal DESC, Lokalnamn, Art, Datum;")
  
  sebms_pool <- sebms_assert_connection()
  res <- DBI::dbGetQuery(sebms_pool, q)
  as_tibble(res)
}

#' Retrieve Species Abundance List per Date
#'
#' Returns a data frame with counts per species and date. This is used in most
#' plot functions: [sebms_abundance_per_species_plot()],
#' [sebms_abundance_year_compare_plot()], and [sebms_species_abundance_plot()]
#'
#' @inheritParams sebms_abundance_per_species_plot
#' @param Art integer; the species uids of interest
#'
#' @import tibble
#' @importFrom glue glue
#' @import dplyr
#' @importFrom stringr str_detect str_to_lower
#' @importFrom DBI dbGetQuery
#'
#' @returns a tibble with species ids and names, filetered for trimmed species
#'   names, the number of individuals, the date, county, region, and
#'   municipality.
#' @export
sebms_species_count_filtered <- function(year = 2020:2021, Art = 1:200, Län = ".", Landskap = ".", Kommun = ".", source = c(54,55,56,63,64,66,67,84)) {
  
  year <- glue("({paste0({year}, collapse = ',')})") # Make year span to a vector of years for the SQL
  Art <- glue("({paste0({Art}, collapse = ',')})")
  source <- glue("({paste0({source}, collapse = ',')})")
  
  Län <- paste0(str_to_lower(Län),collapse = "|") # Make the list of Län to s regex statement
  Landskap <- paste0(str_to_lower(Landskap),collapse = "|") # Make the list of Landskap to s regex statement
  Kommun <- paste0(str_to_lower(Kommun),collapse = "|") # Make the list of Kommun to s regex statement
  
  county <- regID %>% 
    filter(str_detect(reg_name, Län)) %>% # Filter out matching Län from a look up table
    pull(reg_uid) %>% # pull out the id-numbers
    paste0(collapse = ',') # Make the id-numbers a vector palatable to the SQL
  
  region <- regID %>% 
    filter(str_detect(reg_name, Landskap)) %>% 
    pull(reg_uid) %>% 
    paste0(collapse = ',') # Make the id-numbers a vector palatable to the SQL
  
  municipality <- regID %>% 
    filter(str_detect(reg_name, Kommun)) %>% 
    pull(reg_uid) %>% 
    paste0(collapse = ',') # Make the id-numbers a vector palatable to the SQL
  
  
  q <- glue("
          WITH reg AS
           (SELECT reg_uid AS reg_id, reg_name AS län
             FROM reg_region
             WHERE reg_uid IN ({county}) AND reg_group = 'C'),
           lsk AS
           (SELECT reg_uid AS landskaps_id, reg_name AS landskap
             FROM reg_region
             WHERE reg_uid IN ({region}) AND reg_group = 'P'),
           mun AS
           (SELECT reg_uid AS kommun_id, reg_name AS kommun
             FROM reg_region
             WHERE reg_uid IN ({municipality}) AND reg_group = 'M')
   
        SELECT
          spe.spe_uid AS speuid,
          spe.spe_semainname As art,
          --sit.sit_type AS sitetype,
          SUM(obs.obs_count) AS antal,
          --extract('YEAR' from vis_begintime) AS years,
          vis_begintime::date as Datum,
          reg.län,
          lsk.landskap,
          mun.kommun
        FROM obs_observation AS obs
        
        INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
        INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
        INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
        INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
        INNER JOIN  spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid 
        INNER JOIN reg ON sit.sit_reg_countyid = reg.reg_id
        INNER JOIN lsk ON sit.sit_reg_provinceid = lsk.landskaps_id
        INNER JOIN mun ON sit.sit_reg_municipalityid = mun.kommun_id
        WHERE
          extract('YEAR' from vis_begintime) IN {year}
          AND vis_typ_datasourceid IN {source}
          AND ( spv.spv_istrim=TRUE OR spe_uid IN (135,131,132,133,139) ) -- Include nullobs and 4 aggregated species groups
          AND spe.spe_uid IN {Art}
        
        GROUP BY
          spe.spe_uid, Datum, reg.reg_id, reg.län, lsk.landskaps_id, lsk.landskap, mun.kommun_id, mun.kommun --, years
        ORDER BY
          antal DESC;")
  
  sebms_pool <- sebms_assert_connection()
  res <- dbGetQuery(sebms_pool, q)
  as_tibble(res)
  
} 


#' Retrieve Species Abundance List per Year
#' 
#' Return a data frame with abundance data per species and year.
#' 
#' @param year the years of interest
#' 
#' @import tibble
#' @import glue
#' @importFrom DBI dbGetQuery
#' @export
sebms_species_per_year_filtered <- function(year = 2020:2021) {
  
  year <- glue("({paste0({year}, collapse = ',')})")
  
  q <- glue("
      SELECT
        spe.spe_uid AS id,
        spe.spe_semainname As name,
        SUM(obs.obs_count) AS count,
        extract('YEAR' from vis_begintime) AS years
      FROM obs_observation AS obs
      INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
      INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
      INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
      INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
      INNER JOIN  spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid     
      WHERE
        extract('YEAR' from vis_begintime) IN {year}
        AND spv.spv_istrim=TRUE     -- new
      GROUP BY
        spe.spe_uid, years
      ORDER BY
        name, years;")
  
  sebms_pool <- sebms_assert_connection()
  res <- dbGetQuery(sebms_pool, q)
  as_tibble(res)
  
} 



#' Climate data for Naturum sites from SMHI
#' @return data frame with climate data
#' @import purrr
#' @import dplyr
#' @import tidyr 
#' @import tibble 
#' @importFrom lubridate ymd_hms
#' @importFrom jsonlite fromJSON
#' @noRd
#' 
sebms_naturum_climate <- function() {
  
  #api <- function(x, y) paste0("https://opendata-download-metfcst.smhi.se/api/", 
  #  "category/pmp3g/version/2/geotype/point/lon/", x ,"/lat/", y,"/data.json")
  
  api <- function(x, y) paste0("https://opendata-download-metfcst.smhi.se/api/", 
                               "category/pmp3g/version/2/geotype/point/lon/", x ,"/lat/", y,"/data.json")
  
  res <- 
    sebms_data_sites_naturum %>% 
    mutate(api = api(Long, Lat))
  
  smhi_call <- function(api) {
    
    res <- fromJSON(api)$timeSeries  
    names(res$parameters) <- res$validTime
    df <- bind_rows(res$parameters, .id = "id")
    df$values <- unlist(df$values)
    df <- as_tibble(df)
    
    res <- 
      df %>% 
      mutate(date = ymd_hms(id)) %>%
      dplyr::select(date, name, values) %>%
      spread(name, values) %>%
      mutate(is_gogogo = t >= 13 & ws < 7.9 & tcc_mean <= 4 & pmean == 0)
    
    return (res)
  }
  
  naturum_climate <- map(res$api, smhi_call)
  names(naturum_climate) <- res$Namn
  res <- bind_rows(naturum_climate, .id = "id")
  return(res)
}


#' Retrieve Species Abundance List per Site with Coordinates
#'
#' Returns a data frame of counts per species and site with their coordinates in
#' SWEREF. This is used for the [sebms_distribution_map()]
#'
#'
#' @inheritParams sebms_abundance_per_species_plot
#'
#' @import tibble
#' @importFrom glue glue
#' @import dplyr
#' @importFrom stringr str_detect str_to_lower
#' @importFrom DBI dbGetQuery
#'
#' @returns a tibble with species ids and names, filtered for trimmed species
#'   names, the site ids and names, site type, max number of individuals observed for each siote,
#'   the date, county, region, and municipality, and the rank of site 
#' @export
sebms_occurances_distribution <- function(year = 2020:2021, Art = 1:200, Län = ".", Landskap = ".", Kommun = ".", source = c(54,55,56,63,64,66,67,84)) {
  
  year <- glue("({paste0({year}, collapse = ',')})") # Make year span to a vector of years for the SQL
  Art <- glue("({paste0({Art}, collapse = ',')})")
  source <- glue("({paste0({source}, collapse = ',')})")
  
  Län <- paste0(str_to_lower(Län),collapse = "|") # Make the list of Län to s regex statement
  Landskap <- paste0(str_to_lower(Landskap),collapse = "|") # Make the list of Landskap to s regex statement
  Kommun <- paste0(str_to_lower(Kommun),collapse = "|") # Make the list of Kommun to s regex statement
  
  county <- regID %>% 
    filter(str_detect(reg_name, Län)) %>% # Filter out matching Län from a look up table
    pull(reg_uid) %>% # pull out the id-numbers
    paste0(collapse = ',') # Make the id-numbers a vector palatable to the SQL
  
  region <- regID %>% 
    filter(str_detect(reg_name, Landskap)) %>% 
    pull(reg_uid) %>% 
    paste0(collapse = ',') # Make the id-numbers a vector palatable to the SQL
  
  municipality <- regID %>% 
    filter(str_detect(reg_name, Kommun)) %>% 
    pull(reg_uid) %>% 
    paste0(collapse = ',') # Make the id-numbers a vector palatable to the SQL
  
  
  q <- glue("
          WITH reg AS
           (SELECT reg_uid AS reg_id, reg_name AS län
             FROM reg_region
             WHERE reg_uid IN ({county}) AND reg_group = 'C'),
           lsk AS
           (SELECT reg_uid AS landskaps_id, reg_name AS landskap
             FROM reg_region
             WHERE reg_uid IN ({region}) AND reg_group = 'P'),
           mun AS
           (SELECT reg_uid AS kommun_id, reg_name AS kommun
             FROM reg_region
             WHERE reg_uid IN ({municipality}) AND reg_group = 'M')
   
      SELECT
        t.speUId,
        t.art,
        t.sitUId,
        t.Lokalnamn,
        t.sitetype,
        t.landskap,
        t.län,
        t.kommun,
        t.dag,
        t.lat,
        t.lon,
        t.sumval_rank,
        MAX(sumval)
           
      FROM
        (SELECT
          spe.spe_uid AS speuid,
          spe.spe_semainname As art,
          sit.sit_uid AS situid,
          sit.sit_name AS Lokalnamn,
          vis.vis_begintime AS dag,
          sit.sit_type AS sitetype,
          sit.sit_geosweref99tmlat AS lat,
          sit.sit_geosweref99tmlon AS lon,
          reg.län AS län,
          lsk.landskap AS landskap,
          mun.kommun AS kommun,
          EXTRACT (week FROM vis_begintime::date) AS vecka,
          SUM(obs.obs_count) AS sumval,
          dense_rank() OVER (PARTITION BY spe.spe_uid,sit.sit_uid ORDER BY SUM(obs.obs_count) DESC, vis.vis_uid ) AS sumval_rank
        
        FROM obs_observation AS obs
        
          INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
          INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
          INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
          INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
          --INNER JOIN reg_region AS reg ON sit.sit_reg_provinceid = reg.reg_uid
          INNER JOIN spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid      -- så här bör det väl vara?
          INNER JOIN reg ON sit.sit_reg_countyid = reg.reg_id
          INNER JOIN lsk ON sit.sit_reg_provinceid = lsk.landskaps_id
          INNER JOIN mun ON sit.sit_reg_municipalityid = mun.kommun_id
         
        WHERE (spv.spv_istrim=TRUE or spe_uid in (135,131,132,133,139) ) -- Include nullobs and 4 aggregated species groups
          AND extract('YEAR' from vis_begintime) IN {year}
          AND vis_typ_datasourceid IN {source}
          AND not (vis_typ_datasourceid = 55  and sit_reg_countyid=2)
          AND spe.spe_uid IN {Art}
        GROUP BY
          spe.spe_uid, sit.sit_uid, vis.vis_uid, sitetype, reg.reg_id, reg.län, lsk.landskaps_id, lsk.landskap, mun.kommun_id, mun.kommun
        ORDER BY
            spe.spe_uid,sit.sit_uid) AS t -- End of inner select
           
      WHERE t.sumval_rank =1 --between 1 and 3 
          
      GROUP BY
        t.speUId, t.art,t.situid,t.Lokalnamn,t.lat,t.lon,t.sumval_rank, t.dag, t.län, t.kommun, t.landskap, t.sitetype
      ORDER BY
        t.speuid, MAX DESC, t.situid;"
      
  )
  
  t <- glue("SELECT
 
t.speUId,
t.artnamn,
t.sitUId,
t.Lokalnamn,
t.lokaltyp,
t.Landskap,
t.dag,
t.N,
t.E,
t.sumval_rank,
MAX(sumval)
 
FROM
( SELECT
spe.spe_uid AS speUId,
spe.spe_semainname As artnamn,
sit.sit_uid AS sitUId,
sit.sit_name AS Lokalnamn,
reg.reg_name AS Landskap,
vis.vis_begintime AS dag,
sit.sit_type as lokaltyp,
sit.sit_geort9025gonvlat AS N,
sit.sit_geort9025gonvlon AS E,
EXTRACT (week FROM vis_begintime::date) AS vecka,
SUM(obs.obs_count) AS sumval,
dense_rank() OVER (PARTITION BY spe.spe_uid,sit.sit_uid ORDER BY SUM(obs.obs_count) DESC, vis.vis_uid ) AS sumval_rank
FROM obs_observation AS obs
INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
INNER JOIN reg_region AS reg ON sit.sit_reg_provinceid = reg.reg_uid
INNER JOIN  spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid      -- så här bör det väl vara?


WHERE date_trunc('YEAR', vis_begintime) =(DATE '2022-01-01')
AND (spv.spv_istrim=TRUE or spe_uid in (135,131,133,139) )
AND 
vis_typ_datasourceid in (54,55,56,63,64,66,67,84)   
and not (vis_typ_datasourceid = 55  and sit_reg_countyid=2)
--AND sit.sit_uid=6
--AND spe.spe_uid =119
GROUP BY
  spe.spe_uid, sit.sit_uid , vis.vis_uid, reg.reg_name,lokaltyp
ORDER BY
   spe.spe_uid,sit.sit_uid) AS t
 
WHERE t.sumval_rank =1 --between 1 and 3 
GROUP BY
t.speUId, t.artnamn,t.sitUId,t.Lokalnamn,t.N,t.E,t.sumval_rank, t.dag, t.Landskap,t.lokaltyp
ORDER BY
   t.speUId, MAX DESC, t.sitUId
  ")


sebms_pool <- sebms_assert_connection()
res <- dbGetQuery(sebms_pool, q)
as_tibble(res)

} 
