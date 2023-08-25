#' Retrieve Total Species List per Site from SeBMS
#' 
#' This is used eg for the histograms
#' 
#' @import tibble
#' @importFrom DBI dbGetQuery
#' @export
sebms_species_lokal_count <- function(){
  
  q <- " SELECT
 
--t.speUId,
t.Art,
--t.sitUId,
t.Lokalnamn,
t.Antal, --t.sumval,
t.Datum --t.date
--t.vecka,
 
FROM
( SELECT
--spe.spe_uid AS speUId,
spe.spe_semainname As Art,
--sit.sit_uid AS sitUId,
sit.sit_name AS Lokalnamn,
SUM(obs.obs_count) AS Antal,
vis_begintime::date as Datum
--EXTRACT (week FROM vis_begintime::date) AS vecka,

FROM obs_observation AS obs
INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
INNER JOIN spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid      -- så här bör det väl vara?
WHERE extract('YEAR' from vis_begintime) = 2021
AND 
vis_typ_datasourceid in (54,55,56,63,64,66,67)

--AND spv.spv_istrim=TRUE     
--AND sit.sit_uid=6
--AND spe.spe_uid =7
GROUP BY
  Art, Lokalnamn,Datum --spe.spe_uid, sit.sit_uid, date --, vecka
ORDER BY
Art, Lokalnamn) AS t --	spe.spe_uid,sit.sit_uid) AS t
 
 
GROUP BY
t.Art, t.Lokalnamn, t.Antal, t.Datum--t.speUId, t.artnamn,t.sitUId,t.Lokalnamn,t.date,t.sumval
ORDER BY
  t.Art,t.Lokalnamn, t.Datum-- t.speUId, t.Lokalnamn, t.date
 
 "
  
  sebms_assert_connection()
  res <- DBI::dbGetQuery(sebms_pool, q)
  as_tibble(res)
}



#' Retrieve Total Species List from SeBMS
#' 
#' This is used eg for the histograms
#' 
#' @import tibble
#' @importFrom DBI dbGetQuery
#' @export
sebms_species_count <- function(){
  
  q <- "SELECT
 
        --t.speUId,
        t.Art,
        --t.sitUId,
        --t.Lokalnamn,
        t.Antal, --t.sumval,
        t.Datum --t.date
        --t.vecka,
       
      FROM
      ( SELECT
      --spe.spe_uid AS speUId,
      spe.spe_semainname As Art,
      --sit.sit_uid AS sitUId,
      --sit.sit_name AS Lokalnamn,
      SUM(obs.obs_count) AS Antal,
      vis_begintime::date as Datum
      --EXTRACT (week FROM vis_begintime::date) AS vecka,
      
      FROM obs_observation AS obs
      INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
      INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
      INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
      INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
      INNER JOIN spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid      -- så här bör det väl vara?
      WHERE extract('YEAR' from vis_begintime) = 2021
      AND 
      vis_typ_datasourceid in (54,55,56,63,64,66,67)
      
      --AND spv.spv_istrim=TRUE     
      --AND sit.sit_uid=6
      --AND spe.spe_uid =7
      GROUP BY
        Art, Datum --Lokalnamn, spe.spe_uid, sit.sit_uid, date --, vecka
      ORDER BY
      Art) AS t --, Lokalnamn,	spe.spe_uid,sit.sit_uid) AS t
       
       
      GROUP BY
      t.Art, t.Antal, t.Datum -- t.Lokalnamn, t.speUId, t.artnamn,t.sitUId,t.Lokalnamn,t.date,t.sumval
      ORDER BY
        t.Art,t.Datum; -- t.Lokalnamn, t.speUId, t.Lokalnamn, t.date;
 "
  
  sebms_assert_connection()
  res <- DBI::dbGetQuery(sebms_pool, q)
  as_tibble(res)
}


#' Retrieve species list per year
#' 
#' @import tibble
#' @importFrom DBI dbGetQuery
#' @export
sebms_species_per_year <- function() {
  
  q <- "
    SELECT
      spe.spe_uid AS id,
      spe.spe_semainname As name,
      SUM(obs.obs_count) AS count
    FROM obs_observation AS obs
    INNER JOIN vis_visit AS vis ON 
      obs.obs_vis_visitid = vis.vis_uid
    INNER JOIN spe_species AS spe ON 
      obs.obs_spe_speciesid = spe.spe_uid
    INNER JOIN seg_segment AS seg ON 
      obs.obs_seg_segmentid = seg.seg_uid
    INNER JOIN sit_site AS sit ON 
      seg.seg_sit_siteid = sit.sit_uid
    WHERE
    sit.sit_reg_countyid = (SELECT reg_uid FROM reg_region WHERE reg_code = '08' AND reg_group = 'C') AND
    date_trunc('YEAR', vis_begintime) =(DATE '2014-01-01')
    GROUP BY
      spe.spe_uid
    ORDER BY
    count DESC;"
  
  sebms_assert_connection()
  res <- DBI::dbGetQuery(sebms_pool, q)
  as_tibble(res)
}


#' Retrieve species list per year, filter for approved ones
#' 
#' @import tibble
#' @importFrom DBI dbGetQuery
#' @export
sebms_species_per_year_filtered <- function() {
  
  q <- "SELECT
  spe.spe_uid AS id,
  spe.spe_semainname As name,
  SUM(obs.obs_count) AS count
FROM obs_observation AS obs
INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
INNER JOIN  spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid     
WHERE
sit.sit_reg_countyid = (SELECT reg_uid FROM reg_region WHERE reg_code = '08' AND reg_group = 'C')
AND date_trunc('YEAR', vis_begintime) =(DATE '2014-01-01')
AND spv.spv_istrim=TRUE      -- new
GROUP BY
  spe.spe_uid
ORDER BY
count DESC;"
  
  sebms_assert_connection()
  res <- dbGetQuery(sebms_pool, q)
  as_tibble(res)
  
} 

#' Count the number of species per year and site 
#' and filter for approved ones 
#' @import tibble
#' @importFrom DBI dbGetQuery
#' @export
sebms_species_per_year_site_filtered <- function() {
  
  q <- "SELECT
  sit.sit_uid AS id,
  sit.sit_name AS site,
  sit.sit_type AS sitetype,
  COUNT(DISTINCT spe.spe_uid) as species
FROM obs_observation AS obs
INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
INNER JOIN  spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid     
WHERE
sit.sit_reg_countyid = (SELECT reg_uid FROM reg_region WHERE reg_code = '12' AND reg_group = 'C')
AND date_trunc('YEAR', vis_begintime) =(DATE '2014-01-01')
AND spv.spv_istrim=TRUE      -- new
GROUP BY
  sit.sit_uid
ORDER BY
species DESC;"
  
  sebms_assert_connection()
  res <- dbGetQuery(sebms_pool, q)
  as_tibble(res)
}


#' List the species per year & site, add total counts 
#' and filter for approved species 
#' @import tibble
#' @importFrom DBI dbGetQuery
#' @export
sebms_species_per_year_site_counts_filtered <- function() {
  
  q <- "SELECT
  sit.sit_uid AS id,
  sit.sit_name AS site,
  sit.sit_type AS sitetype,
  spe.spe_uid AS species,
  SUM(obs.obs_count) as speciesno
FROM obs_observation AS obs
INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
INNER JOIN  spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid     
WHERE
sit.sit_reg_countyid = (SELECT reg_uid FROM reg_region WHERE reg_code = '12' AND reg_group = 'C')
AND date_trunc('YEAR', vis_begintime) =(DATE '2014-01-01')
AND spv.spv_istrim=TRUE      -- new
GROUP BY
  sit.sit_uid, spe.spe_uid
ORDER BY
id , speciesno DESC;"
  
  sebms_assert_connection()
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
#' @export
#' 
sebms_naturum_climate <- function() {
  
  #api <- function(x, y) paste0("https://opendata-download-metfcst.smhi.se/api/", 
  #  "category/pmp3g/version/2/geotype/point/lon/", x ,"/lat/", y,"/data.json")
  
  api <- function(x, y) paste0("https://opendata-download-metfcst.smhi.se/api/", 
                               "category/pmp2g/version/2/geotype/point/lon/", x ,"/lat/", y,"/data.json")
  
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
  return (res)
}

##' @importFrom utils globalVariables
#if (getRversion() >= "2.15.1")
#  globalVariables(names = unlist(strsplit(split = " ",
# paste0("word1 ",
#  "word2"))))
