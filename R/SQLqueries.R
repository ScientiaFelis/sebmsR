#TODO: Streamline the functions to the ones necessary and so the output variables of the same kind is named consequently.

#' Open the .Renviron File in Editor
#' 
#' This makes it possible to set the credential variables in .Renviron file.
#' It can be in the user home directory or possibly in the R project working directory.
#'
#' @param homepath the path to the home directory
#' 
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
sebms_species_site_count_filtered <- function(year = 2021, Län = ".", Landskap = ".", Kommun = ".", verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84)){
  
  year <- glue("({paste0({year}, collapse = ',')})") # Make year span to a vector of years for the SQL
  source <- glue("({paste0({source}, collapse = ',')})")
  
  verification <- glue("({paste0({verification}, collapse = ',')})")
  
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
          spe.spe_semainname AS Art,
          obs.obs_typ_vfcid AS verification_code,
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
          AND obs.obs_typ_vfcid IN {verification}
        
       GROUP BY
          Art, Lokalnamn,Datum, sitetype, speUId, obs.obs_typ_vfcid, sitUId, reg.reg_id, reg.län, lsk.landskaps_id, lsk.landskap, mun.kommun_id, mun.kommun --, date --, vecka
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
sebms_species_count_filtered <- function(year = 2020:2021, Art = 1:200, Län = ".", Landskap = ".", Kommun = ".", verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84)) {
  
  year <- glue("({paste0({year}, collapse = ',')})") # Make year span to a vector of years for the SQL
  Art <- glue("({paste0({Art}, collapse = ',')})")
  source <- glue("({paste0({source}, collapse = ',')})")
  verification <- glue("({paste0({verification}, collapse = ',')})")
  
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
          obs.obs_typ_vfcid AS verification_code,
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
          AND obs.obs_typ_vfcid IN {verification}
        
        GROUP BY
          spe.spe_uid, obs.obs_typ_vfcid, Datum, reg.reg_id, reg.län, lsk.landskaps_id, lsk.landskap, mun.kommun_id, mun.kommun --, years
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
#' @inheritParams sebms_abundance_per_species_plot
#' 
#' @import tibble
#' @import glue
#' @importFrom DBI dbGetQuery
#' @export
sebms_species_per_year_filtered <- function(year = 2020:2021, verification= c(109,110,111)) {
  
  year <- glue("({paste0({year}, collapse = ',')})")
  verification <- glue("({paste0({verification}, collapse = ',')})")
  
  q <- glue("
      SELECT
        spe.spe_uid AS id,
        spe.spe_semainname As name,
        obs.obs_typ_vfcid AS verification_code,
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
        AND obs.obs_typ_vfcid IN {verification}
      GROUP BY
        spe.spe_uid, years, obs.obs_typ_vfcid
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
#' @param Art integer; the species uids of interest
#' @param Region character or reg ex; which aggregated region you want the data from
#'
#' @import tibble
#' @importFrom glue glue
#' @import dplyr
#' @importFrom stringr str_detect str_to_lower
#' @importFrom DBI dbGetQuery
#'
#' @returns a tibble with species ids and names, filtered for trimmed species
#'   names, the site ids and names, site type, max number of individuals observed for each site,
#'   the date, county, region, and municipality, and the rank of site 
#' @export
sebms_occurances_distribution <- function(year = 2020:2021, Art = 1:200,  Län = ".", Region = ".", Landskap = ".", Kommun = ".", verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84)) {
  
  
  year <- glue("({paste0({year}, collapse = ',')})") # Make year span to a vector of years for the SQL
  species <- glue("({paste0({Art}, collapse = ',')})")
  source <- glue("({paste0({source}, collapse = ',')})")
  verification <- glue("({paste0({verification}, collapse = ',')})")
  
  Län <- paste0(str_to_lower(Län),collapse = "|") # Make the list of Län to s regex statement
  Landskap <- paste0(str_to_lower(Landskap),collapse = "|") # Make the list of Landskap to s regex statement
  Kommun <- paste0(str_to_lower(Kommun),collapse = "|") # Make the list of Kommun to s regex statement
  Region <- paste0(str_to_lower(Region),collapse = "|") # Make the list of Region to s regex statement
  
  county <- regID2 %>% 
    filter(str_detect(reg_name, Län)) %>% # Filter out matching Län from a look up table
    pull(reg_uid) %>% # pull out the id-numbers
    paste0(collapse = ',') # Make the id-numbers a vector palatable to the SQL
  
  province <- regID2 %>% 
    filter(str_detect(reg_name, Landskap)) %>% 
    pull(reg_uid) %>% 
    paste0(collapse = ',') # Make the id-numbers a vector palatable to the SQL
  
  municipality <- regID2 %>% 
    filter(str_detect(reg_name, Kommun)) %>% 
    pull(reg_uid) %>% 
    paste0(collapse = ',') # Make the id-numbers a vector palatable to the SQL
  
  region <- regID2 %>% 
    filter(str_detect(reg_name, Region)) %>% 
    pull(reg_uid) %>% 
    paste0(collapse = ',') # Make the id-numbers a vector palatable to the SQL
  
  #TODO; Add filter for the kommun, län, kandskap and Region JOIN, such that you only get the JOIN for the actual area, e.g. a county.
  # This can then be added to the glue() SQL call as such, {join} instead of all four JOINs 
  
  q <- glue("-- Query input params:
-- > province     - Comma-separated list of reg_uid values corresponding to provinces, reg_group 'P'
-- > region       - Comma-separated list of reg_uid values corresponding to regions, reg_group 'R'
-- > county       - Comma-separated list of reg_uid values corresponding to counties, reg_group 'C'
-- > municipality - Comma-separated list of reg_uid values corresponding to municipalities, reg_group 'M'
-- > year         - Comma-separated list of years in 4-digit integer format, YYYY
-- > source       - Comma-separated list of typ_uid values corresponding to data sources, typ_group 'dts'
-- > species      - Comma-separated list of spe_uid values representing species
-- > verification - Comma-separated list of typ_uid values corresponding to verification codes, typ_group 'vfc'

-- Pre-fetch/-calculate dictionary data related to geographical regions, i.e., Provinces, Regions, Counties, and Municipalities
WITH
  pro AS (SELECT reg_uid AS pro_uid, reg_name AS pro_name FROM reg_region WHERE reg_group = 'P' AND reg_uid IN ({province}) ),
  reg AS (SELECT reg_uid AS reg_uid, reg_name AS reg_name FROM reg_region WHERE reg_group = 'R' AND reg_uid IN ({region}) ),
  cou AS (SELECT reg_uid AS cou_uid, reg_name AS cou_name FROM reg_region WHERE reg_group = 'C' AND reg_uid IN ({county}) ),
  mun AS (SELECT reg_uid AS mun_uid, reg_name AS mun_name FROM reg_region WHERE reg_group = 'M' AND reg_uid IN ({municipality}))

-- Basing on source data retrieved as part of inner query (tmp), now extract data points to be used for final presentation
SELECT
  tmp.spe_uid  AS speuid,
  tmp.spe_name AS Art,
  tmp.sit_uid  AS situid,
  tmp.sit_name AS LokalNamn,
  tmp.sit_type AS sitetype,
  tmp.sit_lat  AS lat,
  tmp.sit_lon  AS lon,
  tmp.pro_name AS Landskap,
  tmp.reg_name AS Region,
  tmp.cou_name AS Län,
  tmp.mun_name AS Kommun,
  tmp.vis_time AS ObsDag,
  tmp.obs_rank AS ObsRank,
  MAX(obs_sum) AS ObsMaxSum
  
FROM
(
  -- Inner query to retrieve source data from obs, vis, sit, seg, spv, and reg tables; note the row aggregation (SUM, RANK)
  SELECT
    spe.spe_uid        AS spe_uid,
    spe.spe_semainname AS spe_name,
    obs.obs_typ_vfcid  AS obs_vfc,
    vis.vis_begintime  AS vis_time,
    sit.sit_uid        AS sit_uid,
    sit.sit_name       AS sit_name,
    sit.sit_type       AS sit_type,
    sit.sit_geosweref99tmlat AS sit_lat,
    sit.sit_geosweref99tmlon AS sit_lon,
    pro.pro_name       AS pro_name,
    reg.reg_name       AS reg_name,
    cou.cou_name       AS cou_name,
    mun.mun_name       AS mun_name,
    EXTRACT(WEEK FROM vis_begintime::date) AS vis_week, -- TODO: Why the need for a cast from timestamp to date here?
    SUM(obs.obs_count) AS obs_sum,
    DENSE_RANK() OVER (
      PARTITION BY
        spe.spe_uid,
        sit.sit_uid 
      ORDER BY
        SUM(obs.obs_count) DESC,
        vis.vis_uid
    ) AS obs_rank
    
  FROM obs_observation AS obs
  -- Inner Joins require non-NULL references in order to return records; thus, beware of column defs that allow NULLs!
  INNER JOIN vis_visit   AS vis ON obs.obs_vis_visitid   = vis.vis_uid
  INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
  INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
  INNER JOIN sit_site    AS sit ON seg.seg_sit_siteid    = sit.sit_uid
  INNER JOIN spv_speciesvalidation AS spv ON spv.spv_spe_speciesid = spe.spe_uid
  INNER JOIN pro ON sit.sit_reg_provinceid     = pro.pro_uid
  INNER JOIN reg ON sit.sit_reg_regionid       = reg.reg_uid
  INNER JOIN cou ON sit.sit_reg_countyid       = cou.cou_uid
  INNER JOIN mun ON sit.sit_reg_municipalityid = mun.mun_uid

  WHERE
    -- Include nullobs and 4 aggregated species groups
    (spv.spv_istrim OR spe.spe_uid IN (135, 131, 132, 133, 139) ) AND
    -- Exclude BioGeo-sourced observations in Blekinge county
    NOT (vis.vis_typ_datasourceid = 55 AND sit.sit_reg_countyid = 2) AND
    -- Filter records based on the query input parameters
    EXTRACT(YEAR FROM vis.vis_begintime) IN {year} AND
    vis.vis_typ_datasourceid IN {source} AND
    spe.spe_uid IN {species} AND
    obs.obs_typ_vfcid IN {verification}
    
  GROUP BY
    spe.spe_uid,
    sit.sit_uid,
    vis.vis_uid,
    obs.obs_typ_vfcid,
    sit.sit_type,
    pro.pro_uid,
    pro.pro_name,
    reg.reg_uid,
    reg.reg_name,
    cou.cou_uid,
    cou.cou_name,
    mun.mun_uid,
    mun.mun_name
    
  ORDER BY
    spe.spe_uid,
    sit.sit_uid
    
) AS tmp
           
WHERE
  -- Rank between 1 and 3
  tmp.obs_rank = 1 
  
GROUP BY
  tmp.spe_uid,
  tmp.spe_name,
  tmp.sit_uid,
  tmp.sit_name,
  tmp.sit_lat,
  tmp.sit_lon,
  tmp.obs_rank,
  tmp.vis_time,
  tmp.pro_name,
  tmp.reg_name,
  tmp.cou_name,
  tmp.mun_name,
  tmp.sit_type
  
ORDER BY
  tmp.spe_uid,
  --MAX ??? DESC, -- TODO: ???
  tmp.sit_uid;")
            
  #           q <- glue("
  #         WITH cou AS
  #          (SELECT reg_uid AS cou_id, reg_name AS län
  #            FROM reg_region
  #            WHERE reg_uid IN ({county}) AND reg_group = 'C'),
  #          lsk AS
  #          (SELECT reg_uid AS landskaps_id, reg_name AS landskap
  #            FROM reg_region
  #            WHERE reg_uid IN ({landsc}) AND reg_group = 'P'),
  #          mun AS
  #          (SELECT reg_uid AS kommun_id, reg_name AS kommun
  #            FROM reg_region
  #            WHERE reg_uid IN ({municipality}) AND reg_group = 'M'),
  #         reg AS
  #          (SELECT reg_uid AS reg_id, reg_name AS regions
  #            FROM reg_region
  #            WHERE reg_uid IN ({region}) AND reg_group = 'C')
  #  
  #     SELECT
  #       t.speUId,
  #       t.art,
  #       t.sitUId,
  #       t.Lokalnamn,
  #       t.sitetype,
  #       t.landskap,
  #       t.län,
  #       t.kommun,
  #       --t.regions,
  #       t.dag,
  #       t.lat,
  #       t.lon,
  #       t.sumval_rank,
  #       MAX(sumval)
  #          
  #     FROM
  #       (SELECT
  #         spe.spe_uid AS speuid,
  #         spe.spe_semainname As art,
  #         obs.obs_typ_vfcid AS verification_code,
  #         sit.sit_uid AS situid,
  #         sit.sit_name AS Lokalnamn,
  #         vis.vis_begintime AS dag,
  #         sit.sit_type AS sitetype,
  #         sit.sit_geosweref99tmlat AS lat,
  #         sit.sit_geosweref99tmlon AS lon,
  #         cou.län AS län,
  #         lsk.landskap AS landskap,
  #         mun.kommun AS kommun,
  #        -- reg.regions AS region,
  #         EXTRACT (week FROM vis_begintime::date) AS vecka,
  #         SUM(obs.obs_count) AS sumval,
  #         dense_rank() OVER (PARTITION BY spe.spe_uid,sit.sit_uid 
  #         ORDER BY SUM(obs.obs_count) DESC, vis.vis_uid ) AS sumval_rank
  #       
  #       FROM obs_observation AS obs
  #       
  #         INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
  #         INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
  #         INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
  #         INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
  #         --INNER JOIN reg_region AS reg ON sit.sit_reg_provinceid = reg.reg_uid
  #         INNER JOIN spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid      -- så här bör det väl vara?
  #         INNER JOIN cou ON sit.sit_reg_countyid = cou.cou_id
  #         INNER JOIN lsk ON sit.sit_reg_provinceid = lsk.landskaps_id
  #         INNER JOIN mun ON sit.sit_reg_municipalityid = mun.kommun_id
  #         --INNER JOIN reg ON sit.sit_reg_municipalityid = reg.reg_id
  #        
  #       WHERE (spv.spv_istrim=TRUE or spe_uid in (135,131,132,133,139) ) -- Include nullobs and 4 aggregated species groups
  #         AND extract('YEAR' from vis_begintime) IN {year}
  #         AND vis_typ_datasourceid IN {source}
  #         AND not (vis_typ_datasourceid = 55  and sit_reg_countyid=2)
  #         AND spe.spe_uid IN {Art}
  #         AND obs.obs_typ_vfcid IN {verification}
  #       GROUP BY
  #         spe.spe_uid, sit.sit_uid, vis.vis_uid, obs.obs_typ_vfcid, sitetype, cou.cou_id, cou.län, lsk.landskaps_id, lsk.landskap, mun.kommun_id, mun.kommun --, reg.reg_id, reg.regions,
  #       ORDER BY
  #           spe.spe_uid,sit.sit_uid) AS t -- End of inner select
  #          
  #     WHERE t.sumval_rank =1 --between 1 and 3 
  #     GROUP BY
  #       t.speUId, t.art,t.situid,t.Lokalnamn,t.lat,t.lon,t.sumval_rank, t.dag, t.län, t.kommun, t.landskap, t.sitetype --, t.regions
  #     ORDER BY
  #       t.speuid, MAX DESC, t.situid;"
  #     
  # )
  # 
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

#' Retrieve Species Data for Trim Functions
#'
#' This function retrieve min and max flight week for each species. Used for the
#' trim indices
#'
#' @inheritParams sebms_species_abundance_plot 
#' @param filterPattern a regex pattern to filter SQL query
#' @param topList logical; whether the top list of species should be used
#' 
#' @import tibble
#' @importFrom glue glue
#' @import dplyr
#' @importFrom lubridate year today
#' @importFrom DBI dbGetQuery
#' 
#' @return a tibble with species IDs, name, and min and max flight time week
#' @export
sebms_trimSpecies <- function(year = 2010:lubridate::year(lubridate::today()), Art = 1:200, filterPattern = NULL, topList = FALSE, source = c(54,55,56,63,64,66,67,84)) {
  
  year <- glue("({paste0({year}, collapse = ',')})") # Make year span to a vector of years for the SQL
  Art <- glue("({paste0({Art}, collapse = ',')})")
  source <- glue("({paste0({source}, collapse = ',')})")
  #verification <- glue("({paste0({verification}, collapse = ',')})")
  
  if (!is.null(filterPattern)) {
    filterPattern <- glue("AND {filterPattern}")
  }
  
  if (topList) {
    
    q <- glue("
         SELECT
            spe.spe_uid as speuid,
            spe.spe_semainname AS species,
            obs.obs_typ_vfcid AS verification_code,
            SUM(obs.obs_count) as speciesno
          FROM obs_observation AS obs
            INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
            INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
            INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
            INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
            INNER JOIN  typ_type as typ on vis.vis_typ_datasourceid = typ.typ_uid
            INNER JOIN  spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid
          WHERE EXTRACT('YEAR' from vis_begintime) IN {year}
            AND vis_typ_datasourceid IN  {source} 
            AND obs.obs_typ_vfcid IN {verification}
            {filterPattern}
            --AND spv.spv_istrim=TRUE 
          GROUP BY
            spe.spe_uid, obs.obs_typ_vfcid
          ORDER BY
            speciesno DESC;
")
  }else {
    q <- glue("SELECT
                spv_flightweekmin AS min,
                spv_flightweekmax AS max,
                spv_spe_speciesid AS speuid,
                spe_semainname AS art
              FROM spv_speciesvalidation AS spv
                INNER JOIN spe_species AS spe ON spe.spe_uid = spv_spe_speciesid
              WHERE
                spv_spe_speciesid IN {Art}
                AND
                spv_flightweekmin IS NOT NULL
              ORDER BY speuid;")
  }
  
  
  
  
  sebms_pool <- sebms_assert_connection(quiet = T)
  res <- dbGetQuery(sebms_pool, q) %>% 
    as_tibble()
  return(res)
}


#' Get Visits for Trim Indices
#'
#' Retrieve data with visits per site per year.
#'
#' @inheritParams sebms_trimSpecies
#' @param minmax the first and last week of interest
#' 
#' @import tibble
#' @importFrom glue glue
#' @import dplyr
#' @importFrom lubridate year today
#' @importFrom DBI dbGetQuery
#' @importFrom pool localCheckout
#' 
#' @return a tibble with visits per year and site.
#' 
#' @export
sebms_trimvisits <- function(year = 2010:lubridate::year(lubridate::today()), minmax = 22:32, source = c(54,55,56,63,64,66,67,84)) {
  
  minmax <- glue("({paste0({minmax}, collapse = ',')})") 
  year <- glue("({paste0({year}, collapse = ',')})") # Make year span to a vector of years for the SQL
  source <- glue("({paste0({source}, collapse = ',')})")
  #verification <- glue("({paste0({verification}, collapse = ',')})")
  
  q <-  glue("SELECT
                sit.sit_uid AS siteuid,
                --obs.obs_typ_vfcid AS verification_code,
                EXTRACT (year FROM vis_begintime::date) AS year,
                COUNT(DISTINCT vis_begintime) AS visit
             FROM obs_observation AS obs
                INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
                INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
                INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
                INNER JOIN rer_regionrelation AS rer ON sit.sit_reg_countyid = rer.rer_reg_childid
                INNER JOIN reg_region AS reg on rer.rer_reg_parentid = reg.reg_uid
                INNER JOIN  typ_type as typ on vis.vis_typ_datasourceid = typ.typ_uid
             WHERE EXTRACT (week FROM vis_begintime::date) IN {minmax}
                AND EXTRACT(YEAR FROM vis_begintime) IN {year}
                AND vis_typ_datasourceid IN {source} --(54,55,56,63,64,66,67)
                --AND obs.obs_typ_vfcid IN 
            GROUP BY
                year, siteuid --, obs.obs_typ_vfcid
            ORDER BY
                siteuid, year;")
  
  sebms_pool <- sebms_assert_connection(quiet = T)
  # con <- pool::localCheckout(sebms_pool)
  res <- dbGetQuery(sebms_pool, q) %>% 
    as_tibble()
  return(res)
  
}


#' Get Species Observations for Trim Indices
#'
#' Retrieve data with observations of individuals per year.
#'
#' @inheritParams sebms_abundance_per_species_plot
#' @param Region character or reg ex; which bioregion you want the data from
#' @param minmax the first and last week of interest
#' 
#' @import tibble
#' @importFrom glue glue
#' @import dplyr
#' @importFrom lubridate year today
#' @importFrom DBI dbGetQuery
#' 
#' @return a tibble with number of observed individuals per year and site.
#' 
#' @export
sebms_trimobs <- function(year = 2010:lubridate::year(lubridate::today()), Art = 1:200, Län = ".", Region = ".", Landskap = ".", Kommun = ".", filterPattern = NULL, minmax = 22:32, verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84)) {
  
  minmax <- glue("({paste0({minmax}, collapse = ',')})") 
  year <- glue("({paste0({year}, collapse = ',')})") # Make year span to a vector of years for the SQL
  source <- glue("({paste0({source}, collapse = ',')})")
  verification <- glue("({paste0({verification}, collapse = ',')})")
  Art <- glue("({paste0({Art}, collapse = ',')})")
  
  Län <- paste0(str_to_lower(Län),collapse = "|") # Make the list of Län to s regex statement
  Landskap <- paste0(str_to_lower(Landskap),collapse = "|") # Make the list of Landskap to s regex statement
  Kommun <- paste0(str_to_lower(Kommun),collapse = "|") # Make the list of Kommun to s regex statement
  Region <- paste0(str_to_lower(Region),collapse = "|") # Make the list of Regioner to s regex statement
  
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
  
  part <- regID %>% 
    filter(str_detect(reg_name, Region)) %>% 
    pull(reg_uid) %>% 
    paste0(collapse = ',') # Make the id-numbers a vector palatable to the SQL
  
  
  if (is.null(filterPattern)) {
    filterPattern <- glue("--NOTHING")
  }else{
    filterPattern <- glue("AND {filterPattern}")
  }
  
  q <-  glue("
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
             WHERE reg_uid IN ({municipality}) AND reg_group = 'M'),
           bioreg AS
           (SELECT reg_uid AS region_id, reg_name AS region
             FROM reg_region
             WHERE reg_uid IN ({part}) AND reg_group = 'R')
           
            SELECT DISTINCT
                sit.sit_uid AS siteuid,
                obs.obs_typ_vfcid AS verification_code,
                EXTRACT (year FROM vis_begintime::date) AS year,
                SUM(obs.obs_count) AS total_number,
                reg.län,
                lsk.landskap,
                mun.kommun --,
             --   bioreg.region
          
             FROM obs_observation AS obs
                INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
                INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
                INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
                INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
                INNER JOIN spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid
                INNER JOIN rer_regionrelation AS rer ON sit.sit_reg_countyid = rer.rer_reg_childid
                --INNER JOIN reg ON rer.rer_reg_parentid = reg.reg_id
                INNER JOIN reg ON sit.sit_reg_countyid = reg.reg_id
                INNER JOIN lsk ON sit.sit_reg_provinceid = lsk.landskaps_id
                INNER JOIN mun ON sit.sit_reg_municipalityid = mun.kommun_id
               -- INNER JOIN bioreg ON reg.reg_id = bioreg.region_id
                INNER JOIN typ_type AS typ on vis.vis_typ_datasourceid = typ.typ_uid
             WHERE EXTRACT (week FROM vis_begintime::date) IN {minmax}
                AND EXTRACT(YEAR FROM vis_begintime) IN {year}
                AND vis_typ_datasourceid IN {source} --(54,55,56,63,64,66,67)
                AND obs.obs_typ_vfcid IN {verification}
                {filterPattern}  -- THIS ADDS A FILTER TO THE SQL FROM THE FUNCTION
                AND spe.spe_uid IN {Art}
            GROUP BY
                year, siteuid, reg.reg_id, reg.län,lsk.landskaps_id, lsk.landskap, mun.kommun_id, mun.kommun, obs.obs_typ_vfcid  --bioreg.region_id, bioreg.region, 
            ORDER BY
                siteuid, year;")
  
  sebms_pool <- sebms_assert_connection(quiet = T)
  res <- dbGetQuery(sebms_pool, q) %>% 
    as_tibble()
  return(res)
}



#' Retrieve Sites for Trim Functions
#'
#' This function retrieve the sites used for the trim indices
#'
#' @inheritParams sebms_trimobs
#' 
#' @import tibble
#' @importFrom glue glue
#' @import dplyr
#' @importFrom lubridate year today
#' @importFrom DBI dbGetQuery
#' 
#' @return a tibble with species IDs, name, and min and max flight time week
#' @export
sebms_trimSites <- function(year = 2010:lubridate::year(lubridate::today()), Landskap = ".", Region = ".", Län = ".", Kommun = ".", source = c(54,55,56,63,64,66,67,84)) {
  
  year <- glue("({paste0({year}, collapse = ',')})") # Make year span to a vector of years for the SQL
  source <- glue("({paste0({source}, collapse = ',')})")
  #verification <- glue("({paste0({verification}, collapse = ',')})")
  Län <- paste0(str_to_lower(Län),collapse = "|") # Make the list of Län to s regex statement
  Landskap <- paste0(str_to_lower(Landskap),collapse = "|") # Make the list of Landskap to s regex statement
  Kommun <- paste0(str_to_lower(Kommun),collapse = "|") # Make the list of Kommun to s regex statement
  Region <- paste0(str_to_lower(Region),collapse = "|") # Make the list of Regioner to s regex statement
  
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
  
  part <- regID %>% 
    filter(str_detect(reg_name, Region)) %>% 
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
             WHERE reg_uid IN ({municipality}) AND reg_group = 'M'),
          part AS
           (SELECT reg_uid AS region_id, reg_name AS region
             FROM reg_region
             WHERE reg_uid IN ({part}) AND reg_group = 'R')
              
              SELECT
                sit.sit_uid AS site,
                reg.reg_code AS region,
                reg.reg_uid AS regID,
                sit.sit_reg_countyid as countyid,
                sit.sit_reg_provinceid as provinceid
              FROM sit_site AS sit
                INNER JOIN rer_regionrelation AS rer ON sit.sit_reg_countyid = rer.rer_reg_childid
                INNER JOIN reg_region AS reg on rer.rer_reg_parentid = reg.reg_uid
              WHERE
                sit_typ_datasourceid IN {source}
                --AND reg.reg_uid IN ({county})
              ORDER BY site;")
  
  sebms_pool <- sebms_assert_connection(quiet = T)
  res <- dbGetQuery(sebms_pool, q) %>% 
    as_tibble() #%>% 
    #filter(regid %in% county)
  return(res)
}
