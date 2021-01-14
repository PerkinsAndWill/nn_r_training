# Part 1

library(tigris)
library(tidyverse)
library(sf)
library(leaflet)
library(stringr)

urb_areas = urban_areas()

head(urb_areas)

portland_urban_area = urb_areas %>%
  filter(str_detect(NAME10,'Portland, OR')) %>%
  st_transform(4326)

#Union, buffer, cast, and simplify all done here to try to remove vertices from boundary, making subsetting faster (but not as precise)
simp_pdx_ua = portland_urban_area %>%
  st_transform(2269) %>%
  st_union() %>%
  st_buffer(1) %>%
  st_cast('POLYGON') %>%
  st_simplify()

#Have to query these objects by state and county separately
or_roads = roads(state='OR',county = c('Multnomah','Washington','Clackamas')) %>%
  st_transform(4326)
wa_roads = roads(state='WA',county = c('Clark'))  %>%
  st_transform(4326)

#Binding sf objects is still a little wonky - have to reassert as sf column and object
bound_roads = bind_rows(
  or_roads %>% as_tibble(),
  wa_roads %>% as_tibble()
) %>%
  mutate(geometry = st_sfc(geometry,crs=4326)) %>%
  st_as_sf() %>%
  st_transform(2269) 

#This will take 30 seconds or so, road data is big!
ggplot()+
  geom_sf(data = bound_roads,color='red')+
  geom_sf(data = simp_pdx_ua,color='black',size=1,
          #Want to have no fill to see roads through polygon
          fill=NA)

#I was going to do the intersection with the actual boundaries, but it was taking a few minutes, so not great for an example. You can do on your own! I did with the convex hull instead. 
# sub_bound_roads  = bound_roads %>%
#   st_intersection(simp_pdx_ua)

#Sometimes if you are doing complex spatial subsetting, you can speed it up using convex hulls
pdx_hull = portland_urban_area %>%
  st_transform(2269) %>%
  st_convex_hull()

#Here is what a convex hull looks like
ggplot()+
  geom_sf(data = portland_urban_area,color='black',size=1,
          #Want to have no fill to see roads through polygon
          fill=NA)+
  geom_sf(data= pdx_hull,color='blue',size=1,fill=NA)

#Codebook for MTFCC feature in TIGER geographies here: https://www2.census.gov/geo/pdfs/reference/mtfccs2019.pdf
big_roads = bound_roads %>%
  filter(MTFCC %in% c('S1100', #Primary roads
                      'S1200')) %>% #Secondary roads
  st_cast('LINESTRING') %>%
  st_intersection(pdx_hull %>%
                    select(geometry)) %>% 
  mutate(sf_type = map_chr(geometry,~class(.x)[2])) %>%
  filter(sf_type=='LINESTRING') %>%
  st_cast('LINESTRING')

#Grab Places to subset as well
orwa_places = places(state=c('OR','WA'))
pdx_places = orwa_places %>%
  st_transform(2269) %>%
  st_intersection(simp_pdx_ua)


leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  #You can put a polygon on a leaflet map as a polyline to only show boundary and not area
  addPolylines(data = simp_pdx_ua %>% st_transform(4326),
               color='black',weight=2) %>%
  addPolygons(data = pdx_places %>% st_transform(4326),
              fillColor='red',fillOpacity = 0.5,color='white',
              weight=1,label=~NAMELSAD,
              #Highlight options helpful when you have overlapping objects
              highlightOptions = highlightOptions(weight=3,sendToBack = TRUE,
                                                  fillOpacity = 0.8)) %>%
  addPolylines(data=big_roads %>% st_transform(4326),
               color='blue',weight=~ifelse(MTFCC=='S1200',4,2),
               label=~FULLNAME)

#Part 2

library(scales)

pdx_cdp_med_income = get_acs(state=c('OR','WA'),geography = 'place',
                             variables = c(medincome = "B19013_001")) %>%
  #Filtering for GEOIDs in PDX Places
  filter(GEOID %in% pdx_places$GEOID)

# pdx_income_shp = get_acs(state=c('OR','WA'),geography = 'place',
#                              variables = c(medincome = "B19013_001"),
#                          geometry = TRUE) %>%
#   filter(GEOID %in% pdx_places$GEOID)
# 
# write_sf(pdx_income_shp,'dir/test_shp.shp')

#Joining ACS data to shape
pdx_cdp_mi_shape = pdx_places %>%
  left_join(pdx_cdp_med_income %>%
              select(GEOID,estimate))

income_pal = colorNumeric(
  #Scales package lets you access color brewer palettes
  palette = brewer_pal(palette = 'Greens')(5),
  domain =  pdx_cdp_mi_shape$estimate
)

leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  #You can put a polygon on a leaflet map as a polyline to only show boundary and not area
  addPolylines(data = simp_pdx_ua %>% st_transform(4326),
               color='black',weight=2) %>%
  addPolygons(data = pdx_cdp_mi_shape %>% st_transform(4326),
              fillColor=~income_pal(estimate),fillOpacity = 0.5,color='white',
              weight=1,label=~paste0(NAMELSAD,': ',dollar(estimate)),
              highlightOptions = highlightOptions(weight=3,sendToBack = TRUE,
                                                  fillOpacity = 0.8)) %>%
  addLegend(pal = income_pal,values =pdx_cdp_mi_shape$estimate,
            labFormat = labelFormat(prefix = '$'),title = 'Median<br>Household<br>Income')

#Part 3
library(lodes)
library(lehdr)
#Can only grab one state at a time right now
orwa_lodes =grab_lodes(state = c('or','wa'),year = 2017,lodes_type = 'od',
                       agg_geo = 'bg',segment = 'S000',state_part = c('aux')) %>%
  bind_rows(grab_lodes(state = c('or','wa'),year = 2017,lodes_type = 'od',
                       agg_geo = 'bg',segment = 'S000',state_part = c('main')))

orwa_xwalk = bind_rows(
  read_xwalk('or'),
  read_xwalk('wa')
)

sub_xwalk = orwa_xwalk %>% 
  filter(stplc %in% pdx_places$GEOID) %>%
  distinct(st,bgrp,stplc) %>%
  #Need to have all codes as character to join properly
  mutate_all(as.character)

agg_plc_lodes = orwa_lodes %>%
  rename(total_jobs = `S000`) %>%
  group_by(h_bg,w_bg) %>%
  summarise(total_jobs = sum(total_jobs)) %>%
  left_join(sub_xwalk %>%
              rename(h_st = st,
                     h_bg = bgrp,
                     h_stplc=stplc)) %>%
  left_join(sub_xwalk %>%
              rename(w_st = st,
                     w_bg = bgrp,
                     w_stplc=stplc)) %>%
  group_by(h_st,h_stplc,w_st,w_stplc) %>%
  summarise(total_jobs = sum(total_jobs))

sorted_plc_lodes = agg_plc_lodes %>%
  filter(!is.na(h_stplc),!is.na(w_stplc)) %>%
  group_by(h_stplc,w_stplc) %>%
  mutate(sorted_id = paste(sort(c(h_stplc,w_stplc)),collapse = '-')) %>%
  ungroup() %>%
  group_by(sorted_id) %>%
  summarise(total_bidir_jobs = sum(total_jobs))

#Going to use centroids to draw OD lines
pdx_plc_cents = pdx_places %>%
  st_centroid()

agg_plc_od_shp = agg_plc_lodes %>%
  ungroup() %>%
  filter(!is.na(h_stplc),!is.na(w_stplc),
         #Filtering out where home and work are same location (no line to draw...)
         h_stplc!=w_stplc) %>%
  mutate(od_line = pmap(.l = list(h_stplc,w_stplc),
                        .f = function(h_stplc,w_stplc){
                          
                          h_cent = pdx_plc_cents %>%
                            filter(GEOID == h_stplc)
                          
                          w_cent = pdx_plc_cents %>%
                            filter(GEOID == w_stplc)
                          
                          hw_line = bind_rows(
                            h_cent %>% st_coordinates() %>% as_tibble(),
                            w_cent %>% st_coordinates() %>% as_tibble()
                          ) %>%
                            #Have to convert to matrix to coerce into linestring
                            as.matrix() %>%
                            st_linestring()
                          
                          return(hw_line)
                          
                        })) %>%
  mutate(od_line = st_sfc(od_line,crs =2269)) %>%
  st_as_sf() %>%
  left_join(pdx_places %>%
              as_tibble() %>%
              select(GEOID,NAME) %>%
              rename(h_stplc = GEOID,
                     home_place = NAME)) %>%
  left_join(pdx_places %>%
              as_tibble() %>%
              select(GEOID,NAME) %>%
              rename(w_stplc = GEOID,
                     work_place = NAME))

#Setting a weight multiplier so job flow can be visualized proportionally on leaflet map
max_job_flow = max(agg_plc_od_shp$total_jobs)
max_weight = 30
weight_mult = max_weight/max_job_flow


leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolylines(data = simp_pdx_ua %>% st_transform(4326),
               color='black',weight=2,group = 'Urban Area Boundary') %>%
  addPolygons(data = pdx_places %>% st_transform(4326),
              fillColor='green',fillOpacity = 0.5,color='white',
              weight=1,label=~NAMELSAD,
              highlightOptions = highlightOptions(weight=3,sendToBack = TRUE,
                                                  fillOpacity = 0.8),
              group='Place Polygons') %>%
  addPolylines(data=big_roads %>% st_transform(4326),
               color='blue',weight=~ifelse(MTFCC=='S1200',4,2),
               label=~FULLNAME,
               group='Roads') %>%
  addPolylines(data = agg_plc_od_shp %>% st_transform(4326),
               color = 'red',weight =~ total_jobs * weight_mult,
               label=~ paste0(home_place,' to ',work_place,': ',comma(total_jobs),' jobs'),
               opacity = 0.5,
               highlightOptions = highlightOptions(sendToBack = TRUE,opacity = 1),
               group='Job Flows') %>%
  addLayersControl(overlayGroups = c('Urban Area Boundary','Roads','Place Polygons','Job Flows'))
