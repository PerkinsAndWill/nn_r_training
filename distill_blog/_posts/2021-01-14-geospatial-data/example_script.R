setwd("C:/Users/Blancb/OneDrive - Perkins and Will/Documents/Internal/R Training/nn_r_training/topics_setup/03_geospatial")

#Load in packages
library(tidyverse)
library(sf)
library(readxl)
library(janitor)
library(leaflet)
library(tigris)

#Read in Excel files
project_cities = read_excel('data/Projects_MASTER.xlsx',sheet='GISJOIN_202004')
nn_offices = read_excel('data/Projects_MASTER.xlsx',sheet='nn_offices')

#Add geometry to project cities using st_as_sf() function
project_cities_sf = project_cities %>%
  clean_names() %>%
  #Filter out international projects
  filter(is.na(international)) %>%
  #Select columns of interest
  select(city,state,x,y,all:other) %>%
  #Can turn data frame into sf data frame by referencing coordinates (for points)
  st_as_sf(coords = c('x','y'),crs = 4326)



#Creating demonstration line
office_line = st_linestring(nn_offices %>%
                              select(X,Y) %>%
                              arrange(X,Y) %>%
                              as.matrix()) %>%
  st_sfc(crs=4326)

leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolylines(data=office_line)

#Demonstration polygons
wa_counties = counties('Washington')

#Same as above
nn_offices_sf = nn_offices %>%
  clean_names() %>%
  st_as_sf(coords = c('x','y'),crs=4326)

#Making custom NN icon based on JPEG image stored on website. 
nn_icon = makeIcon(iconUrl = 'https://perkinsandwill.github.io/nn_r_training/topics_setup/03_geospatial/graphics/NNlogo_RGB_300dpi_JPEG.jpg',
                   #Making icon 10% of original size   
                   iconWidth = 331*0.1,
                   iconHeight = 450*0.1,
                   #Anchor point at center of icon (halfway in X and Y)
                   iconAnchorX = 331*0.1*0.5,
                   iconAnchorY = 450*0.1*0.5)

#Basic map showing offices as icons (icon assembled above)
leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addMarkers(data =nn_offices_sf,icon = nn_icon,
             #Using formula notation, as discussed above, can reference attributes
             label=~city)

nn_office_buffers = nn_offices_sf %>%
  #Going to use Oregon North (2269) even though this will be slightly inaccurate with a nationwide buffer calculation
  st_transform(2269) %>%
  #Create fifty mile buffer
  st_buffer(5280*50) %>%
  #Transform back to WGS84
  st_transform(4326) %>%
  mutate(area_m2 = st_area(geometry))

#Basic map showing buffer polygons
leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(data = nn_office_buffers,
              label=~city)

#Using Intersection to group project cities within 50 miles of an office. 
sub_project_cities_sf = project_cities_sf %>%
  #Intersection only operates with planar coordinates, need to transform to state plane temporarily
  st_transform(2269) %>%
  #Intersecting with office buffers
  st_intersection(nn_office_buffers %>% 
                    select(nn_office_id,city,geometry) %>%
                    rename(office_city = city) %>%
                    st_transform(2269)) %>%
  #Transform back for mapping
  st_transform(4326)

#Mapping offices and project cities using groups and layer control. 
#Sizing circle markers for project cities by number of projects. 
leaflet() %>%
  addProviderTiles('CartoDB.DarkMatter') %>%
  addMarkers(data =nn_offices_sf,icon = nn_icon,
             label=~city,group=~city) %>%
  addCircleMarkers(data=sub_project_cities_sf,
                   radius=~all*1.5, label=~paste0(city,": ",all,' projects'),
                   group=~office_city,
                   fillOpacity = 0.7,color='white',opacity =0.9,
                   fillColor = '#00659A',weight=3) %>%
  addLayersControl(baseGroups = unique(sub_project_cities_sf$office_city))

#Create a summary for plotting
summ_project_cities_by_office = sub_project_cities_sf %>%
  #Use the as_tibble() function to get rid of sf features -- otherwise geometries will not come off in select function. 
  as_tibble() %>%
  select(office_city,transit:other) %>%
  #pivot by project type
  pivot_longer(transit:other,names_to='sector',values_to='num_projects') %>%
  #filter out project types where num_projects is 0
  filter(num_projects>0) %>%
  group_by(office_city,sector) %>%
  summarise(num_projects = sum(num_projects)) %>%
  #computed proportion of projects for second of below plots
  mutate(prop_projects = num_projects/sum(num_projects)) 

#Plot projects by office and sector
ggplot(summ_project_cities_by_office,
       aes(x=office_city,y=num_projects,fill=sector))+
  geom_col()+
  coord_flip()+
  ggtitle('Local Projects by Office and Sector')

#Use proportion to create a 100% bar plot
ggplot(summ_project_cities_by_office,
       aes(x=office_city,y=prop_projects,fill=sector))+
  geom_col()+
  coord_flip()+
  #Use percent labels function from scales package
  scale_y_continuous(labels=scales::percent,name='Proportion of Projects')+
  ggtitle('Proportion of Local Projects by Office and Sector')
