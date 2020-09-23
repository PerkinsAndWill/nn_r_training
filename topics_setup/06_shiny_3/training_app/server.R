server = function(input,output,session){
  
  withProgress(value = 0.3, message = 'Scraping beer data...',{
    
    updateButton(session,"execute",label="Scraping...",style="danger",disabled = TRUE)
    
    raw = read_html('https://www.belmont-station.com/bottles') %>%
      html_nodes('table') %>%
      html_table() %>%
      tibble() %>%
      unnest(cols = c(.))
    
    colnames(raw) = raw[1,]
    
    clean = raw %>%
      clean_names() %>%
      slice(-1)
    
    uq_origins = clean %>%
      distinct(origin) %>%
      mutate(us_state_code = map_chr(origin,function(origin){
        filt_states = us_states %>%
          filter(NAME==str_to_title(origin)) 
        
        if(nrow(filt_states)>0){
          st_cd = filt_states$STUSPS[1]
          return(st_cd)
        }else if (nrow(filt_states)==0){
          return(NA)
        }
      })) %>%
      mutate(
        country_code = map2_chr(origin,us_state_code,function(origin,us_state_code){
          
          if(!is.na(us_state_code)){
            return('USA')
          }else{
            
            filt_countries = world_countries %>%
              filter(str_to_upper(admin)==str_to_upper(origin))
            
            if(nrow(filt_countries)>0){
              return(filt_countries$gu_a3[1])
            }else{
              if(origin %in% c('England','Scotland','Wales','Ireland')){
                return('GBR')
              }else if(origin %in% c('US Macro')){
                return('USA')
              }else{
                return(NA)
              }
            }
            
          }
          
        })
      ) %>%
      filter(!is.na(country_code)) %>%
      mutate(country_name = map_chr(country_code,~world_countries$admin[world_countries$gu_a3==.x]),
             us_state_name = map_chr(us_state_code,function(us_state_code){
              
               if(us_state_code %in% us_states$STUSPS){
                 return(us_states$NAME[us_states$STUSPS==us_state_code])
               }else{
                 return(NA)
               }
             }))
    
    uq_styles = clean %>%
      distinct(style) %>%
      mutate(major_style = map_chr(style,~str_split(.x,'-')[[1]][1]) %>%
               str_trim(),
             minor_style = map_chr(style,~str_split(.x,'-')[[1]][2]) %>%
               str_trim()) %>%
      filter(nchar(style)>0) %>%
      mutate(major_style = case_when(
        major_style=='Cider Dry'~'Cider',
        TRUE~major_style
      )) %>%
      mutate(minor_style = case_when(
        minor_style==''~'<None>',
        is.na(minor_style)~'<None>',
        TRUE~minor_style
      ))
    
    beer_db = clean %>%
      left_join(uq_origins) %>%
      filter(!is.na(country_code)) %>%
      left_join(uq_styles) %>%
      mutate(geog_label = ifelse(country_code=='USA',
                                 paste0('USA: ',us_state_name),
                                 country_name
                                 ))
    
    uq_major_styles = beer_db %>%
      distinct(major_style)
    
    uq_minor_styles = beer_db %>%
      distinct(minor_style)
    
    avail_countries = world_countries %>%
      filter(gu_a3 %in% beer_db$country_code,
             gu_a3 !='USA')
    
    avail_us_states = us_states %>%
      filter(STUSPS %in% beer_db$us_state_code)
    
    beer_geogs = bind_rows(
      avail_countries %>%
        as_tibble(),
      avail_us_states %>%
        as_tibble() %>%
        select(STUSPS,NAME,
               geometry) %>%
        rename(us_state_code = STUSPS,
               us_state_name = NAME) %>%
        mutate(gu_a3='USA')
    ) %>%
      mutate(geog_label = ifelse(is.na(us_state_name),
                                 admin,
                                 paste0('USA: ',us_state_name))) %>%
      mutate(geometry = st_sfc(geometry,crs=4326)) %>%
      st_as_sf()
  
    updatePickerInput(session,'maj_sty',choices = sort(uq_major_styles$major_style),
                      selected = sort(uq_major_styles$major_style))
    
    updatePickerInput(session,'min_sty',choices = sort(uq_minor_styles$minor_style),
                      selected = sort(uq_minor_styles$minor_style))
    
    output$select_map = renderLeaflet({
      
      leaflet() %>%
        addProviderTiles('CartoDB.Positron') %>%
        addPolygons(data = beer_geogs,fillColor = 'red',color='white',
                    fillOpacity = 0.5,opacity = 0.5,weight = 2,
                    highlightOptions = highlightOptions(fillOpacity = 0.9,opacity = 0.9,
                                                        weight = 5),
                    label=~geog_label,layerId =~geog_label)
      
    })
    
    sel_geogs <<- beer_geogs$geog_label
    
    updateButton(session,"execute",label="Execute Filters",style="default",disabled = FALSE)
    
    })
  
  observeEvent(input$select_map_shape_click,{
  
    clicked_geog = input$select_map_shape_click$id
    
    if(clicked_geog %in% sel_geogs){
      #Unselected
      sel_geogs <<- sel_geogs[sel_geogs!=clicked_geog]
      leafletProxy('select_map') %>%
        removeShape(clicked_geog) %>%
        addPolygons(data=beer_geogs %>%
                      filter(geog_label==clicked_geog),
                    fillColor = 'dark grey',color='white',
                    fillOpacity = 0.5,opacity = 0.5,weight = 2,
                    highlightOptions = highlightOptions(fillOpacity = 0.9,opacity = 0.9,
                                                        weight = 5),
                    label=~geog_label,layerId =~geog_label)
    }else{
      #Selected
      sel_geogs <<- c(sel_geogs,clicked_geog)
      leafletProxy('select_map') %>%
        removeShape(clicked_geog) %>%
        addPolygons(data=beer_geogs %>%
                      filter(geog_label==clicked_geog),
                    fillColor = 'red',color='white',
                    fillOpacity = 0.5,opacity = 0.5,weight = 2,
                    highlightOptions = highlightOptions(fillOpacity = 0.9,opacity = 0.9,
                                                        weight = 5),
                    label=~geog_label,layerId =~geog_label)
    }
    
    
      
  })
  
  observeEvent(input$execute,{
    
    sel_maj_sty = input$maj_sty
    sel_min_sty = input$min_sty
    #sel_maj_sty = uq_major_styles$major_style
    #sel_min_sty = uq_minor_styles$minor_style
    
    if(length(sel_maj_sty)==0){
      showNotification('Wait for Styles to Finish Loading',type = 'warning',duration = 3)
    }else{
      updateButton(session,"execute",label="Loading Filters",style="warning")
      
      withProgress(value = 0.1,message = 'Executing filters...',{
        
        sub_beer_db = beer_db %>%
          filter(major_style %in% sel_maj_sty,
                 minor_style %in% sel_min_sty,
                 geog_label %in% sel_geogs)
        
        new_uq_major_styles = sort(unique(sub_beer_db$major_style))
        new_uq_minor_styles = sort(unique(sub_beer_db$minor_style))
        
        updatePickerInput(session,'maj_sty',selected = new_uq_major_styles)
        updatePickerInput(session,'min_sty',selected = new_uq_minor_styles)
        
        updateTabItems(session,'tabs',selected = 'browse')
      })
      
      withProgress(value = 0.3,message='Constructing charts and tables...',{
        
        #Plots by Geography
        pf_1 = sub_beer_db %>%
          group_by(geog_label,major_style) %>%
          summarise(num_beers = n()) %>%
          mutate(prop_within_geog = num_beers/sum(num_beers)) 
        
        summ_geog = pf_1 %>%
          group_by(geog_label) %>%
          summarise(num_beers = sum(num_beers)) %>%
          arrange(num_beers)
        
        summ_style = pf_1 %>%
          group_by(major_style) %>%
          summarise(num_beers = sum(num_beers)) %>%
          arrange(num_beers)
        
        pf_1$geog_label = factor(pf_1$geog_label,ordered=TRUE,
                                 levels=summ_geog$geog_label)
        
        pf_1$major_style = factor(pf_1$major_style,ordered=TRUE,
                                  levels = summ_style$major_style)
        
        output$geog_style_abs = renderPlot({
          ggplot(pf_1,aes(x=geog_label,y=num_beers,fill=major_style))+
            geom_col()+
            coord_flip()+
            scale_fill_brewer(palette = 'Set3',name='Major Style')+
            xlab('Geography')+
            scale_y_continuous(labels=comma,name='# Beverages')+
            ggtitle('Number of Beverages by Geography and Major Style')
        })
        
        output$geog_style_prop = renderPlot({
          ggplot(pf_1,aes(x=geog_label,y=prop_within_geog,fill=major_style))+
            geom_col()+
            coord_flip()+
            scale_fill_brewer(palette = 'Set3',name='Major Style')+
            xlab('Geography')+
            scale_y_continuous(labels=percent,name='% of Beverages')+
            ggtitle('Proportion of Beverages by Major Style within Geography')
        })
        
        #Plots by Style
        pf_2 = sub_beer_db %>%
          group_by(major_style,minor_style) %>%
          summarise(num_beers = n()) %>%
          mutate(prop_within_style = num_beers/sum(num_beers)) %>%
          mutate(maj_style_total = sum(num_beers)) %>%
          ungroup() %>%
          arrange(desc(maj_style_total)) %>%
          mutate(major_style_lab = paste0(major_style,' (N=',maj_style_total,')'),
                 major_style_lab = factor(major_style_lab,ordered=TRUE,
                                          levels = unique(major_style_lab))) %>%
          group_by(minor_style) %>%
          mutate(min_style_total = sum(num_beers)) %>%
          ungroup() %>%
          arrange(major_style,desc(min_style_total)) %>%
          mutate(minor_style = reorder_within(minor_style,num_beers,major_style_lab))
        
        
        output$minor_style = renderPlot({
          ggplot(pf_2,aes(x=minor_style,y=num_beers))+
            geom_col()+
            coord_flip()+
            facet_col(major_style_lab~.,scales='free',space='free')+
            scale_x_reordered(name='Minor Style')+
            scale_y_continuous(labels=comma,name='# Beverages')
        })
        
        output$inventory = renderDataTable({
          sub_beer_db %>%
            select(geog_label,brewery_beer,major_style,minor_style,
                   single_price,multi_pack_price) %>%
            mutate(geog_label = factor(geog_label),
                   minor_style = factor(minor_style),
                   major_style = factor(major_style)) %>%
            rename(`Origin`=geog_label,`Brewery & Beer`=brewery_beer,
                   `Major Style`=major_style,
                   `Minor Style`=minor_style,
                   `Single Price`=single_price,
                   `Multi-Pack Price`=multi_pack_price) %>%
            datatable(filter='top',rownames = FALSE,
                      options = list(scrollY='500px',paging=FALSE),
                      height = 600) %>%
            formatCurrency(5)
        })
        
      })
      
      updateButton(session,"execute",label="Execute Filters",style="default")
    }
    
  })
}
