#PART A---------
library(tidyverse) #Loading the tidyverse
library(readxl) #For reading Excel files
library(janitor) #For cleaning up column names
library(forcats) #For implementing ordered categorical 
library(hms) #For working with times of day

raw_doodle = read_excel('data/r_doodle.xls')

raw_doodle

#Take a look at data in R studio viewer and in Excel. 
#Column names are weird, the information we want is several rows down from the top. 
#Luckily we can use skip some rows and rename columns to make them easier to manipulate

less_raw_doodle = read_excel('data/r_doodle.xls',skip = 3) %>% #Skipping three rows, we will need both weekday and time data from top two new rows
  clean_names() %>%#Cleaning names to make them easier to use, R likes names that don't have spaces, special characters, or leading numbers. 
  rename(x2 = august_2020, # We don't need to know that it is August 2020
         name = x1) #Name is going to be our primary identifier 

#Take a look at the newly read in dataset
less_raw_doodle

#We're going to break this data down into a couple of parts and join it back together 

#Time preferences
time_prefs = less_raw_doodle %>% 
  filter(!is.na(name)) %>% #Get only the rows with people's time preferences indicated
  pivot_longer(cols = starts_with('x'), #All columns except name
               names_to = 'key', #Old convention from gather
               values_to='value') #Old convention from gather

#Time block definitions
time_blocks = less_raw_doodle %>%
  slice(1:2) %>% #Extract first two rows
  select(-name) %>% #Do not need name column here
  pivot_longer(cols = everything(), #All columns
               names_to = 'key',
               values_to = 'value') %>%
  fill(value,.direction = 'down') %>%
  mutate(col_type = ifelse(str_detect(value,':'),'time','date')) %>%
  pivot_wider(id_cols = key,
              names_from=col_type,
              values_from=value)

#Join the split dataset back together
time_prefs_joined = time_prefs %>%
  left_join(time_blocks,by='key') %>%
  select(-key) %>% #Do not need this anymore
  filter(name!='Count') %>%
  mutate(value = case_when(
    value=='OK'~'Yes',
    value=='(OK)'~'Maybe',
    TRUE~'No'
  ),
  value = factor(value,ordered=TRUE,levels = c('Yes','Maybe','No')),
  weekday = str_sub(date,1,3),
  weekday = factor(weekday,ordered=TRUE,levels = c('Mon','Tue','Wed','Thu','Fri'))) %>%
  separate(time,into=c('from_time','to_time'),sep=' – ') %>%
  mutate(from_time = strptime(from_time,format='%I:%M %p') %>%
           as.hms(),
         to_time = strptime(to_time,format='%I:%M %p') %>%
           as.hms()) %>%
  select(-date) %>%
  #this block uses the map() function from purrr, which we will discuss later in the module
  mutate(
    first_name = map_chr(name,function(name){
      (name %>%
         str_split(' ') %>%
         unlist())[1]}),
    last_name = map_chr(name,function(name){
      (name %>%
         str_split(' ') %>%
         unlist())[2]})
  ) %>%
  arrange(desc(last_name),desc(first_name),
          weekday,from_time,to_time) %>%
  mutate(name_factor = factor(name,ordered=TRUE,levels = unique(name)))

#Look how tidy this data is!!! This will make plotting much easier in next steps
time_prefs_joined

#PART B ----------
ggplot(time_prefs_joined)+
  geom_rect(aes(
    #Translating times from hms to POSIXct to take advantage of scale_x_datetime()
    #Unfortunately there is not an hms specific scale yet
    xmin=as.POSIXct(from_time),
    xmax=as.POSIXct(to_time),
    #This is a bit of a hack, and there are multiple ways to handle this. 
    #Here I am translating the name into a number so I can create a 'width' for the rectangle. 
    #Another way to do this would be to use geom_tile()
    ymin = as.numeric(name_factor)-0.5,
    ymax = as.numeric(name_factor)+0.5,
    #The fill color of each rectangle will be determined by what a person filled in the time block in Doodle with. 
    fill = value),
    #Making the rectangles semi-transparent since they overlap
    alpha = 0.5)+
  #I want a separate subplot for each weekday
  facet_wrap(~weekday,nrow=1)+
  #Here is the other part of me using the names as ordinal factors to create a rectangle width
  scale_y_continuous(breaks = 1:length(unique(time_prefs_joined$name_factor)),
                     labels = levels(time_prefs_joined$name_factor),
                     limits = c(0.5,(length(unique(time_prefs_joined$name_factor))+0.5)))+
  ggtitle('Doodle Results for Scheduling R Trainings')+
  xlab('Time of Day (PDT)')+ylab('Participant Name')+
  # Here I am using strftime() formatting codes to simplify the time format displayed
  scale_x_datetime(date_labels = '%I %p',date_breaks = '1 hour')+
  #Rotating x axis labels to make them easier to read
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        text = element_text(size=30))

#PART C--------------
time_prefs_clean = time_prefs_joined %>%
  select(name,name_factor,first_name,last_name,
         weekday,from_time,to_time,value) %>%
  #I want to nest the data specific to each person a data frame to be operated on separately for each iteration
  nest(data = c(weekday,from_time,to_time,value)) %>%
  #I then use map() to apply an anonymous function (a function defined on the fly) below
  mutate(cleaned_time_blocks = map(data,function(data){
    
    #I need to create a group number each time a person's status changes to I can group those time blocks together
    grouped = data %>%
      #Initialize group as 1
      mutate(group_num = 1)
    
    gn = 1
    
    #start on the second entry -- in this case for loops are still necessary 
    #because of the tracking of the current group number across rows
    for(i in 2:nrow(grouped)){
      #If the status has changed or the day has changed, create and assign a new group
      if(grouped$value[i] != grouped$value[i-1] |
         grouped$weekday[i] != grouped$weekday[i-1]){
        gn = gn+1
        grouped$group_num[i] = gn
      }else{
        grouped$group_num[i] = gn
      }
    }
    
    #Calculate the bounding times of each group we created
    clean = grouped %>%
      group_by(weekday,group_num,value) %>%
      summarise(min_time = as.hms(min(from_time)),
                max_time = as.hms(max(to_time)))
    
    #Return the grouped data frame
    return(clean)
  })) %>%
  #Get rid of the old data frame
  select(-data) %>%
  #Unnest the clean data frames, so we have one big data frame for plotting together
  unnest(cleaned_time_blocks)


#Refer to plotting in part B -- very similar implementation
ggplot(time_prefs_clean)+
  geom_rect(aes(xmin=as.POSIXct(min_time),
                xmax=as.POSIXct(max_time),
                ymin = as.numeric(name_factor)-0.5,
                ymax = as.numeric(name_factor)+0.5,
                fill = value),
            color='white',size=1)+
  facet_wrap(~weekday,nrow=1)+
  scale_y_continuous(breaks = 1:length(unique(time_prefs_joined$name_factor)),
                     labels = levels(time_prefs_joined$name_factor),
                     limits = c(0.5,(length(unique(time_prefs_joined$name_factor))+0.5)))+
  ggtitle('Cleaner Doodle Results for Scheduling R Trainings')+
  xlab('Time of Day (PDT)')+ylab('Participant Name')+
  scale_x_datetime(date_labels = '%I %p',date_breaks = '1 hour')+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        text = element_text(size=30))