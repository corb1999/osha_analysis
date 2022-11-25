# :::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO =================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("1999-01-01"), 
                                    author = "", 
                                    proj_name = "", 
                                    script_type = "eda", 
                                    notepad = paste0("")), 
                  seed_set = 6)
metadatar

# start the clock timer, used for monitoring runtimes
clockin <- function() {
  aa <- Sys.time()
  clock_timer_start <<- aa
  return(aa)}

# end the clock timer, used in conjunction with the clockin fun
clockout <- function(x) {
  aa <- clock_timer_start
  bb <- Sys.time()
  cc <- bb - aa
  return(cc)}

# source the script that will load the data and prep it for analysis
sourcerpath <- paste0(getwd(), '/eda/remodel/remodel_script.R')
clockin()
source(file = sourcerpath)
clockout()

# source another script with analysis functions to run
sourcerpath <- paste0(getwd(), '/eda/engine/engine_script.R')
clockin()
source(file = sourcerpath)
clockout()

# cleanup
ls()
trash()
mem_used()

# ^ ====================================
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::

# viz prep ---------------------------------------------------

dt_filters <- fun_dater('2019-01-01', 
                        '2020-01-01')

major_filter <- c('foo', 'bar')

(pltname <- 'hello ' %ps% 
    'world; ' %ps% 
    reduce(major_filter, paste, sep = '; ') %ps% '; ' %ps% 
    dt_filters$date_text_str %ps% 
    '')

dfplt <- dfa %>% 
  filter(major %in% major_filter) %>% 
  filter(date_var >= dt_filters$start_date, 
         date_var <= dt_filters$end_date) %>% 
  filter(foobar == 1)

# ^ -----

# run plots and visuals ------------------------------------




# ^ -----
