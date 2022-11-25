# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ====================================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("1999-01-01"), 
                                    author = "", 
                                    proj_name = "", 
                                    script_type = "etl", 
                                    notepad = paste0("")), 
                  seed_set = 6)
metadatar

# LOAD LIBRARIES **********************************************************
R.version.string
Sys.info()
getwd()
library(lobstr)
library(rlang)
library(tidyverse)
library(tidylog)
library(lubridate)
library(scales)
# library(gt)
library(janitor)
set.seed(metadatar$seed_set[1])
options(digits = 4, max.print = 99, warnPartialMatchDollar = TRUE, 
        tibble.print_max = 30, scipen = 999, nwarnings = 5, 
        stringsAsFactors = FALSE)
mem_used()

# basic helper functions ***************************************************

# function to print object size
sizer <- function(x) {
  aaa <- format(object.size(x), "MB")
  return(aaa)}

# function to quickly run garbage collection
trash <- function(x) {
  gc(verbose = TRUE)}

# function to quickly view a sample of a dataframe
viewer <- function(x) {
  if (is.data.frame(x) == FALSE) {
    print("Error, insert a dataframe")
  } else {
    if(nrow(x) < 95) {
      View(x[sample(1:nrow(x), floor(nrow(x) * 0.5)), ])
    } else {
      View(x[sample(1:nrow(x), 100), ])
    }}}

# a function to make a quick data dictionary of a data frame
data_dictionary <- function(aa) {
  aa <- data.frame(aa)
  dd <- data.frame(column_order = seq(1, ncol(aa)), 
                   column_name_text = colnames(aa), 
                   column_class = sapply(aa, class, simplify = TRUE), 
                   column_nacount = sapply(lapply(aa, is.na), 
                                           sum, simplify = TRUE), 
                   column_uniques = sapply(lapply(aa, unique), 
                                           length, simplify = TRUE), 
                   row_01 = sapply(aa[1, ], as.character, simplify = TRUE), 
                   row_02 = sapply(aa[2, ], as.character, simplify = TRUE),
                   row_03 = sapply(aa[3, ], as.character, simplify = TRUE),
                   row_04 = sapply(aa[4, ], as.character, simplify = TRUE),
                   row_05 = sapply(aa[5, ], as.character, simplify = TRUE),
                   row.names = NULL)
  ee <- list(dims = data.frame(row_n = nrow(aa), col_n = ncol(aa)), 
             obj_size = object.size(aa), 
             c_names = c(colnames(aa)), 
             dict = dd)
  return(ee)}

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

# helps turn a character dollar variable into numeric
#   requires stringr, uncomment last line to turn NA to zero
cash_money <- function(x) {
  aa <- str_remove_all(x, pattern = "\\$")
  bb <- str_remove_all(aa, pattern = ",")
  cc <- as.numeric(bb)
  # cc <- ifelse(is.na(cc), 0, cc)
  return(cc)}

# POST SCRIPT; alt to using paste0() all the time (i saw this on twitter)
'%ps%' <- function(lhs, rhs) {
  return_me <- paste0(lhs, rhs)
  return(return_me)}

# ^ ====================================
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# downloader --------------------------------------------------

dl_fpath <- list('https://www.osha.gov/sites/default/largefiles/ITA%20Data%20CY%202016.zip', 
     'https://www.osha.gov/sites/default/largefiles/ITA%20Data%20CY%202017.zip', 
     'https://www.osha.gov/sites/default/largefiles/ITA%20Data%20CY%202018.zip', 
     'https://www.osha.gov/sites/default/largefiles/ITA%20Data%20CY%202019.zip', 
     'https://www.osha.gov/sites/default/largefiles/ITA-Data-CY-2020.zip', 
     'https://www.osha.gov/sites/default/largefiles/ITA-data-cy2021.zip')

fn_downloader <- function(arg1) {
  save_path <- getwd() %ps% '/etl/ore/' %ps% 
    'download_' %ps% stringr::str_sub(arg1, start = -8L)
  download.file(arg1, destfile = save_path)
}

lapply(dl_fpath, fn_downloader)

# cleanup !!!!!!!!!!!!!!!!!!
rm(fn_downloader, dl_fpath)
ls()
trash()

# ^ -----

# unionizer script --------------------------------------------
# a script to load and UNION ALL a number of datasets together
# require(purrr)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# identify dropzone where files are stored and vector the filenames
filepath_prefix_payload <- paste0(getwd(), "/etl/ore")

# put the files list into a dataframe
payload <- data.frame(file_nm = list.files(path = filepath_prefix_payload)) %>% 
  mutate(file_nm_full = paste0(filepath_prefix_payload, "/", 
                               file_nm), 
         file_nm_unzip = paste0(filepath_prefix_payload, "/upzipped_", 
                                stringr::str_sub(file_nm, end = -5L)),
         file_suffix = stringr::str_sub(file_nm, start = -3L)) %>% 
  filter(file_suffix == 'zip') %>% select(-file_suffix)

# test +++++++++++++++++++++++++++
payload
# payload[1, 1]
payload[1, 2]
payload[1, 3]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# write a function to read each file the same way into r
fun_readfiles <- function(arg1, arg2) {
  unzip(zipfile = arg1, exdir = arg2)
  aa <- list.files(arg2)
  readpath <- paste0(arg2, '/', aa)
  xx <- read.csv(readpath, stringsAsFactors = FALSE)
  return(xx)
}

# test +++++++++++++++++++++++++++
fun_readfiles(payload[1, 2], payload[1, 3])

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# execute the file reading purrr, and time the execution
clockin()
payload_list <- lapply(payload[, 2], FUN = fun_readfiles)
clockout()

# test +++++++++++++++++++++++++++
# payload_list[[1]]
# dim(payload_list[[1]])

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# run checks and validation on the loaded files
payload_stats <- data.frame(col_num_chk = map_dbl(payload_list, 
                                                  ncol), 
                            row_num_chk = map_dbl(payload_list, 
                                                  nrow))

fun_colnames_chk <- function(x) {
  aa <- colnames(x)
  bb <- reduce(aa, paste0)
  return(bb)}

# perform checks !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
payload_stats
length(unique(payload_stats[, 1])) == 1
length(unique(map_chr(payload_list, fun_colnames_chk))) == 1

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# combine into a single dataframe, as long as checks pass
clockin()
df <- map_dfr(payload_list, rbind)
clockout()

# test +++++++++++++++++++++++++++
dim(df)

# perform checks !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
sum(payload_stats$row_num_chk) == nrow(df)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# execute any additional filter and manipulation before writing

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# create and append a metadata tag
metadata_tag <- paste0("unionizer script metadata; ", 
                       "files consumed = ", 
                       nrow(payload_stats), 
                       "; runtime = ", Sys.time(), 
                       "; nrow = ", nrow(df), 
                       "; ncol = ", ncol(df))
metadata_tag
df <- df %>% mutate(metadata_tag = "NA")
df[1, "metadata_tag"] <- metadata_tag

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
















# write the cleaned dataset ---------------------------------------

# write to rds
filename <- paste0(getwd(), "/etl/ingot/dataframe.rds")
clockin()
saveRDS(dfa, file = filename)
clockout()

# ^ ----- 

# summarize and record etl -------------------------------------

(interim <- list(a = Sys.info(), 
                 b = nrow(dfa), 
                 c = ncol(dfa), 
                 d = sizer(dfa)))

# create an etl summary object
etl_metadata <- data.frame(etl_runtime = metadatar$script_starttime, 
                           etl_user = interim$a[[8]], 
                           data_rows = interim$b, 
                           data_cols = interim$c, 
                           data_size = interim$d, 
                           etl_note = 'no notes')
etl_metadata
rm(interim)

# write to csv
filename <- paste0(getwd(), "/etl/etl_metadata.csv")
clockin()
write.csv(etl_metadata, file = filename, row.names = FALSE)
clockout()

# ^ ----- 