# set working directory (works only in RStudio)
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )

# list packages to use
pkgs <- c( "tidyverse", "dplyr", # data wrangling
           "ggplot2", "GGally", # plotting
           "openxlsx" # reading and writing .xlsx
           )

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# prepare a folder for tables, figures, models, and sessions info
sapply( c("tabs","figs","mods","sess"), function(i) if( !dir.exists(i) ) dir.create(i) )  

# ---- data set preparation ----

# read the data required
d <- list(
  dem = read.xlsx( "data/WASI_validace.xlsx", sheet = "0_anamneza" ),
  kos = read.xlsx( "data/WASI_validace.xlsx", sheet = "wasi_kostky", startRow = 2 ) %>% select( kod_ditete, cas_adm, suma_body, contains("scr") ),
  slo = read.xlsx( "data/WASI_validace.xlsx", sheet = "wasi_slovnik" ),
  mat = read.xlsx( "data/WASI_validace.xlsx", sheet = "wasi_matrice" ),
  pod = read.xlsx( "data/WASI_validace.xlsx", sheet = "wasi_podobnosti" )
)

# re-code all "N/A"s to NAs
for ( i in names(d) ) d[[i]][ d[[i]] == "N/A" | d[[i]] == "N/a" ] <- NA

# change all scores variables to numeric and calculate some scores from item scores
for ( i in names(d)[-1] ) d[[i]] <- d[[i]] %>%
  mutate_if( is.character, as.numeric ) %>%
  # sum item scores to get sum scores
  mutate( sum_score = sapply( 1:nrow(.), function(j) sum( .[ j , grepl( "pol|scr", names(.) ) ], na.rm = T ) ) )

# check discrepancies between R-calculated sum scores and sum scores in the table
disc <- lapply( names(d)[-1],
                function(i) d[[i]][ with( d[[i]], !(suma_body == sum_score) ), ] %>% relocate( sum_score, .after = suma_body )
                ) %>% `names<-`( names(d)[-1] )

# extract only the data from the sum scores
d0 <- d$dem %>%
  left_join( d$kos[ , c("kod_ditete","sum_score")] %>% rename( "kostky" = "sum_score"), by = "kod_ditete" ) %>%
  left_join( d$slo[ , c("kod_ditete","sum_score")] %>% rename( "slovnik" = "sum_score"), by = "kod_ditete" ) %>%
  left_join( d$mat[ , c("kod_ditete","sum_score")] %>% rename( "matrice" = "sum_score"), by = "kod_ditete" ) %>%
  left_join( d$pod[ , c("kod_ditete","sum_score")] %>% rename( "podobnosti" = "sum_score"), by = "kod_ditete" ) %>%
  select( kod_ditete, gender, skola, trida, vek_roky, kostky, slovnik, matrice, podobnosti ) %>%
  slice( -which( kod_ditete %in% c(149,158,162,226,317) ) ) # drop red subjects


# ---- descriptive stats ----

# prepate frequency tables
desc <- lapply( c("skola","trida"),
                function(i)
                  table( d0[[i]], d0$gender ) %>%
                  as.data.frame() %>% `colnames<-`( c("var","gender","Freq") ) %>%
                  pivot_wider( names_from = gender, values_from = Freq ) %>%
                  as.data.frame() # changing format to a df for the next loop to work properly
                  ) %>% `names<-`( c("skola","trida") )

# add means and standard deviations of age and WASI scores for each row (school/class)
for ( i in names(desc) ) {
  for ( j in names(d0)[5:9] ) desc[[i]][[j]] <- sapply(
    1:nrow(desc[[i]]),
    function(k)
      paste0(
        ( mean( d0[ d0[[i]] == desc[[i]][k,"var"], j ], na.rm = T ) %>% round(2) %>% sprintf( "%.2f", . ) ), " (",
        ( sd( d0[ d0[[i]] == desc[[i]][k,"var"], j ], na.rm = T ) %>% round(2) %>% sprintf( "%.2f", . ) ), ")"
      )
    )
}

# collapse the table of descriptive stats
desc <- do.call( rbind.data.frame, desc ) %>%
  add_row( var = "school", .before = 1 ) %>%
  add_row( var = "class", .before = 9 ) %>%
  `rownames<-`( 1:nrow(.) )

# save as .csv
write.table( desc, "tabs/wasiCZ_description.csv", sep = ",", row.names = F )


# ---- correlation matrix ----

# set theme of ggplot2
theme_set( theme_bw( base_size = 14 ) )

# plot scatter-dot plots with Pearson's correlation coefficients
ggpairs( d0[ , 5:9 ] )

# save the correlation plot
ggsave( "figs/wasiCZ_correlation_matrix.jpg", dpi = 300, width = 10.5, height = 11.7 )
