to_file_path <- function(name) {
  name <- str_remove_all(name, "[.]")
  tolower(
    str_replace_all(
      name,
      ' ',
      '_'
    )
  )
}


to_name <- function(path_name) {
  str_to_title(
    str_replace_all(
      path_name,
      '_',
      ' '
    )
  )
}

get_corpora_names <- function() {
  lapply(
    str_split(
      list.dirs(path = "./data/corpora", recursive = F), 
      '/'
    ), 
    tail, n = 1L
  )
}

get_corpus_files <- function(corpus) {
  file_path <- paste0(
    "./data/corpora/",
    to_file_path(corpus)
  )
  lapply(
    str_split(
      list.files(path = file_path, recursive = F), 
      '/'
    ), 
    tail, n = 1L
  )
}

get_computed_files <- function(corpus, dictionary) {
  file_path <- paste0(
    "./data/computed/",
    to_file_path(corpus),
    "/",
    to_file_path(dictionary)
  )
  lapply(
    str_split(
      list.files(path = file_path, recursive = F), 
      '/'
    ), 
    tail, n = 1L
  )
}

get_dict_categories <- function(dictionary) {
  file_path <- paste0(
    './data/dictionaries/',
    to_file_path(dictionary),
    '.dic'
  )
  
  category_str <- unlist(
    strsplit(
      readChar(file_path, file.info(file_path)$size),
      "%"
    )
  )[2]
  
  categories <- unlist(
    lapply(
      unlist(
        strsplit(
          substring(category_str, 2),
          "\n"
        )
      ),
      function(x) {
        unlist(
          strsplit(
            x, "\t"
          )
        )[2]
      }
    )
  )

  return(categories)
}

load_monthly <- function(corpus, dictionary) {
  message('loading monthly trends')
  vroom(
    paste0(
      './data/computed/',
      to_file_path(corpus),
      '/',
      to_file_path(dictionary),
      '/monthly.csv'
    )
  )
}

load_unit_speeches <- function(corpus, unit) {
  return(
    vroom(
      paste0(
        './data/corpora/',
        to_file_path(corpus),
        '/',
        to_file_path(unit),
        '.csv'
      ),
      delim = '|'
    )
  )
}

load_demographics <- function(corpus) {
  vroom(
    paste0(
      './data/demographics/',
      to_file_path(corpus),
      '.csv'
    )
  )
}

rebuild_monthly <- function(corpus, dictionary) {
  files <- get_computed_files(corpus, dictionary)
  wide_scores <- data.frame(
    date=Date()
  )
  
  dict_categories <- get_dict_categories(dictionary)
  
  # Generate Monthly Summary
  for (file in files) {
    print(paste("Processing ", file))
    wide_scores <- bind_rows(
      wide_scores,
      vroom(paste0('./data/computed/', corpus, '/', dictionary, '/', file)) %>% 
        select(c('date','unit', dict_categories)) %>%
        mutate(date = floor_date(as.Date(as.character(date), format="%Y%m%d"), 'month')) %>%
        mutate(unit = as.character(unit))
    )
  
  }
  
  long_scores <- wide_scores  %>%
    group_by(unit, date) %>% 
    summarize_at(
      get_dict_categories(dictionary),
      mean,
      n.rm=T
    ) %>%
    gather(key = "category", value = "value", -date, -unit)
  
  vroom_write(
    long_scores,
    paste0(
      './data/computed/',
      to_file_path(corpus),
      '/',
      to_file_path(dictionary),
      '/monthly.csv'
    ),
    delim = ',',
    col_names = T
  )
}

