library(shiny)
library(tidyverse)
library(lubridate)
library(vroom)
library(callr)
source('./scripts/utils.R')

# List of Corpora
corpora <- get_corpora_names()

#####################################
###            SERVER             ###
#####################################
shinyServer(function(input, output, session) {
    ranges <- reactiveValues(x = NULL)
    processes <- reactiveValues(processes = NULL)
    
    get_dict_names <- reactive({
        dict_files <- list.files(path = "./data/dictionaries", recursive = F)
        substr(
            dict_files,
            1,
            nchar(dict_files)-4
        )
    })
    
    ##############
    ### TRENDS ###
    ##############
    
    ### Inputs
    # Corpus
    output$corpusSelectInput_Trends <- renderUI({
        selectInput("selectedCorpus_Trends", "Corpus", lapply(corpora, to_name))
    })
    
    # Dictionary
    output$dictionarySelectInput_Trends <- renderUI({
        selectInput("selectedDictionary_Trends", "Dictionary", lapply(get_dict_names(), to_name))
    })
    
    # Dictionary Vars
    output$dictionaryVarSelectInput_Trends <- renderUI({
        if (is.null(input$selectedDictionary_Trends)) 
            return()
        
        dict_categories <- get_dict_categories(input$selectedDictionary_Trends)
        
        selectInput(
            'selectedDictionaryVars_Trends',
            'Dictionary Categories',
            dict_categories,
            multiple = T,
            selected = dict_categories[1]
        )
    })
    
    # Date Ranges
    output$dateRangeSelectInput_Trends <- renderUI({
        if (is.null(input$selectedDictionary_Trends) | is.null(ranges$x)) 
            return()
        
        monthly_df_Trends <- monthly_df_Trends()
        
        dateRangeInput(
            "dateRange",
            "Date Range",
            start = as.Date(ranges$x[1]),
            end = as.Date(ranges$x[2]),
            min = min(as.Date(as.character(monthly_df_Trends$date), format="%Y%m%d")),
            max = max(as.Date(as.character(monthly_df_Trends$date), format="%Y%m%d"))
        )
    })
    
    ### REACTIVE
    # Demographics
    demographics_Trends <- reactive({
        load_demographics(input$selectedCorpus_Trends)
    })
    
    # Monthly Scores
    monthly_df_Trends <- reactive({
        filter(
            load_monthly(input$selectedCorpus_Trends, input$selectedDictionary_Trends),
            category %in% c(input$selectedDictionaryVars_Trends)
        )
    })
    
    # Individual Speech Scores between date ranges
    load_trend_scores <- reactive({
        units <- monthly_df_Trends() %>% 
            filter(date > as.Date(ranges$x[1]) & date < as.Date(ranges$x[2])) %>%
            distinct(unit)
        
        print(units)
        
        # Instantiate empty dataframe
        df <- data.frame(
            date=Date(),
            category=character(),
            value=double(),
            stringsAsFactors=FALSE
        )
        
        # Loop through units in time frame, fetching scores
        for (unit in units) {
            df <- full_join(
                df,
                vroom(
                    paste0(
                        './data/computed/',
                        to_file_path(input$selectedCorpus_Trends),
                        '/',
                        to_file_path(input$selectedDictionary_Trends),
                        '/',
                        to_file_path(unit),
                        '.csv'
                    )
                ) %>% 
                    filter(word_count > 500) %>% 
                    select(date, speaker_id, speech_id, unit, word_count, c(input$selectedDictionaryVars_Trends)) %>% 
                    mutate(date = as.Date(as.character(date), format="%Y%m%d")) %>%
                    gather(key = "category", value = "value", -date, -speaker_id, -speech_id, -unit, -word_count)
            )
        }
        
        date_units <- date_units()
        
        df %>% mutate(
                time_unit = as.character(
                    format(
                        floor_date(date, date_units$unit),
                        date_units$labels
                    )
                )
            )
    })
    
    # Date Units 
    date_units <- reactive({
        days <- ranges$x[2] - ranges$x[1]
        dates <- list(
            unit = "year",
            breaks = "1 year",
            labels = "%Y"
        )
        
        if (days < 14) {
            dates$unit <- "day"
            dates$breaks <- "1 day"
            dates$labels <- "%d %b %Y"
            
        } else if (days >= 14 & days < 45) {
            dates$unit <- "week"
            dates$breaks <- "1 week"
            dates$labels <- "%d %b %Y"
            
        } else if (days >= 45 & days < 365) {
            dates$unit <- "month"
            dates$breaks <- "1 month"
            dates$labels <- "%b %Y"
            
        } else if (days >= 365 & days < 730) {
            dates$unit <- "quarter"
            dates$breaks <- "4 month"
            dates$labels <- "%b %Y"
            
        } else if (days >= 740 & days < 1825) {
            dates$unit <- "halfyear"
            dates$breaks <- "6 month"
            dates$labels <- "%b %Y"
        }
        
        return(dates)
    })
    
    ### OUTPUTS
    # Yearly Trend Plot
    output$trendPlot <- renderPlot({
        monthly_df_Trends <- monthly_df_Trends()

        ggplot2::ggplot(
            monthly_df_Trends, 
            mapping=aes(
                x=date, 
                y=value
            )
        ) +
        geom_line(aes(color=category), alpha=0.15) +
        geom_smooth(aes(color=category), span = 0.3, se = FALSE) + 
        
        # Scaling & Aesthetics 
        scale_x_date(
            date_breaks = "5 year",
            date_labels = "%Y"
        ) + 
        scale_y_continuous(
            labels = scales::percent,
            sec.axis = sec_axis(trans=~.*1, labels = scales::percent)
        ) +
        theme_classic() +
        theme(
            text=element_text(family= "Helvetica"),
            axis.line = element_line(color="lightgrey"),
            axis.title.y = element_text(color="darkgrey"),
            axis.text.y = element_text(color="darkgrey"),
            axis.text.x = element_text(color="darkgrey"),
            title = element_text(color="darkgrey"),
            legend.position = "bottom"
        ) +
        expand_limits(y=0) +
        labs(
            title="Average Percent of Total Words by Dictionary category by Month"
        ) +
        xlab(NULL) + 
        ylab(NULL) + 
        scale_color_brewer(palette="Set1")
    })
    
    # Focus Plot
    output$focusPlot <- renderPlot({
        if (is.null(ranges$x)) 
            return()

        scores <- load_trend_scores() 
        scores$time_unit <- with(scores, reorder(time_unit, date))
        
        ggplot(
            scores,
            aes(
                x=time_unit,
                y=value
            )
        ) + 
        geom_boxplot(aes(fill=category)) +
        #geom_jitter(width = 0.2) +
        scale_y_continuous(
            labels = scales::percent,
            sec.axis = sec_axis(trans=~.*1, labels = scales::percent)
        ) +
        theme_classic() +
        theme(
            text=element_text(family= "Helvetica"),
            axis.ticks = element_blank(),
            axis.line = element_line(color="lightgrey"),
            axis.title.y = element_text(color="darkgrey"),
            axis.text.y = element_text(color="darkgrey"),
            axis.text.x = element_text(color="darkgrey"),
            title = element_text(color="darkgrey"),
            legend.position = "none"
        ) +
        xlab(NULL) + 
        ylab(NULL) +
        expand_limits(y=0) + 
        scale_fill_brewer(palette="Pastel1")
    })
    
    ### OBSERVERS
    # Select date range on trend plot
    observe({
        brush <- input$trendPlot_brush
        if (!is.null(brush)) {
            ranges$x <- c(as.Date(brush$xmin, origin="1970-01-01"), as.Date(brush$xmax, origin="1970-01-01"))
            
        } else {
            ranges$x <- NULL
        }
    })

    # Select speech on outlier
    observeEvent(input$focusPlot_click, {
        if(is.null(ranges$x))
            return()
        
        selected_point <- nearPoints(
            load_trend_scores(), 
            input$focusPlot_click,
            threshold = 5,
            maxpoints = 1
        )
        
        if (length(selected_point$unit) == 0) {
            return()
        }
        
        print(selected_point)
        
        if (is.na(selected_point$speaker_id)) {
            showModal(
                modalDialog(
                    title="Unknown Speaker",
                    load_unit_speeches(input$selectedCorpus_Trends, selected_point$unit) %>% filter(
                        speech_id == selected_point$speech_id
                    )
                )
            )
        } else {
            x <- left_join(
                load_unit_speeches(input$selectedCorpus_Trends, selected_point$unit) %>% 
                    filter(speech_id == selected_point$speech_id) %>%
                    mutate(speaker_id = selected_point$speaker_id),
                demographics_Trends(),
            )
            
            showModal(
                modalDialog(
                    title=x$full_name,
                    x$speech
                )
            )
        }
    })
    
    #####################
    ### Speech Search ###
    #####################
    
    ### INPUTS
    # Corpus
    output$corpusSelectInput_Search <- renderUI({
        selectInput("selectedCorpus_Search", "Corpus", lapply(corpora, to_name))
    })
    
    # Dictionary
    output$dictionarySelectInput_Search <- renderUI({
        selectInput("selectedDictionary_Search", "Dictionary", lapply(get_dict_names(), to_name))
    })
    
    # Dictionary Vars
    output$dictionaryVarSelectInput_Search <- renderUI({
        if (is.null(input$selectedDictionary_Search)) 
            return()
        
        dict_categories <- get_dict_categories(input$selectedDictionary_Search)
        
        selectInput(
            'selectedDictionaryVars_Search',
            'Dictionary Categories',
            dict_categories,
            multiple = T,
            selected = dict_categories[1]
        )
    })
    
    # Demographics
    demographics_Search <- reactive({
        load_demographics(input$selectedCorpus_Search)
    })
    
    # Demographic Vars
    output$demographicVarSelectInput_Search <- renderUI({
        varSelectInput(
            "selectedDemographicVars_Search", 
            "Demographic Categories", 
            demographics_Search() %>% select(-c("speaker_id")),
            multiple = T
        )
    })
    
    # Date Ranges
    output$dateRangeSelectInput_Search <- renderUI({
        if (is.null(input$selectedDictionary_Search))
            return()
        
        monthly_df_Search <- monthly_df_Search()
        
        dateRangeInput(
            "dateRange_Search",
            "Date Range",
            start = as.Date('1939-01-01'),
            end = as.Date('1940-01-01'),
            min = min(as.Date(as.character(monthly_df_Search$date), format="%Y%m%d")),
            max = max(as.Date(as.character(monthly_df_Search$date), format="%Y%m%d"))
        )
    })
    
    ### REACTIVE
    monthly_df_Search <- reactive({
        filter(
            load_monthly(input$selectedCorpus_Search, input$selectedDictionary_Search),
            category %in% c(input$selectedDictionaryVars_Search)
        )
    })
    
    # Get dataframe for search table
    search_df <- reactive({
        units <- monthly_df_Search() %>% 
            filter(date > as.Date(input$dateRange_Search[1]) & date < as.Date(input$dateRange_Search[2])) %>%
            distinct(unit)
        
        # Instantiate empty dataframe
        df <- data.frame(
            date=Date(),
            category=character(),
            value=double(),
            stringsAsFactors=FALSE
        )
        
        # Loop through units in time frame, fetching scores
        for (unit in units) {
            df <- full_join(
                df,
                vroom(
                    paste0(
                        './data/computed/',
                        to_file_path(input$selectedCorpus_Search),
                        '/',
                        to_file_path(input$selectedDictionary_Search),
                        '/',
                        to_file_path(unit),
                        '.csv'
                    )
                ) %>% 
                    filter(word_count > input$wordCountThreshold_Search) %>% 
                    mutate(speaker_id = ifelse(is.na(speaker_id), 99999999, speaker_id)) %>%
                    select(date, speaker_id, speech_id, unit, word_count, c(input$selectedDictionaryVars_Search)) %>% 
                    mutate(date = as.Date(as.character(date), format="%Y%m%d"))
            )
        }
        
        left_join(
            df,
            demographics_Search(),
            by="speaker_id"
        ) %>% select(c(
            as.character(input$selectedDemographicVars_Search),
            as.character(input$selectedDictionaryVars_Search),
            'full_name',
            'speaker_id',
            'speech_id',
            'date',
            'unit',
            'word_count'
        ))
    })

    ### OUTPUTS
    output$speechSearchTable <- DT::renderDataTable(
        search_df(),
        selection = 'single',
        server = T,
    )
    
    ### OBSERVER
    # Table row click
    observeEvent(input$speechSearchTable_rows_selected, {
        search_df <- search_df()
        row <- search_df[input$speechSearchTable_rows_selected,]
        
        if (row$speaker_id == 99999999) {
            showModal(
                modalDialog(
                    title="Unknown Speaker",
                    load_unit_speeches(input$selectedCorpus_Search, row$unit) %>% filter(
                        speech_id == row$speech_id
                    )
                )
            )
        } else {
            showModal(
                modalDialog(
                    title=row$full_name,
                    load_unit_speeches(input$selectedCorpus_Search, row$unit) %>% 
                        filter(speech_id == row$speech_id) %>%
                        mutate(speaker_id = row$speaker_id)
                )
            )
            
        }
    })
    
    
    
    
    ##############
    ### COUNTS ###
    ##############
    
    ### INPUTS
    # Dictionary Variable Select
    output$dictionaryDetailInput <- renderUI({
        if (is.null(input$dictionaryDetailSelect)) 
            return()
        
        switch(
            input$dictionaryDetailSelect,
            "LIWC" = varSelectInput("dictionaryDetailVar", "Variable", liwc_df %>% select(-c("date"))),
            "Racism" = selectInput("dictionaryDetailVar", "Variable", c("race","allowed","explicit","coded")),
        )
    })
    
    ### REACTIVE DATA
    congress_counts_df <- reactive({
        dict <- tolower(input$dictionaryDetailSelect)
        congress <- tolower(input$congressSelect)
        scores_df <- vroom(
            paste0('data/counts/', congress, '_', dict, '_wcs.csv'), delim=","
        )
    })
    
    ### OUTPUTS
    output$freqTable <- renderDataTable({
        congress_counts_df <- congress_counts_df()
        congress_counts_df %>% 
            filter(category == input$dictionaryDetailVar) %>% 
            group_by(word) %>% 
            summarise(sum = sum(count)) %>% 
            filter(sum > 5) %>% 
            arrange(desc(sum))
    })
    
    
    #####################
    ### Compare Units ###
    #####################
    ### INPUTS
    # Corpus
    output$corpusSelectInput_Compare_Unit <- renderUI({
        selectInput("selectedCorpus_Compare_Unit", "Corpus", lapply(corpora, to_name))
    })
    
    # Dictionary
    output$dictionarySelectInput_Compare_Unit <- renderUI({
        selectInput("selectedDictionary_Compare_Unit", "Dictionary", lapply(get_dict_names(), to_name))
    })
    
    # Dictionary Vars
    output$dictionaryVarSelectInput_Compare_Unit <- renderUI({
        if (is.null(input$selectedDictionary_Compare_Unit)) 
            return()
        
        dict_categories <- get_dict_categories(input$selectedDictionary_Compare_Unit)
        
        selectInput(
            'selectedDictionaryVars_Compare_Unit',
            'Dictionary category',
            dict_categories,
            multiple = F,
            selected = dict_categories[1]
        )
    })
    
    # Units
    output$unitSelectInput_Compare_Unit <- renderUI({
        selectInput(
            "selectedUnits_Compare_Unit", 
            "Units", 
            as.character((monthly_df_Compare_Unit() %>% distinct(unit))$unit),
            multiple = T
        )
    })
    
    ### REACTIVE
    monthly_df_Compare_Unit <- reactive({
        load_monthly(input$selectedCorpus_Compare_Unit, input$selectedDictionary_Compare_Unit) %>% 
            filter(category %in% c(input$selectedDictionaryVars_Compare_Unit)) %>% mutate(unit = as.character(unit)) 
    })
    
    ### Unit Compare
    output$unitCompare <- renderPlot({
        monthly_df_Compare_Unit <- monthly_df_Compare_Unit() %>% 
            filter(unit %in% c(input$selectedUnits_Compare_Unit))

        monthly_df_Compare_Unit$unit <- with(monthly_df_Compare_Unit, reorder(unit, date))
        
        ggplot(
            monthly_df_Compare_Unit,
            aes(
                x=unit,
                y=value
            )
        ) + 
            geom_boxplot(aes(fill=unit)) +
            scale_y_continuous(
                labels = scales::percent,
                sec.axis = sec_axis(trans=~.*1)
            ) +
            theme_classic() +
            theme(
                text=element_text(family= "Helvetica"),
                axis.ticks = element_blank(),
                axis.line = element_line(color="lightgrey"),
                axis.title.y = element_text(color="darkgrey"),
                axis.text.y = element_text(color="darkgrey"),
                axis.text.x = element_text(color="darkgrey"),
                title = element_text(color="darkgrey"),
                legend.position = "none"
            ) +
            xlab(NULL) + 
            ylab(NULL) +
            expand_limits(y=0) + 
            scale_fill_brewer(palette="Pastel1")
    })
    
    
    output$unitCompareTrend <- renderPlot({
        monthly_df_Compare_Unit <- monthly_df_Compare_Unit() %>% 
            filter(unit %in% c(input$selectedUnits_Compare_Unit)) %>% 
            group_by(unit) %>%
            mutate(min_date = min(date)) %>%
            ungroup(unit) %>%
            mutate(days = date - min_date)

        monthly_df_Compare_Unit$unit <- with(monthly_df_Compare_Unit, reorder(unit, date))
        
        ggplot(
            monthly_df_Compare_Unit, 
            mapping=aes(
                x=days, 
                y=value
            )
        ) +
        geom_line(aes(color=unit)) +
        
        # Scaling & Aesthetics  
        scale_y_continuous(
            labels = scales::percent,
            sec.axis = sec_axis(trans=~.*1, labels = scales::percent)
        ) +
        theme_classic() +
        theme(
            text=element_text(family= "Helvetica"),
            axis.line = element_line(color="lightgrey"),
            axis.title.y = element_text(color="darkgrey"),
            axis.text.y = element_text(color="darkgrey"),
            axis.text.x = element_text(color="darkgrey"),
            title = element_text(color="darkgrey"),
            legend.position = "bottom"
        ) +
        expand_limits(y=0) +
        labs(
            title=paste(
                "Average Percent of Total", 
                str_to_title(input$selectedDictionaryVars_Compare_Unit),
                "Words by Unit by Days in Office"
            )
        ) +
        xlab(NULL) + 
        ylab(NULL) +
        scale_color_brewer(palette="Set1")
    })
    
    ############################
    ### Compare Demographics ###
    ############################
    ### INPUTS
    # Corpus
    output$corpusSelectInput_Compare_Demographic <- renderUI({
        selectInput("selectedCorpus_Compare_Demographic", "Corpus", lapply(corpora, to_name))
    })
    
    # Dictionary
    output$dictionarySelectInput_Compare_Demographic <- renderUI({
        selectInput("selectedDictionary_Compare_Demographic", "Dictionary", lapply(get_dict_names(), to_name))
    })
    
    # Dictionary Vars
    output$dictionaryVarSelectInput_Compare_Demographic <- renderUI({
        if (is.null(input$selectedDictionary_Compare_Demographic)) 
            return()
        
        dict_categories <- get_dict_categories(input$selectedDictionary_Compare_Demographic)
        
        selectInput(
            'selectedDictionaryVars_Compare_Demographic',
            'Dictionary category',
            dict_categories,
            multiple = F,
            #selected = dict_categories[1]
        )
    })
    
    # Demographics
    output$demographicSelectInput_Compare_Demographic <- renderUI({
        varSelectInput(
            "selectedDemographics_Compare_Demographic", 
            "Demographics",
            demographics_Compare_Demographic(),
            multiple = T
       )
    })
    
    ### REACTIVE
    # Demographics
    demographics_Compare_Demographic <- reactive({
        load_demographics(input$selectedCorpus_Compare_Demographic)
    })
    
    # monthly
    monthly_df_Compare_Demographic <- reactive({
        filter(
            load_monthly(input$selectedCorpus_Compare_Demographic, input$selectedDictionary_Compare_Demographic),
            category %in% c(input$selectedDictionaryVars_Compare_Demographic)
        )
    })
    
    ########################
    ### Compare Speakers ###
    ########################
    
    #####################
    ### Compare Dates ###
    #####################
    
    # Date Ranges
    output$dateRangeSelectInput_Compare <- renderUI({
        if (is.null(input$selectedDictionary_Compare))
            return()
        
        monthly_df_Compare <- monthly_df_Compare()
        print("Building Date Range")
        
        dateRangeInput(
            "dateRange_Compare",
            "Date Range",
            start = as.Date('1939-01-01'),
            end = as.Date('1940-01-01'),
            min = min(as.Date(as.character(monthly_df_Compare$date), format="%Y%m%d")),
            max = as.Date("19971231", format="%Y%m%d")
        )
    })
    
    ### REACTIVE
    monthly_df_Compare <- reactive({
        filter(
            load_monthly(input$selectedCorpus_Compare, input$selectedDictionary_Compare),
            category %in% c(input$selectedDictionaryVars_Compare)
        )
    })
    
    # Get dataframe for compare
    compare_df <- reactive({
        if(is.null(input$dateRange_Compare))
            return()
        
        units <- monthly_df_Compare() %>% 
            filter(date > as.Date(input$dateRange_Compare[1]) & date < as.Date(input$dateRange_Compare[2])) %>%
            distinct(unit)
        
        # Instantiate empty dataframe
        df <- data.frame(
            date=Date(),
            category=character(),
            value=double(),
            stringsAsFactors=FALSE
        )
        
        # Loop through units in time frame, fetching scores
        for (unit in units) {
            print(unit)
            df <- full_join(
                df,
                vroom(
                    paste0(
                        './data/computed/',
                        to_file_path(input$selectedCorpus_Compare),
                        '/',
                        to_file_path(input$selectedDictionary_Compare),
                        '/',
                        to_file_path(unit),
                        '.csv'
                    )
                ) %>% 
                    filter(word_count > input$wordCountThreshold_Compare) %>% 
                    mutate(speaker_id = ifelse(is.na(speaker_id), 99999999, speaker_id)) %>%
                    select(date, speaker_id, speech_id, unit, word_count, c(input$selectedDictionaryVars_Compare)) %>% 
                    mutate(date = as.Date(as.character(date), format="%Y%m%d"))
            )
        }
        
        if (input$compareTypeSelectInput_Compare == "Demographic") {
            left_join(
                df,
                demographics_Compare(),
                by="speaker_id"
            ) %>% select(c(
                as.character(input$compareSelectInput_Compare),
                as.character(input$selectedDictionaryVars_Compare),
                date
            )) %>% 
                gather(key = "category", value = "value", -as.character(input$compareSelectInput_Compare), -date) %>%
                gather(key = "compare", value="compare_value", -value, -category, -date)
        }
        
    })
    
    
    ### OUTPUTS
    # Trend Compare Plot
    output$compareTrendPlot <- renderPlot({
        compare_df <- compare_df()
        ggplot2::ggplot(
            compare_df,
            mapping=aes(
                x=date,
                y=value
            )
        )  + geom_smooth(aes(color=compare_value), span = 0.3, se = FALSE) + 
            
            # Scaling & Aesthetics 
            scale_x_date(
                date_breaks = "1 year",
                date_labels = "%Y"
            ) + 
            scale_y_continuous(
                sec.axis = sec_axis(trans=~.*1, labels = scales::percent)
            ) +
            theme_classic() +
            theme(
                text=element_text(family= "Helvetica"),
                axis.line = element_line(color="lightgrey"),
                axis.title.y = element_text(color="darkgrey"),
                axis.text.y = element_text(color="darkgrey"),
                axis.text.x = element_text(color="darkgrey"),
                title = element_text(color="darkgrey"),
                legend.position = "bottom"
            ) +
            expand_limits(y=0) +
            labs(
                title=paste(
                    "Average Percent of Total Words by", 
                    input$selectedDemographicVars_Compare,
                    "and",
                    input$selectedDictionaryVars_Compare
                )
            ) +
            xlab(NULL) + 
            ylab(NULL)
    })
    
    # Compare Plot
    output$comparePlot <- renderPlot({
        compare_df <- compare_df()
        
        print(head(compare_df))
        
        ggplot2::ggplot(
            compare_df, 
            mapping=aes(
                x=demographic_value, 
                y=value
            )
        ) +
            geom_boxplot(aes(fill=demographic_value)) +
            scale_y_continuous(
                sec.axis = sec_axis(trans=~.*1, labels = scales::percent)
            ) +
            theme_classic() +
            theme(
                text=element_text(family= "Helvetica"),
                axis.ticks = element_blank(),
                axis.line = element_line(color="lightgrey"),
                axis.title.y = element_text(color="darkgrey"),
                axis.text.y = element_text(color="darkgrey"),
                axis.text.x = element_text(color="darkgrey"),
                title = element_text(color="darkgrey"),
                legend.position = "none"
            ) +
            xlab(NULL) + 
            ylab(NULL) +
            expand_limits(y=0)
    })
    
    
    ###########
    ### Map ###
    ###########
    
    ### REACTIVE DATA
    map_df <- reactive({
        dict <- tolower(input$mapDictSelect)
        congress <- tolower(input$mapCongressSelect)
        scores_df <- vroom(
            paste0('./data/scores/', congress, '_', dict, '_scores.csv'), delim=","
        )
        
        selects <- inner_join(
            scores_df %>% mutate(dictVar = !!input$mapDictionaryVar) %>% select("speaker_id", "dictVar"), 
            demographic %>% filter(congress == input$mapCongressSelect) %>% select("state", "cr_speaker_id") %>% mutate(region = tolower(state)),
            by=c("speaker_id" = "cr_speaker_id")
        ) %>% group_by(region) %>% summarise(summaryVar = mean(dictVar) / max(dictVar))
        
        x <- left_join(
                states_map,
                selects,
                by="region"
        )
        
        return(x)
    })
    
    ### OUTPUTS
    output$usMap <- renderPlot({
        map_df <- map_df()
        ggplot(map_df, aes(long, lat, group = group)) +
            geom_polygon(aes(fill = summaryVar), color = "white") +
            theme_classic()
    })
    
    ##########################
    ### Process Dictionary ###
    ##########################
    
    ### Inputs
    # Corpus
    output$corpusSelectInput_DictUpload <- renderUI({
        selectInput("selectedCorpus_DictUpload", "Corpus", lapply(corpora, to_name))
    })
    
    # Dictionary
    output$dictionarySelectInput_DictUpload <- renderUI({
        selectInput("selectedDictionary_DictUpload", "Dictionary", c(lapply(get_dict_names(), to_name), 'Upload...'))
    })
    
    # Unit File
    output$unitFileSelectInput_DictUpload <- renderUI({
        
        corpus_files <- get_corpus_files(to_file_path(input$selectedCorpus_DictUpload))
        computed_files <- get_computed_files(
            to_file_path(input$selectedCorpus_DictUpload),
            to_file_path(input$selectedDictionary_DictUpload)
        )
        unprocessed_corpus_files <- setdiff(corpus_files, computed_files)
        # selectInput("selectedCorpusFile_DictUpload", "Corpus File", c(unprocessed_corpus_files))
    })
    
    ### Observers
    # Upload Observer
    observeEvent(
        input$selectedDictionary_DictUpload, {
            if (input$selectedDictionary_DictUpload == 'Upload...') {
                showModal(
                    modalDialog(
                        title="Upload Dictionary",
                        fileInput("uploadDictionary", "Upload Dictionary", multiple = FALSE, accept=c('.dic'))
                    )
                )
            }
        }
    )
    
    observeEvent(
        input$uploadDictionary, {
            inFile <- input$uploadDictionary
            if (is.null(inFile)) 
                return()
            
            file.copy(inFile$datapath, file.path("./data/dictionaries", inFile$name))
            get_dict_names()
        }
    )
    
    check <- reactive({
        invalidateLater(millis = 10000, session = session)
        
        if (input$selectedDictionary_DictUpload == 'Upload...') {
            x <- F
        } else if (corpus_processor()$is_alive()) {
            x <- T
        } else {
            if (!file.exists(
                paste0(
                    './data/computed/',
                    to_file_path(input$selectedCorpus_DictUpload),
                    '/',
                    to_file_path(input$selectedDictionary_DictUpload),
                    '/monthly.csv'
                )
            )) {
                num_computed <- length(get_computed_files(
                    to_file_path(input$selectedCorpus_DictUpload),
                    to_file_path(input$selectedDictionary_DictUpload)
                ))
                message(num_computed)
                if (num_computed > 0) {
                    message('Rebuilding Monthly')
                    rebuild_monthly(
                        to_file_path(input$selectedCorpus_DictUpload),
                        to_file_path(input$selectedDictionary_DictUpload)
                    )
                }
            }
            x <- F
        }
        
        return(x)
    })
    
    output$did_it_work <- renderText({
        status <- check()
        if (status) {
            return('Job Running in Background')
        } else {
            return('No Jobs Running')
        }
    })
    
    corpus_processor <- eventReactive(input$processDictionary_DictUpload, {

        # Isolated needed files
        corpus_files <- get_corpus_files(to_file_path(input$selectedCorpus_DictUpload))
        computed_files <- get_computed_files(
            to_file_path(input$selectedCorpus_DictUpload),
            to_file_path(input$selectedDictionary_DictUpload)
        )
        unprocessed_corpus_files <- setdiff(corpus_files, computed_files)
        message(paste('Processing',length(unprocessed_corpus_files),"files"))
        
        # Background Process
        runner <- r_bg(
            function (corpus, dictionary, files) {
                library(callr)
                library(tidyverse)
                source('./scripts/utils.r')
                
                process_corpus_task <- function(corpus, dictionary, corpus_file) {
                    library(reticulate)
                    library(tidyverse)
                    source('./scripts/utils.r')
                    
                    source_python('./scripts/analyze_corpus.py')
                    files <- c(corpus_file)
                    
                    finished <- process_corpus(
                        to_file_path(corpus), 
                        to_file_path(dictionary),
                        np_array(files)
                    )
                    
                    return(TRUE)
                }
                
                jobs <- list()
                for (file in files) {
                    job <- r_bg(
                        process_corpus_task, args = list(
                            corpus = corpus,
                            dictionary =  dictionary,
                            corpus_file = file
                        )
                    )
                    jobs <- append(
                        jobs,
                        job
                    )
                    
                    Sys.sleep(60)
                }
            }, 
            args = list(
                corpus = input$selectedCorpus_DictUpload,
                dictionary =  input$selectedDictionary_DictUpload,
                files = unprocessed_corpus_files
            ),
            supervise = TRUE
        )
        
        return(runner)
    })
})
