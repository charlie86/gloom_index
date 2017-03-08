library(tidyverse)
library(httr)
library(stringr)

## Search for artists by name
get_artists <- function(artist_name) {
    
    # Search Spotify API for artist name
    res <- GET('https://api.spotify.com/v1/search', query = list(q = artist_name, type = 'artist')) %>%
        content %>% .$artists %>% .$items
    
    # Clean response and combine all returned artists into a dataframe
    artists <- map_df(seq_len(length(res)), function(x) {
        list(
            artist_name = res[[x]]$name,
            artist_uri = str_replace(res[[x]]$uri, 'spotify:artist:', ''), # remove meta info from the uri string
            artist_img = ifelse(length(res[[x]]$images) > 0, res[[x]]$images[[1]]$url, NA)
        )
    })
    
    return(artists)
}
artist_info <- get_artists('radiohead')

str(artist_info)

# Filter out other artist matches
artist_info <- artist_info %>% 
    filter(artist_name == 'Radiohead')

## Get artist albums
library(lubridate)

get_albums <- function(artist_uri) {
    albums <- GET(paste0('https://api.spotify.com/v1/artists/', artist_uri,'/albums')) %>% content
    
    map_df(1:length(albums$items), function(x) {
        tmp <- albums$items[[x]]
        
        # Make sure the album_type is not "single"
        if (tmp$album_type == 'album') {
            data.frame(album_uri = str_replace(tmp$uri, 'spotify:album:', ''),
                       album_name = str_replace_all(tmp$name, '\'', ''),
                       album_img = albums$items[[x]]$images[[1]]$url,
                       stringsAsFactors = F) %>%
                mutate(album_release_date = GET(paste0('https://api.spotify.com/v1/albums/', str_replace(tmp$uri, 'spotify:album:', ''))) %>% content %>% .$release_date, # you need a separate call to on "albums" to get release date.
                       album_release_year = ifelse(nchar(album_release_date) == 4, year(as.Date(album_release_date, '%Y')), year(as.Date(album_release_date, '%Y-%m-%d'))) # not all album_release_dates have months, so I created album_release year for sorting
                )
        } else {
            NULL
        }
        
    }) %>% filter(!duplicated(tolower(album_name))) %>%  # Sometimes there are multiple versions (just with different capitalizations) of the same album
        arrange(album_release_year)
}

album_info <- get_albums(artist_info$artist_uri)

str(album_info)

# Filter out remixes and EPs
non_studio_albums <- c('TKOL RMX 1234567', 'In Rainbows Disk 2', 'Com Lag: 2+2=5', 'I Might Be Wrong')
album_info <- filter(album_info, !album_name %in% non_studio_albums)

## Get tracks
get_tracks <- function(artist_info, album_info) {
    
    client_id <- 'xxxxxxxxxxxxxxxxxxxxxx'
    client_secret <- 'xxxxxxxxxxxxxxxxxxxx'
    access_token <- POST('https://accounts.spotify.com/api/token',
                         accept_json(), authenticate(client_id, client_secret),
                         body = list(grant_type='client_credentials'),
                         encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token
    
    track_info <- map_df(album_info$album_uri, function(x) {
        tracks <- GET(paste0('https://api.spotify.com/v1/albums/', x, '/tracks')) %>% 
            content %>% 
            .$items 
        
        uris <- map(1:length(tracks), function(z) {
            gsub('spotify:track:', '', tracks[z][[1]]$uri)
        }) %>% unlist %>% paste0(collapse=',')
        
        res <- GET(paste0('https://api.spotify.com/v1/audio-features/?ids=', uris),
                   query = list(access_token = access_token)) %>% content %>% .$audio_features
        df <- unlist(res) %>% 
            matrix(nrow = length(res), byrow = T) %>% 
            as.data.frame(stringsAsFactors = F)
        names(df) <- names(res[[1]])
        df <- df %>% 
            mutate(album_uri = x,
                   track_number = row_number()) %>% 
            rowwise %>% 
            mutate(track_name = tracks[[track_number]]$name) %>%
            ungroup %>% 
            left_join(album_info, by = 'album_uri') %>% 
            rename(track_uri = id) %>% 
            select(-c(type, track_href, analysis_url, uri))
        return(df)
    }) %>%
        mutate(artist_img = artist_info$artist_img) %>% 
        mutate_at(c('album_uri', 'track_uri', 'album_release_date', 'track_name', 'album_name', 'artist_img'), funs(as.character)) %>%
        mutate_at(c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness', 'album_release_year',
                    'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature', 'track_number'), funs(as.numeric(gsub('[^0-9.-]+', '', as.character(.))))) # for some reason parse_number() from readr doesn't work here
    return(track_info)
}

spotify_df <- get_tracks(artist_info, album_info)

str(spotify_df)

##### Genius Lyrics
token <- 'xxxxxxxxxxxxxxxxxxxx'

genius_get_artists <- function(artist_name, n_results = 10) {
    baseURL <- 'https://api.genius.com/search?q=' 
    requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                         '&per_page=', n_results,
                         '&access_token=', token)
    
    res <- GET(requestURL) %>% content %>% .$response %>% .$hits
    
    map_df(1:length(res), function(x) {
        tmp <- res[[x]]$result$primary_artist
        list(
            artist_id = tmp$id,
            artist_name = tmp$name
        )
    }) %>% unique
}

genius_artists <- genius_get_artists('radiohead')
genius_artists

## Get track urls
baseURL <- 'https://api.genius.com/artists/' 
requestURL <- paste0(baseURL, genius_artists$artist_id[1], '/songs')

track_lyric_urls <- list()
i <- 1
while (i > 0) {
    tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
    track_lyric_urls <- c(track_lyric_urls, tmp$songs)
    if (!is.null(tmp$next_page)) {
        i <- tmp$next_page
    } else {
        break
    }
}

length(track_lyric_urls)
summary(track_lyric_urls[[1]])


## Scrape lyrics from each url
library(rvest)

lyric_scraper <- function(url) {
    read_html(url) %>% 
        html_node('lyrics') %>% 
        html_text
}

genius_df <- map_df(1:length(track_lyric_urls), function(x) {
    # add in error handling
    lyrics <- try(lyric_scraper(track_lyric_urls[[x]]$url))
    if (class(lyrics) != 'try-error') {
        # strip out non-lyric text and extra spaces
        lyrics <- str_replace_all(lyrics, '\\[(Verse [[:digit:]]|Pre-Chorus [[:digit:]]|Hook [[:digit:]]|Chorus|Outro|Verse|Refrain|Hook|Bridge|Intro|Instrumental)\\]|[[:digit:]]|[\\.!?\\(\\)\\[\\],]', '')
        lyrics <- str_replace_all(lyrics, '\\n', ' ')
        lyrics <- str_replace_all(lyrics, '([A-Z])', ' \\1')
        lyrics <- str_replace_all(lyrics, ' {2,}', ' ')
        lyrics <- tolower(str_trim(lyrics))
    } else {
        lyrics <- NA
    }
    
    tots <- list(
        track_name = track_lyric_urls[[x]]$title,
        lyrics = lyrics
    )
    
    return(tots)
})

str(genius_df)

## Reconcile track names between Genius and Spotify
genius_df$track_name[genius_df$track_name == 'Packt Like Sardines in a Crushd Tin Box'] <- 'Packt Like Sardines in a Crushed Tin Box'
genius_df$track_name[genius_df$track_name == 'Weird Fishes / Arpeggi'] <- 'Weird Fishes/ Arpeggi'
genius_df$track_name[genius_df$track_name == 'A Punchup at a Wedding'] <- 'A Punch Up at a Wedding'
genius_df$track_name[genius_df$track_name == 'Dollars and Cents'] <- 'Dollars & Cents'
genius_df$track_name[genius_df$track_name == 'Bullet Proof...I Wish I Was'] <- 'Bullet Proof ... I Wish I was'

genius_df <- genius_df %>% 
    mutate(track_name_join = tolower(str_replace(track_name, '[[:punct:]]', ''))) %>% 
    filter(!duplicated(track_name_join)) %>% 
    select(-track_name)

track_df <- spotify_df %>%
    mutate(track_name_join = tolower(str_replace(track_name, '[[:punct:]]', ''))) %>%
    left_join(genius_df, by = 'track_name_join') %>%
    select(track_name, valence, duration_ms, lyrics, album_name, album_release_year, album_img)

str(track_df)

# Find top 10 songs with lowest valence
track_df %>% 
    select(valence, track_name) %>%
    arrange(valence) %>% 
    slice(1:10)

## Calculate lyrical sadness
library(tidytext)

sad_words <- sentiments %>% 
    filter(lexicon == 'nrc', sentiment == 'sadness') %>% 
    select(word) %>% 
    mutate(sad = T)

sent_df <- track_df %>% 
    unnest_tokens(word, lyrics) %>%
    anti_join(stop_words, by = 'word') %>%
    left_join(sad_words, by = 'word') %>%
    group_by(track_name) %>% 
    summarise(pct_sad = round(sum(sad, na.rm = T) / n(), 4),
              word_count = n()) %>% 
    ungroup

sent_df %>% 
    select(pct_sad, track_name) %>%
    arrange(-pct_sad) %>% 
    head(10)

## Create gloom index
library(scales)

track_df <- track_df %>% 
    left_join(sent_df, by = 'track_name') %>% 
    mutate_at(c('pct_sad', 'word_count'), funs(ifelse(is.na(.), 0, .))) %>% 
    mutate(lyrical_density = word_count / duration_ms * 1000,
           gloom_index = round(rescale(1 - ((1 - valence) + (pct_sad * (1 + lyrical_density))) / 2, to = c(1, 100)), 2)) 

# Find top 10 gloomiest songs
track_df %>%
    select(gloom_index, track_name) %>%
    arrange(gloom_index) %>%
    head(10)

## Generate chart
library(RColorBrewer)
library(highcharter)

plot_df <- track_df %>% 
    rowwise %>% 
    mutate(tooltip = paste0('<a style = "margin-right:', max(max(nchar(track_name), nchar(album_name)) * 7, 55), 'px">', # dynamic sizing
                            '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                            '<b>Album:</b> ', album_name,
                            '<br><b>Track:</b> ', track_name)) %>% 
    ungroup

avg_line <- plot_df %>% 
    group_by(album_release_year, album_name, album_img) %>% 
    summarise(avg = mean(gloom_index)) %>% 
    ungroup %>% 
    transmute(x = as.numeric(as.factor(album_release_year)), 
              y = avg,
              tooltip = paste0('<a style = "margin-right:55px">',
                               '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                               '<b>Album:</b> ', album_name,
                               '<br><b>Average Gloom Index:</b> ', round(avg, 2),
                               '</a>'))
plot_track_df <- plot_df %>% 
    mutate(tooltip = paste0(tooltip, '<br><b>Gloom Index:</b> ', gloom_index, '</a>'),
           album_number = as.numeric(as.factor(album_release_year))) %>% 
    ungroup

album_chart <- hchart(plot_track_df, 'scatter', hcaes(x = as.numeric(as.factor(album_release_year)), y = gloom_index, group = album_name)) %>% 
    hc_add_series(data = avg_line, type = 'line') %>%
    hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
    hc_colors(c(sample(brewer.pal(n_distinct(track_df$album_name), 'Paired')), 'black')) %>% 
    hc_xAxis(title = list(text = 'Album'), labels = list(enabled = F)) %>% 
    hc_yAxis(max = 100, title = list(text = 'Gloom Index')) %>% 
    hc_title(text = 'Data Driven Depression') %>% 
    hc_subtitle(text = 'Radiohead song sadness by album') %>% 
    hc_add_theme(hc_theme_smpl())
album_chart$x$hc_opts$series[[10]]$name <- 'Album Averages'
album_chart

## save album chart as HTML file
library(htmlwidgets)
saveWidget(album_chart, 'album_chart.html')

