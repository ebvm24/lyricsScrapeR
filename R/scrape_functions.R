#' Song Index Function
#' 
#' A function to scrape songs names and 
#' hyperlinks (to said songss) from an artist page
#' 
#' @param artist_page url of letras.com link to specific artist with all songs (or)
#'
#' @return xml_nodeset object with name of every song and hyperlink to song page
#'
#' @examples
#' song_index('https://www.letras.com/cartola/')
#'
#' @export
song_index <- function(artist_page){
  sng_idx <- xml2::read_html(artist_page)
  sng_idx <- rvest::html_nodes(sng_idx,"#cnt-artist-songlist > div.cnt-list--alp > ul")
  sng_idx <- rvest::html_children(sng_idx)
  return(sng_idx)
}

#' Song Information
#'
#' A function to get song information into a list object
#'
#' @param a xml_node containing song name and link to song page
#'
#' @return a list with the name of the song, hyperlink to song page, list of song lyrics, divided by stanza
#'
#' @examples
#' song_info(song_index('https://www.letras.com/cartola/')[[1]])
#'
#' @export
song_info <- function(a){
  d <- rvest::html_children(a)
  song <- list(name=rvest::html_text(d))
  song[['link']] <- paste0(site,rvest::html_attr(d,"href"))
  song[['lyrics']] <- tryCatch(get_lyrics(song$link), error = function(e) e)
  return(song)
}

#' Lyric Scraping Function
#'
#' Function used by song_info to scrape lyrics and get into a list format
#'
#' @param link link to song lyrics page
#'
#' @return song lyrics in a list object
#'
#' @examples
#' get_lyrics('https://www.letras.com/cartola/')
#'
#' @export
get_lyrics <- function(link){
  scrape <- xml2::read_html(link)
  lyrics <- rvest::html_nodes(scrape,"#js-lyric-cnt > div.g-pr.g-sp > div.cnt-letra > article")
  lyrics <- rvest::html_children(lyrics) %>%
  lyrics <- gsub(x = lyrics, pattern = '<br>', replacement = "\n")
  lyrics <- gsub(x = lyrics, pattern = '<.*?>', replacement = '')
  lyrics1 <- strsplit(lyrics,'\n')
  return(lyrics1)
}

#' Scrape and save songs for an artist
#'
#' Function using other scrape_functions to scrape and save all Artist song lyrics
#'
#' @param artist_name Name of the artist to save file as 
#' @param artist_page Website of the artist
#'
#' @return song lyric list
#'
#' @examples
#' scrape_songs('cartola','https://www.letras.com/cartola/')
#'
#' @export
scrape_songs <- function(artist_name, artist_page,write_rds=FALSE,directory = NULL){
  index_page <- song_index(artist_page)
  all_songs <- purrr::map(index_page,song_info)
  names(all_songs) <- purrr::map_chr(all_songs, function(x) x$name)
  if(write_rds){
    if(is.null(directory)){
      directory <- getwd()
    }
    file_name <- file.path(directory,paste(artist_name,'songs.rds',sep='_'))
    saveRDS(all_songs, file = file_name)
    }
    return(all_songs)
}

site <- 'https://www.letras.com' # Site
