% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrape_functions.R
\name{song_index}
\alias{song_index}
\title{Song Index Function}
\usage{
song_index(artist_page)
}
\arguments{
\item{artist_page}{url of letras.com link to specific artist with all songs (or)}
}
\value{
xml_nodeset object with name of every song and hyperlink to song page
}
\description{
A function to scrape songs names and 
hyperlinks (to said songss) from an artist page
}
\examples{
song_index('https://www.letras.com/cartola/')

}
