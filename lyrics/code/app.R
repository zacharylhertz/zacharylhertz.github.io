# Big Thief Lyric Bot Shiny App
# Author: Zachary Lorico Hertz
# Date: June 2025
# Description: Make a 'lyric bot' shiny app for website.

# Load required libraries ------------------------------------------------
library(shiny)
library(shinythemes)
library(here)
library(tidyverse)
library(data.table)

# Data processing (run once when app starts) ----------------------------
# Read in data
dat <- read.csv("big-thief.csv") |> 
  select(-X)

# Chunker function
create_lyric_chunks <- function(lyrics_text, song_id, title, artist, year) {
  # Split on pattern: ] followed by newline (section markers)
  sections <- str_split(lyrics_text, "(?<=\\])\\n")[[1]] |>
    str_trim()
  
  # Remove empty sections
  sections <- sections[nchar(sections) > 0]
  
  chunks <- list()
  
  for(section in sections) {
    # Clean each section - remove bracketed headers and split into lines
    lines <- str_split(section, "\n")[[1]] |>
      str_trim() |>
      str_subset("^(?!\\[.*\\]).*$") |>  # Remove bracketed section markers
      str_subset("^.+$")  # Remove empty lines
    
    # Skip if no actual lyric lines remain
    if(length(lines) == 0) next
    
    # Create 4-line chunks within this section
    for(i in seq(1, length(lines), by = 4)) {
      chunk_lines <- lines[i:min(i+3, length(lines))]
      
      chunks[[length(chunks) + 1]] <- data.frame(
        song_id = song_id,
        title = title,
        artist = artist,
        year = year,
        chunk_id = length(chunks) + 1,
        lyrics_chunk = paste(chunk_lines, collapse = "\n"),
        line_count = length(chunk_lines)
      )
    }
  }
  
  return(bind_rows(chunks))
}

# Apply to entire discography
lyric_chunks <- dat |>
  rowwise() |>
  do(create_lyric_chunks(.$lyrics, .$id, .$title, .$artist, .$year)) |>
  ungroup()

# Random lyric function (modified for Shiny)
get_random_lyric <- function() {
  sample_chunk <- lyric_chunks |> slice_sample(n = 1)
  
  lyrics_text <- sample_chunk$lyrics_chunk
  attribution <- paste0("— ", sample_chunk$title, " by ", sample_chunk$artist, " (", sample_chunk$year, ")")
  
  return(list(lyrics = lyrics_text, attribution = attribution))
}

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Custom CSS for styling
  tags$head(
    tags$style(HTML("
      .main-container {
        max-width: 600px;
        margin: 50px auto;
        text-align: center;
      }
      
      .lyrics-display {
        background: #f8f9fa;
        border-left: 4px solid #007bff;
        padding: 30px;
        margin: 30px 0;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .lyrics-text {
        font-size: 18px;
        line-height: 1.6;
        color: #2c3e50;
        font-style: italic;
        white-space: pre-line;
        margin-bottom: 20px;
      }
      
      .attribution {
        font-size: 14px;
        color: #6c757d;
        font-weight: 500;
      }
      
      .btn-generate {
        font-size: 16px;
        padding: 12px 30px;
        margin: 20px 0;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border: none;
        border-radius: 25px;
        color: white;
        transition: all 0.3s ease;
      }
      
      .btn-generate:hover {
        transform: translateY(-2px);
        box-shadow: 0 5px 15px rgba(0,0,0,0.2);
        background: linear-gradient(135deg, #764ba2 0%, #667eea 100%);
      }
      
      .title {
        color: #2c3e50;
        margin-bottom: 10px;
      }
      
      .subtitle {
        color: #6c757d;
        margin-bottom: 30px;
      }
      
      .stats {
        font-size: 12px;
        color: #868e96;
        margin-top: 30px;
      }
    "))
  ),
  
  div(class = "main-container",
      h1("Big Thief Lyric Generator", class = "title"),
      p("Discover random lyrical snippets from Big Thief's discography.", class = "subtitle"),
      
      actionButton("generate", "✨ Generate Random Lyrics", 
                   class = "btn btn-generate"),
      
      div(class = "lyrics-display",
          div(id = "lyrics-output",
              div(class = "lyrics-text", "🎵 Click the button above to start generating lyrics."),
              div(class = "attribution", "— Ready to explore Adrianne Lenker's penmanship?")
          )
      ),
      
      div(class = "stats",
          paste("Built from", nrow(lyric_chunks), "lyrical chunks across Big Thief's discography.")
      )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Generate new lyrics when button is clicked
  observe({
    result <- get_random_lyric()
    
    # Update the lyrics display
    session$sendCustomMessage("updateLyrics", list(
      lyrics = paste("🎵", result$lyrics, "🎵"),
      attribution = result$attribution
    ))
    
  }) |> bindEvent(input$generate)
}

# Add JavaScript for smooth updates
ui <- tagList(
  ui,
  tags$script(HTML("
    $(document).ready(function() {
      Shiny.addCustomMessageHandler('updateLyrics', function(data) {
        $('#lyrics-output').fadeOut(200, function() {
          $(this).html('<div class=\"lyrics-text\">' + data.lyrics + '</div><div class=\"attribution\">' + data.attribution + '</div>').fadeIn(300);
        });
      });
    });
  "))
)

# Run the app -------------------------------------------------------------
shinyApp(ui = ui, server = server)
