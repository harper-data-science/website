## https://gt.rstudio.com/articles/gt.html

# Take the `islands` dataset and use some
# dplyr functionality to obtain the ten
# biggest islands in the world

## Setup ####
library(gt)

## make df ####
islands_df <-  data.frame(
  name = names(islands),
  size = islands
  ) 
row.names(islands_df) <- NULL

# Display the table
islands_df

## simple table ####
# Create a display table showing ten of
# the largest islands in the world
gt_tbl <- gt(islands_df)

# Show the gt Table
gt_tbl

## add header ####
# Make a display table with the `islands_tbl`
# table; put a heading just above the column labels
gt_tbl <- 
  gt_tbl |>
  tab_header(
    title = "Large Landmasses of the World",
    subtitle = "The top ten largest are presented"
  )

# Show the gt Table
gt_tbl

## subsetting ####
# Use markdown for the heading's `title` and `subtitle` to
# add bold and italicized characters
gt(islands_df[1:2,]) |>
  tab_header(
    title = md("**Large Landmasses of the World**"),
    subtitle = md("The *top two* largest are presented")
  )

## source note ####
# Display the `islands_tbl` data with a heading and
# two source notes
gt_tbl <- 
  gt_tbl |>
  tab_source_note(
    source_note = "Source: The World Almanac and Book of Facts, 1975, page 406."
  ) |>
  tab_source_note(
    source_note = md("Reference: McNeil, D. R. (1977) *Interactive Data Analysis*. Wiley.")
  )

# Show the gt table
gt_tbl

## footnotes ####
# Add footnotes (the same text) to two different
# cell; data cells are targeted with `data_cells()`
gt_tbl <- 
  gt_tbl |>
  tab_footnote(
    footnote = "The Americas.",
    locations = cells_body(columns = name, rows = c(35,39))
  )

# Show the gt table
gt_tbl

## "stub" view for row names ####
# Create a gt table showing ten of the
# largest islands in the world; this
# time with a stub
gt_tbl <- 
  islands_df |>
  gt(rowname_col = "name")

# Show the gt table
gt_tbl

## add stub label ####
# Generate a simple table with a stub
# and add a stubhead label
gt_tbl <- 
  gt_tbl |>
  tab_stubhead(label = "landmass")

# Show the gt table
gt_tbl

## all of the items together ####
# Display the `islands_tbl` data with a stub,
# a heading, source notes, and footnotes

# largest landmass ('Asia')
largest <- islands_df$name[which.max(islands_df$size)]


gt_tbl <- 
  gt_tbl |>
  tab_header(
    title = "Large Landmasses of the World",
    subtitle = "The top ten largest are presented"
  ) |>
  tab_source_note(
    source_note = "Source: The World Almanac and Book of Facts, 1975, page 406."
  ) |>
  tab_source_note(
    source_note = md("Reference: McNeil, D. R. (1977) *Interactive Data Analysis*. Wiley.")
  ) |>
  tab_footnote(
    footnote = md("The **largest** by area."),
    locations = cells_body(
      columns = size, rows = largest
    )
  ) |>
  tab_footnote(
    footnote = "The lowest by population.",
    locations = cells_body(
      columns = size, rows = contains("arc")
    )
  )

# Show the gt table
gt_tbl
