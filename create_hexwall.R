library(magick)
library(purrr)

# I used the following blog post to put this together:
# https://mitchelloharawild.com/blog/hexwall/

# The code here could do with a good cleanup, but that wasn;t my aim, getting the hexwall was all I cared about...

sticker_files <- c("https://github.com/JaseZiv/worldfootballR/raw/main/man/figures/logo.png",
                   "https://github.com/JaseZiv/chessR/raw/master/man/figures/logo.png",
                   "https://github.com/JaseZiv/nblR/raw/main/man/figures/logo.png",
                   "https://github.com/JaseZiv/euroleagueR/raw/main/man/figures/logo.png",
                   "https://github.com/JaseZiv/bettRtab/raw/main/man/figures/logo.png")



stickers <- sticker_files %>% 
  map(compose(image_read, ~ image_transparent(., "white"), image_trim, .dir = "forward")) %>%
  set_names(basename(sticker_files))


# Desired sticker resolution in pixels
sticker_width <- 200

# Scale all stickers to the desired pixel width
stickers <- stickers %>%
  map(image_scale, sticker_width)

# Identify low resolution stickers
stickers %>%
  map_lgl(~ with(
    image_info(.x),
    width < (sticker_width-1)/2 && format != "png"
  ))


# Identify incorrect shapes / proportions (tolerance of +-2 height)
stickers %>%
  map_lgl(~ with(
    image_info(.x),
    height < (median(height)-2) | height > (median(height) + 2)
  ))



# Extract correct sticker height (this could also be calculated directly from width)
sticker_height <- stickers %>%
  map(image_info) %>%
  map_dbl("height") %>%
  median

# Coerce sticker dimensions
stickers <- stickers %>%
  map(image_resize, paste0(sticker_width, "x", sticker_height, "!"))

# stickers[["chessR.png"]]


sticker_row_size <- 3
# Calculate row sizes
sticker_col_size <- ceiling(length(stickers)/(sticker_row_size-0.5))
row_lens <- rep(c(sticker_row_size,sticker_row_size-1), length.out=sticker_col_size)
row_lens[length(row_lens)] <- row_lens[length(row_lens)]  - (length(stickers) - sum(row_lens))

sticker_rows <- map2(row_lens, cumsum(row_lens),
                     ~ seq(.y-.x+1, by = 1, length.out = .x)) %>%
  map(~ stickers[.x] %>%
        invoke(c, .) %>%
        image_append)


# sticker_rows[[1]]


# Add stickers to canvas
canvas <- image_blank(sticker_row_size*sticker_width, 
                      sticker_height + (sticker_col_size-1)*sticker_height/1.33526,
                      "white")


all <- reduce2(sticker_rows, seq_along(sticker_rows), 
        ~ image_composite(
          ..1, ..2,
          offset = paste0("+", ((..3-1)%%2)*sticker_width/2,
                          "+", round((..3-1)*sticker_height/1.33526))
        ),
        .init = canvas)

all



magick::image_write(image = all, "hexwall.png")
