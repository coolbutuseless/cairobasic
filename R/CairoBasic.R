

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Cairo Basic R6 Wrapper
#'
#' @export
#' @import R6
#' @import cairocore
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CairoBasic <- R6::R6Class(
  "CairoBasic",

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field cr cairo context
    #' @field surface cairo surface
    #' @field width,height dimensions of canvas
    #' @field flipy flip the y axis so origin is at bottom left? default: TRUE
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cr      = NULL,
    surface = NULL,
    width   = NULL,
    height  = NULL,
    flipy   = NULL,


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialise
    #'
    #' @param width,height dimensions of canvas
    #' @param bg initial background colour
    #' @param flipy flip the y axis such that the origin is at the lower-left.
    #'        default: TRUE.  If FALSE, then origin is at top-left of
    #'        canvas
    #' @param antialias logical. default TRUE.  If FALSE, then antialiasing will
    #'        be turned off and the everything will look a lot jaggier, but
    #'        will render much faster.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(width, height, bg = 'white', flipy = TRUE, antialias = TRUE) {

      self$width  <- width
      self$height <- height

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create the surface
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self$surface <- cairocore::cairo_image_surface_create(cairo_format_t$CAIRO_FORMAT_ARGB32, width, height)
      self$cr      <- cairocore::cairo_create (self$surface)

      if (!isTRUE(antialias)) {
        cairocore::cairo_set_antialias(self$cr, cairocore::cairo_antialias_t$CAIRO_ANTIALIAS_NONE)
      }

      self$flipy <- isTRUE(flipy)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # flip the y if requested
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (isTRUE(flipy)) {
        mat <- cairocore::cairo_matrix_t(1, 0, 0, -1, 0, height)
        cairocore::cairo_set_matrix(self$cr, mat)
      }


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Fill the background
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      fill <- col2rgb(bg, alpha = TRUE)/255
      cairocore::cairo_set_source_rgba(self$cr, fill[1], fill[2], fill[3], fill[4])
      cairocore::cairo_paint(self$cr)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Show the surface in the plotting window
    #'
    #' @param interpolate interpolate pixels for plotting the raster
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    show = function(interpolate = FALSE) {
      raster_out <- cairocore::cairo_image_surface_get_raster(self$surface, nchannel = 3)
      plot(raster_out, interpolate = interpolate)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Save surface as PNG
    #'
    #' @param filename PNG filename
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    write_png = function(filename) {
      cairocore::cairo_surface_flush(self$surface)
      cairocore::cairo_surface_write_to_png(surface = self$surface, filename = filename)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Get the surface as a raster object
    #'
    #' @param nchannel integer value. 1 = grey, 3 = rgb, 4 = rgba. Default: 3
    #'
    #' @return Raster object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_raster = function(nchannel = 3) {
      cairocore::cairo_image_surface_get_raster(self$surface, nchannel = nchannel)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Get the surface as an array object
    #'
    #' @param nchannel integer value. 1 = grey, 3 = rgb, 4 = rgba. Default: 3
    #'
    #' @return Array
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_array = function(nchannel = 3) {
      cairocore::cairo_image_surface_get_array(self$surface, nchannel = nchannel)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add circles
    #'
    #' @param x,y centres of the circles
    #' @param r radius of circles
    #' @param fill,colour fill and stroke colours. set to NA to not stroke or
    #'        fill.  May be an R colour name or a hex colour
    #' @param color same as 'colour'
    #' @param linewidth line width. default: 1
    #'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_circles = function(x, y, r, linewidth = 1, fill = 'black', colour = NA, color = colour) {

      col  <- col2rgb(color, alpha = TRUE)/255
      fill <- col2rgb(fill , alpha = TRUE)/255

      cairocore::cairo_save(self$cr)

      cairocore::cairo_set_line_width(self$cr, linewidth)

      cairocore::cairo_arc_vec(
        self$cr, x, y, r,
        angle1 = 0, angle2 = 2*pi,
        fill[1,], fill[2,], fill[3,], fill[4,],
        col [1,], col [2,],  col[3,],  col[4,]
      )

      cairocore::cairo_restore(self$cr)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add line segments
    #'
    #' @param x1,y1,x2,y2 segment endpoint coordinates
    #' @param colour segment colour
    #' @param color same as 'colour'
    #' @param linewidth line width. default: 1
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_line_segments = function(x1, y1, x2, y2, linewidth = 1, colour = 'black', color = colour) {

      col  <- col2rgb(color, alpha = TRUE)/255

      cairocore::cairo_save(self$cr)

      cairocore::cairo_set_line_width(self$cr, linewidth)

      cairocore::cairo_segment_vec(
        self$cr, x1, y1, x2, y2,
        col[1,], col[2,], col[3,], col[4,]
      )

      cairocore::cairo_restore(self$cr)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add polygons
    #'
    #' @param x,y polygon coordinates
    #' @param idx An integer vector used to separate locations in x and y into
    #'        multiple polygons. Specifies consecutive blocks of locations which
    #'        make up separate polygons.
    #' @param fill,colour fill and stroke colours. set to NA to not stroke or
    #'        fill.  May be an R colour name or a hex colour
    #' @param color same as 'colour'
    #' @param linewidth line width. default: 1
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_polygons = function(x, y, idx, linewidth = 1, fill = 'black', colour = NA, color = colour) {

      col  <- col2rgb(color, alpha = TRUE)/255
      fill <- col2rgb(fill , alpha = TRUE)/255

      cairocore::cairo_save(self$cr)

      cairocore::cairo_set_line_width(self$cr, linewidth)

      cairocore::cairo_polygon_vec(
        self$cr, x, y, idx,
        fill[1,], fill[2,], fill[3,], fill[4,],
        col [1,], col [2,],  col[3,],  col[4,]
      )

      cairocore::cairo_restore(self$cr)

      invisible(self)
    },



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add rectangles
    #'
    #' @param x,y,width,height rectangle positions
    #' @param fill,colour fill and stroke colours. set to NA to not stroke or
    #'        fill.  May be an R colour name or a hex colour
    #' @param color same as 'colour'
    #' @param linewidth line width. default: 1
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_rectangles = function(x, y, width, height, linewidth = 1, fill = 'black', colour = NA, color = colour) {

      col  <- col2rgb(color, alpha = TRUE)/255
      fill <- col2rgb(fill , alpha = TRUE)/255

      cairocore::cairo_save(self$cr)

      cairocore::cairo_set_line_width(self$cr, linewidth)

      cairocore::cairo_rectangle_vec(
        self$cr, x, y, width, height,
        fill[1,], fill[2,], fill[3,], fill[4,],
        col [1,], col [2,],  col[3,],  col[4,]
      )

      cairocore::cairo_restore(self$cr)

      invisible(self)
    },



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add text
    #'
    #' @param text text string
    #' @param x,y position
    #' @param fontsize font size
    #' @param angle angle in degrees
    #' @param center center the text at the give x,y position. default FALSE
    #' @param colour text colour
    #' @param color same as 'colour'
    #' @param family 'sans' or 'serif'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_text = function(text, x, y, fontsize = 20, angle = 0, center = FALSE,
                              colour = 'black', color = colour,
                              family = 'sans') {

      stopifnot(length(color) == 1)
      stopifnot(length(text) == 1)

      col  <- col2rgb(color, alpha = TRUE)/255


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Select the font. Currently just 'sans'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      cairo_select_font_face (
        self$cr,
        family = 'sans',
        slant  = cairocore::cairo_font_slant_t$CAIRO_FONT_SLANT_NORMAL,
        weight = cairocore::cairo_font_weight_t$CAIRO_FONT_WEIGHT_NORMAL
      )

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Calculate the width of the text and compensate for it
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      cairo_set_font_size (self$cr, fontsize)
      if (isTRUE(center)) {
        te <- cairocore::cairo_text_extents_t()
        cairocore::cairo_text_extents (self$cr, text, te);
        te <- as.list(te)
        x <- x - te$width  / 2 - te$x_bearing
        y <- y - te$height / 2 - te$y_bearing
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Set up a transformation so that the text is rotated correctly
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      cairocore::cairo_save(self$cr)
      cairocore::cairo_translate(self$cr, x, y)
      cairocore::cairo_rotate(self$cr, angle * pi/180)


      if (self$flipy) {
        tmat <- cairocore::cairo_matrix_t(fontsize, 0, 0, -fontsize, 0, 0)
        cairocore::cairo_set_font_matrix(self$cr, tmat)
      }


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Draw the actual text
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      cairo_set_source_rgba(self$cr, col[1], col[2], col[3], col[4])
      cairo_move_to(self$cr, 0, 0)
      cairo_show_text(self$cr, text)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Restore the transformation state prior to rendering the font
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      cairo_restore(self$cr)

      invisible(self)
    }
  )
)
























