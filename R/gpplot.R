gpplot = function(x, y, ...,  color = NULL, shape = NULL) UseMethod("gpplot")

gpplot.default = function(x, y = NULL,
                          ...,
                          xlim = NULL, ylim = NULL,
                          main = NULL,
                          xlab = NULL, ylab = NULL,
                          color = NULL, shape = NULL,
                          color_legend_title = NULL,
                          shape_legend_title = NULL){
  if( is.null(y) ){ y = x ; x = 1:length(y)}
  if( is.null(xlim) ) xlim = c( min(x), max(x))
  if( is.null(ylim) ) ylim = c( min(y), max(y))

  df.plot = data.frame( x = x, y = y)

  if( is.null(color) & is.null(shape)){
    # NULL - NULL
    plot = ggplot(data = df.plot, mapping = aes(x = x, y = y)) +
           geom_point(color = "black")
  }else if( is.null(color) & is.numeric(shape) ){
    # NULL - numeric
    df.plot$shape = shape
    plot =  ggplot(data = df.plot, mapping = aes(x = x, y = y, size = shape)) +
            geom_point(color = "black") +
            scale_size_continuous(shape_legend_title)
  }else if( is.null(color) & is.factor(shape)){
    # NULL - factor
    df.plot$shape = shape

    plot = ggplot(data = df.plot, mapping = aes(x = x, y = y, shape = shape)) +
           geom_point(color = "black") +
           scale_shape(shape_legend_title)
  }else if( is.numeric(color) & is.null(shape)){
    # numeric - NULL
    df.plot$color = color

    plot = ggplot(data = df.plot, mapping = aes(x = x, y = y, color = color)) +
           geom_point() +
           scale_color_continuous(color_legend_title)
  }else if( is.numeric(color) & is.numeric(shape)){
    # numeric - numeric
    df.plot$color = color
    df.plot$shape = shape

    plot = ggplot(data = df.plot, mapping = aes(x = x, y = y, color = color, size = shape)) +
           geom_point() +
           scale_size_continuous(shape_legend_title) +
           scale_color_continuous(color_legend_title)
  }else if( is.numeric(color) & is.factor(shape)){
    # numeric - factor
    df.plot$color = color
    df.plot$shape = shape

    plot = ggplot(data = df.plot, mapping = aes(x = x, y = y, color = color, shape = shape)) +
      geom_point() +
      scale_shape(shape_legend_title) +
      scale_color_continuous(color_legend_title)
  }else if( is.factor(color) & is.null(shape) ){
    df.plot$color = color
    # factor - NULL
    df.plot$color = color

    plot = ggplot(data = df.plot, mapping = aes(x = x, y = y, color = color)) +
           geom_point() +
           scale_color_discrete(color_legend_title)
  }else if( is.factor(color) & is.numeric(shape)){
    # factor - numeric
    df.plot$color = color
    df.plot$shape = shape

    plot = ggplot(data = df.plot, mapping = aes(x = x, y = y, color = color, size = shape)) +
           geom_point() +
           scale_color_discrete(color_legend_title) +
           scale_size_continuous(shape_legend_title)
  }else if( is.factor(color) & is.factor(shape)){
    # factor - factor
    df.plot$color = color
    df.plot$shape = shape

    plot = ggplot(data = df.plot, mapping = aes(x = x, y = y, color = color, shape = shape)) +
           geom_point() +
           scale_color_discrete(color_legend_title) +
           scale_shape_discrete(shape_legend_title)
  }else{
    errorCondition("Invalid input as color or shape (It requires Factor or Numeric).")
  }
  plot + xlab(xlab) + ylab(ylab) + ggtitle(main) + theme_bw() + theme(text = element_text(family = "serif"))
}

gpplot.matrix = function(Matrix, y = NULL,
                          ...,
                          xlim = NULL, ylim = NULL,
                          main = NULL,
                          xlab = NULL, ylab = NULL,
                          color = NULL, shape = NULL,
                          color_legend_title = NULL,
                          shape_legend_title = NULL){
  gpplot.default(
    x = x[,1], y = y[,2],
    xlim = xlim, ylim = ylim,
    main = main,
    xlab = xlab, ylab = ylab,
    color = color, shape = color,
    color_legend_title = color_legend_title,
    shape_legend_title = shape_legend_title)
}

ggplot.table = function(table,
                        ...,
                        xlim = NULL, ylim = NULL,
                        main = NULL,
                        xlab = NULL, ylab = NULL,
                        color = NULL,
                        color_legend_title = NULL){
  if(  length(dim(table)) == 1 ){
    df.plot = data.frame( x = table )
    df.plot$x.Freq = as.factor(df.plot)
    plot = ggplot(data = df.plot, aes( x = x.Var1, y = x.Freq)) +
           geom_bar( stat = "identity", width = .5, color = "black", alpha = .75) +
           geom_errorbar( aes(ymin=x.Freq-sd, ymax=x.Freq+sd), width=0.1, colour="orange") +
           geom_text(aes(label= x.Freq, y = min(x.Freq)), vjust= 1 , col = "grey")
    plot

  }else{

    }


  plot + xlab(xlab) + ylab(ylab) + ggtitle(main) + theme_minimal() +
    scale_y_continuous(breaks = NULL)
    theme(text = element_text(family = "serif"))
}
