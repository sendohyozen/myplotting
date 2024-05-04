#' Title
#'
#' @param data data frame anlysised previously with y value and its 95% CI intervals
#' @param x_value column name of x value in the dataframe
#' @param y_value column name of y value in the dataframe
#' @param y_min column name of y value min (95% low) in the dataframe
#' @param y_max column name of y value max (95% upper) in the dataframe
#' @param group  column name of group in the dataframe
#' @param xlab_level levels of x lab
#' @param group_level levels of group(legends)
#' @param position.dodge position dodge of errbor/points default 0.3
#' @param point.shape shape of the points, can be circle/square, default circle
#' @param point.size size of the points, default 3
#' @param line.width width of the errbar line, default 0.3
#' @param errbar.width length of th errbar, default 0 (No line observed)
#' @param y.limits limits of y axis (x axis if filped), a vector: c(min, max), such as: c(0, 1)
#' @param y.breaks breaks of y axis (x axis if filped), a vector:  seq(min, max, interval), such as: seq(0, 1, 0.2). Or design values you wish
#' @param coord.filp if filp the plot,default T
#' @param title plot title
#' @param x_lab plot x lab
#' @param y_lab plot y lab
#' @param base.size base text size of the theme default 18
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param pal color for filling  default pal_lancet
#' @param plot.theme theme for plot deault classic
#'
#' @return a ggplot2 object can add geom_hline as you wish
#' @export
#'
#' @examples forestplot_identity()
forestplot_identity <-  function(data,  x_value, y_value, y_min, y_max, group,
                                 xlab_level, group_level,
                                 position.dodge =0.3, point.shape='circle', point.size=3,
                                 line.width = 0.5, errbar.width = 0,
                                 y.limits, y.breaks,
                                 coord.filp  = T,
                                 title = NULL, x_lab =NULL, y_lab='',
                                 base.size=18, legend.position = 'top', legend.size =12,
                                 pal= ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9),
                                 plot.theme ){

    cat('Dataframe with analysised results，data contains x , y, 95%CI values! ')


    # plotting data
    if(!missing(group)){

        plot_df = data.frame(x = data[[x_value]],
                             y =  as.numeric(data[[y_value]]),
                             ymin =  as.numeric(data[[y_min]]),
                             ymax =  as.numeric(data[[y_max]]),
                             group = data[[group]])
    }else{
        plot_df = data.frame(x = data[[x_value]],
                             y =  as.numeric(data[[y_value]]),
                             ymin =  as.numeric(data[[y_min]]),
                             ymax =  as.numeric(data[[y_max]]),
                             group = '1')

        legend.position = 'none'
    }

    # factor level
    if(!missing(xlab_level)){
        plot_df$x = factor(plot_df$x, levels = xlab_level)
    }
    if(!missing(group_level)){
        plot_df$group = factor(plot_df$group, levels = group_level)
    }

    # arrange points/errorbar consistently
    pd = position_dodge(position.dodge)

    # point size
    if(point.shape=='circle'){
        shape = 16
    }else if(point.shape=='square'){
        shape = 15
    }else{
        cat('point.shape can only be circle or square !')
    }

    # plotting
    p = ggplot(plot_df) +
        geom_point(aes(x = x, y = y, colour = group),  position = pd, size = point.size, shape = shape ) +  # shape = 15 filled square , shape =16 filled circle
        geom_errorbar(aes(x = x, y = y, ymin = ymin, ymax = ymax, colour = group),
                      position = pd, linewidth = line.width,  width = errbar.width) +    #  error rangeline as linewidth; width as errbar length
        scale_color_manual(values = pal)

    p = p + ggtitle(title) +  xlab(x_lab) +  ylab(y_lab)

    # y limits
    if(!missing(y.limits) & !missing(y.breaks)){
        p = p + scale_y_continuous(limits = y.limits,   # c(0, 1),
                                   # trans = 'log10',
                                   breaks = y.breaks,  # seq(0, 1, 0.2)
                                   # labels = c('<0.1',10^(0:2), '>1000')
        )

    }else{
        cat("Not y limits and breaks both given !")
    }

    # coord filp
    if(coord.filp==T){
        p = p + coord_flip()
    }


    # theme
    if(missing(plot.theme)){
        p = p + theme_classic(base_size = base.size) +
            theme(plot.title = element_text(colour = "black", face = "bold", hjust = 0.5),
                  legend.position = legend.position,
                  legend.title = element_blank(),
                  legend.text= element_text(color="black", size = legend.size),
                  axis.text.x = element_text(colour = 'black', angle = 0, hjust =1),
                  axis.text.y = element_text(colour = 'black'),
                  axis.ticks.x = element_blank())
    }else{
        p = p + plot.theme
    }

    return(p)
}
