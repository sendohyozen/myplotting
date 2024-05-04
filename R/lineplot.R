#' lineplot with data presented identity (as rawdata give mean , sd)
#'
#' @param data data frame anlysised previously with mean, sd
#' @param x_value column name of x value in the dataframe
#' @param y_value column name of y value in the dataframe, such as mean
#' @param group column name of group for line colors
#' @param errbar column name of error bar
#' @param lab_text adding text of y value on the point, default False
#' @param shape.size size of the points(shape), default 3
#' @param line.width width of the lines, default 0.7
#' @param text.size size of the text add on the shapes, default 3
#' @param xlab_level levels of x lab
#' @param group_level levels of group lab
#' @param title plot title
#' @param x_lab plot x lab
#' @param y_lab plot y lab
#' @param base.size base text size of the theme default 18
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param xlab.angle if rotate the x labs defualt 0
#' @param pal color for filling  default pal_lancet
#' @param chinese if text in Chinese ,default False
#' @param plot.theme plot.theme
#'
#' @return ggplot plot object
#' @export
#'
#' @examples lineplot_identity(data, x, y )
lineplot_identity <-  function(data,  x_value, y_value, group,
                               errbar, lab_text=F,
                               shape.size = 3, line.width = 0.7, text.size = 3,
                               xlab_level, group_level,
                               title = NULL, x_lab =NULL, y_lab='',
                               base.size=18, legend.position = 'top', legend.size =12, xlab.angle=0,
                               pal= ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9),
                               chinese = F,
                               plot.theme ){

    cat('plotting raw data as provided, dataframe with x，y，errorbar columns!')

    # if  text have Chinese
    if(chinese==T){
        font.family = "STHeitiSC-Medium"
    }else{
        font.family = "Arial"
    }

    # test the data, must give error value and y value,  as stat = identity
    if(missing(errbar) & missing(y_value)){
        stop("must give a column for y and errbar, if none using the function of lineplotting_conitnuous!")
    }else if(!missing(errbar) & lab_text){
        stop("can't have both error bar and y value text!")
    }

    # plot data
    if(missing(errbar)){
        plot_df = data.frame(x = data[[x_value]],
                             y = data[[y_value]],
                             group = data[[group]])
    }else{
        plot_df = data.frame(x = data[[x_value]],
                             y = data[[y_value]],
                             group = data[[group]],
                             sd = data[[errbar]])
    }

    # lab legend order- seting factors
    if(!missing(xlab_level)){
        plot_df$x = factor(plot_df$x, levels = xlab_level)
    }
    if(!missing(group_level)){
        plot_df$group = factor(plot_df$group, levels = group_level)
    }

    # plotting
    p = ggplot(data = plot_df, aes(x=x, y=y, group = group)) +
        geom_point(aes(colour=group, shape = group), size = shape.size, alpha=0.7) +
        geom_line(aes(color=group), linewidth = line.width)

    p = p + scale_color_manual(values = pal) +
        ggtitle(title) +  xlab(x_lab) +  ylab(y_lab)


    # error bar
    if(!missing(errbar)){
        p = p + geom_errorbar(aes(ymin = y - sd, ymax = y + sd, colour=group),
                              width=0.2, linewidth = line.width-0.1)
    }

    # show text
    if(lab_text==T){
        p = p + geom_text(aes(label = round(y, 2), y = y + max(y)*0.01),
                        colour = '#B0228C', size = text.size, vjust = 0)
    }

    # theme
    if(missing(plot.theme)){
        p = p + theme_classic(base_size = base.size, base_family = font.family ) +
            theme(plot.title = element_text(colour = "black", face = "bold", hjust = 0.5),
                  legend.position = legend.position,
                  legend.title = element_blank(),
                  legend.text= element_text(color="black", size = legend.size),
                  axis.text.x = element_text(colour = 'black', angle = xlab.angle, hjust =0.5, vjust=0.5),
                  axis.text.y = element_text(colour = 'black'),
                  axis.ticks.x = element_blank())
    }else{
        p = p + plot.theme
    }


    return(p)
}












