#' A special bar plot with ordered bars based on y value, and lab show on the bars
#'   the position of labs based on y value
#'
#' @param data  a dataframe with  x lab names (also shown as text); y value (numeric); group for filling colors(not neccisdary, if not provided, bars with only one color)
#' @param x_value column name of x value in the dataframe
#' @param y_value column name of y value in the dataframe, such as mean
#' @param fill_group column name of filling for subtype (if not provided, using the first color in the pallette)
#' @param fill_level levels of fill lab
#' @param descend  if using desceding order of y
#' @param bar.width bar width default 0.6
#' @param lab.size lab text size default 3
#' @param pal color for filling  default pal_lancet
#' @param title plot title default NULL
#' @param x_lab plot x lab default NULL
#' @param y_lab plot x lab default ''
#' @param base.size base text size of the theme default 15
#' @param legend.position  legend position default top
#' @param legend.size legend text size default 12
#' @param lab.position the start position of labs (y value), default 0.1
#' @param chinese if text in Chinese ,default False
#' @param cord.filp if filp the plot,default F
#'
#' @return  a ggplot barplot with order y values ; labs outside the bar(2 sides based on y value, like gsea plot)
#' @export
#'
#' @examples paretoplot(data = plot_data, x_value = 'cell_line', y_value = 'gene_effect', fill_group = 'pick_color', fill_level = c('TRUE', 'FALSE'), descend = T, legend.size = 8)
#'           paretoplot1(data = plot_data, x_value = 'cell_line', y_value = 'gene_effect',  descend = F)
#'
paretoplot_lab <- function(data, x_value, y_value, fill_group,
                           fill_level, descend = F,
                           bar.width = 0.6, lab.size=3, lab.position=0.1,
                           pal = ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9),
                           title = NULL, x_lab =NULL, y_lab='',
                           base.size=15, legend.position = 'top', legend.size =12,
                           chinese = F,
                           cord.filp =F){

    if(chinese==T){
        font.family = "STHeitiSC-Medium"
    }else{
        font.family = "Arial"
    }

    # only one color, no filling group needed, no legend needed
    if(missing(fill_group)){
        cat('No filling group be give, will plot with one color, as the first one of the pallette!')
        fill_group = 'fill_group'
        data$fill_group = 'any value'
        legend.position = 'none'
    }

    # plot data
    plot_df = data.frame(x = data[[x_value]],
                         y = data[[y_value]],
                         fill = data[[fill_group]])
    if(descend == T){
        plot_df = plot_df %>% arrange(desc(y)) %>% as.data.frame()
    }else{
        plot_df = plot_df %>% arrange(y) %>% as.data.frame()
    }

    plot_df$x = factor(plot_df$x, levels = plot_df$x)

    # order for filling
    if(!missing(fill_level)){
        plot_df$fill = factor(plot_df$fill, levels = fill_level)
    }

    # number of factors for filling
    pal = pal[1:length(unique(plot_df$fill))]

    # plotting
    p = ggplot(plot_df, mapping = aes(x=x, y=y)) +
        geom_col(aes(fill= fill), colour = 'black', position="dodge",width = bar.width) +
        scale_fill_manual(values = pal) +
        ggtitle(title) +  xlab(x_lab) +  ylab(y_lab)

    # if flip the plot
    if(cord.filp==T){
        # theme
        p = p + coord_flip() +
            theme_classic(base_size = base.size, base_family = font.family ) +
            theme(plot.title = element_text(colour = "black", face = "bold", hjust = 0.5),
                  legend.position = legend.position,
                  legend.title = element_blank(),
                  legend.text= element_text(color="black", size = legend.size),
                  axis.line.y =  element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())
        # adding text
        p <- p + geom_text(plot_df, mapping =aes(label = x,
                                                 x = x,
                                                 y = ifelse( y < 0,  lab.position, -lab.position), #  text start position
                                                 colour = fill,
                                                 hjust = ifelse(y < 0, "outward",  "inward")),
                           angle = 0,
                           size = lab.size, show.legend = F) +
            scale_color_manual(values = pal)  # color for text


    }else{

        # theme
        p = p + theme_classic(base_size = base.size, base_family = font.family ) +
            theme(plot.title = element_text(colour = "black", face = "bold", hjust = 0.5),
                  legend.position = legend.position,
                  legend.title = element_blank(),
                  legend.text= element_text(color="black", size = legend.size),
                  axis.line.x =  element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())

        # adding text the position based on y value # ref:https://www.saoniuhuo.com/question/detail-2670244.html  # ref:http://www.taodudu.cc/news/show-158164.html?action=onClick
        p <- p + geom_text(plot_df, mapping =aes(label = x,
                                                 x = x,
                                                 y = ifelse( y < 0,  lab.position, -lab.position), #  text start position
                                                 colour = fill,
                                                 hjust = ifelse(y < 0, "outward",  "inward")),
                           angle = 90,
                           size = lab.size, show.legend = F) +
            scale_color_manual(values = pal)  # color for text
    }

    return(p)
}







#' A special bar plot with ordered bars based on y value, and lab show on the x axis
#'   color (< 4) highlight the group on the axis by a filling group with factor data
#'
#' @param data a dataframe with  x lab names (also shown as text); y value (numeric); group for filling colors(not neccisdary, if not provided, bars with only one color)
#' @param x_value column name of x value in the dataframe
#' @param y_value column name of y value in the dataframe, such as mean
#' @param fill_group column name of filling for subtype (if not provided, using the first color in the pallette)
#' @param fill_level  levels of fill lab
#' @param descend if using desceding order of y
#' @param bar.width bar width default 0.6
#' @param x.lab.size  text size on the x axis default 10
#' @param pal color for filling  default pal_lancet
#' @param title plot title default NULL
#' @param x_lab plot x lab default NULL
#' @param y_lab plot y lab default ''
#' @param base.size base text size of the theme default 15
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param chinese if text in Chinese ,default False
#' @param cord.filp if filp the plot,default F
#'
#' @return a ggplot barplot with order y values ; labs outside the axis with filling color (less than 4)
#' @export
#'
#' @examples paretoplot_axis(data = plot_data, x_value = 'cell_line', y_value = 'Expression', fill_group = 'pick_color')
#'           paretoplot_axis(data = plot_data, x_value = 'cell_line', y_value = 'Expression')
#'
paretoplot_axis <- function(data, x_value, y_value, fill_group,
                            fill_level, descend = F,
                            bar.width = 0.6, x.lab.size = 10,
                            pal = ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9),
                            title = NULL, x_lab =NULL, y_lab='',
                            base.size=15, legend.position = 'top', legend.size =12,
                            chinese = F,
                            cord.filp =F){

    if(chinese==T){
        font.family = "STHeitiSC-Medium"
    }else{
        font.family = "Arial"
    }

    # only one color, no filling group needed, no legend needed
    if(missing(fill_group)){
        cat('No filling group be give, will plot with one color, as the first one of the pallette!')
        fill_group = 'fill_group'
        data$fill_group = 'any value'
        legend.position = 'none'
    }

    if(length(unique(data[[fill_group]])) > 4){
        stop('Can not filling on the axis with more than 4 colors!!! the levels of filling better be given')
    }


    # plot data
    plot_df = data.frame(x = data[[x_value]],
                         y = data[[y_value]],
                         fill = data[[fill_group]])
    if(descend == T){
        plot_df = plot_df %>% arrange(desc(y)) %>% as.data.frame()
    }else{
        plot_df = plot_df %>% arrange(y) %>% as.data.frame()
    }

    plot_df$x = factor(plot_df$x, levels = plot_df$x)

    # order for filling
    if(!missing(fill_level)){
        plot_df$fill = factor(plot_df$fill, levels = fill_level)
    }

    # number of factors for filling
    pal = pal[1:length(unique(plot_df$fill))]

    # plotting
    p = ggplot(plot_df, mapping = aes(x=x, y=y)) +
        geom_col(aes(fill= fill), colour = 'black', position="dodge",width = bar.width) +
        scale_fill_manual(values = pal) +
        scale_x_discrete(aes(labels = x)) +     # for x axis filling
        ggtitle(title) +  xlab(x_lab) +  ylab(y_lab)

    # color for x.axis (max 4 color for axis coloring!)
    color.value = unique(plot_df$fill)
    color.value = factor(color.value, levels = fill_level)
    plot_df$color_axis = ifelse(plot_df$fill== color.value[1], pal[1],
                                ifelse(plot_df$fill==color.value[2], pal[2],
                                       ifelse(plot_df$fill==color.value[3], pal[3], pal[4])))
    cat(unique(plot_df$color_axis), '\n')  # show x lab color vector


    # if flip the plot
    if(cord.filp==T){
        p = p + coord_flip() +
            theme_classic(base_size = base.size, base_family = font.family ) +
            theme(plot.title = element_text(colour = "black", face = "bold", hjust = 0.5),
                  legend.position = legend.position,
                  legend.title = element_blank(),
                  legend.text= element_text(color="black", size = legend.size),
                  axis.text.y = element_text(colour =  plot_df$color_axis,   # color for x lab
                                             angle = 0, size = x.lab.size,
                                             hjust = 1, vjust = 0.5),
                  axis.ticks.y = element_blank())
    }else{
        # add color on tha x axis text
        p = p + theme_classic(base_size = base.size, base_family = font.family ) +
            theme(plot.title = element_text(colour = "black", face = "bold", hjust = 0.5),
                  legend.position = legend.position,
                  legend.title = element_blank(),
                  legend.text= element_text(color="black", size = legend.size),
                  axis.text.x = element_text(colour =  plot_df$color_axis,   # color for x lab
                                             angle = 90, size = x.lab.size,
                                             hjust = 1, vjust = 0.5),
                  axis.ticks.x = element_blank())
    }

    return(p)
}






#' A special bar plot with ordered bars based on y value, and lab show beside the bar top (like a neighborhood)
#' can give a lab column you want, not have to be the x lab value
#'
#' @param data a dataframe with  x value; y value (numeric); group for filling colors(not neccisdary, if not provided, bars with only one color); lab column, not neccissary
#' @param x_value column name of x value in the dataframe
#' @param y_value column name of y value in the dataframe, such as mean
#' @param fill column name of filling for subtype (if not provided, using the first color in the pallette)
#' @param add.labs if add labs colunm on the bars
#' @param labs column name of labs in the dataframe, if not provided, using x value instead
#' @param show.xlab if show x lab text; if labs is the same, do not have to show x labs.
#' @param fill_level levels of fill lab
#' @param lab.size lab text size default 3
#' @param descend if using desceding order of y
#' @param y.limit y.limit need a vector with 2 numbers as the ymin and ymax: e.g y.limit = c(-30, 40)
#' @param bar_width bar width default 0.6
#' @param pal color for filling  default pal_lancet
#' @param title plot title default NULL
#' @param x_lab plot x lab default NULL
#' @param y_lab plot y lab default ''
#' @param base.size base text size of the theme default 15
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param chinese if text in Chinese ,default False
#' @param cord.filp if filp the plot,default F
#'
#' @return a ggplot barplot with order y values; labs beside the bar (like adding text on the bar)
#' @export
#'
#' @examples paretoplot_neighbor(data = plot_data, x_value = 'cell_line', y_value = 'Expression', fill = 'pick_color')
paretoplot_neighbor <- function(data, x_value, y_value, fill, add.labs=T, labs, show.xlab=T,
                                fill_level,lab.size=3, descend=F, y.limit,
                                bar_width = 0.6,
                                pal= ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9),
                                title = NULL, x_lab =NULL, y_lab='',
                                base.size=15, legend.position = 'top', legend.size =12,
                                chinese = F,
                                cord.filp =F){
    if(chinese==T){
        font.family = "STHeitiSC-Medium"
    }else{
        font.family = "Arial"
    }

    # only one color, no filling group needed, no legend needed
    if(missing(fill)){
        cat('No filling group be give, will plot with one color, as the first one of the pallette!')
        fill = 'fill'
        data$fill = 'any value'
        legend.position = 'none'
    }

    # plotting data
    if(missing(labs)){
        plot_df = data.frame(x = data[[x_value]],
                             y = data[[y_value]],
                             fill = data[[fill]],
                             lab = data[[x_value]])
    }else{
        plot_df = data.frame(x = data[[x_value]],
                             y = data[[y_value]],
                             fill = data[[fill]],
                             lab = data[[labs]])
    }
    # order the x labs
    if(descend == T){
        plot_df = plot_df %>% arrange(desc(y)) %>% as.data.frame()
    }else{
        plot_df = plot_df %>% arrange(y) %>% as.data.frame()
    }

    plot_df$x = factor(plot_df$x, levels = plot_df$x)


    # lab legend seting factors
    if(!missing(fill_level)){
        plot_df$fill = factor(plot_df$fill, levels = fill_level)
    }

    # arrange text/errorbar consistently with bars
    pd <- position_dodge(bar_width)  # necessary for errorbar and text adding

    # plotting
    p = ggplot(data = plot_df, aes(x=x, y=y, group = fill)) +
        geom_col(aes(fill=fill), colour = 'black', position="dodge", width = bar_width) +  # geom_col ,can't be geom_bar
        scale_fill_manual(values = pal)

    # setting Y axis limits
    if(!missing(y.limit)){
        cat('y.limit need a vector with 2 numbers as the ymin and ymax: e.g y.limit = c(-30, 40)')
        ymin = y.limit[1]
        ymax = y.limit[2]

        p = p +  scale_y_continuous(limits=c(ymin, ymax))  # limit of the Y value
    }

    # titles
    p = p + ggtitle(title) +  xlab(x_lab) +  ylab(y_lab)

    # if flip
    if(cord.filp==T){
        p = p + coord_flip()

        #  adding text for y value
        if(add.labs == T){
            p = p + geom_text(data = plot_df, mapping = aes(label = lab, y = y + max(y)*0.01 , colour = fill), angle =0,
                              position=pd, size = lab.size, hjust=0, vjust = 0.5, show.legend = F) +  # position=position_dodge() must be consistent with bar width
                scale_color_manual(values = pal)
        }

        # theme
        p = p + theme_classic(base_size = base.size, base_family = font.family ) +
            theme(plot.title = element_text(colour = "black", face = "bold", hjust = 0.5),
                  legend.position = legend.position,
                  legend.title = element_blank(),
                  legend.text= element_text(color="black", size = legend.size),
                  axis.text.x = element_text(colour = 'black', angle = 0, hjust = 1, vjust = 0.5),
                  axis.ticks.y = element_blank())
        # if lab is missint using x insead, x lab can be ignored
        if(show.xlab==F){
            p = p + theme( axis.text.y =element_blank())
        }

    }else{
        #  adding text for y value
        if(add.labs == T){
            p = p + geom_text(data = plot_df, mapping = aes(label = lab, y = y + max(y)*0.01 , colour = fill), angle =90,
                              position=pd, size = lab.size, hjust=0, vjust = 0.5, show.legend = F) +  # position=position_dodge() must be consistent with bar width
                scale_color_manual(values = pal)
        }

        # theme
        p = p + theme_classic(base_size = base.size, base_family = font.family ) +
            theme(plot.title = element_text(colour = "black", face = "bold", hjust = 0.5),
                  legend.position = legend.position,
                  legend.title = element_blank(),
                  legend.text= element_text(color="black", size = legend.size),
                  axis.text.x = element_text(colour = 'black', angle = 90, hjust = 1, vjust = 0.5),
                  axis.ticks.x = element_blank())
        # if lab is missint using x insead, x lab can be ignored
        if(show.xlab==F){
            p = p + theme( axis.text.x =element_blank())
        }
    }

    return(p)
}


