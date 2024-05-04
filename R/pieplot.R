#' pie plot using base R, data given raw counts / calculated percentage (a number < 1; e.g. 0.55)
#' suitable for more groups, labs outside of the pie
#'
#' @param data  dataframe with a column of group(as lab names), at least one colunm of values: can be  a colunm of counts or a column with percent
#' @param radius  size of pie ,default 0.6
#' @param Value.Count colunm name for counts
#' @param Value.Prob colunm name for percentage (must be number < 1)
#' @param group.lab group labs of percentage (also could be shown on legend)
#' @param add_group.lab_in_pie if add the lab with percentage on the pie (defalut T)
#' @param title title of the plot
#' @param lab.size text size on the lab
#' @param legend.postion legend position(defalut topleft), if none ,no legend added ; could be one of “bottomright”, “bottom”, “bottomleft”, “left”, “topleft”, “top”, “topright”, “right”, “center”
#' @param legend.size text size on the legend
#' @param pal color of the pie section
#' @param mar mar defautl c(1,1,1,1)
#' @param family family default Arial
#' @param Chinese if the text is Chinese, if T font faminly change to STHeitiSC-Medium, default F
#'
#' @return a base R pie plot with labs outside the pie
#' @export
#'
#' @examples pieplot_base(data = df, Value.Count = 'growth', group.lab = 'region')
#'           pieplot_base(data = df, Value.Prob = 'prob', group.lab = 'region', radius = 0.8, lab.size = 1, add_group.lab_in_pie = T)
pieplot_base <- function(data, radius=0.6, Value.Count, Value.Prob, group.lab, add_group.lab_in_pie = T,
                         title = '', lab.size = 0.8,
                         legend.postion = 'topleft', legend.size = 0.8,
                         pal = ggsci::pal_futurama(alpha = 0.8)(12),
                         mar=c(1,1,1,1),
                         family = 'Arial',
                         Chinese = F){

    num_levels = nrow(data)
    pal = pal[1:num_levels]  # must be consistant with the number of factors
    if(Chinese){family = 'STHeitiSC-Medium'}

    # plotting data
    if(!missing(Value.Count) & missing(Value.Prob)){
        # given counts
        plot_data = data.frame(x = radius,  # in pieplot x have no meaning, preprsent the size of the pie, be a number
                               fill = data[[Value.Count]]/sum(data[[Value.Count]]),
                               pie_text = if(add_group.lab_in_pie){
                                   paste(data[[group.lab]], '\n', round(data[[Value.Count]]/sum(data[[Value.Count]]), 4)*100, '%')
                               }else{
                                   paste(round(data[[Value.Count]]/sum(data[[Value.Count]]), 4)*100, '%')
                               },
                               labs = data[[group.lab]])
        # reorder
        plot_data = plot_data[order(plot_data$fill, decreasing = T), ]

    }else if(missing(Value.Count) & !missing(Value.Prob)){
        # give calculated prob

        if(sum(data[[Value.Prob]]>1) > 0){ stop('probality must be less than 1, wrong column given! ')}  # all values  <1

        plot_data = data.frame(x = radius,  # in pieplot x have no meaning, preprsent the size of the pie, be a number
                               fill = data[[Value.Prob]],
                               pie_text = if(add_group.lab_in_pie){
                                   paste(data[[group.lab]], '\n', round(data[[Value.Prob]]*100, 2), '%')
                               }else{
                                   paste(round(data[[Value.Prob]]*100, 2), '%')
                               },
                               labs = data[[group.lab]])
        # reorder
        plot_data = plot_data[order(plot_data$fill, decreasing = T), ]

    }else{
        # have both or missint  a error
        stop('can not have both raw count and probablity provided! ')
    }



    # plot settings
    org.mar <- par()$mar  # origin mar parameter
    par(family = family,
        mar=mar)   # seting mar values

    # plotting
    pie(radius = radius,
        x = plot_data$fill,
        labels = plot_data$pie_text, init.angle = 90, clockwise = T, cex = lab.size,
        col = pal,
        main = title,
        border = "white",
        lty = 1)

    # legend
    if(legend.postion != 'none'){

        legend(legend.postion,
               legend = plot_data$labs,  cex=legend.size,
               fill = pal,
               bty = 'n',  # no border line for legend
               bg = NA  # transparent background
        )
    }

    par(mar=org.mar) # get back to the origin mar setting

}





#' pie plot using ggpubr::ggpie, data given raw counts / calculated percentage (a number < 1; e.g. 0.55)
#' suitable for few groups, labs inside of the pie
#'
#' @param data dataframe with a column of group(as lab names), at least one colunm of values: can be a colunm of counts or a column with percent,
#'             labs self-defined can also be used
#' @param group colunm name of group for legends
#' @param value colunm name for counts/percentage
#' @param labs colunm name for labs shown on the pie(inside), default percentage
#' @param group.levels  define group order in the legend
#' @param pal color of the pie section
#' @param chinese  if the text is Chinese, if T font faminly change to STHeitiSC-Medium, default F
#' @param lab.size  text size on the lab
#' @param legend.position position of the legend
#' @param legend.size text size on the legend
#'
#' @return a ggplot pie plot with labs in the pie
#' @export
#'
#' @examples myplotting::ggpie_identity(data = df, group = 'group', value = 'value', labs = 'labs')
ggpie_identity <- function(data, group, value, labs, group.levels,
                           pal=ggsci::pal_futurama(alpha = 0.8)(12),
                           chinese=F, lab.size =14,
                           legend.position = 'top', legend.size =12){
    if(chinese==T){
        font.family = "STHeitiSC-Medium"
    }else{
        font.family = "Arial"
    }

    # plotting data
    if(missing(labs)){
        # if not labs on pie provided, use percentage instead
        plot_df = data.frame(value = data[[value]],
                             fill = data[[group]],
                             label = scales::percent(data[[value]]/sum(data[[value]]), accuracy = 0.01))
    }else{
        plot_df = data.frame(value = data[[value]],
                             fill = data[[group]],
                             label = data[[labs]])
    }

    # group levels
    if(!missing(group.levels)){
        plot_df$fill = factor(plot_df$fill, levels = group.levels)
    }

    # plotting
    p = ggpubr::ggpie(plot_df, x = "value", label = "label",
                      lab.pos = "in", lab.font = "black",
                      fill = "fill", color = "white",
                      palette = pal,
                      font.family = font.family)

    p = p + theme_void(base_size = lab.size, base_family = font.family) +
        theme(legend.position = legend.position,
              panel.grid=element_blank(),
              legend.title = element_blank(),
              legend.text= element_text(color="black", size=legend.size),
              # title
              plot.title = element_text(hjust = 0.5)
        )

    return(p)

}




#' pie plot using ggpie::ggpie, data given as long formation, plot show statistic result. don't count the data for preparing.
#'
#' @param data dataframe with at least one column of group (discrete data) , ggpie will give count and ratio by itself.
#' @param group_key column name of the group
#' @param label_info Label information type, chosen from count, ratio and all (count and ratio). Default: all
#' @param label_rotate Label style, chosen from circle, horizon and none (no label). Default: circle.
#' @param label_split Pattern used to split the label, support regular expression. Default: ' ', give a blank space ' ' could make the labels into two lines
#' @param label_size Size of the label. Default: 4.
#' @param label_pos Label position, chosen from in and out. Default: in
#' @param label_threshold Threshold of the ratio to determine label position (in/out pie). Default: NULL. This parameter only works when  label_pos = 'in' and label_rotate = 'horizon'
#' @param pal Colors used. Default: ggsci::pal_futurama(alpha = 0.8)(12)
#' @param label_color Color of the label. Default: black.
#' @param border_color Border color. Default: black.
#' @param border_size Border thickness. Default: 0.5.
#' @param chinese if use Chinese text in group levels
#' @param title plot title Default:NULL
#' @param title.size text size of title ,default 14
#' @param legend.position position of legend , default top
#' @param legend.size text size of legend ,default 10
#'
#' @return a ggplot pie plot
#' @export
#'
#' @examples ggpie_stat(data = diamonds, group_key = "cut", label_info = "all", label_rotate = "circle", label_size = 3, label_pos = "in")
ggpie_stat <- function(data, group_key,
                       label_info = "all", label_rotate = "circle", label_split= ' ',
                       label_size = 4, label_pos = "in", label_threshold = NULL,
                       pal = ggsci::pal_futurama(alpha = 0.8)(12),
                       label_color = 'black',border_color = 'black', border_size = 0.5,
                       chinese=F, title = NULL, title.size=14,
                       legend.position = 'top', legend.size =10){
    # ggpie 包｜解决你的所有饼图绘制 庄闪闪的R语言手册 https://mp.weixin.qq.com/s/sqbnuNHvPikq7tRmP-pZmA   还有甜甜圈图 ，玫瑰图，多层饼图

    if(chinese==T){
        font.family = "STHeitiSC-Medium"
    }else{
        font.family = "Arial"
    }

    # color
    num_levels = length(unique(data[[group_key]]))
    pal = pal[1:num_levels]  # must be consistant with the number of factors


    # plot data
    plot_data = data.frame(group = data[[group_key]])

    # plotting
    p = ggpie::ggpie(data = plot_data, group_key = "group", count_type = "full",  fill_color = pal,
                     label_info = label_info, label_split = label_split, label_color = label_color, label_type = label_rotate,
                     label_pos = label_pos, label_threshold = label_threshold, label_size = label_size,
                     border_color = border_color, border_size = border_size)
    # theme
    p = p + ggtitle(label = title) +
        theme_void(base_size = 12, base_family = font.family) +
        theme(legend.position = legend.position,
              panel.grid=element_blank(),
              legend.title = element_blank(),
              legend.text= element_text(color="black", size = legend.size),
              plot.title = element_text(hjust = 0.5, size = title.size))


    return(p)
}






