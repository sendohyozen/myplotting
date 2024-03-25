#' Boxplot with data presented as long, do not analysis and provide mean or sd or counts. Suitable for y data type of continuous!
#'
#' @param data dataframe with long form (raw data, do not analysis!); y continuous data
#' @param x_value  x column name , data type must be discrete
#' @param y_value Y column name,  data type must be continuous for mean/sd
#' @param group  group column name, data type must be discrete (for filling sub type of the x value)
#' @param paired  if paired, give a column name for paired information
#' @param box.fill if fill the box with palette ,default T ,if false fill will be white
#' @param box.color if give the box line color with palette, default F, line will be black
#' @param jitter if add jitter point on the box
#' @param xlab_level levels of x lab
#' @param group_level levels of group (legend)
#' @param title plot title
#' @param x_lab plot x lab
#' @param y_lab  plot y lab
#' @param base.size base text size of the theme default 15
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param xlab.angle if rotate the x labs defualt 0
#' @param pal color for filling  default pal_lancet
#' @param chinese if text in Chinese ,default False
#' @param plot.theme plot.theme
#'
#' @return  ggplot plot object
#' @export
#'
#' @examples boxplot(data = ToothGrowth, x_value = 'dose', y_value = 'len', group = 'supp', jitter = T , box.fill = T, box.color = T, xlab_level = c("2", "1", "0.5"), plot.theme = ggprism::theme_prism())
#'           boxplot(data = data, x_value = 'group', y_value = 'Gene_Expression', group = 'group', title = '', y_lab = 'Gene Expression')
#'           boxplot(data = data, x_value = 'group', y_value = 'Gene_Expression', group = 'group', title = '', y_lab = 'Gene Expression', paired = 'pair')
boxplot <- function(data, x_value, y_value, group, paired,
                    box.fill = T, box.color = F, jitter = F,
                    xlab_level, group_level,
                    title = NULL, x_lab =NULL, y_lab='',
                    base.size=15, legend.position = 'top', legend.size =12, xlab.angle=0,
                    pal= ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9),
                    chinese = F,
                    plot.theme){

    cat('plotting data as long fromation with y colunm numeric vector, if paired , a paird column needed!')

    # if  text have Chinese
    if(chinese==T){
        font.family = "STHeitiSC-Medium"
    }else{
        font.family = "Arial"
    }


    # plotting data
    if(!missing(paired)){
        plot_df = data.frame(x = data[[x_value]],
                             y = data[[y_value]],
                             group = data[[group]],
                             pair = data[[paired]])
    }else{
        plot_df = data.frame(x = data[[x_value]],
                             y = data[[y_value]],
                             group = data[[group]])
    }

    # lab legend order- seting factors
    if(!missing(xlab_level)){
        plot_df$x = factor(plot_df$x, levels = xlab_level)
    }
    if(!missing(group_level)){
        plot_df$group = factor(plot_df$group, levels = group_level)
    }


    # plotting
    if (box.fill==T & box.color==T){
        p = ggplot(plot_df, aes(x, y)) +
            geom_boxplot(aes(fill=group, colour = group)) +
            scale_fill_manual(values = pal) +
            scale_color_manual(values = pal)

    }else if(box.fill==T & box.color==F){
        p = ggplot(plot_df, aes(x, y)) +
            geom_boxplot(aes(fill=group)) +
            scale_fill_manual(values = pal)

    }else if(box.fill==F & box.color==T){
        p = ggplot(plot_df, aes(x, y)) +
            geom_boxplot(aes(colour = group)) +
            scale_color_manual(values = pal)

    }else{
        stop("error, can not have both fill and color in the box with False !!")
    }


    # add jitpoint
    if(jitter==T){
        p = p + geom_jitter(aes(x = x, y = y, color = group),
                            alpha=0.5, size=0.6,
                            position = position_jitterdodge()) + # position for jit points
            scale_color_manual(values = pal)
    }

    # add paired line
    if(!missing(paired)){
        p = p + geom_line(aes(group = pair), color = 'gray', lwd = 0.2, alpha = 0.6)
    }


    # labs
    p = p + ggtitle(title) +  xlab(x_lab) +  ylab(y_lab)

    # theme
    if(missing(plot.theme)){
        p = p + theme_classic(base_size = base.size, base_family = font.family ) +
            theme(plot.title = element_text(colour = "black", face = "bold", hjust = 0.5),
                  legend.position = legend.position,
                  legend.title = element_blank(),
                  legend.text= element_text(color="black", size = legend.size),
                  axis.text.x = element_text(colour = 'black', angle = xlab.angle, hjust =0.5, vjust=0.5),
                  axis.ticks.x = element_blank())
    }else{
        p = p + plot.theme
    }



    return(p)
}
