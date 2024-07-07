#'violinplot with data presented as long, do not analysis and provide mean or sd or counts. Suitable for y data type of continuous!
#'
#' @param data dataframe with long form (raw data, do not analysis!); y continuous data
#' @param x_value x column name , data type must be discrete
#' @param y_value Y column name,  data type must be continuous for mean/sd
#' @param group group column name, data type must be discrete (for filling sub type of the x value)
#' @param add.box if adding boxplot in the violinplot, default T
#' @param box.color the color of the boxplot ,default white
#' @param box.width the width of the boxplot ,default 0.2
#' @param xlab_level levels of x lab
#' @param group_level levels of group (legend)
#' @param add.p if adding p value on the plot, default F, using ggpubr::stat_compare_means
#' @param method default t.test
#' @param title plot title
#' @param x_lab plot x lab
#' @param y_lab plot y lab
#' @param legend.title plot legend title
#' @param base.size base text size of the theme default 18
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param xlab.angle if rotate the x labs defualt 45
#' @param pal color for filling  default pal_lancet
#' @param alpha.violin alpha of the violin, default 0.8
#' @param chinese if text in Chinese ,default False
#' @param plot.theme plot.theme
#'
#' @return ggplot plot object
#' @export
#'
#' @examples violinplot(data = df, x_value = 'celltype', y_value = 'NES', group = 'group',
#'                      y_lab = 'NES', legend.title = 'TMB', add.box = F, xlab.angle = 45, pal = c("#D2ADA8", "#8CA3C3"), add.p = T)
#'
violinplot <- function(data, x_value, y_value, group,
                       add.box = T, box.color = 'white', box.width=0.2,
                       xlab_level, group_level, add.p = F, method = 't.test',
                       title = NULL, x_lab =NULL, y_lab='', legend.title='',
                       base.size=18, legend.position = 'top', legend.size =12, xlab.angle=45,
                       pal= ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9), alpha.violin =0.8,
                       chinese = F,
                       plot.theme){
    cat('plotting data as long fromation with y colunm numeric vector')

    # if  text have Chinese
    if(chinese==T){
        font.family = "STHeitiSC-Medium"
    }else{
        font.family = "Arial"
    }


    # plotting data
    plot_df = data.frame(x = data[[x_value]],
                         y = data[[y_value]],
                         group = data[[group]])


    # lab legend order- seting factors
    if(!missing(xlab_level)){
        plot_df$x = factor(plot_df$x, levels = xlab_level)
    }
    if(!missing(group_level)){
        plot_df$group = factor(plot_df$group, levels = group_level)
    }


    # plotting
    p = ggplot(plot_df, aes(x, y, fill = group)) +
        geom_violin(aes(colour = group), show.legend = T,
                    alpha = alpha.violin,
                    scale = 'width',
                    position = position_dodge(1))

    # adding box in the violin
    if(add.box == T){
        p = p + geom_boxplot(position = position_dodge(1),
                             outlier.shape = NA, # 隐藏outline点
                             show.legend = F,
                             size=0.5,  #外轮廓描边粗细
                             width = box.width,  #箱子宽度
                             color = box.color) #箱子外边缘轮廓颜色
    }

    # scaling
    p = p + scale_fill_manual(values = pal) +
        scale_color_manual(values = pal)

    # labs
    p = p + ggtitle(title) +  xlab(x_lab) +  ylab(y_lab) + ggplot2::labs(fill = legend.title) + # 增加fill的标题
        guides(colour="none") # 必须加这句删除color的legend，否则两个color和fill 的legend都会显示 https://blog.csdn.net/LeaningR/article/details/114576555


    # theme
    if(missing(plot.theme)){
        p = p + theme_bw(base_size = base.size,  base_family = font.family) +
            theme(plot.title = element_text(colour = "black", face = "bold", hjust = 0.5),
                  legend.position = legend.position,
                  legend.title = element_text(color="black", size = legend.size),
                  legend.text= element_text(color="black", size = legend.size),
                  axis.text.x = element_text(colour = 'black', angle = xlab.angle, hjust =1, vjust=1), # 注意角度存在是hjust和vjust的数值
                  axis.text.y = element_text(colour = 'black'))
    }else{
        p = p + plot.theme
    }

    # adding p values
    if(add.p == T){
        p = p + ggpubr::stat_compare_means(aes(group=group),
                                           method = method,
                                           label = "p.signif", show.legend  = F)
    }


    return(p)
}
