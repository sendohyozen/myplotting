

#' Barplot with data presented identity (as rawdata give mean , sd), subtype shown as bar dodge
#'
#' @param data data frame anlysised previously with mean, sd
#' @param x_value  column name of x value in the dataframe
#' @param y_value column name of y value in the dataframe, such as mean
#' @param fill_group column name of filling for subtype
#' @param errbar column name of error bar
#' @param lab_text adding text of y value on the bar, default False
#' @param xlab_level levels of x lab
#' @param fill_level levels of fill lab
#' @param title plot title
#' @param x_lab plot x lab
#' @param y_lab plot y lab
#' @param bar_width bar width default 0.6
#' @param pal color for filling  default pal_lancet
#' @param plot.theme theme for plot deault classic
#' @param base.size base text size of the theme default 18
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param xlab.angle  if rotate the x labs defualt 0
#' @param chinese if text in Chinese ,default False
#' @param labtext.size text size the the labs add on the bar
#' @param na.rm if remove NA values, default F
#' @param alpha.bar alpha of the bar, default 1
#'
#' @return ggplot2 bar plot
#' @export
#'
#' @examples barplot_identity(data = dat, x_value = 'paper_BRCA_Subtype_PAM50', y_value = 'tmb_mean', fill_group = 'paper_BRCA_Subtype_PAM50', errbar = 'tmb_sd')
barplot_identity <- function(data, x_value, y_value, fill_group, na.rm=F,
                             errbar, lab_text=F, labtext.size = 3,
                             xlab_level, fill_level,
                             title = NULL, x_lab =NULL, y_lab='',
                             bar_width = 0.6,
                             base.size=18, legend.position = 'top', legend.size =12, xlab.angle=0,
                             pal= ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9), alpha.bar = 1,
                             chinese = F,
                             plot.theme) {

    cat('plotting raw data as provided, dataframe with x，y，errorbar columns')

    # if  text have Chinese
    if(chinese==T){
        font.family = "STHeitiSC-Medium"
    }else{
        font.family = "Arial"
    }

    # test the data, must give error value and y value,  as stat = identity
    if(missing(errbar) & missing(y_value)){
        stop("must give a column for y and errbar, if none using the function of barplotting_conitnuous!")
    }else if(!missing(errbar) & lab_text){
        stop("can't have both error bar and y value text!")
    }

    # plotting data
    if(missing(fill_group)){
        #  fill group miss, default give one same value, for only one color
        if(missing(errbar)){
            plot_df = data.frame(x = data[[x_value]],
                                 y = data[[y_value]],
                                 fill = '1')
        }else{
            plot_df = data.frame(x = data[[x_value]],
                                 y = data[[y_value]],
                                 fill = '1',
                                 sd = data[[errbar]])
        }

    }else{
        #  fill group given
        if(missing(errbar)){
            plot_df = data.frame(x = data[[x_value]],
                                 y = data[[y_value]],
                                 fill = data[[fill_group]])
        }else{
            plot_df = data.frame(x = data[[x_value]],
                                 y = data[[y_value]],
                                 fill = data[[fill_group]],
                                 sd = data[[errbar]])
        }
    }


    # 去除NA
    if(na.rm==T){
        plot_df = na.omit(plot_df)
    }


    # lab legend order- seting factors
    if(!missing(xlab_level)){
        plot_df$x = factor(plot_df$x, levels = xlab_level)
    }
    if(!missing(fill_level)){
        plot_df$fill = factor(plot_df$fill, levels = fill_level)
    }


    # arrange text/errorbar consistently with bars
    pd <- position_dodge(bar_width)  # necessary for errorbar and text adding


    # plotting
    p = ggplot(data = plot_df, aes(x=x, y=y, group = fill)) +
        geom_col(aes(fill=fill),
                 alpha = alpha.bar,
                 colour = 'black',
                 position="dodge",
                 width = bar_width)  # geom_col ,can't be geom_bar



    # title and labs
    p = p + scale_fill_manual(values = pal) +
        ggtitle(title) +  xlab(x_lab) +  ylab(y_lab)


    # error bar
    if(!missing(errbar)){
        sd_bar = plot_df$sd
        p = p + geom_errorbar(data = plot_df, mapping = aes(ymin = y, ymax = y + sd_bar),
                              width=0.2, position=pd , size=0.4) }  #  position=position_dodge() must be consistent with bar width;  size for line width

    #  adding text for y value
    if(lab_text){
        p = p + geom_text(data = plot_df, mapping = aes(label = round(y, 2), y = y + max(y)*0.01),
                          colour = '#B0228C', position=pd, size = labtext.size, vjust = 0)  # position=position_dodge() must be consistent with bar width
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







#' Barplot with data presented as long, do not analysis and provide mean or sd or counts. Suitable for xtab counting results presenting!
#'
#' @param data Dataframe with long form (raw data, do not analysis!); all discrete data
#' @param x x column name be counted , data type must be discrete
#' @param count y column name to be counted , data type must be discrete
#' @param bar_location bar position could be dodge(default), fill, stack
#' @param lab_text adding text of y value on the bar, default True
#' @param xlab_level levels of x lab
#' @param bar_width bar width default 0.9
#' @param title plot title
#' @param x_lab plot x lab title
#' @param y_lab plot y lab title
#' @param pal color for filling  default pal_lancet
#' @param plot.theme theme for plot deault classic
#' @param base.size base text size of the theme default 18
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param xlab.angle if rotate the x labs defualt 0
#' @param chinese if text in Chinese ,default False
#' @param na.rm if remove NA values, default F
#' @param alpha.bar alpha of the bar, default 1
#'
#' @return ggplot plot
#' @export
#'
#' @examples barplot_discrete(data = sur_data, x = 'tmb', count= 'paper_BRCA_Subtype_PAM50', xlab_level = c('low', 'high'), lab_text = T)
barplot_discrete <- function(data, x, count, na.rm=F,
                             bar_location = 'dodge',
                             lab_text=T,
                             xlab_level,
                             bar_width = 0.9,
                             title = NULL, x_lab = NULL, y_lab='',
                             base.size=18, legend.position = 'top', legend.size =12, xlab.angle=0,
                             pal = ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9), alpha.bar = 1,
                             chinese = F,
                             plot.theme){

    cat('Dataframe with long form (raw data, do not analysis!)， x-tab counts can not be plotting!  \n Y column not needed ! \n Value must be discrete for counting! ')

    # if  text have Chinese
    if(chinese==T){
        font.family = "STHeitiSC-Medium"
    }else{
        font.family = "Arial"
    }

    # plotting data
    plot_df = data.frame(x = data[[x]],
                         y = data[[count]])

    # 去除NA
    if(na.rm==T){
        plot_df = na.omit(plot_df)
    }

    # 指定x轴因子水平的顺序
    if(!missing(xlab_level)){
        plot_df$x = factor(plot_df$x, levels = xlab_level)
    }

    # bar_width must be consistant with position_dodge
    # plotting
    p = ggplot(plot_df, aes(x = x, fill=y)) +
        geom_bar(stat="count",
                 alpha = alpha.bar,
                 position = bar_location,
                 width = bar_width)


    # add y value text
    if(lab_text){

        if(bar_location=='stack'){
            p = p + geom_text(stat='count', aes(label=..count..), position=position_stack(vjust=0.5))

        }else if(bar_location=='fill' ){
            p = p + geom_text(stat='count',aes(label=scales::percent(..count../sum(..count..))),
                              position=position_fill(0.5))

        }else{
            p =  p + geom_text(stat='count', aes(label=..count..), position=position_dodge(bar_width), vjust=-0.5)   # notice position_dodge
        }
    }

    # color
    p = p + scale_fill_manual(values = pal)


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
                  axis.text.y = element_text(colour = 'black'),
                  axis.ticks.x = element_blank())
    }else{
        p = p + plot.theme
    }


    return(p)
}





#' Barplot (dodge) with data presented as long, do not analysis and provide mean or sd or counts. Suitable for y data type of continuous!
#'
#' @param data Dataframe with long form (raw data, do not analysis!); y continuous data
#' @param x x column name be calulated , data type must be discrete
#' @param sub.x sub x column name be calulated , data type must be discrete (if two varibles need to be caculated for mean/sd)
#' @param value  Y column name,  data type must be continuous for mean/sd
#' @param errbar if  adding error bar, default True
#' @param lab_text if  adding text of y value, default False
#' @param xlab_level levels of x lab
#' @param fill_level levels of fill (legend)
#' @param bar_width bar width default 0.9
#' @param title plot title
#' @param x_lab plot x lab
#' @param y_lab plot y lab
#' @param pal color for filling  default pal_lancet
#' @param plot.theme theme for plot deault classic
#' @param base.size base text size of the theme default 18
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param xlab.angle if rotate the x labs defualt 0
#' @param chinese if text in Chinese ,default False
#' @param labtext.size text size the the labs add on the bar
#' @param na.rm if remove NA values, default F
#' @param alpha.bar alpha of the bar, default 1
#'
#' @return ggplot plot
#' @export
#'
#' @examples  barplot_continuous(data = sur_data, x = 'paper_BRCA_Subtype_PAM50', value = 'total_perMB', lab_text = T, errbar = F)
barplot_continuous <- function(data, x, sub.x, value, na.rm=F,
                               errbar = T, lab_text=F, labtext.size=3,
                               xlab_level, fill_level,
                               bar_width = 0.9,
                               title = NULL, x_lab =NULL, y_lab='',
                               base.size=18, legend.position = 'top', legend.size =12, xlab.angle=0,
                               pal = ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9), alpha.bar = 1,
                               chinese = F,
                               plot.theme ){

    # no Y colunm provided , data summerized in the function
    cat('Dataframe with long form (raw data, do not analysis!)， do not give mean/sd!  \n Y column not needed ! \n Value must be continuous! ')


    # if  text have Chinese
    if(chinese==T){
        font.family = "STHeitiSC-Medium"
    }else{
        font.family = "Arial"
    }


    # plotting data (if two factors given, summerized with two factors!)
    if(!missing(sub.x)){
        plot_df = data.frame(var1 = data[[x]],
                             var2 = data[[`sub.x`]],
                             number = data[[value]])
        # 去除NA
        if(na.rm==T){
            plot_df = na.omit(plot_df)
        }

        # x levels
        if(!missing(xlab_level)){
            plot_df$var1 = factor(plot_df$var1, levels = xlab_level)
        }
        # fill levels (sub.x)--- factor2/var2
        if(!missing(fill_level)){
            plot_df$var2 = factor(plot_df$var2, levels = fill_level)
        }

        # data analysis
        dat = plot_df %>%
            group_by(var1, var2) %>%
            summarise(mean = mean(number, na.rm = T),
                      sd = sd(number, na.rm = T))

        # plotting
        p = ggplot(data = dat, aes(x=var1, y=mean, group = var2)) +
            geom_col(aes(fill=var2),
                     colour = 'black',
                     alpha = alpha.bar,
                     position="dodge", width = bar_width)

    }else{
        # only 1 factors given
        plot_df = data.frame(var1 = data[[x]],
                             number = data[[value]])

        # x levels
        if(!missing(xlab_level)){
            plot_df$var1 = factor(plot_df$var1, levels = xlab_level)
        }

        # data analysis
        dat = plot_df %>%
            group_by(var1) %>%
            summarise(mean = mean(number, na.rm = T),
                      sd = sd(number, na.rm = T))

        # ploting
        p = ggplot(data = dat, aes(x=var1, y=mean, group = var1)) +
            geom_col(aes(fill=var1),
                     colour = 'black',
                     alpha = alpha.bar,
                     position="dodge", width = bar_width)

    }



    # position_dodge consistant with bar width
    pd <- position_dodge(bar_width) # error bar and text need


    #  if  show error bar
    if(errbar){
        p = p + geom_errorbar(data = dat, mapping = aes(ymin = mean, ymax = mean + sd),
                              width=0.2, position=pd , linewidth=0.4)
    }

    # if show y value text
    if(lab_text){
        p = p + geom_text(data = dat, mapping = aes(label = round(mean, 2), y = mean + max(mean)*0.01),
                          colour = '#B0228C', position=pd, size = labtext.size, vjust = 0)
    }

    # color for filling
    p = p + scale_fill_manual(values = pal)


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
                  axis.text.y = element_text(colour = 'black'),
                  axis.ticks.x = element_blank())
    }else{
        p = p + plot.theme
    }


    return(p)
}





#' Barplot (fill position showing percent in the group) with data presented as long, do not analysis and provide mean or sd or counts. Suitable for xtab counting results presenting!
#'
#' @param data Dataframe with long form (raw data, do not analysis!); all discrete data
#' @param x x column name be counted , data type must be discrete
#' @param count y column name to be counted , data type must be discrete
#' @param na.rm if remove NA values, default F
#' @param lab_text if show labs of percent in the group , default T
#' @param lab.text.size the text size of the labs, default 4
#' @param lab.text.color the text colors of the labs, default black
#' @param xlab_level levels of x lab
#' @param bar_width bar width default 0.9
#' @param title plot title
#' @param x_lab plot x lab title
#' @param y_lab plot y lab title
#' @param base.size base text size of the theme default 18
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param xlab.angle if rotate the x labs defualt 0
#' @param pal color for filling  default pal_lancet
#' @param chinese if text in Chinese ,default False
#' @param plot.theme theme for plot deault classic
#' @param alpha.bar alpha of the bar, default 1
#'
#' @return ggplot plot
#' @export
#'
#' @examples barplot_percent_fill(data = sur_data, x = 'paper_BRCA_Subtype_PAM50', count = 'tmb', na.rm = T)
barplot_percent_fill <- function(data, x, count, na.rm=F,
                                 lab_text=T, lab.text.size = 4, lab.text.color = 'black',
                                 xlab_level,
                                 bar_width = 0.9,
                                 title = NULL, x_lab = NULL, y_lab='',
                                 base.size=18, legend.position = 'top', legend.size =12, xlab.angle=0,
                                 pal = ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9), alpha.bar = 1,
                                 chinese = F,
                                 plot.theme){

    cat('Dataframe with long form (raw data, do not analysis!)， x-tab counts can not be plotting!  \n Y column not needed ! \n Value must be discrete for counting! ')

    # if  text have Chinese
    if(chinese==T){
        font.family = "STHeitiSC-Medium"
    }else{
        font.family = "Arial"
    }

    # plotting data (x,y 都是离散变量，y用count表示希望按x分组统计的因子，产生类似列联表的结果)
    plot_df = data.frame(x = data[[x]],
                         y = data[[count]])

    # 去除NA
    if(na.rm==T){
        plot_df = na.omit(plot_df)
    }


    # 重新分组计算组内百分率，用于显示lab
    # 一文简单解决ggplot2图形的标签添加问题 https://www.51xxziyuan.com/87/5534.html
    res <- plot_df %>% # 指定数据集
        group_by(x, y) %>% # 指定分组变量 -x，y 都是分组因子
        summarise(freq = n()) %>%  # 计算频数
        ungroup() %>% # 解除分组
        group_by(x) %>% # 重新指定分组变量（x轴显示的因子）
        mutate(freq_pct = freq/sum(freq)) # 创建新变量，计算频数百分比

    # print(res)



    # 设置x轴样本水平，指定排列顺序（x，y都是离散变量）
    if(!missing(xlab_level)){
        plot_df$x = factor(plot_df$x, levels = xlab_level)
    }

    # bar_width must be consistant with position_dodge
    # plotting
    p = ggplot(plot_df, aes(x = x, fill=y)) +
        geom_bar( alpha = alpha.bar,
            position = "fill",
            width = bar_width)


    # add y value text
    if(lab_text){
        p = p + geom_text(data = res,
                          aes(label=scales::percent(freq_pct), y = freq_pct),
                          position=position_fill(vjust = 0.5),
                          size = lab.text.size, color = lab.text.color)

    }

    # color
    p = p + scale_fill_manual(values = pal)


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
                  axis.text.y = element_text(colour = 'black'),
                  axis.ticks.x = element_blank())
    }else{
        p = p + plot.theme
    }


    return(p)
}




#' Barplot (dodge, adding points and prism theme) with data presented as long, do not analysis and provide mean or sd or counts. Suitable for y data type of continuous!
#'
#' @param data Dataframe with long form (raw data, do not analysis!); y continuous data
#' @param x x column name be counted , data type must be discrete
#' @param value  Y column name,  data type must be continuous for mean/sd
#' @param fill filling column, data type must be discrete, for legend filling column.
#' @param xlab_level levels of x lab
#' @param fill_level levels of fill (legend)
#' @param jitter if adding jitter points, default T
#' @param fill.col if filling the bars with colors, default T
#' @param fill.alpha the alpha of fill colors, default 1
#' @param title plot title, default NULL
#' @param x_lab plot x lab, default NULL
#' @param y_lab plot y lab, default ''
#' @param point.size the size of the jitter points, default 3
#' @param title.text.size text size of the plot title, default 20
#' @param xlab.text.size text size of the x lab, default 18
#' @param ylab.text.size text size of the y lab, default 18
#' @param xtitle.text.size text size of the x title, default 18
#' @param ytitle.text.size text size of the y title, , default 18
#' @param xlab.angle if rotate the x labs, default 0
#' @param legend.position legend position default top
#' @param legend.text.size text size of the legend , default 12
#' @param pal color for filling  default pal_lancet
#' @param plot.theme theme for plot, deault prism
#'
#' @return ggplot plot
#' @export
#'
#' @examples barplot_jitter_prism(data = tmp, x = 'Group', value = 'Value', fill = 'Cell', fill.col = F, jitter = F)
barplot_jitter_prism <- function(data, x, value, fill,
                                 xlab_level, fill_level,
                                 jitter = T, fill.col=T, fill.alpha = 1,
                                 title = NULL, x_lab = NULL, y_lab='',
                                 point.size = 3,
                                 title.text.size =20, xlab.text.size=18, ylab.text.size=18, xtitle.text.size =18, ytitle.text.size =18, xlab.angle =0,
                                 legend.position = 'top', legend.text.size =12,
                                 pal = ggsci::pal_lancet(palette = c("lanonc"))(9),
                                 plot.theme){

    cat('Dataframe with long form (raw data, do not analysis!)， do not give mean/sd!  \n Y column not needed ! \n Value must be continuous!  ')

    # data
    if(missing(fill)){
        plot_df = data.frame(x = data[[x]],
                             y = data[[value]])

    }else{
        plot_df = data.frame(x = data[[x]],
                             y = data[[value]],
                             fill = data[[fill]])
    }


    # x levels
    if(!missing(xlab_level)){
        plot_df$x = factor(plot_df$x, levels = xlab_level)
    }
    # fill levles
    if(!missing(fill_level)){
        plot_df$fill = factor(plot_df$fill, levels = fill_level)
    }



    # plotting
    if(missing(fill)){
        p = ggplot(plot_df, aes(x = x, y = y, col = x, fill = x))
    }else{
        p = ggplot(plot_df, aes(x = x, y = y, col = fill, fill = fill))
    }


    # 柱子 镂空/填色
    if(fill.col==F){
        # 添加柱子--镂空
        p = p + stat_summary(fun = mean, geom = 'col', fill =NA,
                             position = position_dodge2(padding = 0.1),
                             width = 0.9, size =1)

        # 添加散点
        if(jitter == T){

            if(missing(fill)){
                p = p +  geom_point(aes(color = x, fill = x),
                                    show.legend = F,
                                    position = position_jitterdodge(seed = 123,
                                                                    jitter.width = 0.3,
                                                                    dodge.width = 0.9),
                                    shape = 21, size =  point.size)
            }else{
                p = p +  geom_point(aes(color = fill, fill = fill),
                                    show.legend = F,
                                    position = position_jitterdodge(seed = 123,
                                                                    jitter.width = 0.3,
                                                                    dodge.width = 0.9),
                                    shape = 21,size =  point.size)
            }

        }




    }else{

        # 添加柱子--填充
        p = p + stat_summary(fun = mean, geom = 'col',
                             position = position_dodge2(padding = 0),
                             width = 0.9)

        # 添加散点
        if(jitter == T){

            if(missing(fill)){
                p = p +  geom_point(aes(color = x, fill = x),
                                    show.legend = F,
                                    position = position_jitterdodge(seed = 123,
                                                                    jitter.width = 0.3,
                                                                    dodge.width = 0.9),
                                    shape = 21, size =  point.size)
            }else{
                p = p +  geom_point(aes(color = fill, fill = fill),
                                    show.legend = F,
                                    position = position_jitterdodge(seed = 123,
                                                                    jitter.width = 0.3,
                                                                    dodge.width = 0.9),
                                    shape = 21,size =  point.size)
            }

        }

    }

    # 修改颜色
    p = p +  scale_fill_manual(values = alpha(pal, fill.alpha) ) +  # 设置填充颜色的透明度 https://blog.csdn.net/chang349276/article/details/77476848
        scale_colour_manual(values = pal)

    # 添加误差线
    p = p +  stat_summary(fun = mean,geom = 'errorbar',
                          position = position_dodge(width = 0.9),
                          fun.max = function(x) mean(x) + sd(x),
                          fun.min = function(x) mean(x) - sd(x),
                          width = 0.3,size = 0.8, show.legend = F)

    # scale_y_continuous(expand = c(0,0),limits = c(0,30),breaks = seq(0,30,5)) +


    # title
    p = p + labs(title = title, x = x_lab, y = y_lab, fill = NULL, color = NULL)

    # theme
    if(missing(plot.theme)){

        mytheme = theme(axis.line = element_line(colour = 'black'),
                        plot.title = element_text(hjust = 0.5,size = title.text.size),
                        plot.margin = margin(0.5,0.5,0.5,0.5,unit = "cm"),
                        axis.ticks = element_line(size = 1),
                        #axis.ticks.length = unit(-3,"mm"),#负数向内，正数向外
                        axis.text.x = element_text(size = xlab.text.size, angle = xlab.angle, hjust =0.5, vjust=0.5),
                        axis.text.y = element_text(hjust = 0.5,size = ylab.text.size),
                        axis.title.x = element_text(size = xtitle.text.size),
                        axis.title.y = element_text(size = ytitle.text.size),
                        legend.position = legend.position,
                        legend.title = element_blank(),
                        legend.text= element_text(color="black", size = legend.text.size)
        )

        p = p + ggprism::theme_prism() + mytheme

    }else{
        p = p + plot.theme
    }


    return(p)
}



