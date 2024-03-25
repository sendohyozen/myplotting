

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
#' @param base.size base text size of the theme default 15
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param xlab.angle  if rotate the x labs defualt 0
#' @param chinese if text in Chinese ,default False
#' @param labtext.size text size the the labs add on the bar
#'
#' @return ggplot2 bar plot
#' @export
#'
#' @examples barplot_identity(data = dat, x_value = 'paper_BRCA_Subtype_PAM50', y_value = 'tmb_mean', fill_group = 'paper_BRCA_Subtype_PAM50', errbar = 'tmb_sd')
barplot_identity <- function(data, x_value, y_value, fill_group,
                             errbar, lab_text=F, labtext.size = 3,
                             xlab_level, fill_level,
                             title = NULL, x_lab =NULL, y_lab='',
                             bar_width = 0.6,
                             base.size=15, legend.position = 'top', legend.size =12, xlab.angle=0,
                             pal= ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9),
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
        geom_col(aes(fill=fill), colour = 'black', position="dodge", width = bar_width)  # geom_col ,can't be geom_bar

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
#' @param x_lab plot x lab
#' @param y_lab plot y lab
#' @param pal color for filling  default pal_lancet
#' @param plot.theme theme for plot deault classic
#' @param base.size base text size of the theme default 15
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param xlab.angle if rotate the x labs defualt 0
#' @param chinese if text in Chinese ,default False
#'
#' @return ggplot plot
#' @export
#'
#' @examples barplot_discrete(data = sur_data, x = 'tmb', count= 'paper_BRCA_Subtype_PAM50', xlab_level = c('low', 'high'), lab_text = T)
barplot_discrete <- function(data, x, count,
                             bar_location = 'dodge',
                             lab_text=T,
                             xlab_level,
                             bar_width = 0.9,
                             title = NULL, x_lab = NULL, y_lab='',
                             base.size=15, legend.position = 'top', legend.size =12, xlab.angle=0,
                             pal = ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9),
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

    if(!missing(xlab_level)){
        plot_df$x = factor(plot_df$x, levels = xlab_level)
    }

    # bar_width must be consistant with position_dodge
    # plotting
    p = ggplot(plot_df, aes(x = x, fill=y)) +
        geom_bar(stat="count", position = bar_location, width = bar_width)

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
#' @param base.size base text size of the theme default 15
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param xlab.angle if rotate the x labs defualt 0
#' @param chinese if text in Chinese ,default False
#' @param labtext.size text size the the labs add on the bar
#'
#' @return ggplot plot
#' @export
#'
#' @examples  barplot_continuous(data = sur_data, x = 'paper_BRCA_Subtype_PAM50', value = 'total_perMB', lab_text = T, errbar = F)
barplot_continuous <- function(data, x, sub.x, value,
                               errbar = T, lab_text=F, labtext.size=3,
                               xlab_level, fill_level,
                               bar_width = 0.9,
                               title = NULL, x_lab =NULL, y_lab='',
                               base.size=15, legend.position = 'top', legend.size =12, xlab.angle=0,
                               pal = ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9),
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
            geom_col(aes(fill=var2), colour = 'black', position="dodge", width = bar_width)

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
            geom_col(aes(fill=var1), colour = 'black', position="dodge", width = bar_width)
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
                  axis.ticks.x = element_blank())
    }else{
        p = p + plot.theme
    }


    return(p)
}







