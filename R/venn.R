#' VennPlot for sets no more than 4
#'
#' @param set_list a list of vectors with names for each set!
#' @param pal color for each set default ggsci::pal_lancet
#' @param set_name_size text size for set names, defautl 6
#' @param text_size text size for counts of each insects, defautl 6
#' @param show_percentage if show the percent of each insects , defautl F
#'
#' @return a ggplot object of venn plot
#' @export
#'
#' @examples venn_plot(set_list = list,  pal = c("#0099CC", "#FF6666"), set_name_size = 6, text_size = 6, show_percentage = T, title = 'test', title_size = 30)

venn_plot <- function(set_list,
                      pal = ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9),
                      set_name_size = 6, text_size=6,
                      show_percentage = F,
                      title, title_size = 20){

    # names for the different sets
    if(is.null(names(set_list))){
        stop('set list must have name for each element!! \n Such as:    list(a = c(xxx,xxx), b=c(xxx,xxx))!')
    }


    # color for different sets
    pal = pal[1:length(set_list)]

    p = ggvenn::ggvenn(data = set_list,
                       columns = names(set_list),
                       fill_color = pal,
                       set_name_color = pal,
                       set_name_size = set_name_size,
                       text_size = text_size,
                       show_percentage = show_percentage)

    if(!missing(title)){
        p = p + ggtitle(title) + theme(plot.title = element_text(colour = "black", size = title_size, hjust = 0.5))
    }

    return(p)
}
