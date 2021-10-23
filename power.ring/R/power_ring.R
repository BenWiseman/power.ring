colors <- c(
    green_light      = "#77BC1F",
    green_dark       = "#568E30",
    teal_light       = "#00ADBB",
    teal_dark        = "#008B96",
    blue_light       = "#007DA4",
    blue_dark        = "#005971",
    navy_light       = "#002C5C",
    navy_dark        = "#00173B",
    purple_light     = "#920A7A",
    purple_dark      = "#750060",
    red_light        = "#D40ALC",
    red_dark         = "#971310",
    orange_light     = "#FF8300",
    orange_dark      = "#CA6B18",
    yellow_light     = "#FFCD00",
    yellow_dark      = "#DDB307",
    grey_light       = "#DAD8D6",
    grey_dark        = "#919191",
    black_light      = "#000000",
    main_green_dark  = "#06332A",
    main_green_light = "#00634F",
    main_lime_light  = "#C3D500")


draw_confusion_rings <- function(confusion_matrix,
                                 title   = NULL,
                                 type    = c("prediction", "detection"),
                                 acc_bar = c("relative", "raw", "both"),
                                 label_size = 5,
                                 title_size = 19,
                                 col_pos = unname(colors["green_light"]),
                                 col_neg = unname(colors["blue_light"]),
                                 col_acc = unname(colors["purple_light"])){
    # Get CMX table/counts to long/tidy format
    cmdt <- data.table(confusion_matrix$table)

    # This will only work for binary stuff (i.e. no fancy multi cat)
    cmdt$result <- ifelse(cmdt$Prediction==1,
                          "positive",
                          "negative")

    cmdt$result <- ifelse(cmdt$Prediction == cmdt$Reference,
                          paste("True", cmdt$result),
                          paste("False", cmdt$result))

    # Do you want to show detection rates or prediction accuracy?
    if(type[1] == "prediction"){
        cmdt$class  <- factor(x = ifelse(cmdt$Prediction==1, "Predicted 1", "Predicted 0"),
                              levels = c( "Predicted 1", "Predicted 0"))

        col_values = c("True positive"  = col_pos,
                       "False positive" = paste0(col_pos, "33"),
                       "True negative"  = col_neg,
                       "False negative" = paste0(col_neg, "33"))

    } else{
        cmdt$class  <- factor(x = ifelse(cmdt$Reference==1, "Actually 1", "Actually 0"),
                              levels = c( "Actually 1", "Actually 0"))

        # gotta allign hit and miss classes
        col_values = c("True positive"  = col_pos,
                       "False positive" = paste0(col_neg, "33"),
                       "True negative"  = col_neg,
                       "False negative" = paste0(col_pos, "33"))

    }

    # i.e. pos/neg pred value or sens/spec depending on class
    cmdt[, prop := N/sum(N), by = class]

    # label
    cmdt[, perc := paste0(round(prop*100), "%")]

    # for segments/rects
    cmdt[, p_end := cumsum(prop), by = class]
    cmdt[, p_start := c(0, head(p_end , -1)), by = class]

    # now to ggplot/ggforce arc
    lev_1 <- as.character(levels(cmdt$class)[1])
    lev_2 <- as.character(levels(cmdt$class)[2])


    arc_scale <- function(a, b) {
        # coerce to semicircle
        arc_lims <- pi*c(-0.5, 0.5)
        norms    <- scales::rescale(c(a, b), from = 0:1, to = arc_lims)
        return(list(norms[1], norms[2]))
    }

    cmdt[, c("start", "end") := arc_scale(a = p_start, b = p_end), by = .(rownames(cmdt))]

    res_levels <- c("True positive",
                    "False positive",
                    "True negative",
                    "False negative")
    cmdt$result <- factor(cmdt$result, levels = res_levels)

    # sub headings - change focus on sens/spec or pred power
    if(type[1] == "prediction"){
        np_num <- round(confusion_matrix$byClass["Neg Pred Value"],2)*100
        np_lab <- paste0(np_num, "% Correct")

        pp_num <- round(confusion_matrix$byClass["Pos Pred Value"],2)*100
        pp_lab <- paste0(pp_num, "% Correct")
    } else{
        np_num <- round(confusion_matrix$byClass["Specificity"],2)*100
        np_lab <- paste0(np_num, "% Specificity")

        pp_num <- round(confusion_matrix$byClass["Sensitivity"],2)*100
        pp_lab <- paste0(pp_num, "%  Sensitivity")
    }

    # For ring center
    f1_num <- round(confusion_matrix$byClass["F1"],2)*100
    ba_num <- round(confusion_matrix$byClass["Balanced Accuracy"], 2)*100
    f1_lab <- paste0("F1 Accuracy: ", f1_num, "%")
    ba_lab <- paste0("Balanced Accuracy: ", ba_num, "%")

    # Side numbers
    # Overall accurracy :90%
    # Null Information Rate: 50%
    # P(Acc > NIR): 0.02
    # Cohen's Kappa: 0.80

    ka_num <- round(confusion_matrix$overall["Kappa"], 2)
    ka_lab <- paste0("Kappa: ", ka_num)

    ac_num <- round(confusion_matrix$overall["Accuracy"], 2)*100
    ac_lab <- paste0("Overall Accuracy: ", ac_num, "%")

    ni_num <- round(confusion_matrix$overall["AccuracyNull"], 2)*100
    ni_lab <- paste0("Null Information Rate: ", ni_num, "%")
    # beware ni_num resets below if doing absolute accuracy!


    p_val  <- confusion_matrix$overall["AccuracyPValue"]
    if(p_val < 0.0001){
        p_lab <- 'p(Acc > NIR) < 0.0001'
    } else  if(p_val < 0.001){
        p_lab <- "p(Acc > NIR) < 0.001"
    } else  if(p_val < 0.01){
        p_lab <- "p(Acc > NIR) < 0.001"
    } else {
        p_lab <- paste0("p(Acc > NIR) = ", round(p_val, 2))
    }


    # Calc accuracy meter
    # 0 = nir = 88
    # 2 = 100
    # x = acc = 95
    acc_xmin  <- -0.05
    acc_xmax  <- 2.05
    acc_just  <- 0.5
    acc_title <- "Overall Accuracy vs Random Guess"
    if(acc_bar[1] == "relative"){
        acc_range <- scales::rescale(confusion_matrix$overall["Accuracy"],
                                     to =c(acc_xmin, acc_xmax),
                                     from=c(confusion_matrix$overall["AccuracyNull"], 1))
    } else {
        acc_range <- scales::rescale(confusion_matrix$overall["Accuracy"],
                                     to =c(acc_xmin, acc_xmax),
                                     from=c(0, 1))
        # if "both" handler is to be added (stack with transparency)
        rng_range <- scales::rescale(confusion_matrix$overall["AccuracyNull"],
                                     to =c(acc_xmin, acc_xmax),
                                     from=c(0, 1))
        ni_num    <- 0
    }
    if(acc_bar[1] == "raw"){
        acc_title <- paste0("Raw Accuracy")
    }
    # correct hjustification
    if(acc_range/acc_xmax > 0.95) acc_just = 1
    if(acc_range/acc_xmax < 0.05) acc_just = 0

    annotate <- ggplot2::annotate

    gg <- ggplot()+
        ggforce::geom_arc_bar(data =  cmdt[class == lev_1],
                              aes(x0 = .95, y0 = 1.0, r0 = 0.7,r  = 1.0,
                                  start  = (-.5*pi)-start,
                                  end    = (-.5*pi)-end,
                                  fill   = result,
                                  colour = result)) +
        ggforce::geom_arc_bar(data =  cmdt[class == lev_2],
                              aes(x0 = 1.05, y0 = 1.0, r0 = 0.7,r  = 1.0,
                                  start  = (.5*pi)-start,
                                  end    = (.5*pi)-end,
                                  fill   = result,
                                  colour = result)) +
        coord_fixed(expand = .5, xlim = c(-.25, 2.25), ylim = c(-.5, 2.1)) +
        # Make nicer colors
        scale_fill_manual(breaks =  res_levels, values   = col_values, guide = FALSE) +
        scale_colour_manual(breaks =  res_levels, values = col_values, guide = FALSE) +
        # Annotations
        # accuracy bar
        annotate("text", family = "Proxima Nova Rg",
                 x =1, y = -0.14,
                 label    = acc_title,
                 fontface = 1,
                 hjust    = 0.5,
                 size     = label_size*1.00) +
        annotate("rect",
                 xmin  = acc_xmin, xmax=acc_xmax,  ymax=-0.20,  ymin=-0.45,
                 fill  = paste0(col_acc, "33"),
                 color = paste0(col_acc, "10"))

    # If both overlay with transparency
    if(acc_bar[1] == "both"){
        gg <- gg +
            annotate("rect",
                     xmin = acc_xmin, xmax = acc_range, ymax = -0.20, ymin = -0.45,
                     fill = paste0(col_acc, "aa")) +
            annotate("rect",
                     xmin = acc_xmin, xmax = rng_range, ymax = -0.20, ymin = -0.45,
                     fill = col_acc)
    } else{
        gg <- gg +
            annotate("rect",
                     xmin = acc_xmin, xmax = acc_range, ymax = -0.20, ymin = -0.45,
                     fill = col_acc)
    }

    gg <- gg+
        annotate("text", family = "Proxima Nova Rg",
                 x=acc_xmin,  y=-.5, label=paste0(ni_num, "%"), hjust=1, size = label_size*.8, ) +
        annotate("text", family = "Proxima Nova Rg",
                 x=acc_range, y=-.5, label=paste0(ac_num, "%"), hjust=acc_just, size = label_size*.8) +
        annotate("text",family = "Proxima Nova Rg",
                 x=acc_xmax,  y=-.5, label="100%", hjust=0, size = label_size*.8) +
        # metrics - accuracy
        # annotate("text", x = 2.1, y = -0.225, label=ni_lab, fontface=1, hjust=0, size = 3.5) +
        # annotate("text", x = 2.1, y = -0.325, label=ac_lab, fontface=1, hjust=0, size = 3.5) +
        # annotate("text", x = 2.1, y = -0.425, label=p_lab, fontface=1,  hjust=0, size = 3.5) +

        # title
        ggtitle(title) +
        #annotate("text", x=1, y=2.5, label=title) +
        # sub-headings
        annotate("text",family = "Proxima Nova Rg",
                 x = 0, y = 2.15, label=lev_1,  fontface=2, size = label_size*1.2) +
        annotate("text", family = "Proxima Nova Rg",
                 x = 0, y = 2.00, label=pp_lab, fontface=1, size = label_size) +
        annotate("text", family = "Proxima Nova Rg",
                 x = 2, y = 2.15, label=lev_2,  fontface=2, size = label_size*1.2, hjust=0.5) +
        annotate("text", family = "Proxima Nova Rg",
                 x = 2, y = 2.00, label=np_lab, fontface=1, size = label_size, hjust=0.5) +
        # Center
        annotate("text",family = "Proxima Nova Rg",
                 x = 1, y = 1.25, label=f1_lab, fontface=1, size = label_size, hjust=0.5) +
        annotate("text", family = "Proxima Nova Rg",
                 x = 1, y = 1.00, label=ba_lab, fontface=1, size = label_size, hjust=0.5) +
        annotate("text",family = "Proxima Nova Rg",
                 x = 1, y = 0.75, label=ka_lab, fontface=1, size = label_size, hjust=0.5) +
        # Remove bollocks
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              plot.background = element_rect(fill="transparent"),
              panel.background = element_rect(fill="transparent"),
              panel.grid = element_blank(),
              plot.title = element_text(hjust = 0.5, face="bold", size = title_size, family = "Proxima Nova Rg"))

    return(gg)
}
