
# parse caret confusion matrix into nice data.table

make_confusion_dt <- function(confusion_matrix,
                              type = c("prediction", "detection"),
                              col_pos ="#77BC1F",
                              col_neg = "#007DA4",
                              col_acc = "#920A7A"
                              ){

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



}
