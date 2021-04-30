rename_activities <- function(x) {
  # rename activity labels
  x$ACTIVITY[x$ACTIVITY=="Brushing"]="Brushing teeth"
  x$ACTIVITY[x$ACTIVITY=="Catch"]="Catching a ball"
  x$ACTIVITY[x$ACTIVITY=="Eating Soup"]="Eating soup"
  x$ACTIVITY[x$ACTIVITY=="Eating Pasta"]="Eating pasta"
  x$ACTIVITY[x$ACTIVITY=="Eating Sandwich"]="Eating sandwich"
  x$ACTIVITY[x$ACTIVITY=="Eating Chips"]="Eating chips"
  x$ACTIVITY[x$ACTIVITY=="Stairs"]="Taking the stairs"
  x$ACTIVITY[x$ACTIVITY=="Folding"]="Folding clothes"
  
  return(x);
}