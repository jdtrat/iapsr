flex <- flextable(head(IAPSgroups)) %>%
set_header_labels(Group = "Group", Probabilities = "Probabilities") %>%
theme_vanilla() %>%
align(align = "center", part = "all") %>%
bg(bg = "cyan3", part = "header") %>%
bg(bg = "snow3", part = "body") %>%
color(color = "black", part = "all") %>%
bold(bold = TRUE, part = "header") %>%
fontsize(size = 14, part = "all") %>%
set_caption("Table 1: Icon Groupings") %>%
autofit()
flex


flex1 <- flextable(head(IAPSphases)) %>%
  set_header_labels(Phase = "Phase", Trials = "Trials", Associated.Groups = "Associated Groups", Description = "Description") %>%
  theme_vanilla() %>%
  align(align = "center", part = "all") %>%
  bg(bg = "coral2", part = "header") %>%
  bg(bg = "snow3", part = "body") %>%
  color(color = "black", part = "all") %>%
  bold(bold = TRUE, part = "header") %>%
  fontsize(size = 14, part = "all") %>%
  set_caption("Table 2: Description of Phases in IAPS Choice Game") %>%
  autofit()
flex1
