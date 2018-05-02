data(test_df, package = "cxrTargetDiff")

prevs <- test_df %>%
    select(view, one_of(candiOpt(dxs_chr))) %>%
    gather(key = "indication", value = "is_dx", -view) %>%
    filter(is_dx) %>%
    group_by(view, indication) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>% as.df

prevs %>% complete(view, indication) %>% arrange(desc(n)) %>% as.df

test_df$img_id
test_imgs <- test_df$img_id
devtools::use_data(test_imgs)
