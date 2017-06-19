library(dplyr)
aa <- mediDateR::read_data("/Users/chughes/Downloads/testData 2", "rds")

bb <- mediDateR::data_roll_up(aa)

yy <- aa %>%
  dplyr::arrange(id, dtstart) %>%
  dplyr::filter(id %in% c(110149, 402467))

zz <- bb %>%
  dplyr::arrange(id, dtstart) %>%
  dplyr::filter(id %in% c(110149, 402467))

length(unique(aa$id))
length(unique(bb$id))

cc <- aa %>%
  dplyr::arrange(id, dtstart) %>%
  dplyr::filter(id == lag(id), (dtstart <= lag(dtend)))
View(head(cc))

a <- aa %>%
  mutate(span = dtend - dtstart) %>%
  filter(span == 0)
View(a)

b <- aa %>%
  filter(id %in% a$id)

View(b)
