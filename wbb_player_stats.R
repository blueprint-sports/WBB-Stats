install.packages("wehoop")

tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp()
})
tictoc::toc()


wehoop::espn_wbb_game_all()

colnames(wbb_pbp)

unique(wbb_pbp$type_text)
