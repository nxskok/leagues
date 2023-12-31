# edit names, first one is oddschecker, second is ptf (eg Borussia Dortmund)

edit_names <- function(the_sheet) {
  the_sheet %>% 
    mutate(game = str_replace_all(game, "Newport County", "Newport")) %>% 
    mutate(game = str_replace_all(game, "Nottingham Forest", "Nottm Forest")) %>%
    mutate(game = str_replace_all(game, "Sheffield Utd", "Sheff Utd")) %>%
    mutate(game = str_replace_all(game, "Sheffield Wednesday", "Sheff Wed")) %>%
    mutate(game = str_replace_all(game, "Porto", "FC Porto")) %>%
    mutate(game = str_replace_all(game, "Borussia Dortmund", "B Dortmund")) %>%
    mutate(game = str_replace_all(game, "Atletico Madrid", "Atl Madrid")) %>%
    mutate(game = str_replace_all(game, "Borussia Monchengladbach", "B Mgladbach")) %>%
    mutate(game = str_replace_all(game, "Antwerp", "Royal Antwerp")) %>%
    mutate(game = str_replace_all(game, "Braga", "Sporting Braga")) %>%
    mutate(game = str_replace_all(game, "Dynamo Kiev", "Dynamo Kyiv")) %>%
    mutate(game = str_replace_all(game, "Club Brugge", "Club Bruges")) %>%
    mutate(game = str_replace_all(game, "Maccabi Tel Aviv", "Maccabi Tel-Aviv")) %>%
    mutate(game = str_replace_all(game, "PSV", "PSV Eindhoven")) %>%
    mutate(game = str_replace_all(game, "FC Salzburg", "RB Salzburg")) %>%
    mutate(game = str_replace_all(game, "Wolfsberger AC", "RZ Pellets WAC")) %>%
    mutate(game = str_replace_all(game, "Bayer Leverkusen", "B Leverkusen")) %>%
    mutate(game = str_replace_all(game, "Czech Republic", "Czech Rep")) %>%
    mutate(game = str_replace_all(game, "Malmo FF", "Malmö FF")) %>%
    mutate(game = str_replace_all(game, "Atletico Madrid", "Atl Madrid")) %>%
    mutate(game = str_replace_all(game, "Bayer Leverkusen", "B Leverkusen")) %>%
    mutate(game = str_replace_all(game, "Ferencvarosi TC", "Ferencvárosi TC")) %>%
    mutate(game = str_replace_all(game, "Midtjylland", "FC Midtjylland")) %>%
    mutate(game = str_replace_all(game, "Ludogorets Razgrad", "Ludogorets")) %>%
    mutate(game = str_replace_all(game, "Genk", "KRC Genk")) %>%
    mutate(game = str_replace_all(game, "Brondby", "Brøndby IF")) %>%
    mutate(game = str_replace_all(game, "Eintracht Frankfurt", "Frankfurt")) %>%
    mutate(game = str_replace_all(game, "Fenerbahce", "Fenerbahçe")) %>%
    mutate(game = str_replace_all(game, "Atletico Madrid", "Atl Madrid")) %>%
    mutate(game = str_replace_all(game, "ZZZZ", "zzzz")) %>%
    mutate(game = str_replace_all(game, "ZZZZ", "zzzz")) %>%
    mutate(game = str_replace_all(game, "ZZZZ", "zzzz")) %>%
    mutate(game = str_replace_all(game, "ZZZZ", "zzzz")) %>%
    mutate(game = str_replace_all(game, "ZZZZ", "zzzz")) %>%
    mutate(game = str_replace_all(game, "ZZZZ", "zzzz")) %>%
    mutate(game = str_replace_all(game, "ZZZZ", "zzzz")) %>%
    mutate(game = str_replace_all(game, "ZZZZ", "zzzz")) %>%
    select(everything())
}


# get directly from site

make_prob_df = function(url) {
  read_html(url) -> html
  html %>% html_nodes("td") %>% 
    html_attr("data-best-dig") -> odds
  if (length(odds)==0) stop("no odds!")
  html %>% html_nodes("td") %>% 
    html_attr("title") -> title
  tibble(title, odds) %>% drop_na() %>% 
    extract(title, into = "team", regex = "Add (.*) to betslip") %>% 
    mutate(what = rep(c("H", "D", "A"), length.out = nrow(.))) %>% 
    mutate(row = gl(nrow(.)/3, 3)) %>% 
    mutate(prob = 1/as.numeric(odds)) %>%
    group_by(row) %>%
    mutate(prob = prob / sum(prob)) %>%
    ungroup() %>%
    select(-odds) %>%
    pivot_wider(names_from = what,
                values_from = c(team, prob)) %>%
    mutate(game = str_c(team_H, " v ", team_A)) %>%
    select(game, prob_H, prob_D, prob_A)
}


odds2prob <- function(oddstr) {
  # oddstr could be a vector
  tibble(odds = oddstr) %>% 
    separate(odds, into = c("num", "den"), sep = "/") %>% 
    mutate(across(everything(), ~as.numeric(.))) %>% 
    mutate(p = den / (num + den)) %>% 
    pull(p)
}
poiss_probs <- function(l1, l2)  {
  crossing(s1 = 0:10, s2 = 0:10) %>% 
    mutate(p1 = dpois(s1, l1), p2 = dpois(s2, l2)) %>% 
    mutate(p = p1*p2) %>% 
    mutate(res = case_when(
      s1>s2 ~ "H",
      s1<s2 ~ "A",
      TRUE  ~ "D"
    )) %>% 
    group_by(res) %>% 
    summarize(prob = sum(p)) %>% 
    arrange(desc(res))
}
from_target <- function(lambda, target) {
  poiss_probs(lambda[1], lambda[2]) %>% 
    mutate(target = target) %>% 
    summarize(ssq = sum((prob-target)^2)) %>% 
    pull(ssq)
}

opt_lambda <- function(probs, start = c(1, 1)) {
  ans <- optim(start, from_target, target = probs)
  ans$par
}

prob2lambda <- function(probs) {
  # mult <- 0.97
  mult <- 1
  # print("in prob2lambda")
  # View(probs)
  probs %>% 
    mutate(prob_D = prob_D * mult,
           prob_notD = 1 - prob_D,
           prob_H = prob_H / prob_notD,
           prob_A = prob_A / prob_notD
           ) %>% 
    rowwise() %>% 
    mutate(vec = list(c(prob_H, prob_D, prob_A))) %>% 
    mutate(lambda = list(opt_lambda(vec))) %>% 
    unnest_wider(lambda, names_sep = "_") %>% 
    select(game, lambda_1, lambda_2)
}


read_league <- function(sheet_url) {
  make_prob_df(sheet_url) %>%
    prob2lambda()
}

last_stats <- function(short_name) {
  tibble(files = list.files(pattern = "*.txt")) %>% 
    rowwise() %>% mutate(info = file.mtime(files)) %>% 
    ungroup() %>% 
    filter(files != "stats.txt") %>% 
    arrange(desc(info)) %>% 
    slice(1) %>% pull(files) -> last_stats_name
  stats <- read_csv(last_stats_name)
  stats %>% filter(league == short_name) -> stats1
  stats1
}

make_join <- function(sheet, stats) {
  sheet <- edit_names(sheet)
  sheet %>% anti_join(stats) -> mismatches
  # n_mismatch <- nrow(mismatches)
  # if (n_mismatch > 0) {
    # return(list(sheet=sheet, stats=stats, mismatches=mismatches, message="there were mismatches"))
  # }
  stats %>% 
    left_join(sheet) %>% 
    drop_na(lambda_1) 
}

make_all <- function(sheet_url, short_name) {
  sheet <- read_league(sheet_url)
  stats <- last_stats(short_name)
  ans <- make_join(sheet, stats)
  ans
}

# probability stuff

prob_dist <- function(l1, l2) {
  # print(glue::glue("means are {l1} and {l2}"))
  crossing(s1=0:6, s2=0:6) %>% 
    mutate(p = dpois(s1, l1) * dpois(s2, l2))
}

result <- function(s1, s2) {
  case_when(
    s1>s2  ~ "2",
    s1==s2 ~ "1",
    s1<s2  ~ "0",
    TRUE   ~ "error"
  )
}

# this is the one that needs editing for bonuses
# ppd_points is probably the madel

points <- function(p1, p2, s1, s2, res_string, score_string) {
  s_string <- str_c(s1, "-", s2)
  p_result <- result(p1, p2)
  s_result <- result(s1, s2)
  result_correct <- (p_result == s_result)
  score_correct <- (p1 == s1) & (p2 == s2)
  result_bonus <- str_detect(res_string, s_result)
  result_bonus <- result_bonus & result_correct
  score_bonus <- !str_detect(score_string, s_string)
  score_bonus <- score_bonus & score_correct
  result_correct + 2*score_correct +
    2*result_bonus + 2*score_bonus
}


ept <- function(pred1, pred2, dist, res_string, score_string) {
  dist %>% mutate(p1 = pred1, p2 = pred2) %>% 
    mutate(pt = points(s1, s2, p1, p2, res_string, score_string)) %>% 
    summarise(ept = sum(pt*p)) %>% 
    pull(ept)
}


by_expected <- function(dist, res_string, score_string) {
  crossing(p1 = 0:6, p2 = 0:6) %>% 
    rowwise() %>% 
    mutate(expected = ept(p1, p2, dist, res_string, score_string)) %>% 
    arrange(desc(expected))
}

by_expected_lambda <- function(l1, l2, res_string, score_string) {
  dist <- prob_dist(l1, l2)
  by_expected(dist, res_string, score_string)
}

best_expected <- function(l1, l2, res_string, score_string, omit_draw = FALSE) {
  d <- by_expected_lambda(l1, l2, res_string, score_string)
  if (omit_draw) {
    d %>% filter(p1 != p2) -> d
  }
  c(d$p1[1],  d$p2[1])
}

make_preds <- function(sheet_url, short_name, omit_draw = FALSE) {
  print(sheet_url)
  all <- make_all(sheet_url, short_name)
  if (class(all) == "list") {
    warning("mismatches")
    return(all)
  }
  all %>% 
    rowwise() %>% 
    mutate(pred = list(best_expected(lambda_1, lambda_2, 
                                     res, scores, 
                                     omit_draw))) -> ddd
  ddd %>% select(game, pred, lambda_1, lambda_2) %>% 
    unnest_wider(pred, names_sep = "") 
}

