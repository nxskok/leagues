## functions 20180726 (using posteriors from ratings)

# get date of last posteriors

last_posterior_date=function() {
  dir="~/Documents/ratings"
  pat="*._post.rds"
  v=list.files(path=dir,pattern=pat,full.names = T)
  v_short=list.files(path=dir,pattern=pat)
  file.info(v) %>%  
    mutate(fname=v_short) %>% arrange(mtime) %>% 
    select(fname,mtime)
}

read_prior=function(league_name,year) {
  fname=str_c("~/Documents/ratings/",league_name,year,"_prior.csv")
  read_csv(fname) %>% select(id, sw_id, team=name)
}

read_posterior=function(league_name) {
  fname=str_c("~/Documents/ratings/",league_name,"_post.rds")
  readRDS(fname)
}

read_ratings=function(league_name,year) {
  teams=read_prior(league_name,year)
  post=read_posterior(league_name)
  list(teams=teams,post=post)
}

show_teams=function(x) {
  n=nrow(x)
  k=10
  col=as.numeric(gl(n,k,length=n))
  x %>% 
    select(-sw_id) %>% 
    arrange(team) %>% 
    mutate(team_long=str_c(id,team,sep="-")) %>% 
    mutate(column=col) %>%
    mutate(i=row_number()) %>% 
    mutate(row=i %% k) %>% 
    mutate(row=ifelse(row==0,k,row)) %>% 
    select(-i, -team, -id) %>% 
    spread(column,team_long) %>% 
    select(-row)
}

# compare rp and rp-current

rp_to_tibble=function(x) {
  ll=str_split(x,":") 
  names(ll)="x"
  ll %>% as_tibble()
}

compare_rp=function(x,y) {
  xx=rp_to_tibble(x)
  yy=rp_to_tibble(y)
  v1=xx %>% anti_join(yy)
  v2=yy %>% anti_join(xx)
  v1s=str_c(v1$x, collapse = " ")
  v2s=str_c(v2$x, collapse = " ")
  case_when(
    length(v1s)==0 & length(v2s)==0 ~ "",
    length(v1s)==0                  ~ str_c("add ", v2s),
    length(v2s)==0                  ~ str_c("remove ", v1s),
    TRUE                            ~ str_c("remove ",v1s," add ",v2s)
  )
}

test1="1-2:1-1:2-1:0-2"
test2="1-2:1-1:2-1:0-2:1-0:2-0"
compare_rp(test1,test2)

## copies of stuff from ascsv.R

# get the new games as most recently read in

newgames=function(lg,stats) {
  frompred2(lg,stats) %>%
    mutate(t1="", t2="") %>% 
    select(t1,t2,resnow,game,npred,rp) %>% 
    write.csv(quote=F,row.names=F) 
}

# pull out most recent games for this league (two-letter abb)

frompred2=function(lg,stats) {
  stats %>% filter(league==lg) %>% 
    select(game,resnow=res,npred=howmany,rp=scores)
}


# data frame of values from current as text
fromcur=function(txt) {
  read_csv(txt,col_names=c("t1","t2","res","names","np","rpcur"))
}

# return games with different bonuses in current and latest
diffgames=function(y,z,all.games=F) {
  if (all.games) {
    bind_cols(y,z) %>% mutate(row=1:n()) %>% 
      mutate(rpdiff="X") %>% 
      select(row,resnow,res,game,t1,t2,rp,rpcur,rpdiff)
  }
  else {
    bind_cols(y,z) %>% mutate(row=1:n()) %>%  
      mutate(rpdiff=map2_chr(rpcur,rp,~compare_rp(.x,.y))) %>% 
      filter(resnow!=res | rpdiff!="") %>% 
      select(row,resnow,res,game,t1,t2,rp,rpcur,rpdiff)
  }
}

#' return games with resnow and res different, from txt and league name
#' front end to previous
#' 
diff.games=function(txt,lg,all,stats) {
  z=fromcur(txt)
  y=frompred2(lg,stats)
  diffgames(y,z,all)
}


# posterior predictive distribution

ppd=function(post,i,j,ha=T)
{
  pars=rstan::extract(post)
  diff1=pars$o[,i]-pars$d[,j]
  diff2=pars$o[,j]-pars$d[,i]
  if (ha) {
    nu1=diff1+pars$h
    nu2=diff2
  }
  else {
    nu1=diff1+pars$h/2
    nu2=diff2+pars$h/2
  }
  l1=exp(nu1)
  l2=exp(nu2)
  #    print(c(mean(l1),mean(l2)))
  nr=nrow(pars$o)
  r1=rpois(nr,l1)
  r2=rpois(nr,l2)
  tibble(r1,r2) %>% 
    count(r1,r2) %>% 
    mutate(p=n/sum(n))
}

# turn score into result

result_of=function(s1,s2) {
  case_when(
    s1>s2 ~ "2",
    s1==s2 ~ "1",
    s1<s2 ~ "0",
    TRUE ~ "??"
  )
}

# goal difference

gd1=function(obs,pred) {
  case_when(
    obs != pred ~ as.integer(-abs(obs-pred)),
    obs == 0    ~ 1L,
    obs == 1    ~ 1L,
    TRUE        ~ 2L*as.integer(obs)
  )
}

# calculate points

ppd_points=function(pp,s1,s2,res_string,score_string) {
  pred_result=result_of(s1,s2)
  pred_score_string=str_c(s1,'-',s2)
  result_correct_bonus=str_detect(res_string,pred_result)
  score_correct_bonus=!str_detect(score_string,pred_score_string)
  pp %>% 
    mutate(result=result_of(r1,r2)) %>%
    mutate(result_correct=(result==pred_result)) %>% 
    mutate(score_chr=str_c(r1,"-",r2)) %>% 
    mutate(score_correct=(pred_score_string==score_chr)) %>% 
    mutate(result_bonus=(result_correct & result_correct_bonus)) %>% 
    mutate(score_bonus=(score_correct & score_correct_bonus)) %>% 
    mutate(bonuses=score_correct+result_bonus+score_bonus) %>%
    mutate(points=result_correct+2*bonuses) %>%
    mutate(goal_diff=gd1(s1,r1)+gd1(s2,r2)) %>%
    mutate(points=points+goal_diff/10) %>%
    mutate(pt_prob=points*p) %>% 
    summarize(ept=sum(pt_prob)) %>% 
    pull(ept)
}

# and for concorso pronostici

cp_ppd_points=function(pp,s1,s2) {
  pred_result=result_of(s1,s2)
  pred_gd=s1-s2
  pp %>% 
    mutate(result=result_of(r1,r2)) %>% 
    mutate(result_correct=(result==pred_result)) %>% 
    mutate(gd=r1-r2) %>% 
    mutate(off_by1=abs(r1-s1)+abs(r2-s2)+abs(pred_gd-gd)) %>% 
    mutate(off_by2=abs(r1-s1)+abs(r2-s2)) %>%
    mutate(pt1=20-off_by1) %>% 
    mutate(pt2=8-off_by2) %>% 
    mutate(pt=case_when(
      result_correct ~ pmax(pt1,8),
      TRUE           ~ pmax(pt2,0)
    )) %>% 
    mutate(pt_prob=pt*p) %>% 
    summarize(ept=sum(pt_prob)) %>% pluck(1)
}

# highest prob predicted score

hi_prob=function(pp) {
  pp %>% arrange(desc(p)) %>% slice(1:3) -> probs
  s1=pull(probs, r1)
  s2=pull(probs, r2)
  str_c(s1, "-", s2, collapse="; ")
}

# most likely result

hi_res=function(pp) {
  pp %>% mutate(result=result_of(r1,r2)) %>% 
    group_by(result) %>% 
    summarize(total=sum(p)) %>% 
    mutate(tot=round(total*100)) %>% 
    pull(tot) %>% rev() %>% str_c(collapse="; ")
}

# find maximum expected points

max_ep=function(pp,res_string,score_string) {
  crossing(s1=0:6,s2=0:6) %>% 
    mutate(ept=map2_dbl(s1,s2,~ppd_points(pp,.x,.y,res_string,score_string))) %>% 
    arrange(desc(ept)) ->
  exp_pt
  exp_pt %>% pull(s1) -> ss1
  exp_pt %>% pull(s2) -> ss2
  str_c(ss1[1],"-",ss2[1])
}

## for concorso pronostici

cp_max_ep=function(pp) {
  crossing(s1=0:6,s2=0:6) %>% 
    mutate(ept=map2_dbl(s1,s2,~cp_ppd_points(pp,.x,.y))) %>% 
    arrange(desc(ept)) ->
    exp_pt
  exp_pt %>% pull(s1) -> ss1
  exp_pt %>% pull(s2) -> ss2
  str_c(ss1[1],"-",ss2[1])
}


# do that for each row in a thing like output from diff.games

all_pred=function(post,d) {
  d %>% 
    mutate(pp=map2(t1,t2,~ppd(post,.x,.y,ha=T))) %>% 
    mutate(pred=pmap_chr(list(pp,resnow,rp),~max_ep(..1,..2,..3)))
}

# and for cp

cp_all_pred=function(post,d) {
  d %>% 
    mutate(pp=map2(t1,t2,~ppd(post,.x,.y,ha=T))) %>% 
    mutate(pred=map_chr(pp,~cp_max_ep(.)))    
}

# getting team numbers out and in

old_txt_to_numbers=function(txt) {
  read_csv(txt, col_names = F) %>% 
    unite(team_numbers, X1, X2) %>% 
    separate(X4, c("home", "away"), sep=" v ") %>% 
    gather(venue, team, home:away) %>% 
    separate(team_numbers, c("home_no", "away_no")) %>% 
    mutate(number = ifelse(venue=="home", home_no, away_no)) %>% 
    select(team, number) %>% 
    distinct()
}

make_new_txt=function(txt_new, team_tab) {
  read_csv(txt_new) %>% mutate(r=row_number()) %>%
    separate(game, c("home", "away"), sep=" v ") %>% 
    left_join(team_tab, by=c("home"="team")) %>% 
    left_join(team_tab, by=c("away"="team")) %>% 
    unite(game, home, away, sep=" v ") %>% 
    select(t1=number.x, t2=number.y, resnow, game, npred, rp) -> thing
  write.csv(thing, "", quote=F, row.names = F)
}