use strict;
use warnings;
use 5.010;

use LWP::Simple;
use HTML::TreeBuilder;
use WWW::Mechanize;

use open qw/:std :utf8/;

main(@ARGV);

sub main
  {
    my ($which)=@_;
    
    # lookup table (subdomain name)

    my %leagues=( 
		 'se' => 'allsvenskan',
		 'fr' => 'ligue1',
		 'de' => 'bundesliga',
		 'es' => 'laliga',
		 'en2' => 'championship',
		 'eu' => 'euros',
		 'gr' => 'superleague',
		 'sc' => 'scottish-premier',
		 'en' => 'premierleague',
		 'it' => 'seriea',
		 'enf' => 'facup',
		 'euc' => 'championsleague',
		 'eue' => 'europaleague',
		 'wwc' => 'womensworldcup',
		 'ca' => 'copaamerica',
		 'u21' => 'euro-u21',
		 'wc' => "worldcup"
		);

    if ($which eq 'help')
      {
	say "$_: $leagues{$_}" for keys %leagues;
	exit;
      }

    my $l=$leagues{$which};

    # say "\n$l\n";

    die "no match for $which" if not defined $leagues{$which};

    my $site="http://$l.predictthefootball.com/?lang=en_us";

    my $username = 'ken@swiss-soccer.net';
    my $password = '300dart';

    my $mech = WWW::Mechanize->new();
    $mech -> get($site);
    $mech -> form_name('profile-login-form');
    $mech -> field ('LoginForm[email]' => $username);
    $mech -> field ('LoginForm[password]' => $password);
    $mech->click();
    if ($mech->success())
      {
	my $content=$mech->{content};
	my $root=HTML::TreeBuilder->new_from_content($content);
	for my $a ($root->find('a'))
	  {
	    my $c=$a->attr("class");
	    next unless defined $c;
	    next unless $c eq "statsBtn";
	    my $h=$a->attr("href");
	    $mech->follow_link(url => $h);
	    my $r=$mech->response();
	    my $content=$r->decoded_content;
	    my $stuff=HTML::TreeBuilder->new_from_content($content);
	    my @h;
	    for my $div ($stuff->find('div'))
	      {
		my $c=$div->attr('class');
		next unless defined $c;
		next unless $c eq 'modal-header';
		# print $div->as_text(), "\n";
		@h=get_how_many($div->as_text());
	      }
	    my @m;
	    my $rp;
	    for my $tab ($stuff->find('table'))
	      {
		my $c=$tab->attr('class');
		next unless defined $c;
		# say "class: $c";
		if ($c eq 'prediction-stats') {
		  @m=print_line($tab->as_HTML());
		}
		else { # table of result predictions
		  # say $tab->as_HTML();
		  #		say $tab->as_text();
		  $rp=get_res_pred($tab->as_HTML());
		}
	      }
	    my $s="";
	    for my $i (0..2) {
	      if ($h[1]>0 and $m[$i]+100/$h[1]<20) {
		$s.=sprintf "%1d",2-$i;
	      }
	    }
	    $s='X' if $s eq '';
#	    printf "%-45s %6d %s %s\n",@h,$s,$rp;
	    printf "%-45s,%-2s,%-3s,%6d,%s\n",$h[0],$s,$which,$h[1],$rp;
	    $mech->back();
	  }
      }
    else
      {
	say "failed";
      }
  }


sub get_res_pred {
  my ($html)=@_;
  my $root=HTML::TreeBuilder->new_from_content($html);
  my @scores; # that are greater than 5%
  for my $tr ($root->find_by_tag_name('tr')) {
    my @r;
    for my $td ($tr->find_by_tag_name('td')) {
      my $text=$td->as_text();
      push @r, $text;
    }
    # row finished
    # say "@r";
    my $sc="";
    # turn second one into percentage number
    my $pct;
    if ($r[1]=~/\[([\d.]+)\%\]/g) {
      $pct=$1;
    }
    if ($pct>=4) {
      $sc=$r[0];
      $sc=~s/\s//g;
      push @scores, $sc;
    }
    # say "score $sc pct: $pct";
  }
  # say "@scores";
  my $scc=join(":",@scores);
  # say $scc;
  return $scc;
}

sub get_how_many {
  my ($txt)=@_;
  if ($txt=~/^.\s(.*)\sTotal predictions:\s(\d+)/) {
    return ($1,$2);
  }
}

sub print_line {
  my ($html)=@_;
  my @m=( $html=~/\[([\d.]+)\%\]/g ) ;
  return @m;
}
