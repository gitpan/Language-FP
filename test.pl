# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::Simple tests => 12;
use Language::FP qw/perl2fp fp_eval/;
use strict;
ok(1); # If we made it this far, we're ok.

my ($str, $res);

sub fp($) {
    perl2fp(fp_eval shift);
}

ok(fp '/+:<1 2 3>' eq '<6>');
ok(fp '/*:<1 2 3>' eq '<6>');
ok(fp 'apndl:<4 <1 2 3>>' eq '<4 1 2 3>');
ok(fp 'apndr:<<1 2 3> 4>' eq '<1 2 3 4>');
ok(fp 'apndr:<<> 4>' eq '<4>');
ok(fp 'distl:<4 <1 2 3>>' eq '<<4 1> <4 2> <4 3>>');
ok(fp 'distl:<<4> <1 2 3>>' eq '<<<4> 1> <<4> 2> <<4> 3>>');
ok(fp 'distr:<<4> <1 2 3>>' eq '<<4 <1 2 3>>>');
ok(fp <<'END' eq '<12>');
  /+ . @((== . [1, `1] -> `1 ; `0) .
 	(while (> . [2, `0]) (< -> reverse ; id) . [2, -]))
 	. distl . [id, iota]:42
END
# 10 #################################################################
# Calling Perl from FP:

sub explode { split //, shift }
sub compact { (join '', @_) }

ok(fp 'trans . @explode:<"abc" "def">' eq '<<"a" "d"> <"b" "e"> <"c" "f">>');
ok(fp 'compact . @compact . distr:<<"abc" "def"> "!">' eq '<"abc!def!">');

# +2 ################################################################
