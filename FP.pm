#!/usr/bin/env perl
package Language::FP;
use Parse::RecDescent;
use Regexp::Common;

require Exporter;
@EXPORT = qw/fp_eval/;
@EXPORT_OK = qw/perl2fp fp2perl bottom BOTTOM/;
%EXPORT_TAGS = (':all' => [@EXPORT_OK, @EXPORT]);
@ISA = qw(Exporter);

$VERSION = 0.02;

sub BOTTOM () {			# the universal bad value
    if ($::DEBUG =~ /b/) {
	use Carp 'confess';
	confess("Bottom!");
    }
    undef
}

sub bottom {			# check for bottom
    (@_ > 0 && !defined($_[0])) ? 1 : 0
}

sub numeric {			# check for 2 integer args
    return ($_[0] =~ /$RE{num}{real}/o) && ($_[1] =~ /$RE{num}{real}/o);
}

######################################################################
## Parser

sub info {			# pretty debugging output.
    my ($pack, $fn, $line, $subr) = caller 1;
    $subr =~ s/^.*:://;
    print STDERR "[$subr] ", @_, "\n";
}

sub Dparse {			# parse-time debugging output
    goto &info if $::DEBUG =~ /p/;
}

sub Drun {			# run-time debugging.
    goto &info if $::DEBUG =~ /r/;
}

# XXX: this shouldn't be needed.  It makes <X> behave the same as X
# when passed as an argument list.
sub as_array($) {
    my $a = shift;
    if (ref $a eq 'ARRAY') {
	@$a;
    } else {
	$a;
    }
}

# XXX: this is the disgusting inverse of as_array
sub to_array {
    if (@_ == 1) {
	return shift;
    } else {
	return [@_];
    }
}

sub call_it {			# look up a function
    my $f = shift;
    Drun "Calling $f (@_)";
    $f->(@_);
}

sub findsym {		# look up a function
    my ($sym, $type) = @_;
    return $sym if ref $sym eq $type;
    return *{__PACKAGE__.'::'.$sym}{$type} || *{pkg().'::'.$sym}{$type}
	|| undef;
}

sub do_bu {			# bu (i.e. currying)
    my ($f, $o) = @_;
    Dparse "using $f($o, ...)";
    return sub {
	no strict 'refs';
	Drun "bu $f ($o, @_)";
	call_it($f, $o, @_);
    };
}

sub compose {			# '.' operator
    my @funcs = @_;
    Dparse "using (@funcs)";
    return sub {
	no strict 'refs';
	Drun "compose (@funcs)";
	foreach (reverse @funcs) {
	    @_ = call_it($_, @_);
	}
	@_[0..$#_];
    };
}

sub distribute {		# '[...]' list-of-functions
    my @xs = @_;
    Dparse "using (@xs)";
    return sub {
	no strict 'refs';
	Drun "distribute (@xs) : (@_)";
	map { to_array call_it $_, @_ } @xs;
    }
}

sub ifelse {			# 'a -> b ; c' construct
    my ($if, $then, $else) = @_;
    Dparse "if $if then $then else $else";
    return sub {
	# XXX: having to call this in array context sucks, but is necessary.
	Drun "if $if then $then else $else";
	if ((call_it $if, @_)[0]) {
	    call_it $then, @_;
	} else {
	    call_it $else, @_;
	}
    };
}

sub awhile {			# 'while x y'
    my ($while, $do) = @_;
    Dparse "while ($while) $do";
    return sub {
	Drun "while ($while) $do -> (@_)";
	while ((call_it $while, @_)[0]) {
	    @_ = call_it $do, @_;
	}
	@_;
    }
}

sub forall {			# '@' operator, i.e. map
    my $f = shift;
    Dparse "using $f";
    return sub {
	no strict 'refs';
	Drun "forall $f (@_)";
	map { to_array call_it $f, as_array $_ } @_;
    };
}

sub insert {			# '/' operator, i.e. reduce
    my $f = shift;
    Dparse "using $f";
    return sub {
	no strict 'refs';
	Drun "insert $f (@_)";
	return () unless @_;
	my $r = $_[0];
	foreach (@_[1..$#_]) {
	    $r = (call_it $f, $r, $_)[0];
	}
	$r;
    }
}

sub constant {			# constant '`' operator
    my $x = shift;
    Dparse $x;
    return sub {
	Drun "constant $x";
	as_array $x;
    };
}

my %ops;
sub opfunc {			# symbol table for binary operators
    use Carp 'confess';
    confess "$_[0]" unless exists $ops{$_[0]};
    return $ops{$_[0]};
}

local $::fp_caller = 'Language::FP';
sub pkg {			# package in which to bind functions
    $::fp_caller;
}

my $P;				# the parser
sub make_parser {
    $P = new Parse::RecDescent <<'EOG' or die "Can't create parser!";

{
	use Regexp::Common;
	BEGIN {
		no strict 'refs';
		foreach my $sym (qw|as_array Dparse Drun opfunc BOTTOM compose
				    awhile forall ifelse do_bu distribute
				    insert pkg to_array constant call_it
				    findsym|) {
			*{$sym} = *{'Language::FP::'.$sym};
		}
	}
}


thing:	  'val' <commit> id_undef '=' application {
		no strict 'refs';
		@{pkg().'::'.$item{id_undef}} = as_array $item{application};
		Dparse "Defined variable $item{id}";
		$return = 'ok';
	}

	| 'def' <commit> id_undef '=' termlist {
		no strict 'refs';
		*{pkg().'::'.$item{id_undef}} = $item{termlist};
		Dparse "Defined function $item{id}";
		$return = 'ok';
	}
	| application
		{ Dparse "Successful application $item[1]"; $return = $item[1] }
	| <error>

application: termlist ':' <commit> data {
		no strict 'refs';
		Dparse "application of $item[1]";
		my @a = as_array $item{data};
		$return = [ call_it $item[1], @a ];
	}
	| data
		{ $return = $item{data} }

termlist: 'while' <commit> complist termlist
		{ $return = awhile $item{complist}, $item{termlist} }
 	| complist '->' <commit> complist ';' termlist
 		{ $return = ifelse @item[1,4,6] }
	| complist 
		{ $return = $item[1] }
	| <error>

complist: <rightop: func '.' func>
		{ $return = compose @{$item[1]} }

func:	  'bu' <commit> func data
		{ $return = do_bu @item{'func', 'data'} }
	| '/' func
		{ $return = insert $item{func} }
	| '@' <commit> func
		{ $return = forall $item{func};	}
	| '(' <commit> termlist ')'
		{ $return = $item{termlist} }
	| '[' <commit> <rightop: termlist ',' termlist> ']'
		{ $return = distribute @{$item[3]} }
	| '`' <commit> data
		{ $return = constant $item{data} }
	| sfunc
 		{ $return = $item[1] }
	| id
		{ $return = $item[1] }
	| <error>

data:	  '<' <commit> data(s?) '>'
		{ $return = $item[3]; }
	| /$RE{num}{real}/o
		{ $return = $item[1] }
 	| /$RE{num}{int}/o
		{ $return = $item[1] }
	| /$RE{quoted}/o
		{ $return = substr($item[1], 1, length($item[1]) - 2) }
	| m{[a-rt-zA-Z_][\w\d]*}
	  <error?: Undefined variable "$item[1]"> <commit> {
		no strict 'refs';
		$return = findsym($item[1], 'ARRAY') || undef;
	}
	| <error>

sfunc:	  /\d+/ {
		my $x = $item[1];
 		$return = sub { $_[$x - 1] };
	}

id_undef:  m{[a-zA-Z_][\w\d]*}
		{ $return = $item[1] }

id:	  m{[a-zA-Z_][\w\d]*}
	  <error?: Undefined function "$item[1]"> <commit>
		{ $return = findsym($item[1], 'CODE') }
	| m{([!<>=]=) | [+*/<>-] | ([gln]e) | ([gl]t) | eq}x
	  <error?: Undefined operator "$item[1]">
		{ $return = opfunc($item[1]) }

EOG
}

######################################################################
## Builtin functions:

# FP is supposed to be "bottom-preserving".  In other words, once a
# single operaation fails, it taints all results that depend on it.
# The only way to recover from this is to explicitly recognize the
# "bottom" condition using the bottom() test.

sub import {
    Language::FP->export_to_level(1, @_);

    # XXX: maybe consider autoloading these on demand.

    # Build op-functions.
    my %make_ops = (
## List ops #####
# first/last element of list
hd 	=> '@_ ? $_[0] : BOTTOM',
hdr 	=> '@_ ? $_[-1] : BOTTOM',
# rest of list
tl 	=> '@_ ? @_[1..$#_] : BOTTOM',
tlr 	=> '@_ ? @_[0..$#_ - 1] : BOTTOM',
len 	=> 'return BOTTOM if bottom @_; scalar @_',
'reverse' => 'reverse @_',
# append
apndl 	=> '($_[0], @{$_[1]})',
apndr 	=> '(@{$_[0]}, $_[1])',
# Rotate
rotl 	=> '@_ ? @_[1..$#_,0]  : ()',
rotr 	=> '@_ ? @_[$#_, 0..$#_ - 1] : ()',
# Catenate
cat 	=> 'map { as_array $_ } @_',
## Logical ops #####
'and' 	=> '$_[0] &&  $_[1]',
'or' 	=> '$_[0] || $_[1]',
'not' 	=> '!$_[0]',
## Other ops #####
id 	=> '@_',
out 	=> 'print STDERR perl2fp(@_), "\n"; @_',
iota 	=> '1 .. $_[0]',
atom 	=> '@_ == 1 && ref($_[0]) eq "SCALAR"',
null 	=> '@_ == 0',
## "shaping" list-ops #####
distl 	=> q{
    my ($a, $b) = @_;
    return BOTTOM unless !bottom($a) && ref $b eq 'ARRAY';
    map { [$a, $_] } @$b;
},

distr 	=> q{
    my ($a, $b) = @_;
    return BOTTOM unless !bottom($b) && ref $a eq 'ARRAY';
    map { [$_, $b] } @$a;
},

trans 	=> q{
    my @ret;
    return () unless @_;
    my $len = scalar @{$_[0]};
    foreach (@_[1..$#_]) {
	return BOTTOM unless ref $_ eq 'ARRAY' && @$_ == $len;
    }
    for (my $i = 0; $i < $len; $i++) {
	push @ret, [ map { $_->[$i] } @_ ];
    }
    @ret;
},
);

    while (my ($f, $b) = each %make_ops) {
	*{$f} = eval qq{ sub { return BOTTOM if bottom(\@_); $b }};
	die "$f: $@" if $@;
    }

    # Build binary operator functions.
    foreach my $f (qw|+ - * / ** == != < > <= >=|) {
	$ops{$f} = eval qq{sub {
 			return BOTTOM unless numeric(\@_);
			\$_[0] $f \$_[1]
		}
		} || die $@;
    }
    make_parser;
    1;
}

######################################################################
## Exportables:
sub perl2fp {
    my @ret;
    foreach (@_) {
	if (ref eq 'ARRAY') {
	    push @ret, '<'.perl2fp(@$_).'>';
	} elsif (ref) {
	    die "Expecting ARRAY, got ".ref;
	} elsif (/$RE{num}{int}/o || /$RE{num}{real}/o) {
	    push @ret, $_;
	} elsif (defined) {
	    push @ret, qq{"$_"};
	} else {
	    push @ret, '_|_';
	}
    }
    join(' ', @ret);
}

sub fp2perl {
    my $str = shift;
    return to_array($P->data($str));
}

sub fp_eval {
    local $::fp_caller = caller;
    if (@_ == 1) {
	return $P->thing(shift);
    }

    my %o = @_;
    my $in = $o{in} || 'STDIN';
    my $out = $o{out} || 'STDOUT';
    while (<$in>) {
	chomp;
	my $res = $P->thing($_);
	unless ($res) {
	    warn;
	    next;
	}
	print $out perl2fp($res), "\n";
    }
}

1;

__END__

=head1 NAME

Language::FP -- think like Jonh Backus wants you to

=head1 SYNOPSIS

  use Language::FP qw/perl2fp/;

  # Sum of the first 12 integers:
  my $sum = fp_eval '/+ . iota:12'
  print perl2fp($result);
  # prints '<78>'

  # Matrix-vector product:
  fp_eval 'def Ax = @(+ . @* . trans) . distr';
  my @mv = ([[1, 2], [3, 4]], [5, 6]);
  print perl2fp(fp_eval('Ax:' . perl2fp(@mv)));
  # prints '<17 39>'

  # Cross-language calls:
  print join ', ', Ax(@mv);
  # prints '17, 39'

  sub cubes { map { $_ ** 3 } @_ }
  print perl2fp(fp_eval 'cubes:<1 2 3>');
  # prints '<1 8 27>'

  fp_eval in => \*INPUT, out => \*OUTPUT;

=head1 DESCRIPTION

C<Language::FP> is an implementation of John Backus' FP language, a
purely functional language remarkable for its lack of named variables
-- only functions have names.  Note that this is B<not> a deliberately
obfuscated language -- it was designed for actual users (probably
mathematicians).  Since Perl's $calars, @rrays and %ashes advertise
themselves so boldly, I thought programming in a language whose author
thought that named variables led only to confusion and error would be
eye-opening.  I now know why every language since has had named
variables.

While at some point I should probably include a brief FP tutorial, for
the moment please see http://www.cse.sc.edu/~bays/FPlink for more
information on the language's history and basic functions.  There are
a number of subtle syntactic variants of FP described and implemented
on the web.  This unfortunate state of affairs is due at least in part
to the original language's use of non-ASCII characters.  This package
uses a hybrid chosen to be somewhat: (1) legible, (2) faithful to the
original, and (3) predictable to those familiar with Perl.

=head2 Functions

The following functions are useful in evaluating FP expressions and
handling FP data.

=over

=item C<$str = perl2fp @array>

Convert a Perl list-of-lists (LoL) to a string represeting it in FP.

=item C<@array = fp2perl $str>

Convert an FP value to a Perl LoL.

=item C<fp_eval in =E<gt> \*IFH, out =E<gt> \*OFH>

Evaluate the contents of C<IFH> (C<STDIN> by default), writing the
results to C<OFH> (C<STDOUT> by default).

=item C<$result = fp_eval $string>

Evaluate the FP expression C<$string>, returning the result as a Perl
scalar or reference to a LoL.

=back

In addition, all FP builtin functions (B<not> combining forms) may be
called as Perl functions in list context.  For example, to use
C<distl> in Perl, one could write

  my @result = Language::FP::distl $x, @ys

=head2 Debugging

You will experience unexpected behavior when programming in FP.  Some
of it may even be your fault.  When this occurs, setting the global
variable C<$::DEBUG> to a string containing one or more of the
following characters can help:

=over

=item 'p' -- Trace parsing

=item 'r' -- Trace execution

=item 'b' -- Make FP errors ("bottom") fatal

=back

=head1 EXPORTS

C<Language::FP> exports the C<fp_eval> function by default, for
command-line convenience.

=head1 TODO

Documentation -- a lot more needs to be explained a lot better.

Testing -- only lightly tested, though it can handle totient.

Maybe make it more "OO" -- not that important.

=head1 BUGS

While calling user-defined Perl functions from FP works as expected,
it is currently not possible to call Perl builtins.

Argument context is a mess in places.

=head1 AUTHOR

Sean O'Rourke, E<lt>seano@cpan.orgE<gt>

Bug reports welcome, patches even more welcome.

=head1 COPYRIGHT

Copyright (C) 2002 Sean O'Rourke.  All rights reserved, some wrongs
reversed.  This module is distributed under the same terms as Perl
itself.  Let me know if you actually find it useful.

=head1 APPENDIX

For further study, here is an implementation of Euler's totient
function, which computes the number of co-primes less than its
argument.  This may be the longest FP program ever written.

  def totient = /+ . @((== . [1, `1] -> `1 ; `0) .
 	(while (> . [2, `0]) (< -> reverse ; id) . [2, -]))
 	. distl . [id, iota]

=cut

# one-liner version of the above, for cut-and-paste:
# def totient = /+ . @((== . [1, `1] -> `1 ; `0) . (while (> . [2, `0]) (< -> reverse ; id) . [2, -])) . distl . [id, iota]
