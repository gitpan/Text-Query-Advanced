package Text::Query::Advanced;
require 5.005;

$VERSION="0.02";

use strict;

# All class data is temporary
my ($parse_error,@tokens,$token);

sub new {
  my $class=shift;
  my $self={};
  bless $self,$class;
  return $self if !@_;
  return $self->prepare(@_);
}

sub prepare {
  my $self=shift;
  my $qstring=shift;
  my ($t,$t1);
  $self->{parseopts}={-regexp=>0,-litspace=>0,-case=>0,-whole=>0,-near=>10,@_};
  parse_tokens($self,$qstring);
  $parse_error=0;
  $t1=expression($self);
  $parse_error=1 if defined $token;
  return undef if $parse_error;
  $self->{matchexp}=$self->build_final_expression($t1);
  return $self;  
}

sub match {
  my $self=shift;
  my @ra;
  return (shift || $_)=~$self->{matchexp} if @_<=1 && ref($_[0]) ne 'ARRAY';
  my $pa=(@_==1 && ref($_[0]) eq 'ARRAY')?shift:\@_;
  if (ref($pa->[0]) eq 'ARRAY') {
    @ra=grep {$_->[0]=~$self->{matchexp}} @$pa;
  } else {
    @ra=grep {$_=~$self->{matchexp}} @$pa;
  }
  return wantarray?@ra:\@ra;
}

sub matchscalar {
  my $self=shift;
  return (shift || $_)=~$self->{matchexp};
}

#parsing routines

sub expression($) {
  my $rv;
  my $self=shift;
  my $t;
  $token=shift @tokens;
  $rv=conj($self);
  while (defined $token and $token=~/^(or|\|)$/i and !$parse_error) {
    $token=shift @tokens;
    $t=conj($self);
    $rv=$self->build_expression($rv,$t);
  }
  return $self->build_expression_finish($rv); 
}

sub conj($) {
  my $rv;
  my $self=shift;
  my $first=1;
  $rv=concat($self);
  while (defined $token and $token=~/^(and|&)$/i and !$parse_error) {
    $token=shift @tokens;
    $rv=$self->build_conj($rv,concat($self),$first);
    $first=0;
  }
  return $rv;
}

sub concat($) {
  my ($rv,$t,$l);
  my $self=shift;
  $rv=factor($self);
  while (defined $token and ($l=$token)=~/^\e|([(!]|not|near)$/i) {
    $token=shift @tokens if $l=~/^near$/i;
    $t=factor($self);
    if ($l=~/^near$/i) {
      $rv=$self->build_near($rv,$t);
    } else {
      $rv=$self->build_concat($rv,$t);
    }
  }
  return $rv;
}

sub factor($) {
  my ($rv,$t);
  my $self=shift;
  if (!defined ($t=$token)) {
    $parse_error=1;
  }
  elsif ($t eq '(') {
    $rv=expression($self);
    if (defined $token and $token eq ')' and not $parse_error) {
      $token=shift @tokens;
    } else {
      $parse_error=1;
    }
  }
  elsif ($t=~/^(not|!)$/i) {
    $token=shift @tokens;
    $rv=$self->build_negation(factor($self));
  }
  elsif ($t=~s/^\e//) {
    $rv=$self->build_literal($t);
    $token=shift @tokens;
  }
  else {
    $parse_error=1;
  }
  return $rv;
}

#lexical analyzer

sub parse_tokens {
  local($^W) = 0;
  my $self=shift;
  my($line) = @_;
  my($quote, $quoted, $unquoted, $delim, $word);

  @tokens=();
  while (length($line)) {
    ($quote, $quoted, undef, $unquoted, $delim, undef) =
      $line =~ m/^(["'])                 # a $quote
                ((?:\\.|(?!\1)[^\\])*)    # and $quoted text
                \1 		       # followed by the same quote
                ([\000-\377]*)	       # and the rest
	       |                       # --OR--
                ^((?:\\.|[^\\"'])*?)    # an $unquoted text
	        (\Z(?!\n)|(?:\s*([()|&!]|\b(?:and|or|not|near)\b)\s*)|(?!^)(?=["'])) # plus EOL, delimiter, or quote
                ([\000-\377]*)	       # the rest
	       /ix;		       # extended layout

    return() unless( $quote || length($unquoted) || length($delim));
    $line = $+;
    $unquoted=~s/^\s+//;
    $unquoted=~s/\s+$//;
    $word .= defined $quote ? $quoted : $unquoted;
    push(@tokens,"\e$word") if length ($word) and (length($delim) or !length($line));
    $delim=~s/^\s+//;
    $delim=~s/\s+$//;
    push(@tokens, $delim) if length $delim;
    undef $word if length $delim;
  }
}

#code-generation methods to produce regexps
#override to produce trees, etc.

sub build_final_expression {
  my ($self,$t1)=@_;
  my $t;
  $t=($self->{parseopts}{-case})?'':'(?i)';
  return qr/$t$t1/s;
}

sub build_expression {
  my ($self,$l,$r)=@_;
  #factor any common "^" out of the disjunction
  #This really speeds up matching
  if (substr($l,0,1) eq '^' and substr($r,0,1) eq '^') {
    return '^(?:'.substr($l,1).'|'.substr($r,1).')';
  } else {
    return "$l|$r";
  }
}

sub build_expression_finish {
  my ($self,$l)=@_;
  return "(?:$l)";
}

sub build_conj {
  my ($self,$l,$r,$first)=@_;
  $l="^(?=.*$l)" if $first;
  return "$l(?=.*$r)";
}

sub build_near {
  my ($self,$l,$r)=@_;
  my $t1;
  $t1=$self->{parseopts}{-near};
  return "(?:$l\\s*(?:\\S+\\s+){0,$t1}$r)|(?:$r\\s*(?:\\S+\\s+){0,$t1}$l)";
}

sub build_concat {
  my ($self,$l,$r)=@_;
  return "(?:$l\\s*$r)";
}

sub build_negation {
  my ($self,$t)=@_;
  return "(?:^(?:(?!$t).)*\$)";
}

sub build_literal {
  my ($self,$t)=@_;
  if (!$self->{parseopts}{-regexp}) {
    $t=quotemeta($t);
    $t=~s/\\\*/\\w*/g;
  }
  $t=~s/\\? +/\\s+/g if !$self->{parseopts}{-litspace};
  $t="\\b$t\\b" if $self->{parseopts}{-whole};
  $t="(?:$t)" if $self->{parseopts}{-regexp};
  return $t;
}

1;
__END__

=head1 NAME

Text::Query::Advanced - Match text against Boolean expression

=head1 SYNOPSIS

    use Text::Query::Advanced;
    
    # Constructor
    $query = Text::Query::Advanced->new([QSTRING] [OPTIONS]);

    # Methods
    $query->prepare(QSTRING [OPTIONS]);
    $query->match([TARGET]);
    $query->matchscalar([TARGET]);

    # Methods that can be overridden to produce custom query trees, etc.

    $query->build_final_expression(Q1);
    $query->build_expression(Q1,Q2);
    $query->build_expression_finish(Q1);
    $query->build_conj(Q1,Q2,F);
    $query->build_near(Q1,Q2);
    $query->build_concat(Q1,Q2);
    $query->build_negation(Q1);
    $query->build_literal(Q1);

=head1 DESCRIPTION

This module provides an object that matches a string or list of strings 
against a Boolean query expression similar to an AltaVista "advanced 
query".  Elements of the query expression may be regular expressions or 
literal text.

Query expressions are compiled into an internal form (currently, a regular 
expression making use of most of the tricks listed in Recipe 6.17 of _The 
Perl Cookbook_) when a new object is created or the C<prepare> method is 
called; they are not recompiled on each match.

The class provided by this module may be subclassed to produce query 
processors that match against input other than literal strings, e.g. 
indices.

Query expressions consist of literal strings (or regexps) joined by the 
following operators, in order of precedence from lowest to highest:

=over 4

=item OR, |

=item AND, &

=item NEAR

=item NOT, !

=back

Operator names are not case-sensitive.  Note that if you want to use a C<|> 
in a regexp, you need to backwhack it to keep it from being seen as a query 
operator.  Sub-expressions may be quoted in single or double quotes to 
match "and," "or," or "not" literally and may be grouped in parentheses 
(C<(, )>) to alter the precedence of evaluation.

A parenthesized sub-expression may also be concatenated with other sub- 
expressions to match sequences: C<(Perl or Python) interpreter> would match 
either "Perl interpreter" or "Python interpreter".  Concatenation has a 
precedence higher than NOT but lower than AND.  Juxtaposition of 
simple words has the highest precedence of all.


=head1 EXAMPLES

  use Text::Query::Advanced;
  my $q=new Text::Query::Advanced('hello and world');
  die "bad query expression" if not defined $q;
  print if $q->match;
  ...
  $q->prepare('goodbye or adios or ta ta',-litspace=>1,-case=>1);
  #requires single space between the two ta's
  if ($q->match($line)) {
  #doesn't match "Goodbye"
  ...
  $q->prepare('"and" or "or"');
  #quoting operators for literal match
  ...
  $q->prepare('\\bintegrate\\b',-regexp=>1);
  #won't match "disintegrated"

=head1 CONSTRUCTOR

=over 4

=item new ([QSTRING] [OPTIONS])

This is the constructor for a new Text::Query object.  If a C<QSTRING> is 
given it will be compiled to internal form.

C<OPTIONS> are passed in a hash like fashion, using key and value pairs.
Possible options are:

B<-case> - If true, do case-sensitive match.

B<-litspace> - If true, match spaces (except between operators) in 
C<QSTRING> literally.  If false, match spaces as C<\s+>.

B<-near> - Sets the number of words that can occur between two expressions 
and still satisfy the NEAR operator.  The default is 10.

B<-regexp> - If true, treat patterns in C<QSTRING> as regular expressions 
rather than literal text.

B<-whole> - If true, match whole words only, not substrings of words.

The constructor will return C<undef> if a C<QSTRING> was supplied and had 
illegal syntax.

=back

=head1 METHODS

=over 4

=item prepare (QSTRING [OPTIONS])

Compiles the query expression in C<QSTRING> to internal form and sets any 
options (same as in the constructor).  C<prepare> may be used to change 
the query expression and options for an existing query object.  If 
C<OPTIONS> are omitted, any options set by a previous call to the 
constructor or C<prepare> remain in effect.

This method returns a reference to the query object if the syntax of the 
expression was legal, or C<undef> if not.

=item match ([TARGET])

If C<TARGET> is a scalar, C<match> returns a true value if the string 
specified by C<TARGET> matches the query object's query expression.  If 
C<TARGET> is not given, the match is made against C<$_>.

If C<TARGET> is an array, C<match> returns a (possibly empty) list of all 
matching elements.  If the elements of the array are references to sub- 
arrays, the match is done against the first element of each sub-array.  
This allows arbitrary information (e.g. filenames) to be associated with 
each string to match. 

If C<TARGET> is a reference to an array, C<match> returns a reference to 
a (possibly empty) list of all matching elements.  

=item matchscalar ([TARGET])

Behaves just like C<MATCH> when C<TARGET> is a scalar or is not given.  
Slightly faster than C<MATCH> under these circumstances.

=back

=head1 CODE-GENERATION METHODS

The following methods are used to generate regexps based on query elements.  
They may be overridden to generate other forms of matching code, such as 
trees to be used by a custom version of C<match> that evaluates index lists 
or the like.

All these methods return a scalar corresponding to the code that performs 
the specified options.  As supplied, they return regexp strings, but 
overridden methods could return objects, array references, etc.  

Parameters Q1 and Q2 are the same type of scalar as the return values.

=over 4

=item build_final_expression(Q1)

Does any final processing to generate code to match a top-level expression.  
As supplied, optionally adds case-insensitivity code and then uses C<qr//> 
to compile the regexp.  The return value will be stored in the object's 
C<matchexp> field.  It is NOT necessarily of a type that can be passed to 
the other code-generation methods.

=item build_expression(Q1,Q2)

Generate code to match C<Q1> OR C<Q2>

=item build_expression_finish(Q1)

Generate any code needed to enclose an expression.  As supplied, encloses 
the generated regexp in non-capturing parentheses.

=item build_conj(Q1,Q2,F)

Generate code needed to match C<Q1> AND C<Q2>.  F will be true if this is the first 
time this method is called in a sequence of several conjunctions (the 
supplied method uses this to factor a common C<^> out of the generated sub- 
expressions, which greatly speeds up matching).
 
=item build_near(Q1,Q2)

Generate code needed to match C<Q1> NEAR C<Q2>.

=item build_concat(Q1,Q2)

Generate code needed to match C<Q1> immediately followed by C<Q2>.

=item build_negation(Q1)

Generate code needed to match NOT C<Q1>.

=item build_literal(Q1)

Generate code to match C<Q1> as a literal.

=back

=head1 AUTHOR

Eric Bohlman (ebohlman@netcom.com)

The parse_tokens routine was adapted from the parse_line routine in 
Text::Parsewords.

=head1 COPYRIGHT

Copyright (c) 1998 Eric Bohlman. All rights reserved.
This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.
=cut
