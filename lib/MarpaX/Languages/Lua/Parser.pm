package MarpaX::Languages::Lua::Parser;

use strict;
use warnings;
use warnings qw(FATAL utf8); # Fatalize encoding glitches.
use open     qw(:std :utf8); # Undeclared streams in UTF-8.

use Data::Section::Simple 'get_data_section';

use Log::Handler;

use Marpa::R2;

use Moo;

use Path::Tiny; # For path().

use Types::Standard qw/Any ArrayRef Str/;

has grammar =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has input_file_name =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Str,
	required => 1,
);

has input_text =>
(
	default  => sub{return []},
	is       => 'rw',
	isa      => ArrayRef,
	required => 1,
);

has logger =>
(
	default  => sub{return undef},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has maxlevel =>
(
	default  => sub{return 'notice'},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has minlevel =>
(
	default  => sub{return 'error'},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has output_file_name =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Str,
	required => 1,
);

has recce =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

our $VERSION = '1.00';

# ------------------------------------------------

sub BUILD
{
	my($self) = @_;

	if (! defined $self -> logger)
	{
		$self -> logger(Log::Handler -> new);
		$self -> logger -> add
		(
			screen =>
			{
				maxlevel       => $self -> maxlevel,
				message_layout => '%m',
				minlevel       => $self -> minlevel,
				utf8           => 1,
			}
		);
	}

	$self -> input_text([path($self -> input_file_name) -> lines_utf8]);
	$self -> grammar
	(
		Marpa::R2::Scanless::G -> new({source => \get_data_section('Lua.bnf')})
	);
	$self -> recce
	(
		Marpa::R2::Scanless::R -> new
		({
			grammar => $self -> grammar,
		})
	);


} # End of BUILD.

# ------------------------------------------------

sub decode_result
{
	my($self, $result) = @_;
	my(@worklist) = $result;

	my($obj);
	my($ref_type);
	my(@stack);

	do
	{
		$obj      = shift @worklist;
		$ref_type = ref $obj;

		if ($ref_type eq 'ARRAY')
		{
			for (@$obj)
			{
				unshift @worklist, (! defined($_) ? '' : $_);
			}
		}
		elsif ($ref_type eq 'HASH')
		{
			# Hopefully, we can get away with not checking for undef here,
			# because we're outputting to @stack, not @worklist.

			push @stack, {%$obj};
		}
		elsif ($ref_type eq 'REF')
		{
			$obj = $$obj;
			$obj = '' if (! defined($obj) );

			unshift @worklist, $obj;
		}
		elsif ($ref_type)
		{
			die "Unsupported object type $ref_type\n";
		}
		else
		{
			# See comment above about not checking for undef.

			push @stack, $obj;
		}

	} while (@worklist);

	return [@stack];

} # End of decode_result.

# --------------------------------------------------

sub log
{
	my($self, $level, $s) = @_;
	$level = 'notice' if (! defined $level);
	$s     = ''       if (! defined $s);

	$self -> logger -> $level($s) if ($self -> logger);

} # End of log.

# ------------------------------------------------

sub process
{
	my($self)         = @_;
	my($input)        = join('', @{$self -> input_text});
	my($input_ref)    = \$input;
	my($input_length) = length $input;
	my($pos)          = $self -> recce -> read($input_ref);

	READ: while (1)
	{
		EVENT:
		for my $event (@{$self -> recce -> events})
		{
			my($name) = @{$event};

			if ($name eq 'multiline string' )
			{
				my($start, $length)    = $self -> recce -> pause_span;
				my($string_terminator) = $self -> recce -> literal($start, $length);
				$string_terminator     =~ tr/\[/\]/;
				my($terminator_pos)    = index($$input_ref, $string_terminator, $start);

				die "Died looking for $string_terminator. \n" if ($terminator_pos < 0);

				# The string terminator has the same length as the start of string marker.

				my($string_length) = $terminator_pos + $length - $start;

				$self -> recce -> lexeme_read('multiline string', $start, $string_length);

				$pos = $terminator_pos + $length;

				next EVENT;
			}

			if ($name eq 'multiline comment')
			{
				# This is a discard event.

				my(undef, $start, $end) = @{$event};
				my($length)             = $end - $start;
				my($comment_terminator) = $self -> recce -> literal($start, $length);
				$comment_terminator     =~ tr/-//;
				$comment_terminator     =~ tr/\[/\]/;
				my($terminator_pos)     = index( $$input_ref, $comment_terminator, $start);

				die "Died looking for $comment_terminator. \n" if ($terminator_pos < 0);

				# Don't read anything into G1 -- just throw the comment away.

				$pos = $terminator_pos + $length;

				next EVENT;
			}

			if ($name eq 'singleline comment')
			{
				# This is a discard event.

				my(undef, $start, $end) = @{$event};
				my($length)             = $end-$start;
				pos($$input_ref)        = $end - 1;
				$$input_ref             =~ /[\r\n]/gxms;
				my($new_pos)            = pos($$input_ref);

				die "Died looking for singleline comment terminator. \n" if (! defined $new_pos);

				$pos = $new_pos;

				next EVENT;
			}

			die "Unexpected event '$name'\n";

		}

		last READ if ($pos >= $input_length);

		$pos = $self -> recce -> resume($pos);
	}

	# Warning: Don't use if (my($ambiguous_status) = $self -> recce -> ambiguous),
	# since then the 'if' always returns true.

	if (my $ambiguous_status = $self -> recce -> ambiguous)
	{
		die "The Lua source is ambiguous: $ambiguous_status. \n";
	}

	return $self -> recce -> value;

} # End of process.

# ------------------------------------------------

sub run
{
	my($self, %args)      = @_;
	my($value)            = $self -> process($args{input_file_name} || $self -> input_file_name);
	$value                = $self -> decode_result($value);
	my($output_file_name) = $self -> output_file_name;

	path($output_file_name) -> spew_utf8(map{"$_\n"} @$value) if ($output_file_name);

	# Return 0 for success and 1 for failure.

	return 0;

} # End of run.

#-------------------------------------------------

1;

=pod

=head1 NAME

C<MarpaX::Languages::Lua::Parser> - A Lua source code parser

=head1 Synopsis

	#!/usr/bin/env perl

	use strict;
	use warnings;

	use MarpaX::Languages::SVG::Parser;

	# ---------------------------------

	my(%option) =
	(
		input_file_name => 'data/ellipse.01.svg',
	);
	my($parser) = MarpaX::Languages::Lua::Parser -> new(%option);
	my($result) = $parser -> run;

	die "Parse failed\n" if ($result == 1);

This script ships as scripts/synopsis.pl. Run it as:

	shell> perl -Ilib scripts/synopsis.pl

See also scripts/parse.file.pl for code which takes command line parameters. For help, run:

	shell> perl -Ilib scripts/parse.file.pl -h

=head1 Description

C<MarpaX::Languages::Lua::Parser> parse Lua source code files.

=head1 Installation

Install C<MarpaX::Languages::SVG::Parser> as you would for any C<Perl> module:

Run:

	cpanm MarpaX::Languages::SVG::Parser

or run:

	sudo cpan MarpaX::Languages::SVG::Parser

or unpack the distro, and then either:

	perl Build.PL
	./Build
	./Build test
	sudo ./Build install

or:

	perl Makefile.PL
	make (or dmake or nmake)
	make test
	make install

=head1 Constructor and Initialization

C<new()> is called as C<< my($parser) = MarpaX::Languages::SVG::Parser -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<MarpaX::Languages::SVG::Parser>.

Key-value pairs accepted in the parameter list (see also the corresponding methods
[e.g. L</input_file_name([$string])>]):

=over 4

=item o input_file_name => $string

The names the input file to be parsed.

This option is mandatory.

Default: ''.

=item o logger => aLog::HandlerObject

By default, an object of type L<Log::Handler> is created which prints to STDOUT,
but given the default setting (maxlevel => 'info'), nothing is actually printed.

See C<maxlevel> and C<minlevel> below.

Set C<logger> to '' (the empty string) to stop a logger being created.

Default: undef.

=item o maxlevel => logOption1

This option affects L<Log::Handler> objects.

See the L<Log::Handler::Levels> docs.

Since the L</report()> method is always called and outputs at log level C<info>, the first of these produces no output,
whereas the second lists all the parse results. The third adds a tiny bit to the output.

	shell> perl -Ilib scripts/parse.file.pl -i data/ellipse.01.svg
	shell> perl -Ilib scripts/parse.file.pl -i data/ellipse.01.svg -max info
	shell> perl -Ilib scripts/parse.file.pl -i data/ellipse.01.svg -max debug

The extra output produced by C<debug> includes the input file name and the string which L<Marpa::R2> is trying to parse.
This helps debug the BNFs themselves.

Default: 'notice'.

=item o minlevel => logOption2

This option affects L<Log::Handler> object.

See the L<Log::Handler::Levels> docs.

Default: 'error'.

No lower levels are used.

=item o output_file_name => $string

The names the CSV file to be written.

If not set, nothing is written.

Default: ''.

=back

=head1 Methods

=head2 input_file_name([$string])

Here, the [] indicate an optional parameter.

Get or set the name of the file to parse.

Note: C<input_file_name> is a parameter to new().

=head2 log($level, $s)

Calls $self -> logger -> log($level => $s) if ($self -> logger).

=head2 logger([$log_object])

Here, the [] indicate an optional parameter.

Get or set the log object.

C<$log_object> must be a L<Log::Handler>-compatible object.

To disable logging, just set logger to the empty string.

Note: C<logger> is a parameter to new().

=head2 maxlevel([$string])

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if an object of type L<Log::Handler> is created. See L<Log::Handler::Levels>.

Note: C<maxlevel> is a parameter to new().

=head2 minlevel([$string])

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if an object of type L<Log::Handler> is created. See L<Log::Handler::Levels>.

Note: C<minlevel> is a parameter to new().

=head2 new()

This method is auto-generated by L<Moo>.

=head2 run(%args)

The method which does all the work.

C<%args> is a hash with this optional (key => value) pair:

=over 4

=item o encoding => $x

If not specified, it falls back to calling $self -> encoding().

=back

Returns 0 for a successful parse and 1 for failure.

The code dies if L<Marpa::R2> itself can't parse the given string.

=head1 FAQ

=head2 Why did you store Lua's BNF in a __DATA__ section?

This avoids problems with single- and double-quotes in the BNF, and the allegedly unknown escape
sequences \v etc there too.

=head1 Machine-Readable Change Log

The file Changes was converted into Changelog.ini by L<Module::Metadata::Changes>.

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

=head1 Repository

L<https://github.com/ronsavage/MarpaX-Languages-SVG-Parser>

=head1 Support

Email the author, or log a bug on RT:

L<https://rt.cpan.org/Public/Dist/Display.html?Name=MarpaX::Languages::SVG::Parser>.

=head1 Credits

=head1 Author

L<MarpaX::Languages::SVG::Parser> was written by Ron Savage I<E<lt>ron@savage.net.auE<gt>> in 2015.

Home page: L<http://savage.net.au/>.

=head1 Copyright

Australian copyright (c) 2015, Ron Savage.

	All Programs of mine are 'OSI Certified Open Source Software';
	you can redistribute them and/or modify them under the terms of
	The Artistic License 2.0, a copy of which is available at:
	http://www.opensource.org/licenses/index.html

=cut

__DATA__

@@ Lua.bnf

:default ::= action => [name,values]

lexeme default = latm => 1 action => [name,values]

# I(JK)  attempt to follow the order of the Lua grammar in
# section 8 of the Lua 5.1 reference manual.
#
# Names which begin with "Lua" are taken directly from
# the Lua reference manual grammar.

<chunk> ::= <stat list> <optional laststat>
<stat list> ::= <stat item>*
<stat item> ::= <stat> ';'
<stat item> ::= <stat>
<optional laststat> ::= <laststat> ';'
<optional laststat> ::= <laststat>
<optional laststat> ::=

<block> ::= <chunk>

<stat> ::= <varlist> '=' <explist>

<stat> ::= <functioncall>

<stat> ::= <keyword do> <block> <keyword end>

<stat> ::= <keyword while> <exp> <keyword do> <block> <keyword end>

<stat> ::= <keyword repeat> <block> <keyword until> <exp>

<stat> ::= <keyword if> <exp> <keyword then> <block>
    <elseif sequence> <optional else block> <keyword end>
<elseif sequence> ::= <elseif sequence> <elseif block>
<elseif sequence> ::=
<elseif block> ::= <keyword elseif> <exp> <keyword then> <block>
<optional else block> ::= <keyword else> <block>
<optional else block> ::=

<stat> ::= <keyword for> <Name> '=' <exp> ',' <exp> ',' <exp>
    <keyword do> <block> <keyword end>
<stat> ::= <keyword for> <Name> '=' <exp> ',' <exp> <keyword do> <block> <keyword end>

<stat> ::= <keyword for> <namelist> <keyword in> <explist> <keyword do> <block> <keyword end>

<stat> ::= <keyword function> <funcname> <funcbody>

<stat> ::= <keyword local> <keyword function> <Name> <funcbody>

<stat> ::= <keyword local> <namelist> <optional namelist initialization>

<optional namelist initialization> ::=
<optional namelist initialization> ::= '=' <explist>

<laststat> ::= <keyword return> <optional explist>
<laststat> ::= <keyword break>

<optional explist> ::=
<optional explist> ::= <explist>

<funcname> ::= <dotted name> <optional colon name element>
<dotted name> ::= <Name>+ separator => [.] proper => 1
<optional colon name element> ::=
<optional colon name element> ::= ':' <Name>

<varlist> ::= <var>+ separator => [,] proper => 1

<var> ::= <Name>
<var> ::= <prefixexp> '[' <exp> ']'
<var> ::= <prefixexp> '.' <Name>

<namelist> ::= <Name>+ separator => [,] proper => 1

<explist> ::= <exp>+ separator => [,] proper => 1

<exp> ::=
       <var>
     | '(' <exp> ')' assoc => group
    || <exp> <args> assoc => right
    || <exp> ':' <Name> <args> assoc => right
     | <keyword nil>
     | <keyword false>
     | <keyword true>
     | <Number>
     | <String>
     | '...'
     | <tableconstructor>
     | <function>
    || <exp> '^' <exp> assoc => right
    || <keyword not> <exp>
     | '#' <exp>
     | '-' <exp>
    || <exp> '*' <exp>
     | <exp> '/' <exp>
     | <exp> '%' <exp>
    || <exp> '+' <exp>
     | <exp> '-' <exp>
    || <exp> '..' <exp> assoc => right
    || <exp> '<' <exp>
     | <exp> '<=' <exp>
     | <exp> '>' <exp>
     | <exp> '>=' <exp>
     | <exp> '==' <exp>
     | <exp> '~=' <exp>
    || <exp> <keyword and> <exp>
    || <exp> <keyword or> <exp>

<prefixexp> ::= <var>
<prefixexp> ::= <functioncall>
<prefixexp> ::= '(' <exp> ')'

<functioncall> ::= <prefixexp> <args>
<functioncall> ::= <prefixexp> ':' <Name> <args>

<args> ::= '(' <optional explist> ')'
<args> ::= <tableconstructor>
<args> ::= <String>

<function> ::= <keyword function> <funcbody>

<funcbody> ::= '(' <optional parlist> ')' <block> <keyword end>

<optional parlist> ::= <namelist>
<optional parlist> ::= <namelist> ',' '...'
<optional parlist> ::= '...'
<optional parlist> ::=

# A lone comma is not allowed in an empty fieldlist,
# apparently. This is why I use a dedicated rule
# for an empty table and a '+' sequence,
# instead of a '*' sequence.

<tableconstructor> ::= '{' '}'
<tableconstructor> ::= '{' <fieldlist> '}'
<fieldlist> ::= <field>+ separator => [,;]

<field> ::= '[' <exp> ']' '=' <exp>
<field> ::= <Name> '=' <exp>
<field> ::= <exp>

:lexeme ~ <keyword and> priority => 1
<keyword and> ~ 'and'
:lexeme ~ <keyword break> priority => 1
<keyword break> ~ 'break'
:lexeme ~ <keyword do> priority => 1
<keyword do> ~ 'do'
:lexeme ~ <keyword else> priority => 1
<keyword else> ~ 'else'
:lexeme ~ <keyword elseif> priority => 1
<keyword elseif> ~ 'elseif'
:lexeme ~ <keyword end> priority => 1
<keyword end> ~ 'end'
:lexeme ~ <keyword false> priority => 1
<keyword false> ~ 'false'
:lexeme ~ <keyword for> priority => 1
<keyword for> ~ 'for'
:lexeme ~ <keyword function> priority => 1
<keyword function> ~ 'function'
:lexeme ~ <keyword if> priority => 1
<keyword if> ~ 'if'
:lexeme ~ <keyword in> priority => 1
<keyword in> ~ 'in'
:lexeme ~ <keyword local> priority => 1
<keyword local> ~ 'local'
:lexeme ~ <keyword nil> priority => 1
<keyword nil> ~ 'nil'
:lexeme ~ <keyword not> priority => 1
<keyword not> ~ 'not'
:lexeme ~ <keyword or> priority => 1
<keyword or> ~ 'or'
:lexeme ~ <keyword repeat> priority => 1
<keyword repeat> ~ 'repeat'
:lexeme ~ <keyword return> priority => 1
<keyword return> ~ 'return'
:lexeme ~ <keyword then> priority => 1
<keyword then> ~ 'then'
:lexeme ~ <keyword true> priority => 1
<keyword true> ~ 'true'
:lexeme ~ <keyword until> priority => 1
<keyword until> ~ 'until'
:lexeme ~ <keyword while> priority => 1
<keyword while> ~ 'while'

# multiline comments are discarded.  The lexer only looks for
# their beginning, and uses an event to throw away the rest
# of the comment

:discard ~ <singleline comment> event => 'singleline comment'
<singleline comment> ~ <singleline comment start>
<singleline comment start> ~ '--'

:discard ~ <multiline comment> event => 'multiline comment'
<multiline comment> ~ '--[' <optional equal signs> '['

<optional equal signs> ~ [=]*

:discard ~ whitespace

# Lua whitespace is locale dependant and so
# is Perl's, hopefully in the same way.
# Anyway, it will be close enough for the moment.

whitespace ~ [\s]+

# Good practice is to *not* use locale extensions for identifiers,
# and we enforce that, so all letters must be a-z or A-Z

<Name> ~ <identifier start char> <optional identifier chars>
<identifier start char> ~ [a-zA-Z_]
<optional identifier chars> ~ <identifier char>*
<identifier char> ~ [a-zA-Z0-9_]

<String> ::= <single quoted string>
<single quoted string> ~ ['] <optional single quoted chars> [']
<optional single quoted chars> ~ <single quoted char>*

# anything other than vertical space or a single quote

<single quoted char> ~ [^\v'\x5c] # Extra ' for syntax hiliter in UltraEdit (uex).
<single quoted char> ~ '\' [\d\D] # Also an escaped char. Another ' for uex.

<String> ::= <double quoted string>
<double quoted string> ~ ["] <optional double quoted chars> ["]
<optional double quoted chars> ~ <double quoted char>*

# anything other than vertical space or a double quote

<double quoted char> ~ [^\v"\x5c] # Extra " for uex.
<double quoted char> ~ '\' [\d\D] # also an escaped char. Another ' for uex.

<String> ::= <multiline string>
:lexeme ~ <multiline string> pause => before event => 'multiline string'
<multiline string> ~ '[' <optional equal signs> '['

<Number> ~ <hex number>
<Number> ~ <C90 strtod decimal>
<Number> ~ <C90 strtol hex>

<hex number> ~ '0x' <hex digit> <hex digit>
<hex digit> ~ [0-9a-fA-F]

# Numeric representation in Lua is also not an
# exact science -- it is farmed out to the
# implementation's strtod() (for decimal)
# or strtoul() (for hex, if strtod failed).
# This is an attempt at the C90-conformant subset.

<C90 strtod decimal> ~ <optional sign> <decimal digits> <optional exponent>
<C90 strtod decimal> ~ <optional sign> <decimal digits> '.' <optional exponent>
<C90 strtod decimal> ~ <optional sign> '.' <decimal digits> <optional exponent>
<C90 strtod decimal> ~
    <optional sign> <decimal digits> '.' <decimal digits> <optional exponent>
<optional exponent> ~
<optional exponent> ~ [eE] <optional sign> <decimal digits>
<optional sign> ~
<optional sign> ~ [-+]
<C90 strtol hex> ~ [0] [xX] <hex digits>
<decimal digits> ~ [0-9]+
<hex digits> ~ [a-fA-F0-9]+
