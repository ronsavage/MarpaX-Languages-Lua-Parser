#!/usr/bin/env perl

use strict;
use warnings;
use warnings qw(FATAL utf8); # Fatalize encoding glitches.

use Getopt::Long;

use MarpaX::Languages::Lua::Parser;

use Pod::Usage;

# -----------------------------------------------

my($option_parser) = Getopt::Long::Parser -> new();

my(%option);

if ($option_parser -> getoptions
(
	\%option,
	'help',
	'input_file_name=s',
	'maxlevel=s',
	'minlevel=s',
	'output_file_name=s',
) )
{
	pod2usage(1) if ($option{'help'});

	exit MarpaX::Languages::Lua::Parser -> new(%option) -> run;
}
else
{
	pod2usage(2);
}

__END__

=pod

=head1 NAME

parse.file.pl - Parsing any Lua source code file

=head1 SYNOPSIS

parser.file.pl [options]

	Options:
	-help
	-input_file_name aLuaFileName
	-maxlevel aString
	-minlevel aString
	-output_file_name aCSVFileName

All switches can be reduced to a single letter.

Exit value: 0.

=head1 OPTIONS

=over 4

=item o -help

Print help and exit.

=item o -input_file_name aLuaFileName

The name of an Lua file to process.

See data/*.svg for some samples.

This option is mandatory.

Default: ''.

=item o -maxlevel logOption1

This option affects Log::Handler.

See the Log::handler docs.

Default: 'notice'.

=item o -minlevel logOption2

This option affects Log::Handler.

See the Log::handler docs.

Default: 'error'.

No lower levels are used.

=item o -output_file_name aCSVFileName

The name of a CSV file to write, of parsed tokens.

By default, nothing is written.

Default: ''.

=back

=cut
