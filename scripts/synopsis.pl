#!/usr/bin/env perl

use strict;
use warnings;

use MarpaX::Languages::Lua::Parser;

# ---------------------------------

my(%option) =
(
	input_file_name => 'lua.sources/bisect.lua',
	maxlevel        =>'debug',
);
my($parser) = MarpaX::Languages::Lua::Parser -> new(%option);
my($result) = $parser -> run;

die "Parse failed\n" if ($result == 1);

for my $item (@{$parser -> items -> print})
{
	print sprintf "%-16s  %-16s  %s\n", $$item{type}, $$item{name}, $$item{value};
}
