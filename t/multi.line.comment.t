#!/usr/bin/env perl

use strict;
use warnings;

use File::Spec;
use File::Temp;

use Test::More;

BEGIN {use_ok('MarpaX::Languages::Lua::Parser'); }

my($lua_sources_dir)	= 'lua.sources';
$lua_sources_dir		= qq{$lua_sources_dir} unless $ENV{HARNESS_ACTIVE};
my($parser)				= MarpaX::Languages::Lua::Parser -> new
(
    input_file_name	=> "$lua_sources_dir/multi.line.comment.lua",
    logger			=> '',
);

eval { $parser -> run };

like $@, qr/Died looking for multiline comment terminator/, 'Malformed multiline comment';

done_testing;
