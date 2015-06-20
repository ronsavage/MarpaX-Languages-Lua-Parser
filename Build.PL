use strict;
use warnings;

use Module::Build;

require 5.014002; # For the utf8 stuff.

# -----------------------------------------------

my $class = Module::Build->subclass(code => <<'EOF');

sub ACTION_authortest
{
	my($self) = @_;

	$self->depends_on('build');
	$self->depends_on('manifest');
	$self->depends_on('distmeta');

	$self->test_files( qw< t xt/author > );
	$self->recursive_test_files(1);

	$self->depends_on('test');

	return;
}
EOF

$class -> new
(
	module_name    => 'MarpaX::Languages::Lua::Parser',
	license        => 'artistic_2',
	dist_abstract  => 'A Lua parser',
	dist_author    => 'Ron Savage <ron@savage.net.au>',
	build_requires =>
	{
		'Test::More' => 1.001014,
		'Test::Pod'  => 1.48,
	},
	configure_requires =>
	{
		'Module::Build' => 0.4211,
	},
	meta_merge =>
	{
		resources =>
		{
			'repository' => 'https://github.com/ronsavage/MarpaX-Languages-Lua-Parser',
		},
	},
	requires =>
	{
		'Data::Section::Simple' => 0.07,
		'File::Spec' => 3.40,
		'File::Temp' => 0.2304,
		'Getopt::Long' => 2.41,
		'Log::Handler' => 0.84,
		'Marpa::R2' => 3.000000,
		'Moo' => 2.000001,
		'open' => 0,
		'Path::Tiny' => 0.068,
		'perl' => 0,
		'Pod::Usage' => 1.63,
		'strict' => 0,
		'Types::Standard' => 1.000005,
		'warnings' => 0,
	},
	resources =>
	{
		'bugtracker' => 'https://rt.cpan.org/Public/Dist/Display.html?Name=MarpaX-Languages-Lua-Parser',
		'license' => 'http://www.perlfoundation.org/artistic_license_2_0',
	},
) -> create_build_script();
