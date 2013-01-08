#snips out the assembly that is generated
use warnings;
use strict;
use 5.010;

open my $fh, '<', $ARGV[0] or die($!);
local $/ = undef;
my $contents = <$fh>;
close $fh;

open $fh, '>', $ARGV[0].'asm_snip';
#only get the middle
$contents =~ s/(.*?#SS START)(.*?)(#SS STOP(.*))/$2/msg;

print $fh $contents;
close $fh;
