use strict;
use warnings;

#pass in filename  has the debug handler.
my $filename = shift @ARGV;
my $fh;
my $file;

open $fh, '<', $filename or die($!);
{
    local $/ = undef;
    $file = <$fh>;
    close $fh;
}

#co
$file =~ s/entsp/#entsp/;
$file =~ s/entsp(.*?)#APP/#APP/msg;
#remove the ending junk.
$file =~ s/dret(.*)\retsp(.*)\n/dret\n/msg;

open $fh, '>', $filename or die($!);
print $fh $file;
close $fh;
