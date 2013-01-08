#############################################################################data
# Paul Nathan
# MS Thesis at U of Idaho 2010
# 

#This script parses the XCORE simulator scripts and emits the
#instructions in a JSON format that will be standardized between this
#and the device that reads from the tracer from the XCORE

use 5.010;			# A Modern Perl
use strict;
use warnings;
use JSON;			# A modern data-exchange format in ASCII, wee.


my $fh;
die("Need to have a trace passed in") unless $ARGV[0] ;
open $fh, "<", $ARGV[0] or die("$ARGV[0]: $!");
my $counter = 0;
my %datastore;
while(<$fh>)
{    
    my $simline = $_;
    chomp $simline;
    #say $simline;
    if($simline =~ /DBG_INT/)
    {
	#print $simline;
	next;
    }
    if($simline =~ /TRAP ET/)
    {
	#print $simline;
	next;
    }
    if($simline =~ /Unhandled exception/)
    {
	print $simline;
	next;
    }
    
    $simline =~ /(-|\.)
([A-F0-9a-f]{8})
\s*
\(
 (.+?)
\)
\s*:\s*
(\w*?)
\s+
(.*?)\s+
\@\d+?
/msx;

    #print "addr: $2 \t";               # Instruction addr in memory
    #print "func: $3\t";    		# instruction name
    #print "inst: $4\t";                # instruction code
    #print "Regs: $5\t";                # registers
    #say "";
my @regs = split(/,/, $5);
@regs = map { $_ =~ s/^\s*(.*?)\s*$/$1/; $_;} @regs;
$counter++;
$datastore{$counter}{$2}{$4} = \@regs;

}
close $fh;

my $json = JSON->new->allow_nonref;

#my $json_text   = $json->encode( \%datastore );

#print $json_text;
print $json->pretty->encode( \%datastore ); # pretty-printing
